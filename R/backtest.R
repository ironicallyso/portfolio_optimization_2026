# === BACKTEST FUNCTIONS ===
# Two-level hierarchical rebalancing engine.
#   Level 1 (inter-cluster) : calendar trigger (triple-witching Mondays)
#                             and/or proportional drift threshold
#   Level 2 (intra-cluster) : resets only when inter-cluster fires
#
# Within-cluster weights: inverse vol via inv_vol_weights() from R/weights.R
# Proportional threshold: fires if |current_wt - target_wt| / target_wt > prop_threshold
#
# Dependencies: R/weights.R, R/data.R

library(tidyverse)
library(PerformanceAnalytics)
library(xts)
library(lubridate)

# ── HELPER: TRIPLE WITCHING MONDAYS ──────────────────────────────────────────
# Triple witching = third Friday of March, June, September, December.
# Rebalance trigger = the Monday following that Friday.
# If that Monday is not a trading day (holiday), rolls to the next trading day.

get_triple_witching_mondays <- function(trading_dates) {
  trading_dates <- as.Date(trading_dates)
  years         <- unique(year(trading_dates))
  tw_months     <- c(3L, 6L, 9L, 12L)

  third_fridays <- map_dfr(years, function(y) {
    map_dfr(tw_months, function(m) {
      first_of_month <- as.Date(paste(y, m, "01", sep = "-"))
      last_of_month  <- first_of_month + days(days_in_month(first_of_month) - 1)
      month_days     <- seq(first_of_month, last_of_month, by = "day")
      fridays        <- month_days[weekdays(month_days) == "Friday"]
      if (length(fridays) < 3L) return(NULL)
      tibble(tw_friday = fridays[3L])
    })
  })

  third_fridays %>%
    mutate(target_monday = tw_friday + days(3L)) %>%
    rowwise() %>%
    mutate(
      rebal_date = {
        candidates <- trading_dates[trading_dates >= target_monday]
        if (length(candidates) == 0L) NA_character_ else as.character(candidates[1L])
      }
    ) %>%
    ungroup() %>%
    filter(!is.na(rebal_date)) %>%
    mutate(rebal_date = as.Date(rebal_date)) %>%
    pull(rebal_date)
}

# ── HELPER: PROPORTIONAL THRESHOLD CHECK ─────────────────────────────────────
# Returns TRUE if any cluster has drifted more than prop_threshold * its target weight.

prop_threshold_triggered <- function(current_cluster_wts, target_cluster_wts, prop_threshold) {
  if (is.null(prop_threshold) || is.na(prop_threshold)) return(FALSE)
  relative_drift <- abs(current_cluster_wts - target_cluster_wts) / target_cluster_wts
  any(relative_drift > prop_threshold, na.rm = TRUE)
}

# ── CORE: TWO-LEVEL BACKTEST ──────────────────────────────────────────────────
# Arguments:
#   returns_wide         : aligned wide returns data frame (date + symbol columns)
#   cluster_assignments  : tibble with columns symbol, cluster, cluster_label
#   target_cluster_wts   : named numeric vector of between-cluster target weights
#   use_calendar         : if TRUE, triple-witching Mondays always trigger rebalance
#   prop_threshold       : proportional drift threshold (e.g. 0.20 = 20%); NULL = off
#   threshold_check_freq : "daily" = check every trading day;
#                          "monthly" = check only on triple-witching Mondays
#   vol_window           : years of trailing history for within-cluster vol recompute
#   rf_annual            : annual risk-free rate
#
# Returns: list(stats, daily_values, daily_cluster_weights, rebal_log)

run_two_level_backtest <- function(
    returns_wide,
    cluster_assignments,
    target_cluster_wts,
    use_calendar         = TRUE,
    prop_threshold       = NULL,
    threshold_check_freq = c("monthly", "daily"),
    vol_window           = 3,
    rf_annual            = 0.033
) {
  threshold_check_freq <- match.arg(threshold_check_freq)

  if (!use_calendar && is.null(prop_threshold)) {
    stop("Must enable at least one trigger: use_calendar = TRUE or a prop_threshold")
  }

  rf_daily <- (1 + rf_annual)^(1/252) - 1
  syms     <- cluster_assignments$symbol

  missing <- setdiff(syms, colnames(returns_wide))
  if (length(missing) > 0) stop("Missing symbols in returns_wide: ", paste(missing, collapse = ", "))

  ret_df  <- returns_wide %>% select(date, all_of(syms))
  dates   <- ret_df$date
  ret_mat <- as.matrix(ret_df %>% select(-date))
  n_days  <- length(dates)

  tw_mondays <- get_triple_witching_mondays(dates)

  threshold_check_dates <- switch(threshold_check_freq,
    daily   = dates,
    monthly = tw_mondays
  )

  # ── INITIALIZE ──────────────────────────────────────────────────────────────
  # Use all available history before backtest start for initial within-cluster weights.
  # Falls back to first vol_window years of backtest data if pre-history is thin.

  init_slice <- returns_wide %>% filter(date < dates[1])
  if (nrow(init_slice) < 30) {
    warning("< 30 days pre-backtest history; using first ", vol_window,
            " years of backtest for initial within-cluster weights (mild lookahead)")
    init_slice <- returns_wide %>%
      filter(date >= dates[1], date <= dates[1] + years(vol_window))
  }

  # inv_vol_weights() returns a tibble; deframe() to named vector for internal use
  current_within_wts <- inv_vol_weights(
    init_slice %>% select(date, all_of(syms)),
    cluster_assignments
  ) %>%
    select(symbol, within_wt) %>%
    deframe()

  # Initial asset values: target_cluster_wt * within_cluster_wt, portfolio = $1
  asset_values <- map_dbl(syms, function(s) {
    cl <- cluster_assignments$cluster_label[cluster_assignments$symbol == s]
    target_cluster_wts[cl] * current_within_wts[s]
  })
  names(asset_values) <- syms

  # ── STORAGE ─────────────────────────────────────────────────────────────────

  daily_values          <- numeric(n_days)
  daily_cluster_weights <- matrix(
    NA_real_, nrow = n_days, ncol = length(target_cluster_wts),
    dimnames = list(NULL, names(target_cluster_wts))
  )
  rebal_log <- tibble(
    date          = as.Date(character()),
    trigger       = character(),
    max_rel_drift = numeric(),
    n_rebal       = integer()
  )
  n_rebal <- 0L

  # ── SIMULATION LOOP ─────────────────────────────────────────────────────────

  for (i in seq_len(n_days)) {
    day         <- dates[i]
    day_returns <- ret_mat[i, ]
    day_returns[is.na(day_returns)] <- 0

    asset_values    <- asset_values * (1 + day_returns)
    portfolio_value <- sum(asset_values)

    current_cluster_wts <- map_dbl(names(target_cluster_wts), function(cl) {
      cl_syms <- cluster_assignments$symbol[cluster_assignments$cluster_label == cl]
      sum(asset_values[cl_syms]) / portfolio_value
    })
    names(current_cluster_wts) <- names(target_cluster_wts)

    daily_values[i]            <- portfolio_value
    daily_cluster_weights[i, ] <- current_cluster_wts

    # ── REBALANCE DECISION ───────────────────────────────────────────────────
    if (i < n_days) {
      on_calendar   <- use_calendar && (day %in% tw_mondays)
      on_check_date <- day %in% threshold_check_dates
      over_thresh   <- on_check_date &&
        prop_threshold_triggered(current_cluster_wts, target_cluster_wts, prop_threshold)

      if (on_calendar || over_thresh) {
        n_rebal <- n_rebal + 1L

        trigger <- case_when(
          on_calendar & over_thresh ~ "both",
          on_calendar               ~ "calendar",
          TRUE                      ~ "threshold"
        )

        rel_drifts    <- abs(current_cluster_wts - target_cluster_wts) / target_cluster_wts
        max_rel_drift <- max(rel_drifts, na.rm = TRUE)

        rebal_log <- bind_rows(rebal_log, tibble(
          date          = day,
          trigger       = trigger,
          max_rel_drift = round(max_rel_drift, 4),
          n_rebal       = n_rebal
        ))

        # Recompute within-cluster weights from trailing vol window
        window_start <- day - years(vol_window)
        vol_slice    <- returns_wide %>%
          filter(date >= window_start, date <= day) %>%
          select(date, all_of(syms))

        if (nrow(vol_slice) >= 60) {
          current_within_wts <- inv_vol_weights(vol_slice, cluster_assignments) %>%
            select(symbol, within_wt) %>%
            deframe()
        }

        # Reset asset values to target (inter + intra simultaneously)
        asset_values <- map_dbl(syms, function(s) {
          cl       <- cluster_assignments$cluster_label[cluster_assignments$symbol == s]
          cl_value <- portfolio_value * target_cluster_wts[cl]
          cl_value * current_within_wts[s]
        })
        names(asset_values) <- syms
      }
    }
  }

  # ── PERFORMANCE STATS ───────────────────────────────────────────────────────

  port_returns   <- c(0, diff(daily_values) / daily_values[-n_days])
  excess_returns <- port_returns - rf_daily
  port_xts       <- xts(port_returns, order.by = dates)
  max_dd         <- as.numeric(maxDrawdown(port_xts))
  ann_ret        <- prod(1 + port_returns)^(252 / n_days) - 1

  stats <- tibble(
    use_calendar         = use_calendar,
    prop_threshold       = ifelse(is.null(prop_threshold), NA_real_, prop_threshold),
    threshold_check_freq = if (is.null(prop_threshold)) NA_character_ else threshold_check_freq,
    ann_return           = round(ann_ret, 4),
    ann_vol              = round(sd(port_returns) * sqrt(252), 4),
    sharpe               = round(mean(excess_returns) / sd(excess_returns) * sqrt(252), 4),
    max_drawdown         = round(max_dd, 4),
    calmar               = round(ann_ret / max_dd, 4),
    n_rebalances         = n_rebal
  )

  list(
    stats                 = stats,
    daily_values          = tibble(date = dates, value = daily_values),
    daily_cluster_weights = as_tibble(daily_cluster_weights) %>% mutate(date = dates),
    rebal_log             = rebal_log
  )
}
