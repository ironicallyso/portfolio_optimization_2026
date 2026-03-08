# === TWO-LEVEL REBALANCING ENGINE ===
# Implements a hierarchical rebalancing framework:
#   Level 1 (inter-cluster) : calendar trigger and/or proportional drift threshold
#   Level 2 (intra-cluster) : resets only when inter-cluster fires
#
# Calendar cadence: Monday after triple-witching Friday (Mar/Jun/Sep/Dec)
# Threshold check cadence: configurable — daily or same triple-witching-Monday schedule
#
# Sweep matrix:
#   1. Calendar-only    — fires on every triple-witching Monday, no threshold
#   2. Threshold-only, daily check    — checks drift every trading day
#   3. Threshold-only, monthly check  — checks drift on triple-witching Mondays only
#   4. Calendar + Threshold           — OR logic, monthly check (same dates as calendar)
#
# Proportional threshold: fires if |current_wt - target_wt| / target_wt > prop_threshold
# Within-cluster weights: inverse vol, recomputed on each rebalance from trailing window
# Intra-cluster has NO independent trigger — only resets as consequence of inter-cluster
#
# Assumes:
#   - portfolio_data/ CSVs already exist (from portfolio_optimization.Rmd)
#   - DBC used as commodity proxy; KRBN excluded

# install.packages(c("tidyverse", "lubridate", "PerformanceAnalytics"))
library(tidyverse)
library(lubridate)
library(PerformanceAnalytics)

# ── CONFIG ────────────────────────────────────────────────────────────────────

data_dir   <- "portfolio_data"
rf_annual  <- 0.033
rf_daily   <- (1 + rf_annual)^(1/252) - 1
vol_window <- 3   # years of trailing history for within-cluster vol computation

cluster_assignments <- tribble(
  ~symbol, ~cluster, ~cluster_label,
  "BND",   1,        "Fixed_Income",
  "TIP",   1,        "Fixed_Income",
  "VGIT",  1,        "Fixed_Income",
  "IGF",   2,        "Real_Assets",
  "IYR",   2,        "Real_Assets",
  "EEM",   3,        "Equities",
  "EES",   3,        "Equities",
  "GRID",  3,        "Equities",
  "SPY",   3,        "Equities",
  "DBC",   4,        "Commodities"
)

target_cluster_wts <- c(
  Fixed_Income = 0.25,
  Real_Assets  = 0.20,
  Equities     = 0.45,
  Commodities  = 0.10
)
stopifnot(abs(sum(target_cluster_wts) - 1) < 1e-9)

symbols <- cluster_assignments$symbol

# ── LOAD DATA ─────────────────────────────────────────────────────────────────

portfolio_data <- map(symbols, function(sym) {
  path <- file.path(data_dir, paste0(sym, ".csv"))
  if (!file.exists(path)) stop("Missing file: ", path)
  read_csv(path, show_col_types = FALSE) %>%
    select(date, adj_close) %>%
    arrange(date)
})
names(portfolio_data) <- symbols

returns_wide <- map(symbols, function(sym) {
  portfolio_data[[sym]] %>%
    mutate(return = adj_close / lag(adj_close) - 1) %>%
    filter(!is.na(return)) %>%
    select(date, return) %>%
    rename(!!sym := return)
}) %>%
  reduce(inner_join, by = "date") %>%
  arrange(date)

message("Aligned history: ", min(returns_wide$date), " to ", max(returns_wide$date))
message("Trading days:    ", nrow(returns_wide))

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

# ── HELPER: WITHIN-CLUSTER WEIGHTS ───────────────────────────────────────────
# Inverse-vol weights. Returns named vector (symbol -> within-cluster weight).
# Weights sum to 1 within each cluster independently.

compute_within_weights <- function(returns_slice, cluster_assignments) {
  vol_df <- returns_slice %>%
    pivot_longer(-date, names_to = "symbol", values_to = "return") %>%
    group_by(symbol) %>%
    summarise(ann_vol = sd(return, na.rm = TRUE) * sqrt(252), .groups = "drop")
  
  cluster_assignments %>%
    left_join(vol_df, by = "symbol") %>%
    group_by(cluster) %>%
    mutate(within_wt = (1 / ann_vol) / sum(1 / ann_vol)) %>%
    ungroup() %>%
    select(symbol, within_wt) %>%
    deframe()
}

# ── HELPER: PROPORTIONAL THRESHOLD CHECK ─────────────────────────────────────
# Returns TRUE if any cluster has drifted more than prop_threshold * its target weight.

prop_threshold_triggered <- function(current_cluster_wts, target_cluster_wts, prop_threshold) {
  if (is.null(prop_threshold) || is.na(prop_threshold)) return(FALSE)
  relative_drift <- abs(current_cluster_wts - target_cluster_wts) / target_cluster_wts
  any(relative_drift > prop_threshold, na.rm = TRUE)
}

# ── CORE: TWO-LEVEL BACKTEST FUNCTION ────────────────────────────────────────
# Key parameters:
#   use_calendar        : if TRUE, triple-witching Mondays always trigger a rebalance
#   prop_threshold      : proportional drift threshold; NULL = no threshold trigger
#   threshold_check_freq: "daily" = check threshold every trading day
#                         "monthly" = check threshold only on triple-witching Mondays
#
# Note: when use_calendar = TRUE, threshold_check_freq = "monthly" means both
# triggers share the same look dates (OR logic on those dates only).
#
# Returns: list(stats, daily_values, daily_cluster_weights, rebal_log)

run_two_level_backtest <- function(
    returns_wide,
    cluster_assignments,
    target_cluster_wts,
    use_calendar          = TRUE,
    prop_threshold        = NULL,
    threshold_check_freq  = c("monthly", "daily"),
    vol_window            = 3,
    rf_annual             = 0.033
) {
  threshold_check_freq <- match.arg(threshold_check_freq)
  
  if (!use_calendar && is.null(prop_threshold)) {
    stop("Must enable at least one trigger: use_calendar=TRUE or a prop_threshold")
  }
  
  rf_daily <- (1 + rf_annual)^(1/252) - 1
  syms     <- cluster_assignments$symbol
  
  missing <- setdiff(syms, colnames(returns_wide))
  if (length(missing) > 0) stop("Missing symbols: ", paste(missing, collapse = ", "))
  
  ret_df  <- returns_wide %>% select(date, all_of(syms))
  dates   <- ret_df$date
  ret_mat <- as.matrix(ret_df %>% select(-date))
  n_days  <- length(dates)
  
  # Triple-witching Mondays — used for both calendar trigger and monthly threshold check
  tw_mondays <- get_triple_witching_mondays(dates)
  
  # On which days do we evaluate the threshold?
  threshold_check_dates <- switch(threshold_check_freq,
                                  daily   = dates,           # every trading day
                                  monthly = tw_mondays       # same cadence as calendar
  )
  
  # ── INITIALIZE ──────────────────────────────────────────────────────────────
  
  init_slice <- returns_wide %>% filter(date < dates[1])
  if (nrow(init_slice) < 30) {
    warning("< 30 days pre-backtest history; using first ", vol_window,
            " years of backtest for initial within-cluster weights (mild lookahead)")
    init_slice <- returns_wide %>%
      filter(date >= dates[1], date <= dates[1] + years(vol_window))
  }
  current_within_wts <- compute_within_weights(
    init_slice %>% select(date, all_of(syms)),
    cluster_assignments
  )
  
  # Initial asset values: target_cluster_wt * within_cluster_wt, portfolio = $1
  asset_values <- map_dbl(syms, function(s) {
    cl <- cluster_assignments$cluster_label[cluster_assignments$symbol == s]
    target_cluster_wts[cl] * current_within_wts[s]
  })
  names(asset_values) <- syms
  
  # ── STORAGE ─────────────────────────────────────────────────────────────────
  
  daily_values          <- numeric(n_days)
  daily_cluster_weights <- matrix(NA_real_, nrow = n_days,
                                  ncol = length(target_cluster_wts),
                                  dimnames = list(NULL, names(target_cluster_wts)))
  rebal_log <- tibble(date = as.Date(character()), trigger = character(),
                      max_rel_drift = numeric(), n_rebal = integer())
  n_rebal   <- 0L
  
  # ── SIMULATION LOOP ─────────────────────────────────────────────────────────
  
  for (i in seq_len(n_days)) {
    day         <- dates[i]
    day_returns <- ret_mat[i, ]
    day_returns[is.na(day_returns)] <- 0
    
    asset_values    <- asset_values * (1 + day_returns)
    portfolio_value <- sum(asset_values)
    
    # Derive current cluster weights from asset values
    current_cluster_wts <- map_dbl(names(target_cluster_wts), function(cl) {
      cl_syms <- cluster_assignments$symbol[cluster_assignments$cluster_label == cl]
      sum(asset_values[cl_syms]) / portfolio_value
    })
    names(current_cluster_wts) <- names(target_cluster_wts)
    
    daily_values[i]            <- portfolio_value
    daily_cluster_weights[i, ] <- current_cluster_wts
    
    # ── REBALANCE DECISION ───────────────────────────────────────────────────
    if (i < n_days) {
      on_calendar <- use_calendar && (day %in% tw_mondays)
      
      # Only evaluate threshold on permitted check dates
      on_check_date <- day %in% threshold_check_dates
      over_thresh   <- on_check_date &&
        prop_threshold_triggered(current_cluster_wts,
                                 target_cluster_wts,
                                 prop_threshold)
      
      if (on_calendar || over_thresh) {
        n_rebal <- n_rebal + 1L
        
        trigger <- case_when(
          on_calendar & over_thresh ~ "both",
          on_calendar               ~ "calendar",
          TRUE                      ~ "threshold"
        )
        
        rel_drifts    <- abs(current_cluster_wts - target_cluster_wts) / target_cluster_wts
        max_rel_drift <- max(rel_drifts, na.rm = TRUE)
        
        rebal_log <- bind_rows(rebal_log,
                               tibble(date = day, trigger = trigger,
                                      max_rel_drift = round(max_rel_drift, 4),
                                      n_rebal = n_rebal))
        
        # Recompute within-cluster weights from trailing vol window
        window_start <- day - years(vol_window)
        vol_slice    <- returns_wide %>%
          filter(date >= window_start, date <= day) %>%
          select(date, all_of(syms))
        
        if (nrow(vol_slice) >= 60) {
          current_within_wts <- compute_within_weights(vol_slice, cluster_assignments)
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

# ── SWEEP MATRIX ──────────────────────────────────────────────────────────────
# Four methodology families x threshold range where applicable:
#
#   1. Calendar-only         (1 config)
#   2. Threshold-only, daily check    (5 thresholds)
#   3. Threshold-only, monthly check  (5 thresholds)
#   4. Calendar + Threshold, monthly  (5 thresholds)

thresholds <- c(0.10, 0.15, 0.20, 0.25, 0.30)
thresh_labels <- scales::percent(thresholds, accuracy = 1)

configs <- bind_rows(
  # 1. Calendar-only
  tibble(
    label                = "Cal only",
    use_calendar         = TRUE,
    prop_threshold       = list(NULL),
    threshold_check_freq = "monthly"   # irrelevant when no threshold, kept for consistency
  ),
  # 2. Threshold-only, daily check
  tibble(
    label                = paste0("Thresh-daily ",   thresh_labels),
    use_calendar         = FALSE,
    prop_threshold       = as.list(thresholds),
    threshold_check_freq = "daily"
  ),
  # 3. Threshold-only, monthly check (triple-witching Mondays)
  tibble(
    label                = paste0("Thresh-monthly ", thresh_labels),
    use_calendar         = FALSE,
    prop_threshold       = as.list(thresholds),
    threshold_check_freq = "monthly"
  ),
  # 4. Calendar + Threshold (monthly check, OR logic)
  tibble(
    label                = paste0("Cal+T ",          thresh_labels),
    use_calendar         = TRUE,
    prop_threshold       = as.list(thresholds),
    threshold_check_freq = "monthly"
  )
)

message("\nRunning ", nrow(configs), " configs...")

all_results <- pmap(configs, function(label, use_calendar, prop_threshold, threshold_check_freq) {
  message("  -> ", label)
  result <- run_two_level_backtest(
    returns_wide         = returns_wide,
    cluster_assignments  = cluster_assignments,
    target_cluster_wts   = target_cluster_wts,
    use_calendar         = use_calendar,
    prop_threshold       = prop_threshold,
    threshold_check_freq = threshold_check_freq,
    vol_window           = vol_window,
    rf_annual            = rf_annual
  )
  result$stats <- result$stats %>% mutate(label = label)
  result$label <- label
  result
})
names(all_results) <- configs$label

# ── RESULTS TABLE ─────────────────────────────────────────────────────────────

results_df <- map_dfr(all_results, ~.x$stats) %>%
  mutate(
    methodology = case_when(
      use_calendar & is.na(prop_threshold)                    ~ "1. Calendar only",
      !use_calendar & threshold_check_freq == "daily"         ~ "2. Threshold-only (daily check)",
      !use_calendar & threshold_check_freq == "monthly"       ~ "3. Threshold-only (monthly check)",
      use_calendar  & threshold_check_freq == "monthly"       ~ "4. Calendar + Threshold"
    )
  ) %>%
  select(methodology, label, prop_threshold, threshold_check_freq,
         ann_return, ann_vol, sharpe, max_drawdown, calmar, n_rebalances)

cat("\n── Full Sweep Results ───────────────────────────────────────────────\n")
results_df %>%
  mutate(
    prop_threshold = ifelse(is.na(prop_threshold), "—",
                            scales::percent(prop_threshold, accuracy = 1)),
    ann_return   = scales::percent(ann_return,   accuracy = 0.1),
    ann_vol      = scales::percent(ann_vol,       accuracy = 0.1),
    max_drawdown = scales::percent(max_drawdown,  accuracy = 0.1),
    sharpe       = round(sharpe, 3),
    calmar       = round(calmar, 3)
  ) %>%
  arrange(methodology, prop_threshold) %>%
  print(n = Inf)

# ── BEST PER METHODOLOGY ──────────────────────────────────────────────────────

cat("\n── Best Config per Methodology (by Sharpe) ──────────────────────────\n")
results_df %>%
  group_by(methodology) %>%
  slice_max(sharpe, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    prop_threshold = ifelse(is.na(prop_threshold), "—",
                            scales::percent(prop_threshold, accuracy = 1)),
    ann_return   = scales::percent(ann_return,   accuracy = 0.1),
    ann_vol      = scales::percent(ann_vol,       accuracy = 0.1),
    max_drawdown = scales::percent(max_drawdown,  accuracy = 0.1),
    sharpe       = round(sharpe, 3),
    calmar       = round(calmar, 3)
  ) %>%
  select(methodology, label, prop_threshold, ann_return, ann_vol,
         sharpe, max_drawdown, calmar, n_rebalances) %>%
  print()

# ── REBALANCE TRIGGER BREAKDOWN ───────────────────────────────────────────────

cat("\n── Rebalance Trigger Breakdown ──────────────────────────────────────\n")
walk(all_results, function(r) {
  cat("\n", r$label, ":\n", sep = "")
  if (nrow(r$rebal_log) == 0) {
    cat("  (no rebalances triggered)\n")
  } else {
    print(table(r$rebal_log$trigger))
  }
})

# ── EQUITY CURVES — PANEL BY METHODOLOGY ─────────────────────────────────────

equity_df <- map_dfr(all_results, function(r) {
  r$daily_values %>%
    mutate(
      label       = r$label,
      methodology = results_df$methodology[results_df$label == r$label]
    )
})

equity_df %>%
  ggplot(aes(x = date, y = value, color = label)) +
  geom_line(linewidth = 0.55, alpha = 0.85) +
  facet_wrap(~methodology, ncol = 2) +
  labs(
    title    = "Two-Level Rebalancing: Equity Curves by Methodology",
    subtitle = "Calendar = Monday after triple-witching Friday | FI=25%, EQ=45%, RA=20%, Comm=10%",
    x        = NULL, y = "Growth of $1", color = NULL
  ) +
  theme_minimal() +
  theme(legend.position  = "right",
        legend.text      = element_text(size = 7),
        strip.text       = element_text(face = "bold"))

# ── THRESHOLD SENSITIVITY: DAILY vs MONTHLY CHECK ────────────────────────────
# Side-by-side Sharpe comparison across check frequencies, threshold-only configs

results_df %>%
  filter(grepl("Threshold-only", methodology)) %>%
  mutate(
    check_freq = ifelse(threshold_check_freq == "daily", "Daily check", "Monthly check"),
    thresh_pct = scales::percent(prop_threshold, accuracy = 1),
    thresh_pct = factor(thresh_pct, levels = scales::percent(sort(thresholds), accuracy = 1))
  ) %>%
  ggplot(aes(x = thresh_pct, y = sharpe, color = check_freq, group = check_freq)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Daily check" = "firebrick", "Monthly check" = "steelblue")) +
  labs(
    title    = "Threshold-Only: Sharpe by Threshold Level and Check Frequency",
    subtitle = "Daily check catches drift immediately; monthly check tolerates intra-period spikes",
    x        = "Proportional Threshold",
    y        = "Sharpe Ratio",
    color    = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# ── KEY METRICS PANEL: ALL METHODOLOGIES ─────────────────────────────────────

results_df %>%
  mutate(label = factor(label, levels = configs$label)) %>%
  pivot_longer(cols = c(sharpe, calmar, n_rebalances),
               names_to = "metric", values_to = "value") %>%
  mutate(
    metric = recode(metric,
                    "sharpe"       = "Sharpe Ratio",
                    "calmar"       = "Calmar Ratio",
                    "n_rebalances" = "# Rebalances"
    ),
    methodology = factor(methodology, levels = c(
      "1. Calendar only",
      "2. Threshold-only (daily check)",
      "3. Threshold-only (monthly check)",
      "4. Calendar + Threshold"
    ))
  ) %>%
  ggplot(aes(x = label, y = value, fill = methodology)) +
  geom_col(width = 0.7) +
  facet_wrap(~metric, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = c(
    "1. Calendar only"                   = "steelblue",
    "2. Threshold-only (daily check)"    = "firebrick",
    "3. Threshold-only (monthly check)"  = "darkorange",
    "4. Calendar + Threshold"            = "forestgreen"
  )) +
  labs(
    title    = "Key Metrics: All Methodology Configs",
    subtitle = "Triple-witching-Monday calendar | Proportional threshold | FI=25%, EQ=45%, RA=20%, Comm=10%",
    x = NULL, y = NULL, fill = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 6.5),
    legend.position = "bottom",
    strip.text      = element_text(face = "bold")
  )

# ── CLUSTER WEIGHT DRIFT — REFERENCE RUNS ────────────────────────────────────
# Show drift chart for three illustrative configs: Cal-only, Thresh-daily 20%, Cal+T 20%

drift_labels <- c("Cal only", "Thresh-daily 20%", "Cal+T 20%")

walk(drift_labels, function(lbl) {
  if (!lbl %in% names(all_results)) {
    message("Skipping drift chart for '", lbl, "' — not in results")
    return()
  }
  ref <- all_results[[lbl]]
  
  p <- ref$daily_cluster_weights %>%
    pivot_longer(-date, names_to = "cluster", values_to = "weight") %>%
    ggplot(aes(x = date, y = weight, color = cluster)) +
    geom_line(linewidth = 0.6, alpha = 0.8) +
    geom_hline(
      data = tibble(cluster = names(target_cluster_wts),
                    target  = as.numeric(target_cluster_wts)),
      aes(yintercept = target, color = cluster),
      linetype = "dashed", linewidth = 0.4
    ) +
    geom_vline(
      data = ref$rebal_log,
      aes(xintercept = as.numeric(date)),
      color = "gray50", linetype = "dotted", alpha = 0.4
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title    = paste0("Cluster Weight Drift: ", lbl),
      subtitle = paste0("n_rebalances = ", nrow(ref$rebal_log),
                        " | Solid = realized | Dashed = target | Dotted = rebalance events"),
      x = NULL, y = "Cluster Weight", color = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p)
})

message("\nDone.")