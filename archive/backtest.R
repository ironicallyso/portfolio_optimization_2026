# === BACKTEST FUNCTIONS ===
# PLACEHOLDER — current implementation uses flat absolute threshold.
# TODO: Rewrite run_backtest() with two-level proportional threshold per design doc.
# TODO: Add unit tests for rebalancing trigger logic, weight drift, and return calcs.

library(tidyverse)
library(PerformanceAnalytics)
library(xts)

# Generate calendar-based rebalance dates
get_rebal_dates <- function(dates, freq) {
  dates <- as.Date(dates)
  switch(freq,
         daily     = dates,
         weekly    = dates[weekdays(dates) == "Monday"],
         monthly   = dates[!duplicated(format(dates, "%Y-%m"))],
         quarterly = dates[format(dates, "%m") %in% c("01","04","07","10") &
                             !duplicated(format(dates, "%Y-%m"))],
         stop("freq must be: daily, weekly, monthly, or quarterly")
  )
}

# NOTE: threshold here is flat absolute (e.g. 0.05 = 5% drift).
# Proportional threshold (e.g. 20% relative) to be implemented in rewrite.
run_backtest <- function(returns,
                         target_weights,
                         rebal_freq      = "quarterly",
                         rebal_threshold = NULL,
                         rf_annual       = 0.033) {
  
  returns_mat <- as.matrix(returns)
  syms        <- colnames(returns_mat)
  
  target_weights <- target_weights[syms]
  target_weights <- target_weights / sum(target_weights)
  
  dates    <- as.Date(rownames(returns_mat))
  n_days   <- length(dates)
  n_assets <- length(syms)
  rf_daily <- (1 + rf_annual)^(1/252) - 1
  
  cal_rebal_dates <- if (!is.null(rebal_freq)) {
    get_rebal_dates(dates, rebal_freq)
  } else {
    as.Date(character(0))
  }
  
  portfolio_value <- 1.0
  current_weights <- target_weights
  
  daily_values  <- numeric(n_days)
  daily_weights <- matrix(NA, nrow = n_days, ncol = n_assets,
                          dimnames = list(NULL, syms))
  rebal_log     <- data.frame(date      = as.Date(character()),
                              trigger   = character(),
                              max_drift = numeric())
  
  for (i in seq_len(n_days)) {
    day         <- dates[i]
    day_returns <- returns_mat[i, ]
    day_returns[is.na(day_returns)] <- 0
    
    asset_values    <- portfolio_value * current_weights * (1 + day_returns)
    portfolio_value <- sum(asset_values)
    
    current_weights <- if (!is.na(portfolio_value) && portfolio_value > 0) {
      asset_values / portfolio_value
    } else {
      target_weights
    }
    
    daily_values[i]   <- portfolio_value
    daily_weights[i,] <- current_weights
    
    max_drift   <- max(abs(current_weights - target_weights))
    on_calendar <- day %in% cal_rebal_dates
    over_thresh <- !is.null(rebal_threshold) && max_drift > rebal_threshold
    
    if ((on_calendar || over_thresh) && i < n_days) {
      trigger <- case_when(
        on_calendar & over_thresh ~ "both",
        on_calendar               ~ "calendar",
        TRUE                      ~ "threshold"
      )
      rebal_log       <- bind_rows(rebal_log,
                                   data.frame(date = day, trigger = trigger,
                                              max_drift = round(max_drift, 4)))
      current_weights <- target_weights
    }
  }
  
  port_returns   <- c(0, diff(daily_values) / daily_values[-n_days])
  excess_returns <- port_returns - rf_daily
  
  ann_return <- prod(1 + port_returns)^(252/n_days) - 1
  ann_vol    <- sd(port_returns) * sqrt(252)
  sharpe     <- mean(excess_returns) / sd(excess_returns) * sqrt(252)
  
  port_xts <- xts(port_returns, order.by = dates)
  max_dd   <- as.numeric(maxDrawdown(port_xts))
  
  list(
    params = list(
      rebal_freq      = rebal_freq,
      rebal_threshold = rebal_threshold
    ),
    stats = tibble(
      ann_return   = round(ann_return, 4),
      ann_vol      = round(ann_vol, 4),
      sharpe       = round(sharpe, 4),
      max_drawdown = round(max_dd, 4),
      n_rebalances = nrow(rebal_log)
    ),
    daily_values  = tibble(date = dates, value = daily_values),
    daily_weights = as_tibble(daily_weights) %>% mutate(date = dates),
    rebal_log     = rebal_log
  )
}