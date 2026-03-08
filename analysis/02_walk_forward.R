# === 02: WALK-FORWARD VALIDATION ===
# Runs out-of-sample validation across Equal Weight, ERC, Min Variance.
# Dependency: 01_data.R must have been run first.
# Output: cache/fold_results.rds, cache/fold_stats.rds

source("config/accounts.R")
source("R/data.R")
source("R/weights.R")

library(lubridate)

# Load and align backtest returns
prices       <- load_prices(ACCOUNT$symbols_backtest, DATA_DIR)
returns_wide <- build_returns(prices)

message("Aligned history: ", min(returns_wide$date), " to ", max(returns_wide$date))

# Cluster assignments for backtest symbols only
clusters_backtest <- ACCOUNT$clusters %>%
  filter(symbol %in% ACCOUNT$symbols_backtest)

# Fold definitions
full_start       <- min(returns_wide$date)
full_end         <- max(returns_wide$date)
first_test_start <- full_start + years(TRAIN_YEARS)
test_starts      <- seq(first_test_start, full_end - months(TEST_MONTHS), by = "6 months")

folds <- map_dfr(test_starts, function(ts) {
  tibble(
    train_start = full_start,
    train_end   = ts - days(1),
    test_start  = ts,
    test_end    = min(ts + months(TEST_MONTHS) - days(1), full_end)
  )
}) %>% mutate(fold = row_number())

message("Folds: ", nrow(folds))

methods <- c("Equal_Weight", "Min_Variance", "ERC")

fold_results <- map_dfr(folds$fold, function(f) {
  train_start <- folds$train_start[f]
  train_end   <- folds$train_end[f]
  test_start  <- folds$test_start[f]
  test_end    <- folds$test_end[f]
  
  message("Fold ", f, ": train to ", train_end, " | test ", test_start, " - ", test_end)
  
  returns_train <- returns_wide %>% filter(date >= train_start, date <= train_end)
  returns_test  <- returns_wide %>% filter(date >= test_start,  date <= test_end)
  
  if (nrow(returns_train) < 60 || nrow(returns_test) < 5) {
    warning("Fold ", f, ": insufficient data, skipping.")
    return(NULL)
  }
  
  within_wts <- inv_vol_weights(returns_train, clusters_backtest)
  meta_train <- meta_asset_returns(returns_train, within_wts, clusters_backtest)
  meta_test  <- meta_asset_returns(returns_test,  within_wts, clusters_backtest)
  
  wts <- list(
    Equal_Weight = weights_equal(meta_train),
    Min_Variance = weights_minvar(meta_train),
    ERC          = weights_erc(meta_train)
  )
  
  map_dfr(methods, function(method) {
    w        <- wts[[method]]
    ret_mat  <- as.matrix(meta_test %>% select(-date))
    port_ret <- as.numeric(ret_mat %*% w)
    
    tibble(
      fold        = f,
      method      = method,
      test_start  = test_start,
      test_end    = test_end,
      date        = meta_test$date,
      port_return = port_ret
    )
  })
})

rf_daily <- (1 + RF_ANNUAL)^(1/252) - 1

fold_stats <- fold_results %>%
  group_by(fold, method, test_start, test_end) %>%
  summarise(
    ann_return   = round(prod(1 + port_return)^(252/n()) - 1, 4),
    ann_vol      = round(sd(port_return) * sqrt(252), 4),
    sharpe       = round(mean(port_return - rf_daily) / sd(port_return) * sqrt(252), 4),
    max_drawdown = round(1 - min(cumprod(1 + port_return) / cummax(cumprod(1 + port_return))), 4),
    .groups      = "drop"
  )

# Save to cache
saveRDS(fold_results, file.path(CACHE_DIR, "fold_results.rds"))
saveRDS(fold_stats,   file.path(CACHE_DIR, "fold_stats.rds"))

message("Saved fold_results and fold_stats to ", CACHE_DIR, "/")