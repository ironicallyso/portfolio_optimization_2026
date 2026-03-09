# === 03: WEIGHT SENSITIVITY SWEEP ===
# Sweeps FI and Commodity target weights, evaluates in-sample performance.
# Dependency: 01_data.R must have been run first.
# Output: cache/sweep_results.rds

source("config/accounts.R")
source("R/data.R")
source("R/weights.R")
source("R/sweep.R")

# Load and align backtest returns (DBC proxy, KRBN excluded)
prices       <- load_prices(ACCOUNT$symbols_backtest, DATA_DIR)
returns_wide <- build_returns(prices)

message("Aligned history: ", min(returns_wide$date), " to ", max(returns_wide$date))

# Cluster assignments for backtest symbols only
clusters_backtest <- ACCOUNT$clusters %>%
  filter(symbol %in% ACCOUNT$symbols_backtest)

# Within-cluster weights and meta-asset returns over full history
within_wts   <- inv_vol_weights(returns_wide, clusters_backtest)
meta_returns <- meta_asset_returns(returns_wide, within_wts, clusters_backtest)

# Run sweep
sweep_results <- run_sweep(meta_returns, rf_annual = RF_ANNUAL)

# Save to cache
saveRDS(sweep_results, file.path(CACHE_DIR, "sweep_results.rds"))

message("Saved sweep_results to ", CACHE_DIR, "/")

# Print top 5 by Calmar
cat("\nTop 5 by Calmar:\n")
sweep_results %>%
  arrange(desc(calmar)) %>%
  slice_head(n = 10) %>%
  mutate(across(c(fi_wt, comm_wt, eq_wt, ra_wt), ~scales::percent(.x, accuracy = 1))) %>%
  print()