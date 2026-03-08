# === 04: FINAL WEIGHTS ===
# Computes live symbol-level weights using target between-cluster weights
# from config. Uses full available history for within-cluster vol estimates.
# Dependency: 01_data.R must have been run first.
# Output: cache/weights_final.rds

source("config/accounts.R")
source("R/data.R")
source("R/weights.R")

# Load live symbols (includes KRBN)
prices       <- load_prices(ACCOUNT$symbols_live, DATA_DIR)
returns_wide <- build_returns(prices)

message("Live symbol window: ", min(returns_wide$date), " to ", max(returns_wide$date))

# Cluster assignments for live symbols only
clusters_live <- ACCOUNT$clusters %>%
  filter(symbol %in% ACCOUNT$symbols_live)

# Within-cluster weights
within_wts <- inv_vol_weights(returns_wide, clusters_live)

# Apply target between-cluster weights from config
weights_final <- full_symbol_weights(within_wts, MAIN$target_weights)

# Print
cat("\n── Final Symbol Weights ─────────────────────────────────────────────\n")
weights_final %>%
  mutate(across(c(within_wt, between_wt, final_wt),
                ~scales::percent(.x, accuracy = 0.1))) %>%
  arrange(cluster) %>%
  print(n = Inf)

cat("\nTotal weight:", sum(weights_final$final_wt), "\n")

# Save to cache
saveRDS(weights_final, file.path(CACHE_DIR, "weights_final.rds"))

message("Saved weights_final to ", CACHE_DIR, "/")