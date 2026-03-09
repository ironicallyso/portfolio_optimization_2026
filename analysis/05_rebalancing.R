# === 05: REBALANCING SWEEP ===
# Sweeps rebalancing methodologies and threshold levels to calibrate
# the two-level proportional threshold engine.
# Dependency: 01_data.R must have been run first.
# Output: cache/rebal_results.rds

source("config/accounts.R")
source("R/data.R")
source("R/weights.R")
source("R/backtest.R")

# Load backtest symbols (DBC proxy, KRBN excluded)
prices       <- load_prices(ACCOUNT$symbols_backtest, DATA_DIR)
returns_wide <- build_returns(prices)

message("Aligned history: ", min(returns_wide$date), " to ", max(returns_wide$date))

clusters_backtest <- ACCOUNT$clusters %>%
  filter(symbol %in% ACCOUNT$symbols_backtest)

# ── SWEEP CONFIGS ──────────────────────────────────────────────────────────────
# Four methodology families:
#   1. Calendar-only          — no threshold
#   2. Threshold-only, daily  — checks drift every trading day
#   3. Threshold-only, monthly— checks drift on triple-witching Mondays only
#   4. Calendar + Threshold   — OR logic, monthly check

thresholds    <- c(0.10, 0.15, 0.20, 0.25, 0.30)
thresh_labels <- scales::percent(thresholds, accuracy = 1)

configs <- bind_rows(
  tibble(
    label                = "Cal only",
    use_calendar         = TRUE,
    prop_threshold       = list(NULL),
    threshold_check_freq = "monthly"
  ),
  tibble(
    label                = paste0("Thresh-daily ",   thresh_labels),
    use_calendar         = FALSE,
    prop_threshold       = as.list(thresholds),
    threshold_check_freq = "daily"
  ),
  tibble(
    label                = paste0("Thresh-monthly ", thresh_labels),
    use_calendar         = FALSE,
    prop_threshold       = as.list(thresholds),
    threshold_check_freq = "monthly"
  ),
  tibble(
    label                = paste0("Cal+T ",          thresh_labels),
    use_calendar         = TRUE,
    prop_threshold       = as.list(thresholds),
    threshold_check_freq = "monthly"
  )
)

message("\nRunning ", nrow(configs), " configs...")

# ── RUN SWEEP ──────────────────────────────────────────────────────────────────

all_results <- pmap(configs, function(label, use_calendar, prop_threshold, threshold_check_freq) {
  message("  -> ", label)
  result <- run_two_level_backtest(
    returns_wide         = returns_wide,
    cluster_assignments  = clusters_backtest,
    target_cluster_wts   = ACCOUNT$target_weights,
    use_calendar         = use_calendar,
    prop_threshold       = prop_threshold,
    threshold_check_freq = threshold_check_freq,
    vol_window           = TRAIN_YEARS,
    rf_annual            = RF_ANNUAL
  )
  result$stats <- result$stats %>% mutate(label = label)
  result$label <- label
  result
})
names(all_results) <- configs$label

# ── RESULTS TABLE ──────────────────────────────────────────────────────────────

results_df <- map_dfr(all_results, ~.x$stats) %>%
  mutate(
    methodology = case_when(
      use_calendar & is.na(prop_threshold)              ~ "1. Calendar only",
      !use_calendar & threshold_check_freq == "daily"   ~ "2. Threshold-only (daily)",
      !use_calendar & threshold_check_freq == "monthly" ~ "3. Threshold-only (monthly)",
      use_calendar  & threshold_check_freq == "monthly" ~ "4. Calendar + Threshold"
    )
  )

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
  select(methodology, prop_threshold, threshold_check_freq,
         ann_return, ann_vol, sharpe, max_drawdown, calmar, n_rebalances) %>%
  print(n = Inf)

cat("\n── Best Config per Methodology (by Sharpe) ──────────────────────────\n")
results_df %>%
  group_by(methodology) %>%
  slice_max(sharpe, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    prop_threshold = ifelse(is.na(prop_threshold), "—",
                            scales::percent(prop_threshold, accuracy = 1)),
    ann_return   = scales::percent(ann_return,   accuracy = 0.1),
    max_drawdown = scales::percent(max_drawdown,  accuracy = 0.1),
    sharpe       = round(sharpe, 3),
    calmar       = round(calmar, 3)
  ) %>%
  select(methodology, prop_threshold, ann_return, sharpe, max_drawdown, calmar, n_rebalances) %>%
  print()

cat("\n── Rebalance Trigger Breakdown ──────────────────────────────────────\n")
walk(all_results, function(r) {
  cat("\n", r$label, ":\n", sep = "")
  if (nrow(r$rebal_log) == 0) {
    cat("  (no rebalances triggered)\n")
  } else {
    print(table(r$rebal_log$trigger))
  }
})

# ── SAVE ──────────────────────────────────────────────────────────────────────

saveRDS(all_results, file.path(CACHE_DIR, "rebal_results.rds"))
message("\nSaved rebal_results to ", CACHE_DIR, "/")
