# === 03: WEIGHT SENSITIVITY SWEEP ===
# Runs all pairwise cluster weight sweeps to identify a robust target allocation.
# For each combination of two fixed clusters, sweeps the split of the remainder.
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

# ── RUN ALL PAIRWISE SWEEPS ───────────────────────────────────────────────────
# Fixed values come from ACCOUNT$target_weights in config/accounts.R.
# All 6 pair combinations are run automatically.

message("\nRunning all pairwise sweeps...")
sweep_results <- run_all_pairwise_sweeps(
  meta_returns   = meta_returns,
  target_weights = ACCOUNT$target_weights,
  rf_annual      = RF_ANNUAL
)

# ── RESULTS ───────────────────────────────────────────────────────────────────

# Top 5 by Calmar per fixed pair
cat("\n── Top 5 by Calmar per Fixed Pair ───────────────────────────────────\n")
sweep_results %>%
  group_by(fixed_pair) %>%
  slice_max(calmar, n = 5, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(across(where(is.double) & !c(sharpe, calmar, ann_return, ann_vol, max_drawdown),
                ~scales::percent(.x, accuracy = 1))) %>%
  arrange(fixed_pair, desc(calmar)) %>%
  print(n = Inf)

# Tangency portfolio: max Sortino per fixed pair
cat("\n── Tangency Portfolio (Max Sortino) per Fixed Pair ─────────────────\n")
sweep_results %>%
  group_by(fixed_pair) %>%
  slice_max(sortino, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(across(where(is.double) & !c(sharpe, sortino, calmar, ann_return, ann_vol, max_drawdown),
                ~scales::percent(.x, accuracy = 1))) %>%
  arrange(desc(sortino)) %>%
  print(n = Inf)

# Convergence: which weight combinations appear in the top 5 across multiple sweeps?
cat("\n── Convergence: Top Allocations Across Sweeps ───────────────────────\n")
top_per_sweep <- sweep_results %>%
  group_by(fixed_pair) %>%
  slice_max(calmar, n = 5, with_ties = FALSE) %>%
  ungroup()

cluster_cols <- names(ACCOUNT$target_weights)

top_per_sweep %>%
  select(all_of(cluster_cols), calmar, fixed_pair) %>%
  mutate(across(all_of(cluster_cols), ~round(.x, 2))) %>%
  group_by(across(all_of(cluster_cols))) %>%
  summarise(
    n_sweeps   = n(),
    avg_calmar = round(mean(calmar), 4),
    .groups    = "drop"
  ) %>%
  filter(n_sweeps > 1) %>%
  arrange(desc(avg_calmar), desc(n_sweeps)) %>%
  print(n = 20)

# ── PLOTS ─────────────────────────────────────────────────────────────────────

# Calmar by sweep — line chart showing the sweep trajectory for each fixed pair
sweep_results %>%
  mutate(
    free_cluster1       = names(ACCOUNT$target_weights)[
      !names(ACCOUNT$target_weights) %in% str_extract_all(fixed_pair, paste(names(ACCOUNT$target_weights), collapse="|"))[[1]]
    ][1],
    free1_wt = pmap_dbl(
      select(., all_of(cluster_cols)),
      function(...) {
        wts <- c(...)
        names(wts) <- cluster_cols
        free <- setdiff(cluster_cols, str_extract_all(fixed_pair[1], paste(cluster_cols, collapse="|"))[[1]])
        wts[free[1]]
      }
    )
  )

# Simpler approach: plot calmar vs each free cluster weight, faceted by fixed pair
walk(unique(sweep_results$fixed_pair), function(fp) {
  pair_results <- sweep_results %>% filter(fixed_pair == fp)
  fixed_names  <- str_extract_all(fp, paste(cluster_cols, collapse = "|"))[[1]]
  free_names   <- setdiff(cluster_cols, fixed_names)

  if (length(free_names) < 2) return(NULL)

  free1 <- free_names[1]

  p <- pair_results %>%
    ggplot(aes(x = .data[[free1]], y = calmar)) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_point(color = "steelblue", size = 2.5) +
    geom_vline(
      xintercept = ACCOUNT$target_weights[free1],
      linetype   = "dashed", color = "firebrick", alpha = 0.7
    ) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title    = paste0("Calmar vs ", free1, " Weight"),
      subtitle = paste0(fp, "  |  Dashed = current config"),
      x        = paste0(free1, " Weight"),
      y        = "Calmar Ratio"
    ) +
    theme_minimal()

  print(p)
})

# ── SAVE ──────────────────────────────────────────────────────────────────────

dir.create(CACHE_DIR, showWarnings = FALSE)
saveRDS(sweep_results, file.path(CACHE_DIR, "sweep_results.rds"))
message("\nSaved sweep_results to ", CACHE_DIR, "/")
