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

dir.create(CACHE_DIR, showWarnings = FALSE)
saveRDS(all_results, file.path(CACHE_DIR, "rebal_results.rds"))
message("\nSaved rebal_results to ", CACHE_DIR, "/")

# ── PLOTS ──────────────────────────────────────────────────────────────────────

library(ggplot2)

# Best config per methodology (by Sharpe), excluding Cal+T
best_configs <- results_df %>%
  filter(!grepl("Calendar \\+ Threshold", methodology)) %>%
  group_by(methodology) %>%
  slice_max(sharpe, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  pull(label)

# ── PLOT 1A: EQUITY CURVES (thinner lines, transparency) ─────────────────────

ggplot(equity_df, aes(x = date, y = value, color = label)) +
  geom_line(linewidth = 0.5, alpha = 0.75) +
  scale_color_manual(values = c("steelblue", "firebrick", "darkorange")) +
  labs(
    title    = "Equity Curves: Best Config per Methodology",
    subtitle = "Calendar only | Best threshold-daily | Best threshold-monthly",
    x        = NULL,
    y        = "Growth of $1",
    color    = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# ── PLOT 1B: SPREAD VS CALENDAR-ONLY ─────────────────────────────────────────
# Shows each method's daily difference from calendar-only.
# Zero = identical to calendar; deviations reveal when methods diverge.

cal_values <- all_results[["Cal only"]]$daily_values %>%
  rename(cal_value = value)

spread_df <- map_dfr(setdiff(best_configs, "Cal only"), function(lbl) {
  all_results[[lbl]]$daily_values %>%
    inner_join(cal_values, by = "date") %>%
    mutate(
      spread = value - cal_value,
      label  = lbl
    )
})

ggplot(spread_df, aes(x = date, y = spread, color = label)) +
  geom_line(linewidth = 0.5, alpha = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("firebrick", "darkorange")) +
  labs(
    title    = "Portfolio Value Spread vs Calendar-Only",
    subtitle = "Positive = outperforming calendar | Negative = underperforming",
    x        = NULL,
    y        = "Difference in Growth of $1",
    color    = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# ── PLOT 1C: ZOOM — 2018–2022 ─────────────────────────────────────────────────
# COVID crash + 2022 rate hike: where rebalancing decisions matter most.

equity_df %>%
  filter(date >= as.Date("2018-01-01"), date <= as.Date("2022-12-31")) %>%
  ggplot(aes(x = date, y = value, color = label)) +
  geom_line(linewidth = 0.5, alpha = 0.75) +
  scale_color_manual(values = c("steelblue", "firebrick", "darkorange")) +
  labs(
    title    = "Equity Curves: Zoom 2018–2022",
    subtitle = "COVID crash + 2022 rate hike environment",
    x        = NULL,
    y        = "Growth of $1",
    color    = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
# ── PLOT 2: CLUSTER WEIGHT DRIFT ──────────────────────────────────────────────

drift_configs <- c("Cal only", "Thresh-daily 30%", "Thresh-monthly 20%")

walk(drift_configs, function(lbl) {
  ref <- all_results[[lbl]]
  
  p <- ref$daily_cluster_weights %>%
    pivot_longer(-date, names_to = "cluster", values_to = "weight") %>%
    left_join(
      tibble(cluster = names(ACCOUNT$target_weights),
             target  = as.numeric(ACCOUNT$target_weights)),
      by = "cluster"
    ) %>%
    ggplot(aes(x = date, y = weight, color = cluster)) +
    geom_line(linewidth = 0.6, alpha = 0.8) +
    geom_hline(
      data = tibble(cluster = names(ACCOUNT$target_weights),
                    target  = as.numeric(ACCOUNT$target_weights)),
      aes(yintercept = target, color = cluster),
      linetype = "dashed", linewidth = 0.4
    ) +
    geom_vline(
      data  = ref$rebal_log,
      aes(xintercept = as.numeric(date)),
      color = "gray50", linetype = "dotted", alpha = 0.5
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title    = paste0("Cluster Weight Drift: ", lbl),
      subtitle = paste0("n_rebalances = ", nrow(ref$rebal_log),
                        "  |  Solid = realized  |  Dashed = target  |  Dotted = rebalance events"),
      x        = NULL,
      y        = "Cluster Weight",
      color    = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p)
})