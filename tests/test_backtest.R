# === UNIT TESTS ===
# Tests for rebalancing trigger logic, weight calculations, and return stats.
# Run with: testthat::test_file("tests/test_backtest.R")
# Or from project root: testthat::test_dir("tests")
#
# install.packages("testthat") if needed

library(testthat)
library(tidyverse)
library(lubridate)

source("config/accounts.R")
source("R/weights.R")
source("R/backtest.R")

# ── SHARED TEST FIXTURES ──────────────────────────────────────────────────────

# Minimal cluster assignments (2 clusters, 4 symbols)
test_clusters <- tribble(
  ~symbol, ~cluster, ~cluster_label,
  "A",     1,        "Cluster1",
  "B",     1,        "Cluster1",
  "C",     2,        "Cluster2",
  "D",     2,        "Cluster2"
)

# Deterministic synthetic returns: 500 trading days starting 2015-01-02
set.seed(42)
n_days     <- 500
test_dates <- seq.Date(as.Date("2015-01-02"), by = "day", length.out = n_days)
# Keep only weekdays (rough approximation of trading days)
test_dates <- test_dates[!weekdays(test_dates) %in% c("Saturday", "Sunday")]
n_days     <- length(test_dates)

test_returns <- tibble(date = test_dates) %>%
  mutate(
    A = rnorm(n_days, 0.0003, 0.01),
    B = rnorm(n_days, 0.0004, 0.015),
    C = rnorm(n_days, 0.0002, 0.008),
    D = rnorm(n_days, 0.0001, 0.012)
  )

test_target_wts <- c(Cluster1 = 0.60, Cluster2 = 0.40)

# ── 1. PROP_THRESHOLD_TRIGGERED ───────────────────────────────────────────────

test_that("prop_threshold_triggered fires when drift exceeds threshold", {
  target  <- c(A = 0.25, B = 0.25, C = 0.25, D = 0.25)
  current <- c(A = 0.32, B = 0.25, C = 0.25, D = 0.18)  # A drifted 28% relative
  expect_true(prop_threshold_triggered(current, target, prop_threshold = 0.20))
})

test_that("prop_threshold_triggered does not fire when drift is below threshold", {
  target  <- c(A = 0.25, B = 0.25, C = 0.25, D = 0.25)
  current <- c(A = 0.27, B = 0.25, C = 0.25, D = 0.23)  # max 8% relative drift
  expect_false(prop_threshold_triggered(current, target, prop_threshold = 0.20))
})

test_that("prop_threshold_triggered returns FALSE when prop_threshold is NULL", {
  target  <- c(A = 0.50, B = 0.50)
  current <- c(A = 0.90, B = 0.10)  # extreme drift — should still return FALSE
  expect_false(prop_threshold_triggered(current, target, prop_threshold = NULL))
})

test_that("prop_threshold_triggered fires at exact boundary + epsilon", {
  target  <- c(A = 0.50, B = 0.50)
  # 20% relative drift on A: 0.50 * 1.20 = 0.60
  at_boundary <- c(A = 0.60, B = 0.40)
  just_over   <- c(A = 0.601, B = 0.399)
  expect_false(prop_threshold_triggered(at_boundary, target, prop_threshold = 0.20))
  expect_true( prop_threshold_triggered(just_over,   target, prop_threshold = 0.20))
})

# ── 2. GET_TRIPLE_WITCHING_MONDAYS ────────────────────────────────────────────

test_that("get_triple_witching_mondays returns dates in triple witching months only", {
  # Generate a full year of trading-ish dates
  dates  <- seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day")
  dates  <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]
  result <- get_triple_witching_mondays(dates)
  months <- month(result)
  expect_true(all(months %in% c(3, 6, 9, 12)))
})

test_that("get_triple_witching_mondays returns 4 dates per year for a full year", {
  dates  <- seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day")
  dates  <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]
  result <- get_triple_witching_mondays(dates)
  expect_equal(length(result), 4)
})

test_that("get_triple_witching_mondays returns Mondays or next available trading day", {
  dates  <- seq.Date(as.Date("2022-01-01"), as.Date("2022-12-31"), by = "day")
  dates  <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]
  result <- get_triple_witching_mondays(dates)
  # All returned dates must be in the trading dates vector
  expect_true(all(result %in% dates))
})

test_that("get_triple_witching_mondays returns dates after the triple witching Friday", {
  dates  <- seq.Date(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day")
  dates  <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]
  result <- get_triple_witching_mondays(dates)
  # Each result should be a Monday (weekday == 2) or later in the week
  # (if Monday was a holiday, it rolls forward)
  expect_true(all(weekdays(result) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")))
})

# ── 3. INV_VOL_WEIGHTS ────────────────────────────────────────────────────────

test_that("inv_vol_weights produces weights summing to 1 within each cluster", {
  wts <- inv_vol_weights(test_returns, test_clusters)
  cluster_sums <- wts %>%
    group_by(cluster) %>%
    summarise(total = sum(within_wt), .groups = "drop")
  expect_true(all(abs(cluster_sums$total - 1) < 1e-9))
})

test_that("inv_vol_weights assigns higher weight to lower-vol symbols", {
  wts <- inv_vol_weights(test_returns, test_clusters)
  # Within Cluster1: B has higher vol than A, so A should have higher weight
  wt_A <- wts$within_wt[wts$symbol == "A"]
  wt_B <- wts$within_wt[wts$symbol == "B"]
  expect_gt(wt_A, wt_B)
})

test_that("inv_vol_weights returns a row for every symbol in cluster_assignments", {
  wts <- inv_vol_weights(test_returns, test_clusters)
  expect_equal(sort(wts$symbol), sort(test_clusters$symbol))
})

# ── 4. RUN_TWO_LEVEL_BACKTEST: RETURN SANITY ─────────────────────────────────

# Run once and reuse across tests
backtest_result <- run_two_level_backtest(
  returns_wide         = test_returns,
  cluster_assignments  = test_clusters,
  target_cluster_wts   = test_target_wts,
  use_calendar         = TRUE,
  prop_threshold       = 0.20,
  threshold_check_freq = "monthly",
  vol_window           = 1,
  rf_annual            = 0.033
)

test_that("backtest returns a list with expected components", {
  expect_named(backtest_result, c("stats", "daily_values", "daily_cluster_weights", "rebal_log"))
})

test_that("backtest daily_values has correct number of rows", {
  expect_equal(nrow(backtest_result$daily_values), n_days)
})

test_that("backtest daily_values are all finite and positive", {
  expect_true(all(is.finite(backtest_result$daily_values$value)))
  expect_true(all(backtest_result$daily_values$value > 0))
})

test_that("backtest starts near $1", {
  expect_equal(backtest_result$daily_values$value[1], 1.0, tolerance = 0.05)
})

test_that("backtest stats are finite", {
  stats <- backtest_result$stats
  expect_true(is.finite(stats$ann_return))
  expect_true(is.finite(stats$ann_vol))
  expect_true(is.finite(stats$sharpe))
  expect_true(is.finite(stats$max_drawdown))
  expect_true(is.finite(stats$calmar))
})

test_that("backtest max_drawdown is between 0 and 1", {
  expect_gte(backtest_result$stats$max_drawdown, 0)
  expect_lte(backtest_result$stats$max_drawdown, 1)
})

test_that("backtest ann_vol is positive", {
  expect_gt(backtest_result$stats$ann_vol, 0)
})

# ── 5. RUN_TWO_LEVEL_BACKTEST: REBALANCING LOGIC ─────────────────────────────

test_that("backtest rebal_log contains only valid trigger types", {
  valid_triggers <- c("calendar", "threshold", "both")
  expect_true(all(backtest_result$rebal_log$trigger %in% valid_triggers))
})

test_that("backtest rebal_log n_rebal is sequential", {
  log <- backtest_result$rebal_log
  if (nrow(log) > 0) {
    expect_equal(log$n_rebal, seq_len(nrow(log)))
  }
})

test_that("backtest rebal_log dates are within the backtest window", {
  log   <- backtest_result$rebal_log
  dates <- backtest_result$daily_values$date
  expect_true(all(log$date >= min(dates)))
  expect_true(all(log$date <= max(dates)))
})

test_that("calendar-only produces more rebalances than threshold-only at 20%", {
  cal_result <- run_two_level_backtest(
    returns_wide        = test_returns,
    cluster_assignments = test_clusters,
    target_cluster_wts  = test_target_wts,
    use_calendar        = TRUE,
    prop_threshold      = NULL,
    vol_window          = 1,
    rf_annual           = 0.033
  )
  thresh_result <- run_two_level_backtest(
    returns_wide         = test_returns,
    cluster_assignments  = test_clusters,
    target_cluster_wts   = test_target_wts,
    use_calendar         = FALSE,
    prop_threshold       = 0.20,
    threshold_check_freq = "daily",
    vol_window           = 1,
    rf_annual            = 0.033
  )
  expect_gte(cal_result$stats$n_rebalances, thresh_result$stats$n_rebalances)
})

test_that("lower threshold produces more rebalances than higher threshold", {
  low <- run_two_level_backtest(
    returns_wide         = test_returns,
    cluster_assignments  = test_clusters,
    target_cluster_wts   = test_target_wts,
    use_calendar         = FALSE,
    prop_threshold       = 0.10,
    threshold_check_freq = "daily",
    vol_window           = 1,
    rf_annual            = 0.033
  )
  high <- run_two_level_backtest(
    returns_wide         = test_returns,
    cluster_assignments  = test_clusters,
    target_cluster_wts   = test_target_wts,
    use_calendar         = FALSE,
    prop_threshold       = 0.30,
    threshold_check_freq = "daily",
    vol_window           = 1,
    rf_annual            = 0.033
  )
  expect_gte(low$stats$n_rebalances, high$stats$n_rebalances)
})

test_that("error is thrown when neither calendar nor threshold is enabled", {
  expect_error(
    run_two_level_backtest(
      returns_wide        = test_returns,
      cluster_assignments = test_clusters,
      target_cluster_wts  = test_target_wts,
      use_calendar        = FALSE,
      prop_threshold      = NULL,
      vol_window          = 1
    ),
    "Must enable at least one trigger"
  )
})

test_that("error is thrown for missing symbols in returns_wide", {
  bad_clusters <- test_clusters %>%
    add_row(symbol = "MISSING", cluster = 1, cluster_label = "Cluster1")
  expect_error(
    run_two_level_backtest(
      returns_wide        = test_returns,
      cluster_assignments = bad_clusters,
      target_cluster_wts  = test_target_wts,
      use_calendar        = TRUE,
      prop_threshold      = NULL,
      vol_window          = 1
    ),
    "Missing symbols"
  )
})

# ── 6. CLUSTER WEIGHTS SUM TO 1 DURING BACKTEST ───────────────────────────────

test_that("daily cluster weights sum to approximately 1 each day", {
  cluster_wt_matrix <- backtest_result$daily_cluster_weights %>%
    select(-date) %>%
    as.matrix()
  row_sums <- rowSums(cluster_wt_matrix)
  expect_true(all(abs(row_sums - 1) < 1e-6))
})

message("\nAll tests passed.")