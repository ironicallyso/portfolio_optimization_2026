# === WEIGHT SENSITIVITY SWEEP ===
# Sweeps target allocations for Commodities and Fixed Income clusters.
# Equities and Real Assets absorb the remainder equally.
# Within-cluster weights: inverse volatility (recomputed each run).
# Evaluation: in-sample over full aligned history.
# DBC used as commodity proxy (longer history); KRBN excluded.
#
# Packages:
# install.packages(c("tidyverse", "lubridate"))

library(tidyverse)
library(lubridate)

# ── CONFIG ────────────────────────────────────────────────────────────────────

data_dir <- "portfolio_data"
rf_annual <- 0.033
rf_daily  <- (1 + rf_annual)^(1/252) - 1

# Fixed cluster assignments — DBC replaces CMDY, KRBN excluded
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

symbols        <- cluster_assignments$symbol
cluster_labels <- cluster_assignments %>% distinct(cluster, cluster_label) %>% arrange(cluster)

# Sweep ranges
fi_targets   <- c(0.05, 0.10, 0.15, 0.20, 0.25)   # Fixed Income
comm_targets <- c(0.05, 0.10, 0.15, 0.20, 0.25)   # Commodities

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

# ── WITHIN-CLUSTER WEIGHTS (inverse vol, computed once over full history) ─────

vol_df <- returns_wide %>%
  pivot_longer(-date, names_to = "symbol", values_to = "return") %>%
  group_by(symbol) %>%
  summarise(ann_vol = sd(return, na.rm = TRUE) * sqrt(252), .groups = "drop")

within_weights <- cluster_assignments %>%
  left_join(vol_df, by = "symbol") %>%
  group_by(cluster) %>%
  mutate(within_wt = (1 / ann_vol) / sum(1 / ann_vol)) %>%
  ungroup()

# ── META-ASSET RETURNS ────────────────────────────────────────────────────────

wt_vec <- within_weights %>% select(symbol, within_wt) %>% deframe()

meta_returns <- cluster_labels %>%
  pmap(function(cluster, cluster_label) {
    syms <- cluster_assignments %>% filter(cluster == !!cluster) %>% pull(symbol)
    wts  <- wt_vec[syms]
    returns_wide %>%
      select(date, all_of(syms)) %>%
      rowwise() %>%
      mutate(!!cluster_label := sum(c_across(all_of(syms)) * wts, na.rm = TRUE)) %>%
      ungroup() %>%
      select(date, all_of(cluster_label))
  }) %>%
  reduce(inner_join, by = "date")

# ── PERFORMANCE FUNCTION ──────────────────────────────────────────────────────

calc_stats <- function(returns, weights_vec) {
  ret_mat  <- as.matrix(returns %>% select(-date))
  port_ret <- as.numeric(ret_mat %*% weights_vec)
  dates    <- returns$date
  n        <- length(port_ret)
  
  ann_return   <- prod(1 + port_ret)^(252/n) - 1
  ann_vol      <- sd(port_ret) * sqrt(252)
  sharpe       <- mean(port_ret - rf_daily) / sd(port_ret) * sqrt(252)
  cum          <- cumprod(1 + port_ret)
  max_drawdown <- 1 - min(cum / cummax(cum))
  
  tibble(
    ann_return   = round(ann_return,   4),
    ann_vol      = round(ann_vol,      4),
    sharpe       = round(sharpe,       4),
    max_drawdown = round(max_drawdown, 4)
  )
}

# ── SWEEP ─────────────────────────────────────────────────────────────────────
# For each (fi_wt, comm_wt) combination where they sum to <= 1,
# split remainder equally between Equities and Real Assets.

sweep_grid <- expand_grid(fi_wt = fi_targets, comm_wt = comm_targets) %>%
  filter(fi_wt + comm_wt <= 0.90) %>%   # leave at least 10% for growth clusters
  mutate(
    remainder  = 1 - fi_wt - comm_wt,
    eq_wt      = remainder / 2,
    ra_wt      = remainder / 2
  )

message("\nRunning ", nrow(sweep_grid), " weight combinations...")

sweep_results <- sweep_grid %>%
  pmap_dfr(function(fi_wt, comm_wt, remainder, eq_wt, ra_wt) {
    # Between-cluster weight vector aligned to meta_returns columns
    between_wts <- c(
      Fixed_Income = fi_wt,
      Real_Assets  = ra_wt,
      Equities     = eq_wt,
      Commodities  = comm_wt
    )
    # Align to column order of meta_returns
    wts_aligned <- between_wts[names(meta_returns %>% select(-date))]
    
    stats <- calc_stats(meta_returns, wts_aligned)
    
    bind_cols(
      tibble(
        fi_wt   = fi_wt,
        comm_wt = comm_wt,
        eq_wt   = eq_wt,
        ra_wt   = ra_wt
      ),
      stats
    )
  })

# ── RESULTS TABLE ─────────────────────────────────────────────────────────────

cat("\n── Sweep Results (sorted by Sharpe) ────────────────────────────────\n")
sweep_results %>%
  arrange(desc(sharpe)) %>%
  mutate(across(c(fi_wt, comm_wt, eq_wt, ra_wt), ~scales::percent(.x, accuracy = 1))) %>%
  mutate(across(c(ann_return, ann_vol, sharpe, max_drawdown), ~round(.x, 3))) %>%
  print(n = Inf)

# ── HEATMAPS ─────────────────────────────────────────────────────────────────

# Helper to create ordered percent labels
pct_factor <- function(x) {
  lvls <- paste0(scales::percent(sort(unique(x)), accuracy = 1))
  factor(paste0(scales::percent(x, accuracy = 1)), levels = lvls)
}

# Sharpe heatmap
sweep_results %>%
  mutate(
    fi_label   = pct_factor(fi_wt),
    comm_label = pct_factor(comm_wt)
    #fi_label   = paste0("FI: ",   scales::percent(fi_wt,   accuracy = 1)),
    #comm_label = paste0("Comm: ", scales::percent(comm_wt, accuracy = 1))
  ) %>%
  ggplot(aes(x = comm_label, y = fi_label, fill = sharpe)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(sharpe, 2)), size = 3.5) +
  scale_fill_gradient2(low = "firebrick", mid = "white", high = "steelblue",
                       midpoint = median(sweep_results$sharpe)) +
  labs(
    title    = "Sharpe Ratio by Cluster Target Weights",
    subtitle = "Remainder split equally between Equities and Real Assets",
    x        = "Commodities Target Weight",
    y        = "Fixed Income Target Weight",
    fill     = "Sharpe"
  ) +
  theme_minimal()

# Annualized return heatmap
sweep_results %>%
  mutate(
    fi_label   = pct_factor(fi_wt),
    comm_label = pct_factor(comm_wt)
    #fi_label   = paste0("FI: ",   scales::percent(fi_wt,   accuracy = 1)),
    #comm_label = paste0("Comm: ", scales::percent(comm_wt, accuracy = 1))
  ) %>%
  ggplot(aes(x = comm_label, y = fi_label, fill = ann_return)) +
  geom_tile(color = "white") +
  geom_text(aes(label = scales::percent(ann_return, accuracy = 0.1)), size = 3.5) +
  scale_fill_gradient2(low = "firebrick", mid = "white", high = "steelblue",
                       midpoint = median(sweep_results$ann_return)) +
  labs(
    title    = "Annualized Return by Cluster Target Weights",
    subtitle = "Remainder split equally between Equities and Real Assets",
    x        = "Commodities Target Weight",
    y        = "Fixed Income Target Weight",
    fill     = "Ann. Return"
  ) +
  theme_minimal()

# Max drawdown heatmap
sweep_results %>%
  mutate(
    fi_label   = pct_factor(fi_wt),
    comm_label = pct_factor(comm_wt)
    #fi_label   = paste0("FI: ",   scales::percent(fi_wt,   accuracy = 1)),
    #comm_label = paste0("Comm: ", scales::percent(comm_wt, accuracy = 1))
  ) %>%
  ggplot(aes(x = comm_label, y = fi_label, fill = -max_drawdown)) +
  geom_tile(color = "white") +
  geom_text(aes(label = scales::percent(max_drawdown, accuracy = 0.1)), size = 3.5) +
  scale_fill_gradient2(low = "firebrick", mid = "white", high = "steelblue",
                       midpoint = median(-sweep_results$max_drawdown)) +
  labs(
    title    = "Max Drawdown by Cluster Target Weights",
    subtitle = "Lower is better — blue = smaller drawdown",
    x        = "Commodities Target Weight",
    y        = "Fixed Income Target Weight",
    fill     = "(-) Drawdown"
  ) +
  theme_minimal()

# Calmar Ratio heatmap
sweep_results %>%
  mutate(
    fi_label   = pct_factor(fi_wt),
    comm_label = pct_factor(comm_wt),
    calmar     = ann_return / max_drawdown
  ) %>%
  ggplot(aes(x = comm_label, y = fi_label, fill = calmar)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(calmar, 2)), size = 3.5) +
  scale_fill_gradient2(low = "firebrick", mid = "white", high = "steelblue",
                       midpoint = median(sweep_results$ann_return / sweep_results$max_drawdown)) +
  labs(
    title    = "Calmar Ratio by Cluster Target Weights",
    subtitle = "Annualized Return / Max Drawdown — higher is better",
    x        = "Commodities Target Weight",
    y        = "Fixed Income Target Weight",
    fill     = "Calmar"
  ) +
  theme_minimal()

# ── TOP COMBINATIONS ──────────────────────────────────────────────────────────

cat("\n── Top 5 by Sharpe ──────────────────────────────────────────────────\n")
sweep_results %>%
  arrange(desc(sharpe)) %>%
  slice_head(n = 5) %>%
  mutate(across(c(fi_wt, comm_wt, eq_wt, ra_wt), ~scales::percent(.x, accuracy = 1))) %>%
  print()

cat("\n── Top 5 by Return ──────────────────────────────────────────────────\n")
sweep_results %>%
  arrange(desc(ann_return)) %>%
  slice_head(n = 5) %>%
  mutate(across(c(fi_wt, comm_wt, eq_wt, ra_wt), ~scales::percent(.x, accuracy = 1))) %>%
  print()

cat("\n── Top 5 by Minimum Drawdown ────────────────────────────────────────\n")
sweep_results %>%
  arrange(max_drawdown) %>%
  slice_head(n = 5) %>%
  mutate(across(c(fi_wt, comm_wt, eq_wt, ra_wt), ~scales::percent(.x, accuracy = 1))) %>%
  print()

sweep_results <- sweep_results %>% mutate(calmar = round(ann_return / max_drawdown, 4))

cat("\n── Top 5 by Calmar ──────────────────────────────────────────────────\n")
sweep_results %>%
  arrange(desc(calmar)) %>%
  slice_head(n = 5) %>%
  mutate(across(c(fi_wt, comm_wt, eq_wt, ra_wt), ~scales::percent(.x, accuracy = 1))) %>%
  print()

# ── EQUITY vs REAL ASSETS SWEEP (FI=25%, Comm=5% fixed) ──────────────────────

eq_splits <- seq(0.10, 0.60, by = 0.10)  # Equities share of the 70% remainder

era_sweep <- tibble(
  fi_wt   = 0.25,
  comm_wt = 0.05,
  eq_wt   = eq_splits,
  ra_wt   = 0.70 - eq_splits
) %>%
  pmap_dfr(function(fi_wt, comm_wt, eq_wt, ra_wt) {
    between_wts <- c(Fixed_Income = fi_wt, Real_Assets = ra_wt,
                     Equities = eq_wt, Commodities = comm_wt)
    wts_aligned <- between_wts[names(meta_returns %>% select(-date))]
    stats <- calc_stats(meta_returns, wts_aligned)
    bind_cols(tibble(fi_wt=fi_wt, comm_wt=comm_wt, eq_wt=eq_wt, ra_wt=ra_wt), stats)
  }) %>%
  mutate(calmar = round(ann_return / max_drawdown, 4))

cat("\n── Equity vs Real Assets Sweep (FI=25%, Comm=5%) ───────────────────\n")
era_sweep %>%
  mutate(across(c(fi_wt, comm_wt, eq_wt, ra_wt), ~scales::percent(.x, accuracy = 1))) %>%
  arrange(desc(calmar)) %>%
  print(n = Inf)

# Plot
era_sweep %>%
  pivot_longer(cols = c(ann_return, sharpe, calmar, max_drawdown),
               names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = eq_wt, y = value)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2.5) +
  facet_wrap(~metric, scales = "free_y") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title    = "Equity vs Real Assets Tradeoff (FI=25%, Comm=5%)",
    subtitle = "Remaining 70% split between Equities (x-axis) and Real Assets",
    x        = "Equities Weight",
    y        = NULL
  ) +
  theme_minimal()
