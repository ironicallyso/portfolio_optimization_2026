# === WITHIN-CLUSTER WEIGHTING: Inverse Volatility ===
# Assumes cluster_analysis.R has been sourced, OR re-runs the minimum
# setup needed to reproduce the 4-cluster assignments independently.
#
# Outputs:
#   1. Within-cluster weights (inverse vol)
#   2. A synthetic "meta-asset" return series per cluster
#   3. Summary table: symbol → cluster → within-cluster weight

# --- Packages ---
library(tidyverse)
library(lubridate)

# ── CONFIG ────────────────────────────────────────────────────────────────────

data_dir      <- "portfolio_data"
rolling_years <- 3
anchor_date   <- NULL   # NULL = most recent 3-year window

# Hardcoded k=4 cluster assignments based on dendrogram analysis
# Revisit if symbol universe changes or window shifts materially
cluster_assignments <- tribble(
  ~symbol,  ~cluster, ~cluster_label,
  "BND",    1,        "Fixed Income",
  "TIP",    1,        "Fixed Income",
  "VGIT",   1,        "Fixed Income",
  "IGF",    2,        "Real Assets",
  "IYR",    2,        "Real Assets",
  "EEM",    3,        "Equities",
  "EES",    3,        "Equities",
  "GRID",   3,        "Equities",
  "SPY",    3,        "Equities",
  "CMDY",   4,        "Commodities",
  "KRBN",   4,        "Commodities"
)

symbols <- cluster_assignments$symbol

# ── LOAD & BUILD RETURNS ──────────────────────────────────────────────────────

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

# ── APPLY 3-YEAR WINDOW ───────────────────────────────────────────────────────

end_date   <- if (!is.null(anchor_date)) anchor_date else max(returns_wide$date)
start_date <- end_date - years(rolling_years)

returns_window <- returns_wide %>%
  filter(date >= start_date, date <= end_date)

message("Weighting window: ", min(returns_window$date), " to ", max(returns_window$date))

# ── INVERSE VOLATILITY WEIGHTS ────────────────────────────────────────────────

# Annualized volatility per symbol over the window
vol_df <- returns_window %>%
  pivot_longer(-date, names_to = "symbol", values_to = "return") %>%
  group_by(symbol) %>%
  summarise(ann_vol = sd(return, na.rm = TRUE) * sqrt(252), .groups = "drop")

# Join with cluster assignments, compute inverse-vol weights within each cluster
weights_df <- cluster_assignments %>%
  left_join(vol_df, by = "symbol") %>%
  group_by(cluster, cluster_label) %>%
  mutate(
    inv_vol        = 1 / ann_vol,
    within_wt      = inv_vol / sum(inv_vol)   # weights sum to 1 within each cluster
  ) %>%
  ungroup() %>%
  select(cluster, cluster_label, symbol, ann_vol, within_wt)

cat("\n── Within-Cluster Weights (Inverse Volatility) ──────────────────────\n")
weights_df %>%
  mutate(
    ann_vol   = scales::percent(ann_vol,   accuracy = 0.1),
    within_wt = scales::percent(within_wt, accuracy = 0.1)
  ) %>%
  arrange(cluster, desc(within_wt)) %>%
  print(n = Inf)

# ── SYNTHETIC META-ASSET RETURNS ──────────────────────────────────────────────
# Each cluster collapses to a single daily return series using within-cluster weights.
# This is what we'll feed into the between-cluster optimization next.

# Build a weight vector aligned to returns columns
weight_vector <- weights_df %>%
  select(symbol, within_wt) %>%
  deframe()   # named numeric vector

# For each cluster, compute weighted daily return
cluster_returns <- cluster_assignments %>%
  distinct(cluster, cluster_label) %>%
  arrange(cluster) %>%
  pmap(function(cluster, cluster_label) {
    syms_in_cluster <- cluster_assignments %>%
      filter(cluster == !!cluster) %>%
      pull(symbol)
    
    wts <- weight_vector[syms_in_cluster]
    col_name <- paste0("cluster_", cluster, "_", gsub(" ", "_", cluster_label))
    
    returns_window %>%
      select(date, all_of(syms_in_cluster)) %>%
      rowwise() %>%
      mutate(!!col_name := sum(c_across(all_of(syms_in_cluster)) * wts, na.rm = TRUE)) %>%
      ungroup() %>%
      select(date, all_of(col_name))
  }) %>%
  reduce(inner_join, by = "date") %>%   # join side-by-side, not stacked
  arrange(date)

# Quick sanity check: annualized stats per meta-asset
cat("\n── Meta-Asset Summary Stats ─────────────────────────────────────────\n")
cluster_returns %>%
  pivot_longer(-date, names_to = "cluster", values_to = "return") %>%
  group_by(cluster) %>%
  summarise(
    ann_return = round((prod(1 + return, na.rm = TRUE)^(252/n()) - 1), 4),
    ann_vol    = round(sd(return, na.rm = TRUE) * sqrt(252), 4),
    sharpe     = round((mean(return, na.rm = TRUE) / sd(return, na.rm = TRUE)) * sqrt(252), 4),
    .groups    = "drop"
  ) %>%
  print()

# ── CUMULATIVE RETURN PLOT ────────────────────────────────────────────────────

cluster_returns %>%
  pivot_longer(-date, names_to = "cluster", values_to = "return") %>%
  group_by(cluster) %>%
  mutate(cum_return = cumprod(1 + return)) %>%
  ungroup() %>%
  ggplot(aes(x = date, y = cum_return, color = cluster)) +
  geom_line(linewidth = 1) +
  labs(
    title    = "Meta-Asset Cumulative Returns (Within-Cluster Inverse Vol Weighted)",
    subtitle = paste0("3-year window ending ", end_date),
    x        = NULL,
    y        = "Growth of $1",
    color    = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# ── EXPORT ───────────────────────────────────────────────────────────────────
# These two objects feed directly into the next step: between-cluster weighting

# weights_df      : symbol-level weights within each cluster
# cluster_returns : daily return series for each meta-asset
message("\nReady for between-cluster optimization:")
message("  weights_df      — ", nrow(weights_df), " symbols with within-cluster weights")
message("  cluster_returns — ", nrow(cluster_returns), " trading days x ", ncol(cluster_returns)-1, " meta-assets")
