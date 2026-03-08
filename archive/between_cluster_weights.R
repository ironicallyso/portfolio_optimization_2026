# === BETWEEN-CLUSTER WEIGHTING: Sharpe vs Equal Weight ===
# Consumes outputs from cluster_weights.R:
#   - weights_df      : symbol-level within-cluster weights
#   - cluster_returns : daily meta-asset return series
#
# Outputs:
#   1. Cluster-level weights under both methods
#   2. Full symbol-level weights (within * between)
#   3. Cumulative return comparison plot
#   4. Summary stats table

library(tidyverse)
library(lubridate)

# ── ASSUMPTION: cluster_weights.R has already been sourced ────────────────────
# If running standalone, source it first:
# source("cluster_weights.R")

# ── BETWEEN-CLUSTER WEIGHTS ───────────────────────────────────────────────────

# Pull Sharpe from meta-asset stats
meta_stats <- cluster_returns %>%
  pivot_longer(-date, names_to = "cluster", values_to = "return") %>%
  group_by(cluster) %>%
  summarise(
    ann_return = prod(1 + return, na.rm = TRUE)^(252/n()) - 1,
    ann_vol    = sd(return, na.rm = TRUE) * sqrt(252),
    sharpe     = (mean(return, na.rm = TRUE) / sd(return, na.rm = TRUE)) * sqrt(252),
    .groups    = "drop"
  )

# Map cluster column names back to cluster numbers/labels
cluster_map <- cluster_assignments %>%
  distinct(cluster, cluster_label) %>%
  arrange(cluster) %>%
  mutate(col_name = paste0("cluster_", cluster, "_", gsub(" ", "_", cluster_label)))

meta_stats <- meta_stats %>%
  left_join(cluster_map, by = c("cluster" = "col_name"))

# Compute between-cluster weights
between_weights <- meta_stats %>%
  mutate(
    # Sharpe-weighted: normalize positive Sharpes
    sharpe_wt = pmax(sharpe, 0) / sum(pmax(sharpe, 0)),
    # Equal weight: 1/k per cluster
    equal_wt  = 1 / n()
  )

cat("\n── Between-Cluster Weights ──────────────────────────────────────────\n")
between_weights %>%
  select(cluster_label, sharpe, sharpe_wt, equal_wt) %>%
  mutate(
    sharpe    = round(sharpe, 3),
    sharpe_wt = scales::percent(sharpe_wt, accuracy = 0.1),
    equal_wt  = scales::percent(equal_wt,  accuracy = 0.1)
  ) %>%
  print()

# ── FULL SYMBOL-LEVEL WEIGHTS ─────────────────────────────────────────────────
# Final weight = within-cluster weight × between-cluster weight

make_full_weights <- function(between_col) {
  weights_df %>%
    left_join(
      between_weights %>% select(cluster_label, !!between_col),
      by = "cluster_label"
    ) %>%
    mutate(final_wt = within_wt * !!sym(between_col)) %>%
    select(cluster, cluster_label, symbol, within_wt, between_wt = !!between_col, final_wt)
}

full_weights_sharpe <- make_full_weights("sharpe_wt")
full_weights_equal  <- make_full_weights("equal_wt")

cat("\n── Full Symbol Weights: Sharpe-Weighted ─────────────────────────────\n")
full_weights_sharpe %>%
  mutate(across(c(within_wt, between_wt, final_wt), ~scales::percent(.x, accuracy = 0.1))) %>%
  arrange(cluster) %>%
  print(n = Inf)

cat("\n── Full Symbol Weights: Equal-Weighted ──────────────────────────────\n")
full_weights_equal %>%
  mutate(across(c(within_wt, between_wt, final_wt), ~scales::percent(.x, accuracy = 0.1))) %>%
  arrange(cluster) %>%
  print(n = Inf)

# ── PORTFOLIO RETURNS ─────────────────────────────────────────────────────────
# Apply between-cluster weights to meta-asset returns

compute_portfolio_returns <- function(between_col) {
  wts <- between_weights %>%
    select(col_name = cluster, wt = !!between_col) %>%
    deframe()
  
  cluster_returns %>%
    rowwise() %>%
    mutate(portfolio_return = sum(c_across(-date) * wts[colnames(cluster_returns)[-1]], na.rm = TRUE)) %>%
    ungroup() %>%
    select(date, portfolio_return)
}

port_sharpe <- compute_portfolio_returns("sharpe_wt") %>% rename(Sharpe_Weighted = portfolio_return)
port_equal  <- compute_portfolio_returns("equal_wt")  %>% rename(Equal_Weighted  = portfolio_return)

port_combined <- inner_join(port_sharpe, port_equal, by = "date")

# ── SUMMARY STATS ─────────────────────────────────────────────────────────────

rf_annual <- 0.033
rf_daily  <- (1 + rf_annual)^(1/252) - 1

port_stats <- port_combined %>%
  pivot_longer(-date, names_to = "method", values_to = "return") %>%
  group_by(method) %>%
  summarise(
    ann_return   = round(prod(1 + return, na.rm = TRUE)^(252/n()) - 1, 4),
    ann_vol      = round(sd(return, na.rm = TRUE) * sqrt(252), 4),
    sharpe       = round((mean(return - rf_daily, na.rm = TRUE) / sd(return, na.rm = TRUE)) * sqrt(252), 4),
    max_drawdown = round(1 - min(cumprod(1 + return) / cummax(cumprod(1 + return))), 4),
    .groups      = "drop"
  )

cat("\n── Portfolio Summary Stats ───────────────────────────────────────────\n")
print(port_stats)

# ── CUMULATIVE RETURN PLOT ────────────────────────────────────────────────────

port_combined %>%
  pivot_longer(-date, names_to = "method", values_to = "return") %>%
  group_by(method) %>%
  mutate(cum_return = cumprod(1 + return)) %>%
  ungroup() %>%
  ggplot(aes(x = date, y = cum_return, color = method)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("Sharpe_Weighted" = "steelblue", "Equal_Weighted" = "firebrick")) +
  labs(
    title    = "Portfolio Cumulative Return: Sharpe-Weighted vs Equal-Weighted",
    subtitle = paste0("3-year window ending ", end_date),
    x        = NULL,
    y        = "Growth of $1",
    color    = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
