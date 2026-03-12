# === SWEEP FUNCTIONS ===
# Sensitivity sweep across target weights.
# run_pairwise_sweep() is the generalized function — works for any number of clusters.
# run_all_pairwise_sweeps() runs every combination of fixed pairs automatically.

library(tidyverse)

# ── PERFORMANCE HELPER ────────────────────────────────────────────────────────

calc_port_stats <- function(meta_returns, weights_vec, rf_annual = 0.033) {
  rf_daily <- (1 + rf_annual)^(1/252) - 1
  ret_mat  <- as.matrix(meta_returns %>% select(-date))
  
  # Align weights to column order
  weights_vec <- weights_vec[colnames(ret_mat)]
  
  port_ret     <- as.numeric(ret_mat %*% weights_vec)
  n            <- length(port_ret)
  ann_return   <- prod(1 + port_ret)^(252/n) - 1
  ann_vol      <- sd(port_ret) * sqrt(252)
  sharpe       <- mean(port_ret - rf_daily) / sd(port_ret) * sqrt(252)
  cum          <- cumprod(1 + port_ret)
  max_drawdown <- 1 - min(cum / cummax(cum))

  tibble(
    ann_return   = round(ann_return,   4),
    ann_vol      = round(ann_vol,      4),
    sharpe       = round(sharpe,       4),
    max_drawdown = round(max_drawdown, 4),
    calmar       = round(ann_return / max_drawdown, 4)
  )
}

# ── PAIRWISE SWEEP ────────────────────────────────────────────────────────────
# Fix any subset of cluster weights; sweep the split of the remainder
# across the remaining clusters.
#
# Arguments:
#   meta_returns : data frame with date + one column per cluster
#   fixed        : named numeric vector of clusters to hold constant
#                  e.g. c(Fixed_Income = 0.25, Commodities = 0.10)
#   sweep_steps  : number of steps across the remainder split (default 19 = 5% increments)
#   rf_annual    : risk-free rate
#
# Works for any number of clusters. With 2 free clusters: 1D sweep of their split.
# With 3+ free clusters: splits remainder equally among all but the first free cluster,
# sweeping only the first free cluster's share (a simplification — use fixed to
# narrow down if you need finer control).

run_pairwise_sweep <- function(meta_returns,
                               fixed,
                               sweep_steps = 19,
                               rf_annual   = 0.033) {
  all_clusters  <- names(meta_returns %>% select(-date))
  fixed_clusters <- names(fixed)
  free_clusters  <- setdiff(all_clusters, fixed_clusters)

  missing <- setdiff(fixed_clusters, all_clusters)
  if (length(missing) > 0) stop("Fixed clusters not in meta_returns: ", paste(missing, collapse = ", "))

  remainder <- 1 - sum(fixed)
  if (remainder < 0 || remainder > 1) stop("Fixed weights sum to ", sum(fixed), " — must be between 0 and 1")
  if (length(free_clusters) == 0) stop("No free clusters to sweep — reduce the number of fixed clusters")
  if (length(free_clusters) == 1) {
    # Fully determined — no sweep needed, just evaluate the single point
    wts <- c(fixed, setNames(remainder, free_clusters))
    return(bind_cols(
      as_tibble(as.list(wts)),
      calc_port_stats(meta_returns, wts, rf_annual)
    ))
  }

  # 1D sweep: vary share of remainder going to free_clusters[1]
  # Remaining free clusters split the rest equally
  n_free     <- length(free_clusters)
  free1      <- free_clusters[1]
  free_rest  <- free_clusters[-1]

  # free1 share ranges from ~0 to ~1 of remainder, in sweep_steps increments
  free1_shares <- seq(0.05, 0.95, length.out = sweep_steps)

  map_dfr(free1_shares, function(s) {
    free1_wt   <- remainder * s
    rest_wt    <- remainder * (1 - s) / length(free_rest)
    free_wts   <- c(setNames(free1_wt, free1),
                    setNames(rep(rest_wt, length(free_rest)), free_rest))
    wts        <- c(fixed, free_wts)

    bind_cols(
      as_tibble(as.list(round(wts, 4))),
      calc_port_stats(meta_returns, wts, rf_annual)
    )
  })
}

# ── RUN ALL PAIRWISE COMBINATIONS ─────────────────────────────────────────────
# Automatically generates every combination of fixed pairs from target_weights,
# runs a sweep for each, and returns results labeled by which pair was fixed.
#
# Arguments:
#   meta_returns   : data frame with date + one column per cluster
#   target_weights : named numeric vector (from ACCOUNT$target_weights)
#   sweep_steps    : passed through to run_pairwise_sweep()
#   rf_annual      : risk-free rate

run_all_pairwise_sweeps <- function(meta_returns,
                                    target_weights,
                                    sweep_steps = 19,
                                    rf_annual   = 0.033) {
  all_clusters <- names(meta_returns %>% select(-date))

  # All combinations of n-2 clusters to fix
  fixed_pairs <- combn(all_clusters, length(all_clusters) - 2, simplify = FALSE)

  map_dfr(fixed_pairs, function(pair) {
    fixed <- target_weights[pair]
    label <- paste0("Fix: ", paste(pair, collapse = " + "))

    message("  -> ", label)

    run_pairwise_sweep(
      meta_returns = meta_returns,
      fixed        = fixed,
      sweep_steps  = sweep_steps,
      rf_annual    = rf_annual
    ) %>%
      mutate(fixed_pair = label)
  })
}
