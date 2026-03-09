# === SWEEP FUNCTIONS ===
# Sensitivity sweep across target weights.
# Evaluates in-sample performance across FI/Commodity weight combinations.

library(tidyverse)

# Run sweep across FI and Commodity target weight combinations.
# Remainder split between Equities and Real Assets equally (default)
# or at a specified equity share via eq_share argument.
run_sweep <- function(meta_returns,
                      fi_targets   = c(0.05, 0.10, 0.15, 0.20, 0.25),
                      comm_targets = c(0.05, 0.10, 0.15, 0.20, 0.25),
                      rf_annual    = 0.033) {
  
  rf_daily <- (1 + rf_annual)^(1/252) - 1
  
  calc_stats <- function(meta_returns, weights_vec) {
    ret_mat  <- as.matrix(meta_returns %>% select(-date))
    port_ret <- as.numeric(ret_mat %*% weights_vec)
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
  
  expand_grid(fi_wt = fi_targets, comm_wt = comm_targets) %>%
    filter(fi_wt + comm_wt <= 0.90) %>%
    mutate(eq_wt = (1 - fi_wt - comm_wt) / 2,
           ra_wt = (1 - fi_wt - comm_wt) / 2) %>%
    pmap_dfr(function(fi_wt, comm_wt, eq_wt, ra_wt) {
      wts <- c(Fixed_Income = fi_wt, Real_Assets = ra_wt,
               Equities = eq_wt, Commodities = comm_wt)
      wts <- wts[names(meta_returns %>% select(-date))]
      bind_cols(
        tibble(fi_wt = fi_wt, comm_wt = comm_wt, eq_wt = eq_wt, ra_wt = ra_wt),
        calc_stats(meta_returns, wts)
      )
    }) %>%
    mutate(calmar = round(ann_return / max_drawdown, 4))
}

# Equity vs Real Assets sweep at fixed FI and Commodity weights
run_era_sweep <- function(meta_returns,
                          fi_wt    = 0.25,
                          comm_wt  = 0.05,
                          rf_annual = 0.033) {
  
  remainder  <- 1 - fi_wt - comm_wt
  eq_splits  <- seq(0.10, remainder, by = 0.05)
  
  run_sweep(
    meta_returns = meta_returns,
    fi_targets   = fi_wt,
    comm_targets = comm_wt,
    rf_annual    = rf_annual
  )
}

# Heatmap plot helper
plot_sweep_heatmap <- function(sweep_results, metric = "sharpe") {
  pct_factor <- function(x) {
    factor(scales::percent(x, accuracy = 1),
           levels = scales::percent(sort(unique(x)), accuracy = 1))
  }
  
  label_fn <- if (metric %in% c("ann_return", "ann_vol", "max_drawdown")) {
    function(x) scales::percent(x, accuracy = 0.1)
  } else {
    function(x) round(x, 2)
  }
  
  sweep_results %>%
    mutate(
      fi_label   = pct_factor(fi_wt),
      comm_label = pct_factor(comm_wt),
      value      = .data[[metric]]
    ) %>%
    ggplot(aes(x = comm_label, y = fi_label, fill = value)) +
    geom_tile(color = "white") +
    geom_text(aes(label = label_fn(value)), size = 3.5) +
    scale_fill_gradient2(low = "firebrick", mid = "white", high = "steelblue",
                         midpoint = median(sweep_results[[metric]])) +
    labs(
      title = paste0("Sweep: ", metric),
      x     = "Commodities Target Weight",
      y     = "Fixed Income Target Weight",
      fill  = metric
    ) +
    theme_minimal()
}