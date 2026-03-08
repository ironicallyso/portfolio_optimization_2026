# === WEIGHTING FUNCTIONS ===
# Within-cluster and between-cluster weighting methods.
# Assumes cluster_assignments has columns: symbol, cluster, cluster_label

library(tidyverse)
library(quadprog)

# --- Within-cluster: inverse volatility ---
inv_vol_weights <- function(returns_wide, cluster_assignments) {
  vol_df <- returns_wide %>%
    pivot_longer(-date, names_to = "symbol", values_to = "return") %>%
    group_by(symbol) %>%
    summarise(ann_vol = sd(return, na.rm = TRUE) * sqrt(252), .groups = "drop")
  
  cluster_assignments %>%
    left_join(vol_df, by = "symbol") %>%
    group_by(cluster, cluster_label) %>%
    mutate(
      inv_vol   = 1 / ann_vol,
      within_wt = inv_vol / sum(inv_vol)
    ) %>%
    ungroup() %>%
    select(cluster, cluster_label, symbol, ann_vol, within_wt)
}

# --- Meta-asset returns (collapse each cluster to one return series) ---
meta_asset_returns <- function(returns_wide, weights_df, cluster_assignments) {
  wt_vec <- weights_df %>% select(symbol, within_wt) %>% deframe()
  
  cluster_assignments %>%
    distinct(cluster, cluster_label) %>%
    arrange(cluster) %>%
    pmap(function(cluster, cluster_label) {
      syms <- cluster_assignments %>%
        filter(cluster == !!cluster) %>%
        pull(symbol)
      wts     <- wt_vec[syms]
      col_name <- cluster_label
      
      returns_wide %>%
        select(date, all_of(syms)) %>%
        rowwise() %>%
        mutate(!!col_name := sum(c_across(all_of(syms)) * wts, na.rm = TRUE)) %>%
        ungroup() %>%
        select(date, all_of(col_name))
    }) %>%
    reduce(inner_join, by = "date") %>%
    arrange(date)
}

# --- Between-cluster: equal weight ---
weights_equal <- function(meta_returns) {
  k   <- ncol(meta_returns) - 1
  wts <- rep(1 / k, k)
  names(wts) <- names(meta_returns)[-1]
  wts
}

# --- Between-cluster: minimum variance ---
weights_minvar <- function(meta_returns) {
  R     <- as.matrix(meta_returns %>% select(-date))
  Sigma <- cov(R) * 252
  k     <- ncol(R)
  
  Dmat <- 2 * Sigma
  dvec <- rep(0, k)
  Amat <- cbind(rep(1, k), diag(k))
  bvec <- c(1, rep(0, k))
  
  tryCatch({
    sol      <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
    wts      <- pmax(sol$solution, 0)
    names(wts) <- colnames(R)
    wts
  }, error = function(e) {
    warning("Min variance failed, falling back to equal weight: ", e$message)
    weights_equal(meta_returns)
  })
}

# --- Between-cluster: equal risk contribution ---
weights_erc <- function(meta_returns, tol = 1e-8, max_iter = 1000) {
  R     <- as.matrix(meta_returns %>% select(-date))
  Sigma <- cov(R) * 252
  k     <- ncol(R)
  
  vols <- sqrt(diag(Sigma))
  w    <- (1 / vols) / sum(1 / vols)
  
  for (i in seq_len(max_iter)) {
    port_vol  <- sqrt(as.numeric(t(w) %*% Sigma %*% w))
    mrc       <- as.numeric(Sigma %*% w) / port_vol
    rc        <- w * mrc
    target_rc <- port_vol / k
    grad      <- rc - target_rc
    if (max(abs(grad)) < tol) break
    w <- w - 0.05 * grad
    w <- pmax(w, 1e-6)
    w <- w / sum(w)
  }
  names(w) <- colnames(R)
  w
}

# --- Full symbol-level weights (within * between) ---
full_symbol_weights <- function(weights_df, between_weights_vec) {
  weights_df %>%
    left_join(
      tibble(
        cluster_label = names(between_weights_vec),
        between_wt    = between_weights_vec
      ),
      by = "cluster_label"
    ) %>%
    mutate(final_wt = within_wt * between_wt) %>%
    select(cluster, cluster_label, symbol, within_wt, between_wt, final_wt)
}