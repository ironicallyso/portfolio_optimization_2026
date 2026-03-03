# === WALK-FORWARD VALIDATION: Between-Cluster Weighting Methods ===
# Compares Equal Weight, ERC, and Minimum Variance on out-of-sample test windows.
#
# Design:
#   - Fixed cluster structure (k=4, economically grounded)
#   - 3-year training window → compute between-cluster weights
#   - 6-month test window   → evaluate realized performance
#   - Roll forward 6 months, repeat
#   - Within-cluster weights: inverse volatility (recomputed each training window)
#   - KRBN excluded (insufficient history); Commodities = CMDY only
#
# Packages:
# install.packages(c("tidyverse", "lubridate", "quadprog"))

library(tidyverse)
library(lubridate)
library(quadprog)   # for minimum variance optimization

# ── CONFIG ────────────────────────────────────────────────────────────────────

data_dir <- "portfolio_data"

# Fixed cluster assignments (k=4) — KRBN dropped
cluster_assignments <- tribble(
  ~symbol,  ~cluster, ~cluster_label,
  "BND",    1,        "Fixed_Income",
  "TIP",    1,        "Fixed_Income",
  "VGIT",   1,        "Fixed_Income",
  "IGF",    2,        "Real_Assets",
  "IYR",    2,        "Real_Assets",
  "EEM",    3,        "Equities",
  "EES",    3,        "Equities",
  "GRID",   3,        "Equities",
  "SPY",    3,        "Equities",
  "CMDY",   4,        "Commodities"
)

symbols        <- cluster_assignments$symbol
clusters       <- sort(unique(cluster_assignments$cluster))
cluster_labels <- cluster_assignments %>% distinct(cluster, cluster_label) %>% arrange(cluster)

train_years  <- 3
test_months  <- 6
rf_annual    <- 0.033
rf_daily     <- (1 + rf_annual)^(1/252) - 1

# ── LOAD DATA ─────────────────────────────────────────────────────────────────

portfolio_data <- map(symbols, function(sym) {
  path <- file.path(data_dir, paste0(sym, ".csv"))
  if (!file.exists(path)) stop("Missing file: ", path)
  read_csv(path, show_col_types = FALSE) %>%
    select(date, adj_close) %>%
    arrange(date)
})
names(portfolio_data) <- symbols

# Build aligned returns (inner join = common dates only)
returns_wide <- map(symbols, function(sym) {
  portfolio_data[[sym]] %>%
    mutate(return = adj_close / lag(adj_close) - 1) %>%
    filter(!is.na(return)) %>%
    select(date, return) %>%
    rename(!!sym := return)
}) %>%
  reduce(inner_join, by = "date") %>%
  arrange(date)

message("Full aligned history: ", min(returns_wide$date), " to ", max(returns_wide$date))
message("Total trading days:   ", nrow(returns_wide))

# ── FOLD DEFINITIONS ──────────────────────────────────────────────────────────

full_start <- min(returns_wide$date)
full_end   <- max(returns_wide$date)

# First test window starts after first training window
first_test_start <- full_start + years(train_years)
# Generate test window start dates rolling every 6 months
test_starts <- seq(first_test_start, full_end - months(test_months), by = "6 months")

folds <- map_dfr(test_starts, function(ts) {
  tibble(
    train_start = full_start,           # always from beginning — expanding window
    train_end   = ts - days(1),
    test_start  = ts,
    test_end    = min(ts + months(test_months) - days(1), full_end)
  )
}) %>%
  mutate(fold = row_number())

message("\nWalk-forward folds:")
print(folds %>% select(fold, train_start, train_end, test_start, test_end))
message("Note: KRBN excluded; ~", nrow(folds), " folds available given CMDY history.")

# ── WEIGHTING FUNCTIONS ───────────────────────────────────────────────────────

# Inverse-vol within-cluster weights (recomputed each training window)
get_within_weights <- function(returns_train) {
  cluster_assignments %>%
    left_join(
      returns_train %>%
        pivot_longer(-date, names_to = "symbol", values_to = "return") %>%
        group_by(symbol) %>%
        summarise(ann_vol = sd(return, na.rm = TRUE) * sqrt(252), .groups = "drop"),
      by = "symbol"
    ) %>%
    group_by(cluster) %>%
    mutate(within_wt = (1 / ann_vol) / sum(1 / ann_vol)) %>%
    ungroup()
}

# Compute meta-asset returns given within-cluster weights
get_meta_returns <- function(returns_df, within_weights) {
  wt_vec <- within_weights %>% select(symbol, within_wt) %>% deframe()
  
  cluster_labels %>%
    pmap(function(cluster, cluster_label) {
      syms <- cluster_assignments %>% filter(cluster == !!cluster) %>% pull(symbol)
      wts  <- wt_vec[syms]
      col  <- cluster_label
      
      returns_df %>%
        select(date, all_of(syms)) %>%
        rowwise() %>%
        mutate(!!col := sum(c_across(all_of(syms)) * wts, na.rm = TRUE)) %>%
        ungroup() %>%
        select(date, all_of(col))
    }) %>%
    reduce(inner_join, by = "date")
}

# --- Equal Weight ---
weights_equal <- function(meta_returns) {
  k <- ncol(meta_returns) - 1
  rep(1/k, k)
}

# --- Minimum Variance (quadprog) ---
weights_minvar <- function(meta_returns) {
  R   <- as.matrix(meta_returns %>% select(-date))
  Sigma <- cov(R) * 252
  k     <- ncol(R)
  
  # Solve: min w'Σw s.t. sum(w)=1, w>=0
  Dmat <- 2 * Sigma
  dvec <- rep(0, k)
  # Equality: sum(w) = 1
  Amat <- cbind(rep(1, k), diag(k))
  bvec <- c(1, rep(0, k))
  
  tryCatch({
    sol <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
    pmax(sol$solution, 0)  # numerical cleanup
  }, error = function(e) {
    warning("Min variance optimization failed, falling back to equal weight: ", e$message)
    rep(1/k, k)
  })
}

# --- ERC: Equal Risk Contribution ---
# Iterative algorithm: Newton-style update until risk contributions equalize
weights_erc <- function(meta_returns, tol = 1e-8, max_iter = 1000) {
  R     <- as.matrix(meta_returns %>% select(-date))
  Sigma <- cov(R) * 252
  k     <- ncol(R)
  
  # Start from inverse-vol
  vols <- sqrt(diag(Sigma))
  w    <- (1/vols) / sum(1/vols)
  
  for (i in seq_len(max_iter)) {
    port_vol  <- sqrt(as.numeric(t(w) %*% Sigma %*% w))
    mrc       <- as.numeric(Sigma %*% w) / port_vol   # marginal risk contribution
    rc        <- w * mrc                               # risk contribution per asset
    target_rc <- port_vol / k                          # equal target
    
    # Gradient step
    grad <- rc - target_rc
    if (max(abs(grad)) < tol) break
    w <- w - 0.05 * grad
    w <- pmax(w, 1e-6)      # keep positive
    w <- w / sum(w)         # renormalize
  }
  w
}

# ── WALK-FORWARD LOOP ─────────────────────────────────────────────────────────

methods <- c("Equal_Weight", "Min_Variance", "ERC")

fold_results <- map_dfr(folds$fold, function(f) {
  fold <- folds[f, ]
  message("Fold ", f, ": train to ", fold$train_end, " | test ", fold$test_start, " – ", fold$test_end)
  
  # Slice training and test data
  returns_train <- returns_wide %>% filter(date >= fold$train_start, date <= fold$train_end)
  returns_test  <- returns_wide %>% filter(date >= fold$test_start,  date <= fold$test_end)
  
  if (nrow(returns_train) < 60 || nrow(returns_test) < 5) {
    warning("Fold ", f, ": insufficient data, skipping.")
    return(NULL)
  }
  
  # Within-cluster weights from training data
  within_wts <- get_within_weights(returns_train)
  
  # Meta-asset returns for training (used to fit between-cluster weights)
  meta_train <- get_meta_returns(returns_train, within_wts)
  # Meta-asset returns for test (used to evaluate)
  meta_test  <- get_meta_returns(returns_test,  within_wts)
  
  # Compute between-cluster weights for each method
  wts <- list(
    Equal_Weight = weights_equal(meta_train),
    Min_Variance = weights_minvar(meta_train),
    ERC          = weights_erc(meta_train)
  )
  
  # Evaluate each method on test window
  fold_results <- list()
  
  for (f in folds$fold) {
    train_start <- folds$train_start[f]
    train_end   <- folds$train_end[f]
    test_start  <- folds$test_start[f]
    test_end    <- folds$test_end[f]
    
    message("Fold ", f, ": train to ", train_end, " | test ", test_start, " – ", test_end)
    
    returns_train <- returns_wide %>% filter(date >= train_start, date <= train_end)
    returns_test  <- returns_wide %>% filter(date >= test_start,  date <= test_end)
    
    if (nrow(returns_train) < 60 || nrow(returns_test) < 5) {
      warning("Fold ", f, ": insufficient data, skipping.")
      next
    }
    
    within_wts <- get_within_weights(returns_train)
    meta_train <- get_meta_returns(returns_train, within_wts)
    meta_test  <- get_meta_returns(returns_test,  within_wts)
    
    wts <- list(
      Equal_Weight = weights_equal(meta_train),
      Min_Variance = weights_minvar(meta_train),
      ERC          = weights_erc(meta_train)
    )
    
    for (method in methods) {
      w        <- wts[[method]]
      ret_mat  <- as.matrix(meta_test %>% select(-date))
      port_ret <- as.numeric(ret_mat %*% w)
      
      fold_results[[paste0(f, "_", method)]] <- tibble(
        fold        = f,
        method      = method,
        train_end   = train_end,
        test_start  = test_start,
        test_end    = test_end,
        date        = meta_test$date,
        port_return = port_ret
      )
    }
  }
  
  fold_results <- bind_rows(fold_results)
  
# ── AGGREGATE RESULTS ─────────────────────────────────────────────────────────

# Per-fold stats
fold_stats <- fold_results %>%
  group_by(fold, method, test_start, test_end) %>%
  summarise(
    ann_return   = round(prod(1 + port_return)^(252/n()) - 1, 4),
    ann_vol      = round(sd(port_return) * sqrt(252), 4),
    sharpe       = round(mean(port_return - rf_daily) / sd(port_return) * sqrt(252), 4),
    max_drawdown = round(1 - min(cumprod(1 + port_return) / cummax(cumprod(1 + port_return))), 4),
    .groups      = "drop"
  )

cat("\n── Per-Fold Sharpe Ratios ───────────────────────────────────────────\n")
fold_stats %>%
  select(fold, test_start, test_end, method, sharpe) %>%
  pivot_wider(names_from = method, values_from = sharpe) %>%
  print(n = Inf)

# Overall out-of-sample stats (concatenate all test periods)
cat("\n── Overall Out-of-Sample Stats ──────────────────────────────────────\n")
overall_stats <- fold_results %>%
  group_by(method) %>%
  summarise(
    n_folds      = n_distinct(fold),
    ann_return   = round(prod(1 + port_return)^(252/n()) - 1, 4),
    ann_vol      = round(sd(port_return) * sqrt(252), 4),
    sharpe       = round(mean(port_return - rf_daily) / sd(port_return) * sqrt(252), 4),
    max_drawdown = round(1 - min(cumprod(1 + port_return) / cummax(cumprod(1 + port_return))), 4),
    .groups      = "drop"
  ) %>%
  left_join(
    fold_stats %>%
      group_by(method) %>%
      summarise(avg_fold_sharpe = round(mean(sharpe), 4), .groups = "drop"),
    by = "method"
  )

print(overall_stats)

# ── CUMULATIVE RETURN PLOT ────────────────────────────────────────────────────

fold_results %>%
  arrange(method, date) %>%
  group_by(method) %>%
  mutate(cum_return = cumprod(1 + port_return)) %>%
  ungroup() %>%
  ggplot(aes(x = date, y = cum_return, color = method)) +
  geom_line(linewidth = 1) +
  geom_vline(
    data = folds,
    aes(xintercept = as.numeric(test_start)),
    linetype = "dotted", color = "gray50", alpha = 0.6
  ) +
  labs(
    title    = "Walk-Forward: Out-of-Sample Cumulative Returns",
    subtitle = paste0(train_years, "-year train | 6-month test | ", n_distinct(fold_results$fold), " folds"),
    x        = NULL,
    y        = "Growth of $1",
    color    = NULL,
    caption  = "Dotted lines = fold boundaries. KRBN excluded; Commodities = CMDY only."
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# ── FOLD SHARPE HEATMAP ───────────────────────────────────────────────────────

fold_stats %>%
  mutate(fold_label = paste0("Fold ", fold, "\n(", format(test_start, "%b %Y"), ")")) %>%
  ggplot(aes(x = fold_label, y = method, fill = sharpe)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(sharpe, 2)), size = 3.5) +
  scale_fill_gradient2(low = "firebrick", mid = "white", high = "steelblue", midpoint = 0) +
  labs(
    title = "Out-of-Sample Sharpe by Fold and Method",
    x     = NULL,
    y     = NULL,
    fill  = "Sharpe"
  ) +
  theme_minimal()
