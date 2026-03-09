# Portfolio Optimization Framework

A hierarchical portfolio optimization framework in R, implementing a two-level
cluster-based weighting and rebalancing engine across multiple accounts.

---

## Overview

This framework builds and validates a portfolio allocation strategy using
hierarchical clustering to group assets into synthetic asset classes, then
applies a two-level weighting and rebalancing system within and between those
clusters.

The primary portfolio uses four clusters:

| Cluster | Symbols |
|---|---|
| Fixed Income | BND, TIP, VGIT |
| Real Assets | IGF, IYR |
| Equities | SPY, EEM, EES, GRID |
| Commodities | CMDY (live), DBC (backtest) |

---

## Project Structure

```
R/                  # Shared function library
  data.R            # Download, load, and align price data
  weights.R         # Within- and between-cluster weighting functions
  backtest.R        # Two-level rebalancing engine
  sweep.R           # Pairwise weight sensitivity sweep

analysis/           # Execution scripts (run in order)
  01_data.R         # Download/update price CSVs
  02_walk_forward.R # Out-of-sample validation across weighting methods
  03_sweep.R        # Pairwise target weight sensitivity sweep
  04_weights_final.R# Compute final live symbol weights
  05_rebalancing.R  # Rebalancing methodology sweep and calibration

config/
  accounts.R        # All account-specific parameters (symbols, weights, thresholds)

tests/
  test_backtest.R   # Unit tests (testthat)

tools/              # Standalone exploratory scripts
archive/            # Prior implementations retained for reference
portfolio_data/     # Downloaded price CSVs (gitignored)
cache/              # .rds output files (gitignored)
```

---

## Methodology

**Clustering:** Hierarchical agglomerative clustering (Ward.D2 linkage) on a
rolling 3-year correlation matrix using angular distance. Cluster count k=4
selected on dendrogram and economic interpretability.

**Within-cluster weights:** Inverse volatility. Assets within a cluster are
highly correlated, so risk-based weighting is appropriate.

**Between-cluster weights:** Equal weight method, validated via walk-forward
out-of-sample testing against ERC (Equal Risk Contribution) and Minimum Variance alternatives. Target
allocations per cluster are configurable in `config/accounts.R` and subject
to ongoing calibration via the pairwise sensitivity sweep.

**Rebalancing:** Two-level engine — inter-cluster triggers on a quarterly
calendar (Monday after triple-witching Friday) and/or a proportional drift
threshold. Intra-cluster weights reset only when inter-cluster rebalancing
fires.

---

## How to Run

1. Open `portfolio_optimization_2026.Rproj` in RStudio.
2. Install dependencies (see below).
3. Run scripts in order from `analysis/`:

```r
source("analysis/01_data.R")         # downloads price data
source("analysis/02_walk_forward.R") # walk-forward validation
source("analysis/03_sweep.R")        # weight sensitivity sweep
source("analysis/04_weights_final.R")# final live weights
source("analysis/05_rebalancing.R")  # rebalancing calibration
```

All account parameters are controlled from `config/accounts.R`. No hardcoded
values appear in the analysis scripts.

---

## Dependencies

```r
install.packages(c(
  "tidyverse",
  "lubridate",
  "quantmod",
  "xts",
  "quadprog",
  "cluster",
  "factoextra",
  "ggdendro",
  "PerformanceAnalytics",
  "scales",
  "testthat"
))
```

---

## Running Tests

```r
testthat::test_file("tests/test_backtest.R")
```

---

## Disclaimers

**For personal research and educational use only.**

- This framework and all outputs are provided **as-is**, with no warranty of
  any kind, express or implied.
- Nothing in this repository constitutes financial, investment, or legal advice.
- No content here is a recommendation to buy, sell, or hold any specific
  security, ETF, or asset class.
- Past performance of any backtest or simulation does not guarantee future
  results. Backtests are subject to look-ahead bias, survivorship bias, and
  overfitting, and are not indicative of live trading results.
- The author is not a registered investment advisor, broker-dealer, or
  financial planner.
- All investment decisions carry risk, including the possible loss of principal.
  You are solely responsible for any investment decisions you make.
- Any ETF tickers or symbols mentioned are used for illustrative and analytical
  purposes only and do not constitute endorsement or recommendation of those
  securities.

---

## License

This project is for personal use. No license is granted for redistribution or
commercial use.
