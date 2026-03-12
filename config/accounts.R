# === PORTFOLIO CONFIGURATION ===
# All account-specific parameters live here.
# Scripts in analysis/ source this file — nothing is hardcoded elsewhere.

# --- Libraries ---
library(tidyverse)
library(lubridate)

# --- Shared parameters ---
RF_ANNUAL   <- 0.033
TRAIN_YEARS <- 3
TEST_MONTHS <- 6
DATA_DIR    <- "portfolio_data"
CACHE_DIR   <- "cache"

# --- Primary account ---
MAIN <- list(

  # Live symbols
  symbols_live = c("BND", "TIP", "VGIT",
                   "IGF", "VNQ",
                   "LCTU", "EEM", "EES", "GRID",
                   "CMDY", "KRBN"),

  # Backtest symbols (uses proxies or make exclusions for shorter history symbols)
  symbols_backtest = c("BND", "TIP", "VGIT",
                       "IGF", "IYR",
                       "SPY", "EEM", "EES", "GRID",
                       "DBC"),

  # Hardcoded k=4 cluster assignments
  clusters = tribble(
    ~symbol,  ~cluster, ~cluster_label,
    "BND",    1,        "Fixed_Income",
    "TIP",    1,        "Fixed_Income",
    "VGIT",   1,        "Fixed_Income",
    "IGF",    2,        "Real_Assets",
    "IYR",    2,        "Real_Assets",
    "VNQ",    2,        "Real_Assets",
    "SPY",    3,        "Equities",
    "LCTU",    3,        "Equities",
    "EEM",    3,        "Equities",
    "EES",    3,        "Equities",
    "GRID",   3,        "Equities",
    "CMDY",   4,        "Commodities",
    "KRBN",   4,        "Commodities",
    "DBC",    4,        "Commodities"
  ),

  # Target between-cluster weights
  target_weights = c(
    Equities     = 0.44,
    Fixed_Income = 0.35,
    Real_Assets  = 0.11,
    Commodities  = 0.10
  ),

  # Rebalancing
  rebal_freq      = "quarterly",
  rebal_threshold = 0.20        # proportional (20% relative drift)
)

# --- Kids account (placeholder) ---
KIDS <- list(
  # Live symbols
  symbols_live = c("VGIT",
                   "VNQ",
                   "LCTU", "EES"
                   ),
  
  # Backtest symbols (uses proxies or make exclusions for shorter history symbols)
  symbols_backtest = c("VGIT",
                       "VNQ",
                       "SPY", "EES"
                       ),
  
  # Hardcoded k=4 cluster assignments
  clusters = tribble(
    ~symbol,  ~cluster, ~cluster_label,
    "VGIT",   1,        "Fixed_Income",
    "VNQ",    2,        "Real_Assets",
    "SPY",    3,        "Equities",
    "LCTU",   3,        "Equities",    
    "EES",    3,        "Equities"
  ),
  
  # Target between-cluster weights
  target_weights = c(
    Equities     = 0.6,
    Fixed_Income = 0.35,
    Real_Assets  = 0.05
  )
)

# --- HSA account (placeholder) ---
# HSA <- list(...)

# --- Active account ---
ACCOUNT <- MAIN   # swap to KIDS or HSA as needed