# === 01: DATA ===
# Downloads and updates price CSVs for all symbols.
# Re-run whenever you want fresh prices.
# Dependency: none
# Output: portfolio_data/ CSVs (not cached as .rds — raw data lives in portfolio_data/)

source("config/accounts.R")
source("R/data.R")

# Download/update all unique symbols across live and backtest sets
all_symbols <- unique(c(ACCOUNT$symbols_live, ACCOUNT$symbols_backtest))

walk(all_symbols, ~download_or_update(.x, DATA_DIR))

message("Done. CSVs updated in ", DATA_DIR, "/")