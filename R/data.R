# === DATA FUNCTIONS ===
# Handles downloading, updating, loading, and aligning returns.
# No hardcoded symbols or paths — all passed as arguments.

library(quantmod)
library(tidyverse)
library(lubridate)

# Download or incrementally update a single symbol's CSV cache
download_or_update <- function(symbol, data_dir, start_date = "1990-01-01") {
  file_path <- file.path(data_dir, paste0(symbol, ".csv"))
  
  if (file.exists(file_path)) {
    existing  <- read_csv(file_path, show_col_types = FALSE)
    last_date <- max(existing$date)
    fetch_from <- last_date + days(1)
    
    if (fetch_from >= Sys.Date()) {
      message(symbol, ": already up to date (", last_date, ")")
      return(existing)
    }
    message(symbol, ": updating from ", fetch_from)
  } else {
    existing   <- NULL
    fetch_from <- as.Date(start_date)
    message(symbol, ": fresh download from ", fetch_from)
  }
  
  tryCatch({
    raw <- getSymbols(symbol, src = "yahoo",
                      from = fetch_from, to = Sys.Date(),
                      auto.assign = FALSE)
    
    new_data <- data.frame(
      date      = index(raw),
      open      = as.numeric(Op(raw)),
      high      = as.numeric(Hi(raw)),
      low       = as.numeric(Lo(raw)),
      close     = as.numeric(Cl(raw)),
      volume    = as.numeric(Vo(raw)),
      adj_close = as.numeric(Ad(raw))
    )
    
    combined <- bind_rows(existing, new_data) %>%
      distinct(date, .keep_all = TRUE) %>%
      arrange(date)
    
    write_csv(combined, file_path)
    return(combined)
    
  }, error = function(e) {
    warning("Failed to download ", symbol, ": ", e$message)
    return(existing)
  })
}

# Load saved CSVs for a vector of symbols
load_prices <- function(symbols, data_dir) {
  data <- map(symbols, function(sym) {
    path <- file.path(data_dir, paste0(sym, ".csv"))
    if (!file.exists(path)) stop("Missing file: ", path)
    read_csv(path, show_col_types = FALSE) %>%
      select(date, adj_close) %>%
      arrange(date)
  })
  names(data) <- symbols
  data
}

# Convert price list to aligned wide returns data frame
build_returns <- function(price_list) {
  map(names(price_list), function(sym) {
    price_list[[sym]] %>%
      mutate(return = adj_close / lag(adj_close) - 1) %>%
      filter(!is.na(return)) %>%
      select(date, return) %>%
      rename(!!sym := return)
  }) %>%
    reduce(inner_join, by = "date") %>%
    arrange(date)
}

# Print coverage summary for a returns data frame
print_coverage <- function(returns_wide) {
  symbols <- setdiff(names(returns_wide), "date")
  map_dfr(symbols, function(sym) {
    tibble(
      symbol     = sym,
      first_date = min(returns_wide$date),
      last_date  = max(returns_wide$date),
      n_days     = nrow(returns_wide)
    )
  }) %>% print()
  
  message("Common window: ", min(returns_wide$date), " to ", max(returns_wide$date))
}