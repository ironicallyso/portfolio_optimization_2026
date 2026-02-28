# === PROXY VALIDATION: DBC vs CMDY ===
# Download CMDY (just for comparison - not added to main symbols)
cmdy_raw <- getSymbols("CMDY", src = "yahoo", auto.assign = FALSE)
cmdy <- data.frame(
  date      = index(cmdy_raw),
  adj_close = as.numeric(Ad(cmdy_raw))
) %>%
  mutate(cmdy_return = adj_close / lag(adj_close) - 1) %>%
  filter(!is.na(cmdy_return))

# Pull DBC over the same window
dbc <- portfolio_data[["DBC"]] %>%
  filter(date >= min(cmdy$date)) %>%
  mutate(dbc_return = adj_close / lag(adj_close) - 1) %>%
  filter(!is.na(dbc_return))

# Join on shared dates
proxy_check <- inner_join(
  dbc %>% select(date, dbc_return),
  cmdy %>% select(date, cmdy_return),
  by = "date"
)

# Stats
correlation  <- cor(proxy_check$dbc_return, proxy_check$cmdy_return)
tracking_err <- sd(proxy_check$dbc_return - proxy_check$cmdy_return) * sqrt(252)

message("Overlap period: ", min(proxy_check$date), " to ", max(proxy_check$date))
message("N trading days: ", nrow(proxy_check))
message("Correlation:    ", round(correlation, 4))
message("Tracking error (annualized): ", round(tracking_err, 4))

# Cumulative return comparison plot
proxy_check %>%
  mutate(
    dbc_cum  = cumprod(1 + dbc_return),
    cmdy_cum = cumprod(1 + cmdy_return)
  ) %>%
  pivot_longer(cols = c(dbc_cum, cmdy_cum), names_to = "symbol", values_to = "cum_return") %>%
  ggplot(aes(x = date, y = cum_return, color = symbol)) +
  geom_line() +
  labs(title = "DBC vs CMDY: Cumulative Return (Overlap Period)",
       x = NULL, y = "Growth of $1") +
  theme_minimal()