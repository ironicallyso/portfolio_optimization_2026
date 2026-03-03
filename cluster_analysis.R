# === CLUSTER ANALYSIS: Asset Class Discovery ===
# Reads saved CSVs from portfolio_data/, computes a rolling 3-year correlation
# matrix, clusters assets hierarchically, and helps identify a cut point.
#
# Outputs:
#   1. Dendrogram with gap-based cut suggestion
#   2. Silhouette score plot across candidate cluster counts
#   3. Final cluster assignments printed to console

# --- Packages ---
# install.packages(c("tidyverse", "lubridate", "cluster", "factoextra", "ggdendro"))
library(tidyverse)
library(lubridate)
library(cluster)       # silhouette()
library(factoextra)    # fviz_silhouette()
library(ggdendro)      # ggdendrogram()

# ── CONFIG ────────────────────────────────────────────────────────────────────

data_dir <- "portfolio_data"   # folder containing your saved CSVs

symbols <- c("BND", "EES", "EEM", "GRID", "IGF", "IYR", "SPY", "TIP", "VGIT", "CMDY", "KRBN")

rolling_years  <- 3            # correlation window
anchor_date    <- NULL         # NULL = use most recent 3-year window;
# or set e.g. as.Date("2023-12-31") to pin end date

# ── LOAD DATA ─────────────────────────────────────────────────────────────────

portfolio_data <- map(symbols, function(sym) {
  path <- file.path(data_dir, paste0(sym, ".csv"))
  if (!file.exists(path)) stop("Missing file: ", path)
  read_csv(path, show_col_types = FALSE) %>%
    select(date, adj_close) %>%
    arrange(date)
})
names(portfolio_data) <- symbols

# ── BUILD RETURNS (full history, then window later) ───────────────────────────

returns_wide <- map(symbols, function(sym) {
  portfolio_data[[sym]] %>%
    mutate(return = adj_close / lag(adj_close) - 1) %>%
    filter(!is.na(return)) %>%
    select(date, return) %>%
    rename(!!sym := return)
}) %>%
  reduce(inner_join, by = "date") %>%   # inner_join = common dates only
  arrange(date)

# ── APPLY ROLLING 3-YEAR WINDOW ───────────────────────────────────────────────

end_date   <- if (!is.null(anchor_date)) anchor_date else max(returns_wide$date)
start_date <- end_date - years(rolling_years)

returns_window <- returns_wide %>%
  filter(date >= start_date, date <= end_date)

message("Clustering window: ", min(returns_window$date), " to ", max(returns_window$date))
message("Trading days in window: ", nrow(returns_window))

# ── CORRELATION & DISTANCE MATRIX ─────────────────────────────────────────────

cor_mat  <- cor(returns_window %>% select(-date), use = "complete.obs")
dist_mat <- as.dist(sqrt((1 - cor_mat) / 2))  # angular distance; range [0, 1]

cat("\nCorrelation matrix (window):\n")
print(round(cor_mat, 2))

# ── HIERARCHICAL CLUSTERING ───────────────────────────────────────────────────

hc <- hclust(dist_mat, method = "ward.D2")  # ward.D2: minimizes within-cluster variance

# --- Plot 1: Dendrogram ---
# Visual inspection: look for the tallest bar before the merge height jumps.
# That jump suggests a natural cut point.

par(mar = c(4, 4, 3, 1))
plot(hc,
     main  = paste0("Dendrogram — ", rolling_years, "-Year Rolling Window"),
     xlab  = "",
     sub   = "",
     ylab  = "Merge Height (Ward distance)",
     hang  = -1,
     cex   = 0.9)

# Overlay the suggested cut based on largest gap in merge heights
heights    <- hc$height
gaps       <- diff(heights)
best_cut_k <- length(heights) - which.max(rev(gaps)) + 1  # clusters at the biggest jump
cut_height <- heights[length(heights) - best_cut_k + 1] - 1e-6

abline(h    = cut_height,
       col  = "firebrick",
       lty  = 2,
       lwd  = 1.5)
text(x      = 0.5,
     y      = cut_height + 0.005,
     labels = paste0("Gap-suggested cut: k = ", best_cut_k),
     col    = "firebrick",
     adj    = 0,
     cex    = 0.8)

# ── SILHOUETTE ANALYSIS ───────────────────────────────────────────────────────
# For each candidate k, cut the dendrogram and compute silhouette score.
# Higher = better-defined clusters. Prefer k where score peaks.

k_range <- 2:min(7, length(symbols) - 1)

sil_scores <- map_dfr(k_range, function(k) {
  labels <- cutree(hc, k = k)
  sil    <- silhouette(labels, dist_mat)
  tibble(k = k, avg_silhouette = mean(sil[, "sil_width"]))
})

# --- Plot 2: Silhouette scores ---
best_sil_k <- sil_scores$k[which.max(sil_scores$avg_silhouette)]

sil_plot <- ggplot(sil_scores, aes(x = k, y = avg_silhouette)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(size = 3, color = "steelblue") +
  geom_vline(xintercept = best_sil_k, linetype = "dashed", color = "firebrick") +
  annotate("text",
           x      = best_sil_k + 0.15,
           y      = min(sil_scores$avg_silhouette),
           label  = paste0("Best k = ", best_sil_k),
           color  = "firebrick",
           hjust  = 0,
           size   = 3.5) +
  scale_x_continuous(breaks = k_range) +
  labs(title    = "Silhouette Score by Number of Clusters",
       subtitle = paste0(rolling_years, "-year window ending ", end_date),
       x        = "Number of clusters (k)",
       y        = "Average silhouette width") +
  theme_minimal()

print(sil_plot)

# ── FINAL CLUSTER ASSIGNMENTS ─────────────────────────────────────────────────
# We print both the gap-suggested and silhouette-suggested k.
# You decide which to use — or pick something in between.

cat("\n─────────────────────────────────────────\n")
cat("Gap-suggested k:        ", best_cut_k, "\n")
cat("Silhouette-suggested k: ", best_sil_k, "\n")
cat("─────────────────────────────────────────\n")

# Print assignments for a range of k values so you can compare
for (k in sort(unique(c(best_cut_k, best_sil_k, 2:min(5, length(symbols)-1))))) {
  cat("\nk =", k, "assignments:\n")
  labels <- cutree(hc, k = k)
  assignment_df <- tibble(symbol = names(labels), cluster = labels) %>%
    arrange(cluster, symbol)
  print(assignment_df)
}

# ── EXPORT cluster assignments for downstream use ─────────────────────────────
# Uses whichever k you prefer — default: silhouette suggestion
chosen_k   <- best_sil_k   # ← change this if you prefer a different k
cluster_assignments <- tibble(
  symbol  = names(cutree(hc, k = chosen_k)),
  cluster = cutree(hc, k = chosen_k)
) %>% arrange(cluster, symbol)

message("\nFinal assignments (k = ", chosen_k, "):")
print(cluster_assignments)

# cluster_assignments is available in your environment for the next step
# (within-cluster weighting)

