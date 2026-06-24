# Generate the optimal-threshold c* intuition figure for slides02.
# c* = C(FP) / (C(FP) + C(FN)) = 1 / (1 + ratio),  ratio = C(FN) / C(FP).
#
# Run from chapter dir:  Rscript rsrc/make_threshold_cstar_intuition.R

library(ggplot2)

# Armenian flag colors
BLUE <- "#0033A0"
RED  <- "#D90012"

# Smooth curve
df_curve <- data.frame(
  ratio = seq(0.1, 20, length.out = 500)
)
df_curve$c_star <- 1 / (1 + df_curve$ratio)

# Example points to mark
df_pts <- data.frame(ratio = c(1, 2, 4, 10))
df_pts$c_star <- 1 / (1 + df_pts$ratio)
df_pts$label  <- sprintf("FN = %dx FP\nc* = %.2f", df_pts$ratio, df_pts$c_star)

p <- ggplot() +
  geom_line(data = df_curve, aes(x = ratio, y = c_star),
            color = BLUE, linewidth = 1.1) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray60") +
  annotate("text", x = 14, y = 0.525, label = "default threshold 0.5",
           color = "gray40", size = 3.2, hjust = 0) +
  geom_point(data = df_pts, aes(x = ratio, y = c_star),
             color = RED, size = 3.2) +
  geom_text(data = df_pts, aes(x = ratio, y = c_star, label = label),
            color = RED, size = 3.0, hjust = -0.1, vjust = -0.3,
            lineheight = 0.85) +
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, 2.5)) +
  scale_y_continuous(limits = c(0, 0.6)) +
  labs(
    x = "cost ratio  cost(FN) / cost(FP)",
    y = expression("optimal threshold " * italic(c) * "*"),
    title = expression(italic(c) * "* = " * C(FP) / (C(FP) + C(FN)) *
                       ":  higher FN cost  " %->% "  lower threshold")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 11, hjust = 0),
    panel.grid.minor = element_blank()
  )

out_path <- "figure/threshold_cstar_intuition.pdf"
ggsave(out_path, plot = p, width = 7, height = 4, units = "in")
cat("Saved:", normalizePath(out_path), "\n")
