# ==============================================================================
# 57_state_hypothesis_chart.R -> graphics/fig_state_hypothesis_scan.png
# Forest plot of all hypothesis-scan correlations (R/56), sorted by point
# estimate, colored by category, with 95% CI whiskers and a p<0.10 marker.
# ==============================================================================
suppressMessages({library(dplyr); library(readr); library(ggplot2); library(forcats)})

NAVY <- "#2c3254"; RED <- "#ff8361"; GREEN <- "#70ad8f"; GREY <- "#8d8b7f"; GOLD <- "#e0a23c"
my_style <- list(
  theme_minimal(base_size = 12),
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey88", linewidth = .3),
        plot.title = element_text(face = "bold", color = NAVY, size = 13),
        plot.title.position = "plot",
        plot.subtitle = element_text(color = NAVY, size = 9.5),
        plot.caption = element_text(color = "grey40", size = 8),
        legend.position = "bottom", legend.title = element_blank(),
        axis.title = element_text(size = 9)))

scan <- read_csv("output/state_hypothesis_scan.csv", show_col_types = FALSE) %>%
  filter(!is.na(r)) %>%
  mutate(label = fct_reorder(label, r),
         sig = p_value < 0.10)

CAT_COLORS <- c(
  "Labor demand" = NAVY, "Wages" = GOLD, "Labor supply" = GREEN,
  "Placebo sector" = GREY, "CPS: native outcomes" = "#5b6ea8",
  "CPS: foreign-born outcomes" = "#8c4fae", "Confound / baseline" = RED
)

p <- ggplot(scan, aes(r, label, color = category)) +
  geom_vline(xintercept = 0, color = "grey50", linewidth = .4) +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), height = 0, linewidth = .6, alpha = .55) +
  geom_point(aes(shape = sig), size = 2.6) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), guide = "none") +
  scale_color_manual(values = CAT_COLORS) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 3))) +
  labs(
    title = "Kicking the Tires: 23 State-Level Correlations With ICE Enforcement Intensity",
    subtitle = "Pearson r (95% CI), each outcome vs. arrests per 100k residents. Filled dot = p < 0.10. DC excluded throughout. n = 43-50 states.",
    x = "Correlation with ICE arrests per 100,000 residents",
    y = NULL,
    caption = "Sources: Deportation Data Project; BLS SAE and LAUS via tidyusmacro::getBLSFiles(); IPUMS CPS; Census Bureau. Mike Konczal."
  ) +
  my_style

ggsave("graphics/fig_state_hypothesis_scan.png", p, width = 12, height = 8.7, dpi = 200)
cat("Wrote graphics/fig_state_hypothesis_scan.png\n")
cat("DONE 57_state_hypothesis_chart.R\n")
