# ==============================================================================
# 57c_state_hypothesis_chart_p05.R -> graphics/fig_state_hypothesis_scan_p05.png
# Same forest plot as R/57, same underlying data and correlations -- only the
# "filled dot" significance threshold changes, from p < 0.10 to the more
# conventional p < 0.05, per request. No re-estimation, just a stricter mark.
# ==============================================================================
suppressMessages({library(dplyr); library(readr); library(ggplot2); library(forcats)})
source("R/00_blog_style.R")

NAVY <- "#2c3254"; RED <- "#ff8361"; GREEN <- "#70ad8f"; GREY <- "#8d8b7f"; GOLD <- "#e0a23c"
NAVY <- BLOG_NAVY; RED <- BLOG_RED; GREEN <- BLOG_GREEN; GREY <- BLOG_GREY; GOLD <- BLOG_GOLD
my_style <- list(blog_theme,
  theme(panel.grid.major.y = element_blank(), legend.position = "bottom",
        axis.text.y = element_text(size = 8), legend.text = element_text(size = 8),
        legend.location = "plot"))

scan <- read_csv("output/state_hypothesis_scan.csv", show_col_types = FALSE) %>%
  filter(!is.na(r)) %>%
  mutate(label = fct_reorder(label, r),
         sig = p_value < 0.05)

n_sig <- sum(scan$sig)
cat("Rows significant at p < 0.05:", n_sig, "of", nrow(scan), "\n")
print(scan %>% filter(sig) %>% select(label, r, p_value))

CAT_COLORS <- c(
  "Labor demand" = NAVY, "Wages" = GOLD, "Labor supply" = GREEN,
  "Placebo sector" = GREY, "CPS: native outcomes" = "#5b6ea8",
  "CPS: foreign-born outcomes" = "#8c4fae", "Confound / baseline" = RED
)

p <- ggplot(scan, aes(r, label, color = category)) +
  geom_vline(xintercept = 0, color = "grey50", linewidth = .4) +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), height = 0, linewidth = .6, alpha = .55) +
  geom_point(aes(shape = sig), size = 2) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), guide = "none") +
  scale_color_manual(values = CAT_COLORS) +
  guides(color = guide_legend(nrow = 3, byrow = TRUE, override.aes = list(size = 3))) +
  labs(
    title = hyp_title(12, "23 State-Level Correlations With ICE Enforcement"),
    subtitle = "Pearson r (95% CI), each outcome vs. ICE arrests per 100k residents. Filled dot = p < 0.05.\nDC excluded throughout. n = 43-50 states.",
    x = "Correlation with ICE arrests per 100,000 residents",
    y = NULL,
    caption = "Sources: Deportation Data Project; BLS SAE and LAUS; IPUMS CPS; Census Bureau. Mike Konczal."
  ) +
  my_style

blog_save("graphics/fig_state_hypothesis_scan_p05.png", p, height = 7.2)
cat("Wrote graphics/fig_state_hypothesis_scan_p05.png\n")
cat("DONE 57c_state_hypothesis_chart_p05.R\n")
