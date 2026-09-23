# ==============================================================================
# 57b_state_construction_chart.R -> graphics/fig_state_construction_vs_jobs.png
# Companion to R/52: same state cross-section (ICE arrests per 100k vs. job
# growth, Jan 2025 - Jul 2026), but two panels side by side -- total nonfarm
# payroll growth, and construction payroll growth specifically, since
# construction is the sector a reference chart on this topic singled out.
# Built to answer "does restricting to construction change the picture,"
# not as a new research design: same DC-excluded-from-trend convention as
# R/52, same Texas-labeled-as-leverage-point convention as R/56/53.
# ==============================================================================
suppressMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2); library(scales)})

NAVY <- "#2c3254"; RED <- "#ff8361"; GREY <- "#8d8b7f"
my_style <- list(
  theme_minimal(base_size = 12),
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey88", linewidth = .3),
        plot.title = element_text(face = "bold", color = NAVY, size = 13),
        plot.title.position = "plot",
        plot.subtitle = element_text(color = NAVY, size = 9),
        plot.caption = element_text(color = "grey40", size = 8),
        legend.position = "none",
        axis.title = element_text(size = 9),
        strip.text = element_text(face = "bold", color = NAVY, size = 10)),
  coord_cartesian(clip = "off"))
W <- 11; H <- 5.4; DPI <- 200

base <- read_csv("output/state_enforcement_vs_jobs.csv", show_col_types = FALSE) %>%
  select(state_name, arrests_per_100k, `Total nonfarm` = job_growth)

con <- read_csv("data/sae_state_sector_growth.csv", show_col_types = FALSE) %>%
  filter(industry_name == "Construction") %>%
  select(state_name, `Construction` = growth)

m <- base %>% inner_join(con, by = "state_name")

label_states <- c("Texas", "Florida", "California", "New York", "District of Columbia")
m <- m %>% mutate(
  lbl = if_else(state_name %in% label_states,
                if_else(state_name == "District of Columbia", "DC", state.abb[match(state_name, state.name)]),
                NA_character_),
  is_dc = state_name == "District of Columbia")

long <- m %>% pivot_longer(c(`Total nonfarm`, `Construction`), names_to = "series", values_to = "growth") %>%
  mutate(series = factor(series, levels = c("Total nonfarm", "Construction")))

fit_one <- function(s) {
  d <- filter(long, series == s, !is_dc)
  ct <- cor.test(d$arrests_per_100k, d$growth)
  sprintf("%s: r = %.2f, n = %d, p = %.2f", s, ct$estimate, nrow(d), ct$p.value)
}
sub_line <- paste(sapply(levels(long$series), fit_one), collapse = "   |   ")
cat(sub_line, "\n")

p <- ggplot(long, aes(arrests_per_100k, growth)) +
  geom_smooth(data = filter(long, !is_dc), method = "lm", se = TRUE,
              color = RED, fill = RED, alpha = 0.15, linewidth = 1) +
  geom_point(aes(alpha = is_dc), color = NAVY, size = 2.2) +
  scale_alpha_manual(values = c(`TRUE` = 0.35, `FALSE` = 0.85)) +
  ggrepel::geom_text_repel(aes(label = lbl), size = 2.9, color = NAVY,
                            min.segment.length = 0, seed = 1) +
  facet_wrap(~series, scales = "free_y") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    title = "State Enforcement vs. Job Growth: Overall and Construction",
    subtitle = paste0("ICE arrests per 100,000 residents vs. payroll employment growth, Jan 2025 - Jul 2026.\n", sub_line, " (DC excluded from fit)."),
    x = "ICE arrests per 100,000 residents, cumulative over window",
    y = "Payroll employment growth (SA)",
    caption = paste0(
      "Source: Deportation Data Project (FOIA'd ICE arrest records); BLS State and Area Employment (SAE), seasonally adjusted.\n",
      "Construction was picked because it is the sector a reference chart on this topic highlighted, not because it fit best.\n",
      "Arrests are not deportations; correlation is not causation. Mike Konczal."
    )
  ) +
  my_style

ggsave("graphics/fig_state_construction_vs_jobs.png", p, width = W, height = H, dpi = DPI)
cat("Wrote graphics/fig_state_construction_vs_jobs.png\n")

# Texas-leverage check for both panels, printed for the record (not on the chart)
for (s in levels(long$series)) {
  d1 <- filter(long, series == s, !is_dc)
  d2 <- filter(d1, state_name != "Texas")
  ct1 <- cor.test(d1$arrests_per_100k, d1$growth)
  ct2 <- cor.test(d2$arrests_per_100k, d2$growth)
  cat(sprintf("%s: r=%.3f (n=%d) all-minus-DC | r=%.3f (n=%d) also minus Texas\n",
              s, ct1$estimate, nrow(d1), ct2$estimate, nrow(d2)))
}
cat("DONE 57b_state_construction_chart.R\n")
