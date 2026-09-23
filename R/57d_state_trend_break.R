# ==============================================================================
# 57d_state_trend_break.R -> graphics/fig_state_trend_break.png
# Direct answer to: "isn't this just high-immigrant states growing faster
# already?" Instead of correlating enforcement intensity with the LEVEL of
# job growth during the enforcement window, difference each state against
# its own pre-period trend: pre-period growth (Jul 2023 - Jan 2025, 18
# months) vs. post-period growth (Jan 2025 - Jul 2026, 18 months, matched
# length). The outcome is the change in growth rate -- acceleration or
# deceleration relative to the state's own trajectory -- which nets out any
# state-level fixed difference (e.g. Texas/Florida just growing faster on
# every margin for reasons that have nothing to do with 2025 enforcement).
# Same convention as the 2018-2019 placebo used elsewhere in this project.
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

PRE_START  <- as.Date("2023-07-01"); PRE_END  <- as.Date("2025-01-01")   # 18 months
POST_START <- as.Date("2025-01-01"); POST_END <- as.Date("2026-07-01")  # 18 months

sae <- readRDS("data_raw/sae_raw.rds")

growth_for <- function(ind) {
  d <- sae %>% filter(industry_name == ind, data_type_text == "All Employees, In Thousands",
                       seasonal_text == "Seasonally Adjusted", area_code == "00000",
                       state_name %in% state.name,
                       date %in% c(PRE_START, PRE_END, POST_START, POST_END)) %>%
    select(state_name, date, value) %>%
    pivot_wider(names_from = date, values_from = value)
  names(d) <- c("state_name", "pre_start", "pre_end_post_start", "post_end")
  # PRE_END == POST_START (2025-01-01), so pivot_wider collapses them into one column
  d %>% transmute(state_name,
                  pre_growth  = pre_end_post_start / pre_start - 1,
                  post_growth = post_end / pre_end_post_start - 1,
                  diff        = post_growth - pre_growth)
}

nf  <- growth_for("Total Nonfarm") %>% rename_with(~paste0(.x, "_nf"), -state_name)
con <- growth_for("Construction")  %>% rename_with(~paste0(.x, "_con"), -state_name)

ice <- read_csv("output/state_enforcement_vs_jobs.csv", show_col_types = FALSE) %>%
  select(state_name, arrests_per_100k)

m <- ice %>% inner_join(nf, by = "state_name") %>% inner_join(con, by = "state_name")
cat("Merged N =", nrow(m), "states (no DC in state.name join)\n")

label_states <- c("Texas", "Florida", "California", "New York")
m <- m %>% mutate(lbl = if_else(state_name %in% label_states, state.abb[match(state_name, state.name)], NA_character_))

long <- m %>%
  select(state_name, arrests_per_100k, lbl,
         `Total nonfarm` = diff_nf, `Construction` = diff_con) %>%
  pivot_longer(c(`Total nonfarm`, `Construction`), names_to = "series", values_to = "diff") %>%
  mutate(series = factor(series, levels = c("Total nonfarm", "Construction")))

fit_one <- function(s, excl_tx = FALSE) {
  d <- filter(long, series == s)
  if (excl_tx) d <- filter(d, state_name != "Texas")
  ct <- cor.test(d$arrests_per_100k, d$diff)
  tibble(series = s, excl_texas = excl_tx, n = nrow(d), r = unname(ct$estimate),
         ci_lo = ct$conf.int[1], ci_hi = ct$conf.int[2], p_value = ct$p.value)
}
res <- bind_rows(
  fit_one("Total nonfarm", FALSE), fit_one("Total nonfarm", TRUE),
  fit_one("Construction", FALSE),  fit_one("Construction", TRUE)
)
cat("\n=== Correlation: ICE arrests per 100k vs. CHANGE in growth rate (post-period minus pre-period) ===\n")
print(res %>% mutate(across(c(r, ci_lo, ci_hi, p_value), ~round(.x, 4))))
write_csv(res, "output/state_trend_break.csv")

sub_line <- res %>% filter(!excl_texas) %>%
  mutate(lab = sprintf("%s: r = %.2f, n = %d, p = %.2f", series, r, n, p_value)) %>%
  pull(lab) %>% paste(collapse = "   |   ")

p <- ggplot(long, aes(arrests_per_100k, diff)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = .3) +
  geom_smooth(method = "lm", se = TRUE, color = RED, fill = RED, alpha = 0.15, linewidth = 1) +
  geom_point(color = NAVY, size = 2.2, alpha = .85) +
  ggrepel::geom_text_repel(aes(label = lbl), size = 2.9, color = NAVY,
                            min.segment.length = 0, seed = 1) +
  facet_wrap(~series, scales = "free_y") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    title = "Did Enforcement Line Up With an Acceleration From Each State's Own Trend?",
    subtitle = paste0("Change in state payroll growth rate, post-period (Jan 2025-Jul 2026) minus pre-period\n",
                      "(Jul 2023-Jan 2025, matched 18-month windows), vs. ICE arrests per 100,000 residents.\n", sub_line),
    x = "ICE arrests per 100,000 residents, cumulative Jan 2025-Jul 2026",
    y = "Change in growth rate (post minus pre)",
    caption = paste0(
      "Source: BLS State and Area Employment (SAE), seasonally adjusted; Deportation Data Project.\n",
      "This nets out each state's own pre-existing growth trend -- the test the level comparison in the companion chart cannot do.\n",
      "Arrests are not deportations; correlation is not causation. Mike Konczal."
    )
  ) +
  my_style

ggsave("graphics/fig_state_trend_break.png", p, width = W, height = H, dpi = DPI)
cat("\nWrote graphics/fig_state_trend_break.png\n")
cat("DONE 57d_state_trend_break.R\n")
