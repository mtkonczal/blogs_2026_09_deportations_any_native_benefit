# ==============================================================================
# 58_blog_post_charts.R  -> three new graphics needed for blog_post.md
# H3 (native men prime-age EPOP), H5 (native p10 real wage level, single panel),
# H9 (EPOP by citizen generation vs. foreign-born). Everything else in the post
# reuses an existing figure from graphics/ as-is.
# ==============================================================================
suppressMessages({library(tidyverse); library(lubridate); library(scales)})
dir.create("graphics", showWarnings = FALSE)
NAVY <- "#2c3254"; RED <- "#ff8361"; GREEN <- "#70ad8f"; GREY <- "#8d8b7f"; GOLD <- "#e0a23c"
my_style <- list(
  theme_minimal(base_size = 12),
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey88", linewidth = .3),
        plot.title = element_text(face = "bold", color = NAVY, size = 13),
        plot.title.position = "plot",
        plot.subtitle = element_text(color = NAVY, size = 9.5),
        plot.caption = element_text(color = "grey40", size = 8),
        legend.position = "top", legend.title = element_blank(),
        axis.title = element_text(size = 9)),
  coord_cartesian(clip = "off"))
W <- 7.2; H <- 4.4; DPI <- 200
V <- as.Date("2025-01-20")
CAP_CPS <- "Source: IPUMS CPS microdata, author's calculations. No household survey in October 2025.\nMike Konczal."

# ---- H3: native-born men, prime-age EPOP, indexed to Dec 2024 --------------
mm <- read_csv("output/b2_h24_male_levels_monthly.csv", show_col_types = FALSE)
base <- mm %>% filter(date == as.Date("2024-12-01")) %>% pull(epop_nat_men)
p_h3 <- mm %>% filter(date >= as.Date("2022-01-01")) %>%
  transmute(date, idx = epop_nat_men - base) %>%
  ggplot(aes(date, idx)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = .3) +
  geom_vline(xintercept = V, linetype = "dashed", color = GREY) +
  geom_line(linewidth = .9, color = NAVY) +
  scale_y_continuous(labels = function(x) paste0(round(100*x, 1), " pp")) +
  labs(title = "Prime-Age Employment Fell for Native-Born Men, Too",
       subtitle = "Change in the prime-age (25-54) employment-population ratio for native-born men since\nDecember 2024, percentage points, seasonally adjusted",
       x = NULL, y = "Change since Dec 2024", caption = CAP_CPS) + my_style
ggsave("graphics/fig13_native_men_epop.png", p_h3, width = W, height = H, dpi = DPI)

# ---- H5: native-born 10th percentile real wage, single panel ---------------
w8 <- read_csv("output/h6_wages_jan_aug.csv", show_col_types = FALSE)
p_h5 <- w8 %>% filter(grp == "Native-born", yr >= 2019) %>%
  ggplot(aes(yr, p10)) +
  geom_vline(xintercept = 2025, linetype = "dashed", color = GREY) +
  geom_line(linewidth = 1, color = RED) + geom_point(size = 1.8, color = RED) +
  scale_x_continuous(breaks = seq(2019, 2026, 1)) +
  scale_y_continuous(labels = dollar_format(accuracy = 0.1)) +
  labs(title = "Real Wages at the 10th Percentile Fell",
       subtitle = "Native-born hourly wage, 10th percentile, outgoing rotation groups,\nJanuary-August of each year, 2026 dollars",
       x = NULL, y = "Real hourly wage", caption = CAP_CPS) + my_style
ggsave("graphics/fig14_wage_p10.png", p_h5, width = W, height = H, dpi = DPI)

# ---- H9: EPOP by citizen generation vs. foreign-born ------------------------
gg <- read_csv("output/h11_by_generation.csv", show_col_types = FALSE)
p_h9 <- gg %>% filter(yr >= 2019) %>%
  mutate(gen = factor(gen, levels = c("2nd gen native", "3rd+ gen native", "Foreign born"))) %>%
  ggplot(aes(yr, epop, color = gen)) +
  geom_vline(xintercept = 2025, linetype = "dashed", color = GREY) +
  geom_line(linewidth = 1) + geom_point(size = 1.8) +
  scale_color_manual(values = c("2nd gen native" = NAVY, "3rd+ gen native" = GOLD,
                                "Foreign born" = RED)) +
  scale_x_continuous(breaks = seq(2019, 2026, 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Both Generations of Native-Born Citizens Fell, Not Foreign-Born",
       subtitle = "Prime-age (25-54) employment-population ratio by generation. 2026 covers January-August.",
       x = NULL, y = "Prime-age employment rate", caption = CAP_CPS) + my_style
ggsave("graphics/fig15_generation_epop.png", p_h9, width = W, height = H, dpi = DPI)

cat("DONE 58_blog_post_charts.R\n")
