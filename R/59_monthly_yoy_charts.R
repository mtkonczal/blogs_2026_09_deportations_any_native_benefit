# ==============================================================================
# 59_monthly_yoy_charts.R -> month-by-month NSA charts, 2024 vs 2026, native-born
# only. Replaces the SA nativity-comparison style for Figs 1-3 of blog_post.md:
# the user trusts the raw NSA month-over-month comparison more than the
# author's own seasonal adjustment, and foreign-born NSA series are dropped
# because non-citizen CPS response rates fell (see fig5_nonresponse.png), which
# makes a foreign-born vs. native-born NSA comparison unreliable.
# ==============================================================================
stop("59_monthly_yoy_charts.R is superseded by 60_bls_official_monthly.R, which writes the same three graphics.")
suppressMessages({library(tidyverse); library(lubridate); library(scales)})
dir.create("graphics", showWarnings = FALSE)

BG    <- "#faf6ec"
NAVY  <- "#1c2340"
GOLD  <- "#d9a066"
GREY  <- "#8d8b7f"

monthly_style <- list(
  theme_minimal(base_size = 13),
  theme(plot.background = element_rect(fill = BG, color = NA),
        panel.background = element_rect(fill = BG, color = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey82", linewidth = .35),
        plot.title = element_text(face = "bold", color = NAVY, size = 16),
        plot.title.position = "plot",
        plot.subtitle = element_text(color = NAVY, size = 11),
        plot.caption = element_text(color = "grey45", size = 8.5),
        legend.position = "top", legend.title = element_blank(),
        legend.text = element_text(size = 11),
        axis.title = element_blank(),
        axis.text = element_text(color = NAVY, size = 10)),
  coord_cartesian(clip = "off"))
W <- 8; H <- 5; DPI <- 200
MON <- scale_x_continuous(breaks = 1:12, labels = month.abb, limits = c(1, 12.6))
YEAR_COLORS <- c(`2024` = GOLD, `2025` = "#a9a79c", `2026` = NAVY)
CAP <- "Source: BLS Current Population Survey, author's calculations. Mike Konczal, Economic Security Project."

label_end <- function(df) df %>% group_by(year) %>% filter(month == max(month)) %>% ungroup()

# ---- Fig 1: native-born unemployment rate, NSA, by month -------------------
d <- read_csv("data/nativity_published.csv", show_col_types = FALSE) %>%
  mutate(year = year(date), month = month(date)) %>%
  filter(year %in% c(2024, 2026)) %>%
  transmute(year = factor(year), month, value = unrate_nat)
lab <- label_end(d)
p1 <- d %>% ggplot(aes(month, value, color = year, group = year)) +
  geom_line(linewidth = 1.1) + geom_point(size = 2.2) +
  ggrepel::geom_text_repel(data = lab, aes(label = percent(value, accuracy = 0.01)),
                           nudge_x = .6, direction = "y", hjust = 0, size = 3.6,
                           color = NAVY, segment.color = NA, show.legend = FALSE) +
  scale_color_manual(values = YEAR_COLORS) +
  scale_y_continuous(labels = percent_format(accuracy = 0.5)) +
  MON +
  labs(title = "Native-Born Unemployment Is Higher Than a Year Ago, Month by Month",
       subtitle = "Native-born unemployment rate by month, not seasonally adjusted, 2024 vs. 2026",
       caption = CAP) + monthly_style
ggsave("graphics/fig1_unrate_native_monthly.png", p1, width = W, height = H, dpi = DPI, bg = BG)

# ---- Fig 2: native-born prime-age EPOP, NSA, by month -----------------------
d2 <- read_csv("data/nativity_published.csv", show_col_types = FALSE) %>%
  mutate(year = year(date), month = month(date)) %>%
  filter(year %in% c(2024, 2026)) %>%
  transmute(year = factor(year), month, value = prime_epop_nat)
lab2 <- label_end(d2)
p2 <- d2 %>% ggplot(aes(month, value, color = year, group = year)) +
  geom_line(linewidth = 1.1) + geom_point(size = 2.2) +
  ggrepel::geom_text_repel(data = lab2, aes(label = percent(value, accuracy = 0.01)),
                           nudge_x = .6, direction = "y", hjust = 0, size = 3.6,
                           color = NAVY, segment.color = NA, show.legend = FALSE) +
  scale_color_manual(values = YEAR_COLORS) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  MON +
  labs(title = "Native-Born Prime-Age Employment Is Lower Than a Year Ago",
       subtitle = "Native-born prime-age (25-54) employment-population ratio by month, not seasonally adjusted, 2024 vs. 2026",
       caption = CAP) + monthly_style
ggsave("graphics/fig2_prime_epop_native_monthly.png", p2, width = W, height = H, dpi = DPI, bg = BG)

# ---- Fig 3: native-born men prime-age EPOP, NSA, by month -------------------
d3 <- read_csv("output/b2_h24_male_levels_monthly.csv", show_col_types = FALSE) %>%
  mutate(year = year(date), month = month(date)) %>%
  filter(year %in% c(2024, 2025, 2026)) %>%   # 2025 added for Fig 3 only
  transmute(year = factor(year), month, value = epop_nat_men_prime)
lab3 <- label_end(d3)
p3 <- d3 %>% ggplot(aes(month, value, color = year, group = year)) +
  geom_line(linewidth = 1.1) + geom_point(size = 2.2) +
  ggrepel::geom_text_repel(data = lab3, aes(label = percent(value, accuracy = 0.01)),
                           nudge_x = .6, direction = "y", hjust = 0, size = 3.6,
                           color = NAVY, segment.color = NA, show.legend = FALSE) +
  scale_color_manual(values = YEAR_COLORS) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  MON +
  labs(title = "Native-Born Men's Prime-Age Employment Has Not Risen Since 2024",
       subtitle = "Native-born men, prime-age (25-54) employment-population ratio by month, not seasonally adjusted, 2024-2026",
       caption = CAP) + monthly_style
ggsave("graphics/fig3_native_men_epop_monthly.png", p3, width = W, height = H, dpi = DPI, bg = BG)

cat("DONE 59_monthly_yoy_charts.R\n")
