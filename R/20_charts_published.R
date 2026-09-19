suppressMessages({library(tidyverse); library(lubridate); library(scales)})
dir.create("graphics", showWarnings = FALSE)
NAVY <- "#2c3254"; RED <- "#ff8361"; GREEN <- "#70ad8f"; GREY <- "#8d8b7f"
my_style <- list(
  theme_minimal(base_size = 12),
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey88", linewidth = .3),
        plot.title = element_text(face = "bold", color = NAVY),
        plot.title.position = "plot",
        plot.subtitle = element_text(color = NAVY, size = 9.5),
        plot.caption = element_text(color = "grey40", size = 8),
        legend.position = "top", legend.title = element_blank(),
        axis.title = element_text(size = 9)),
  coord_cartesian(clip = "off"))
W <- 7.2; H <- 4.4; DPI <- 200
CAP <- "Source: BLS Current Population Survey. No household survey was conducted in October 2025.\nMike Konczal."

d <- read_csv("data/nativity_published.csv", show_col_types = FALSE)

# Fig 1: native vs foreign unemployment rate, SA
p1 <- d %>% filter(date >= as.Date("2022-06-01")) %>%
  select(date, `Native-born` = unrate_nat_sa, `Foreign-born` = unrate_for_sa) %>%
  pivot_longer(-date) %>% filter(!is.na(value)) %>%
  ggplot(aes(date, value, color = name)) +
  geom_vline(xintercept = as.Date("2025-01-20"), linetype = "dashed", color = GREY) +
  annotate("text", x = as.Date("2025-02-15"), y = Inf, label = "Jan 2025",
           hjust = 0, vjust = 1.6, size = 3, color = GREY) +
  geom_line(linewidth = .9) +
  scale_color_manual(values = c("Native-born" = NAVY, "Foreign-born" = RED)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.5)) +
  labs(title = "Native-Born Unemployment Rose. Foreign-Born Unemployment Fell.",
       subtitle = "Unemployment rate by nativity, seasonally adjusted by the author (BLS publishes these series NSA only)",
       x = NULL, y = "Unemployment rate", caption = CAP) + my_style
ggsave("graphics/fig1_unrate_by_nativity.png", p1, width = W, height = H, dpi = DPI)

# Fig 2: prime-age EPOP by nativity, indexed to Dec 2024
p2 <- d %>% filter(date >= as.Date("2022-01-01")) %>%
  select(date, `Native-born` = prime_epop_nat_sa, `Foreign-born` = prime_epop_for_sa) %>%
  pivot_longer(-date) %>% filter(!is.na(value)) %>%
  group_by(name) %>%
  mutate(idx = value - value[date == as.Date("2024-12-01")]) %>% ungroup() %>%
  ggplot(aes(date, idx, color = name)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = .3) +
  geom_vline(xintercept = as.Date("2025-01-20"), linetype = "dashed", color = GREY) +
  geom_line(linewidth = .9) +
  scale_color_manual(values = c("Native-born" = NAVY, "Foreign-born" = RED)) +
  scale_y_continuous(labels = function(x) paste0(round(100*x,1), " pp")) +
  labs(title = "Prime-Age Employment Fell for Natives and Rose for the Foreign-Born",
       subtitle = "Change in prime-age (25-54) employment-population ratio since December 2024, percentage points, seasonally adjusted",
       x = NULL, y = "Change since Dec 2024", caption = CAP) + my_style
ggsave("graphics/fig2_prime_epop.png", p2, width = W, height = H, dpi = DPI)

# Fig 3: H13 measurement, raw vs control-adjusted foreign-born employment
m <- read_csv("output/h13_control_adjusted_levels.csv", show_col_types = FALSE)
p3 <- m %>% filter(date >= as.Date("2023-01-01")) %>%
  select(date, `As published` = emp_for,
         `Adjusted for January population controls` = emp_for_chained) %>%
  pivot_longer(-date) %>% filter(!is.na(value)) %>%
  ggplot(aes(date, value/1000, color = name)) +
  geom_vline(xintercept = as.Date("2025-01-20"), linetype = "dashed", color = GREY) +
  geom_line(linewidth = .9) +
  scale_color_manual(values = c("As published" = GREY,
                                "Adjusted for January population controls" = NAVY)) +
  scale_y_continuous(labels = comma) +
  labs(title = "The Published Level Series Hides the Foreign-Born Employment Decline",
       subtitle = "Foreign-born employment, millions. The CPS resets population controls each January and does not revise prior months,\nso the published level series absorbs the revision as if it were a real jump.",
       x = NULL, y = "Millions employed", caption = CAP) + my_style +
  theme(legend.text = element_text(size = 8))
ggsave("graphics/fig3_measurement.png", p3, width = W, height = H, dpi = DPI)

# Fig 4: H12 tightness
t <- read_csv("output/h12_tightness.csv", show_col_types = FALSE)
p4 <- t %>% filter(date >= as.Date("2018-01-01")) %>%
  select(date, `Vacancies per unemployed worker` = vu, `Quits rate` = quits_rate) %>%
  pivot_longer(-date) %>%
  ggplot(aes(date, value, color = name)) +
  geom_vline(xintercept = as.Date("2025-01-20"), linetype = "dashed", color = GREY) +
  geom_line(linewidth = .9) + facet_wrap(~name, scales = "free_y") +
  scale_color_manual(values = c(NAVY, RED), guide = "none") +
  labs(title = "The Labor Market Got Looser, Not Tighter",
       subtitle = "A labor supply contraction against stable demand would raise both series. Both fell.",
       x = NULL, y = NULL,
       caption = "Source: BLS JOLTS and CPS. Mike Konczal.") +
  my_style + theme(strip.text = element_text(face = "bold", color = NAVY))
ggsave("graphics/fig4_tightness.png", p4, width = W, height = H, dpi = DPI)

cat("wrote 4 published-series charts\n")
