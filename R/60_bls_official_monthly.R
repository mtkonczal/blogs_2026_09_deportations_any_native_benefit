# ==============================================================================
# 60_bls_official_monthly.R
# Refreshes Figs 1-3 of blog_post.md so that Fig 1 and Fig 2 are pulled straight
# from officially published BLS nativity series (no microdata, no author-side
# rate computation where BLS publishes the rate directly). Fig 3 is the one
# exception: BLS has no monthly published series crossing nativity x sex x
# prime-age (25-54); that cross exists only in the annual foreign-born report,
# which stops at 2025 with no 2026 figure. Per instruction, Fig 3 keeps the
# CPS-microdata-derived series (output/b2_h24_male_levels_monthly.csv) and is
# captioned accordingly so it is never confused with a published BLS number.
#
# Fig 1: LNU04073413, "Unemployment Rate - Native born" - a directly published
#        rate. No computation at all.
# Fig 2: BLS publishes prime-age (25-54) EPOP by nativity only as an ANNUAL
#        series (LNU0231CE93, 2023-2025, no 2026 yet). The monthly version is
#        built the same way 03_aggregates_published.R already does: sum three
#        officially published monthly age-band LEVEL series (25-34/35-44/45-54,
#        population and employment, native-born) and take the ratio. Still
#        100% BLS levels, zero microdata - just not packaged as one series by
#        BLS, so re-pulled and cached fresh here.
# ==============================================================================
suppressMessages({library(tidyverse); library(lubridate); library(blsR); library(scales)})
bls_set_key(Sys.getenv("BLS_KEY"))
dir.create("data_raw/bls_cache", showWarnings = FALSE, recursive = TRUE)  # gitignored (data_raw/)
dir.create("graphics", showWarnings = FALSE)

pull <- function(ids, sy = 2018, ey = 2026) {
  get_n_series_table(ids, api_key = bls_get_key(), start_year = sy,
                     end_year = ey, tidy = TRUE) %>%
    mutate(across(everything(), as.numeric),
           date = as.Date(paste0(year, "/", month, "/1"))) %>%
    select(-year, -month) %>% arrange(date)
}

# --- Fig 1 series: the published rate itself, no computation ----------------
fig1_ids <- c("LNU04073413")  # Unemployment Rate - Native born, NSA, monthly
fig1_raw <- pull(fig1_ids)
write_csv(fig1_raw, "data_raw/bls_cache/lnu04073413_unrate_native.csv")
fig1 <- fig1_raw %>% transmute(date, unrate_nat = LNU04073413 / 100)

# --- Fig 2 series: three official monthly age-band levels, native-born ------
fig2_ids <- c("LNU00073417","LNU00073418","LNU00073419",   # native pop 25-34,35-44,45-54
             "LNU02073417","LNU02073418","LNU02073419")   # native emp, same bands
fig2_raw <- pull(fig2_ids)
write_csv(fig2_raw, "data_raw/bls_cache/lnu_prime_age_native_levels.csv")
fig2 <- fig2_raw %>% transmute(
  date,
  ppop_nat = LNU00073417 + LNU00073418 + LNU00073419,
  pemp_nat = LNU02073417 + LNU02073418 + LNU02073419,
  prime_epop_nat = pemp_nat / ppop_nat
)

write_csv(fig1, "data/fig1_native_unrate_official.csv")
write_csv(fig2, "data/fig2_prime_epop_native_official.csv")

cat("Fig 1 date range:", format(min(fig1$date)), "to", format(max(fig1$date)), "\n")
cat("Fig 2 date range:", format(min(fig2$date)), "to", format(max(fig2$date)), "\n")

# --- sanity check against the previous level-ratio computation --------------
old <- read_csv("data/nativity_published.csv", show_col_types = FALSE) %>%
  select(date, unrate_nat_old = unrate_nat, prime_epop_nat_old = prime_epop_nat)
chk <- fig1 %>% left_join(fig2, by = "date") %>% left_join(old, by = "date") %>%
  filter(date >= as.Date("2024-01-01")) %>%
  mutate(d_unrate = unrate_nat - unrate_nat_old, d_epop = prime_epop_nat - prime_epop_nat_old)
cat("\nMax abs diff, published rate vs. old un_nat/lf_nat computation:",
    max(abs(chk$d_unrate), na.rm = TRUE), "\n")
cat("Max abs diff, re-pulled prime-age EPOP vs. old:",
    max(abs(chk$d_epop), na.rm = TRUE), "\n")

# ==============================================================================
# Charts (same NSA, native-only, 2024-vs-2026 style as before)
# ==============================================================================
BG <- "#faf6ec"; NAVY <- "#1c2340"; GOLD <- "#d9a066"; GREY <- "#8d8b7f"
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
label_end <- function(df) df %>% group_by(year) %>% filter(month == max(month)) %>% ungroup()

# ---- Fig 1: native-born unemployment rate, official published rate ---------
d1 <- fig1 %>% mutate(year = year(date), month = month(date)) %>%
  filter(year %in% c(2024, 2026)) %>% transmute(year = factor(year), month, value = unrate_nat)
lab1 <- label_end(d1)
p1 <- d1 %>% ggplot(aes(month, value, color = year, group = year)) +
  geom_line(linewidth = 1.1) + geom_point(size = 2.2) +
  ggrepel::geom_text_repel(data = lab1, aes(label = percent(value, accuracy = 0.01)),
                           nudge_x = .6, direction = "y", hjust = 0, size = 3.6,
                           color = NAVY, segment.color = NA, show.legend = FALSE) +
  scale_color_manual(values = YEAR_COLORS) +
  scale_y_continuous(labels = percent_format(accuracy = 0.5)) +
  MON +
  labs(title = "Native-Born Unemployment Is Higher Than a Year Ago, Month by Month",
       subtitle = "Native-born unemployment rate by month, not seasonally adjusted, 2024 vs. 2026",
       caption = "Source: BLS series LNU04073413 (Unemployment Rate, Native born, NSA), published directly by BLS.\nMike Konczal, Economic Security Project.") +
  monthly_style
ggsave("graphics/fig1_unrate_native_monthly.png", p1, width = W, height = H, dpi = DPI, bg = BG)

# ---- Fig 2: native-born prime-age EPOP, from official monthly age-band levels
d2 <- fig2 %>% mutate(year = year(date), month = month(date)) %>%
  filter(year %in% c(2024, 2026)) %>% transmute(year = factor(year), month, value = prime_epop_nat)
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
       caption = paste0("Source: BLS series LNU00073417-419 and LNU02073417-419 (population and employment levels,\n",
                        "native-born, ages 25-34/35-44/45-54, NSA), summed. Published levels, author's ratio. Mike Konczal, ESP.")) +
  monthly_style
ggsave("graphics/fig2_prime_epop_native_monthly.png", p2, width = W, height = H, dpi = DPI, bg = BG)

# ---- Fig 3: native-born men prime-age EPOP - CPS microdata, clearly labeled -
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
       caption = paste0("Author's calculation from IPUMS CPS microdata (WTFINL-weighted), not an official BLS series.\n",
                        "BLS publishes this exact cross (nativity x sex x age 25-54) only annually, through 2025. Mike Konczal, ESP.")) +
  monthly_style
ggsave("graphics/fig3_native_men_epop_monthly.png", p3, width = W, height = H, dpi = DPI, bg = BG)

cat("DONE 60_bls_official_monthly.R\n")
