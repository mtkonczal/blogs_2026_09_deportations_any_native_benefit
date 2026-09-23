# ==============================================================================
# 47_charts_batch2.R  -> graphics/b2_*.png
# Every figure reads from output/, so nothing here can drift from the analysis.
# ==============================================================================
suppressMessages({library(tidyverse); library(lubridate); library(scales); library(zoo)})
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
source("R/00_blog_style.R")  # hyp_title() for graphics used in blog_post.md
YRX <- scale_x_continuous(breaks = seq(2014, 2026, 2))
CAP_BLS <- "Source: BLS. Mike Konczal."
CAP_CPS <- "Source: IPUMS CPS microdata, author's calculations. No household survey in October 2025.\nMike Konczal."
rd <- function(f) read_csv(file.path("output", f), show_col_types = FALSE)
V <- as.Date("2025-01-20")

# --- b1: the labor share -----------------------------------------------------
ls_q <- rd("b2_h14_labor_share_quarterly.csv")
last <- ls_q %>% slice_tail(n = 1)
p <- ls_q %>% filter(date >= as.Date("1990-01-01")) %>%
  ggplot(aes(date, labor_share_nfb)) +
  geom_vline(xintercept = as.Date("2024-12-01"), linetype = "dashed", color = GREY) +
  geom_line(linewidth = .9, color = NAVY) +
  geom_point(data = last, color = RED, size = 2.2) +
  annotate("text", x = as.Date("2025-04-01"), y = last$labor_share_nfb - 1.6,
           label = sprintf("%.1f, the lowest\nin the series", last$labor_share_nfb),
           hjust = 1, size = 3, color = RED, lineheight = .95) +
  labs(title = hyp_title(8, "The Labor Share Did Not Rise. It Fell to a Postwar Low."),
       subtitle = "Labor share of nonfarm business sector income, index 2017 = 100. Dashed line: December 2024.",
       x = NULL, y = "Index, 2017 = 100", caption = CAP_BLS) + my_style
ggsave("graphics/b2_fig1_labor_share.png", p, width = W, height = H, dpi = DPI)

# --- b2: the within-year employment gains ------------------------------------
wy <- rd("b2_h15_within_year.csv")
p <- wy %>% filter(yr != 2020) %>%
  select(yr, `Native employment` = d_emp_nat, `Native population` = d_pop_nat) %>%
  pivot_longer(-yr) %>%
  ggplot(aes(factor(yr), value, fill = name)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = .3) +
  geom_col(position = position_dodge(width = .78), width = .72) +
  scale_fill_manual(values = c("Native employment" = NAVY, "Native population" = RED)) +
  scale_y_continuous(labels = comma_format(scale = 1e-3, suffix = "M", accuracy = .1)) +
  labs(title = "The Within-Year Native \"Job Gains\" Are Population, and the Population Is Not Real",
       subtitle = paste("January-to-December change using that year's own weights, the method used to sidestep the",
                        "\nJanuary population controls. 2026 covers January to August. 2020 omitted."),
       x = NULL, y = "Change within the year", caption = CAP_CPS) +
  my_style
ggsave("graphics/b2_fig2_within_year.png", p, width = W, height = H, dpi = DPI)

# --- b3: men vs women payrolls ------------------------------------------------
ces <- rd("b2_ces_sex_monthly.csv")
b <- ces %>% filter(date == as.Date("2024-12-01"))
p <- ces %>% filter(date >= as.Date("2022-01-01")) %>%
  transmute(date, Men = men_nf - b$men_nf, Women = women_nf - b$women_nf) %>%
  pivot_longer(-date) %>%
  ggplot(aes(date, value, color = name)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = .3) +
  geom_vline(xintercept = V, linetype = "dashed", color = GREY) +
  geom_line(linewidth = .9) +
  scale_color_manual(values = c("Men" = NAVY, "Women" = RED)) +
  scale_y_continuous(labels = comma_format(scale = 1e-3, suffix = "M", accuracy = .5)) +
  labs(title = "Since December 2024 Every Net Payroll Job Has Gone to Women",
       subtitle = "Cumulative change in nonfarm payroll employment by sex, thousands, December 2024 = 0",
       x = NULL, y = "Cumulative change since Dec 2024", caption = CAP_BLS) + my_style
ggsave("graphics/b2_fig3_men_women_payrolls.png", p, width = W, height = H, dpi = DPI)

# --- b4: the overlap index, the clean replacement test ------------------------
ov <- rd("b2_h24_overlap_over_time.csv")
p <- ov %>% filter(yr >= 2015, grp %in% c("Native men","Native women",
                                          "Naturalized citizens","Noncitizens")) %>%
  ggplot(aes(yr, overlap, color = grp)) +
  geom_vline(xintercept = 2025, linetype = "dashed", color = GREY) +
  geom_line(linewidth = .9) + geom_point(size = 1.2) +
  scale_color_manual(values = c("Native men" = NAVY, "Native women" = GREEN,
                                "Naturalized citizens" = GOLD, "Noncitizens" = RED)) +
  YRX +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Native Men Did Not Move Into the Work Immigrants Left",
       subtitle = paste("Overlap index: each group's employment distributed across occupations, weighted by each occupation's",
                        "\n2022-24 foreign-born share. Rising means moving into immigrant-intensive work."),
       x = NULL, y = "Exposure-weighted overlap", caption = CAP_CPS) + my_style
ggsave("graphics/b2_fig4_overlap.png", p, width = W, height = H, dpi = DPI)

# --- b5: native men's unemployment inside each exposure quartile ---------------
nm <- rd("b2_h24_native_men_by_exposure.csv")
p <- nm %>% filter(yr >= 2018) %>%
  ggplot(aes(yr, unrate, color = exp_q)) +
  geom_vline(xintercept = 2025, linetype = "dashed", color = GREY) +
  geom_line(linewidth = .9) + geom_point(size = 1.2) +
  scale_color_manual(values = c("Q1 lowest" = GREY, "Q2" = GREEN,
                                "Q3" = GOLD, "Q4 highest" = RED)) +
  YRX +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Native Men's Unemployment Rose Fastest Where Immigrant Labor Was Thickest",
       subtitle = "Unemployment rate for native-born men, by quartile of the occupation's 2022-24 foreign-born share",
       x = NULL, y = "Unemployment rate", caption = CAP_CPS) + my_style
ggsave("graphics/b2_fig5_native_men_exposure.png", p, width = W, height = H, dpi = DPI)

# --- b6: the matched wage tracker by quartile ---------------------------------
tq <- rd("b2_h19_tracker_by_quartile.csv") %>% filter(grp == "Native-born")
p <- tq %>% filter(yr >= 2019) %>%
  ggplot(aes(yr, tracker, color = qtile)) +
  geom_vline(xintercept = 2025, linetype = "dashed", color = GREY) +
  geom_line(linewidth = .9) + geom_point(size = 1.2) +
  scale_color_manual(values = c("Q1 lowest" = RED, "Q2" = GOLD,
                                "Q3" = GREEN, "Q4 highest" = NAVY)) +
  YRX +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "The Bottom-Quartile Wage Premium Was a 2022 Phenomenon. It Is Gone.",
       subtitle = paste("Median 12-month wage growth for native-born workers matched to themselves a year earlier,",
                        "\nby quartile of their own average wage. Atlanta Fed construction, rebuilt from CPS microdata."),
       x = NULL, y = "Median 12-month wage growth", caption = CAP_CPS) + my_style
ggsave("graphics/b2_fig6_tracker_quartile.png", p, width = W, height = H, dpi = DPI)

# --- b7: the matched tracker by occupational exposure -------------------------
tx <- rd("b2_h19_tracker_by_exposure.csv")
p <- tx %>% filter(yr >= 2019) %>%
  ggplot(aes(yr, tracker, color = exp_q)) +
  geom_vline(xintercept = 2025, linetype = "dashed", color = GREY) +
  geom_line(linewidth = .9) + geom_point(size = 1.2) +
  scale_color_manual(values = c("Q1 lowest" = GREY, "Q2" = GREEN,
                                "Q3" = GOLD, "Q4 highest" = RED)) +
  YRX +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Native Wage Growth Has No Gradient in Immigrant Exposure",
       subtitle = paste("Median matched 12-month wage growth for native-born workers, by quartile of their occupation's",
                        "\n2022-24 foreign-born share. The substitution story requires the red line to pull away."),
       x = NULL, y = "Median 12-month wage growth", caption = CAP_CPS) + my_style
ggsave("graphics/b2_fig7_tracker_exposure.png", p, width = W, height = H, dpi = DPI)

# --- b8: the citizenship gradient ---------------------------------------------
cz <- rd("b2_h25_by_citizenship.csv")
p <- cz %>% filter(yr >= 2018) %>%
  ggplot(aes(yr, epop, color = citgrp)) +
  geom_vline(xintercept = 2025, linetype = "dashed", color = GREY) +
  geom_line(linewidth = .9) + geom_point(size = 1.2) +
  scale_color_manual(values = c("Native-born" = NAVY, "Naturalized citizen" = GOLD,
                                "Noncitizen" = RED)) +
  YRX +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "The Employment Rate Ordering Follows Exposure to Enforcement, Not Substitution",
       subtitle = paste("Prime-age employment-population ratio by citizenship. The group being removed has the rising line,",
                        "\nwhich is what selective departure and selective non-response look like."),
       x = NULL, y = "Prime-age employment rate", caption = CAP_CPS) + my_style
ggsave("graphics/b2_fig8_citizenship.png", p, width = W, height = H, dpi = DPI)

# --- b9: rents ----------------------------------------------------------------
cpi <- rd("b2_h20_cpi_shelter.csv")
pk <- cpi %>% filter(date >= as.Date("2021-01-01")) %>% slice_max(yoy_rent, n = 1)
p <- cpi %>% filter(date >= as.Date("2018-01-01"), !is.na(yoy_rent)) %>%
  select(date, `Rent of primary residence` = yoy_rent,
         `Owners' equivalent rent` = yoy_oer) %>%
  pivot_longer(-date) %>%
  ggplot(aes(date, value, color = name)) +
  geom_vline(xintercept = V, linetype = "dashed", color = GREY) +
  geom_vline(xintercept = pk$date, linetype = "dotted", color = RED) +
  annotate("text", x = pk$date, y = Inf, label = " peak, March 2023", hjust = 0,
           vjust = 1.6, size = 3, color = RED) +
  geom_line(linewidth = .9) +
  scale_color_manual(values = c("Rent of primary residence" = NAVY,
                                "Owners' equivalent rent" = RED)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Rent Disinflation Started Two Years Before the Enforcement Campaign",
       subtitle = "12-month change in CPI shelter components, seasonally adjusted. Dashed line: January 2025.",
       x = NULL, y = "12-month change", caption = CAP_BLS) + my_style
ggsave("graphics/b2_fig9_rents.png", p, width = W, height = H, dpi = DPI)

# --- b10: young native men -----------------------------------------------------
ym <- rd("b2_h16_young_men_nocollege.csv")
p <- ym %>% filter(yr >= 2015) %>%
  select(yr, `Employment rate` = epop, `Labor force participation` = lfpr) %>%
  pivot_longer(-yr) %>%
  ggplot(aes(yr, value, color = name)) +
  geom_vline(xintercept = 2025, linetype = "dashed", color = GREY) +
  geom_line(linewidth = .9) + geom_point(size = 1.2) +
  scale_color_manual(values = c("Employment rate" = NAVY,
                                "Labor force participation" = RED)) +
  YRX +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Young Native-Born Men Without a Degree Did Not Come Back",
       subtitle = "Native-born men aged 20-34 without a bachelor's degree, annual averages of monthly rates",
       x = NULL, y = "Share of population", caption = CAP_CPS) + my_style
ggsave("graphics/b2_fig10_young_men.png", p, width = W, height = H, dpi = DPI)

# --- b11: the twenty occupations, one dot each --------------------------------
oc <- rd("b2_h21_top20_occupations.csv")
p <- oc %>%
  mutate(lab = str_trunc(occ_name, 30)) %>%
  ggplot(aes(foreign_share, d_nat_unrate)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = .3) +
  geom_smooth(method = "lm", se = FALSE, color = GREY, linewidth = .5, linetype = "dashed") +
  geom_point(aes(size = n_nat_1), color = NAVY, alpha = .7) +
  ggrepel::geom_text_repel(aes(label = lab), size = 2.3, color = "grey30",
                           max.overlaps = 12, seed = 1) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_size_continuous(range = c(1.2, 5), guide = "none") +
  labs(title = "In the Most Immigrant-Intensive Occupations, Native Unemployment Went Up",
       subtitle = paste("The twenty occupations with the highest 2022-24 foreign-born employment share.",
                        "\nChange in the native-born unemployment rate, 2023m9-2024m8 to 2025m9-2026m8."),
       x = "Foreign-born share of the occupation, 2022-24",
       y = "Change in native unemployment rate", caption = CAP_CPS) + my_style
ggsave("graphics/b2_fig11_occupations.png", p, width = W, height = H + .6, dpi = DPI)

# --- b12: construction ---------------------------------------------------------
cn <- rd("b2_h18_construction_industry.csv")
p <- cn %>% filter(yr >= 2015) %>%
  select(yr, `Native unemployment rate` = nat_unrate,
         `Foreign-born share of employment` = for_share) %>%
  pivot_longer(-yr) %>%
  ggplot(aes(yr, value, color = name)) +
  geom_vline(xintercept = 2025, linetype = "dashed", color = GREY) +
  geom_line(linewidth = .9) + geom_point(size = 1.2) +
  facet_wrap(~name, scales = "free_y") +
  scale_color_manual(values = c("Native unemployment rate" = NAVY,
                                "Foreign-born share of employment" = RED)) +
  YRX +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Construction: the Immigrant Share Fell and Native Unemployment Rose",
       subtitle = "Construction industry, annual averages of monthly rates",
       x = NULL, y = NULL, caption = CAP_CPS) +
  my_style + theme(legend.position = "none",
                   strip.text = element_text(color = NAVY, face = "bold", size = 9))
ggsave("graphics/b2_fig12_construction.png", p, width = W, height = H, dpi = DPI)

cat("DONE 47_charts_batch2.R\n")
cat(paste(list.files("graphics", pattern = "^b2_"), collapse = "\n"), "\n")
