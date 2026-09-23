suppressMessages({library(tidyverse); library(lubridate); library(scales); library(data.table)})
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
CAP <- "Source: IPUMS CPS microdata, author's calculations. No household survey in October 2025.\nMike Konczal."
CAP_CES <- "Source: BLS Current Employment Statistics (establishment survey); non-citizen shares from\n2024 ACS PUMS, crosswalked to CES industries. No CPS microdata used. Mike Konczal."

# ---- Fig 5: measurement, unweighted respondent counts ----------------------
raw <- read_csv("output/h13_unweighted_counts.csv", show_col_types = FALSE) %>%
  arrange(date) %>%
  mutate(across(c(n_noncit, n_natzd, n_native, n_total), ~ zoo::rollmean(.x, 12, fill = NA, align = "right")))
b <- raw %>% filter(date == as.Date("2024-12-01"))
p5 <- raw %>% filter(date >= as.Date("2019-01-01")) %>%
  transmute(date,
            `Non-citizen respondents` = 100*n_noncit/b$n_noncit,
            `Native-born respondents` = 100*n_native/b$n_native,
            `All respondents` = 100*n_total/b$n_total) %>%
  pivot_longer(-date) %>% filter(!is.na(value)) %>%
  ggplot(aes(date, value, color = name)) +
  geom_hline(yintercept = 100, color = "grey70", linewidth = .3) +
  geom_vline(xintercept = as.Date("2025-01-20"), linetype = "dashed", color = GREY) +
  geom_line(linewidth = .9) +
  scale_color_manual(values = c("Non-citizen respondents" = RED,
                                "Native-born respondents" = NAVY,
                                "All respondents" = GREY)) +
  labs(title = "Non-Citizens Stopped Answering the Survey",
       subtitle = "Unweighted CPS respondents age 16+, 12-month moving average, Dec 2024 = 100.\nRaw interview counts carry no weights, so they are immune to the population-control artifact.",
       x = NULL, y = "Index, Dec 2024 = 100", caption = CAP) + my_style
ggsave("graphics/fig5_nonresponse.png", p5, width = W, height = H, dpi = DPI)

# ---- Fig 6: H7 native job-finding rate -------------------------------------
# Keep the existing 12-observation smoother. The two missing transitions
# around October 2025 extend affected windows beyond 12 calendar months.
fl <- read_csv("output/h7_flows_monthly.csv", show_col_types = FALSE) %>%
  arrange(grp, date) %>% group_by(grp) %>%
  mutate(UE12 = zoo::rollmean(UE, 12, fill = NA, align = "right")) %>% ungroup()
p6 <- fl %>% filter(date >= as.Date("2018-01-01"), !is.na(UE12),
                    grp == "Native prime-age") %>%
  ggplot(aes(date, UE12)) +
  geom_vline(xintercept = as.Date("2025-01-20"), linetype = "dashed", color = GREY) +
  geom_line(linewidth = .9, color = NAVY) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = hyp_title(7, "Unemployed Native-Born Workers Are Finding Jobs More Slowly, Not Faster"),
       subtitle = paste0("Share of unemployed native-born prime-age workers employed the following month.\n",
                         "Moving average of 12 available monthly observations."),
       x = NULL, y = "Monthly job-finding rate",
       caption = paste0("Two transitions are unavailable around October 2025, extending affected windows beyond 12 calendar months.\n",
                        "Source: IPUMS CPS microdata, author's calculations. Mike Konczal.")) + my_style
ggsave("graphics/fig6_job_finding.png", p6, width = W, height = H, dpi = DPI)

# ---- Fig 7: H5 native unemployment by education -----------------------------
edu <- read_csv("output/h5_native_by_education_jan_aug.csv", show_col_types = FALSE)
p7 <- edu %>% filter(yr >= 2019) %>%
  mutate(educ_grp = factor(educ_grp, levels = c("Less than HS","HS to some college","BA+"))) %>%
  ggplot(aes(yr, unrate, color = educ_grp)) +
  geom_vline(xintercept = 2025, linetype = "dashed", color = GREY) +
  geom_line(linewidth = 1) + geom_point(size = 1.8) +
  scale_color_manual(values = c("Less than HS" = RED, "HS to some college" = GOLD, "BA+" = NAVY)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(2019, 2026, 1)) +
  labs(title = "The Least-Educated Native Workers Did Not Gain",
       subtitle = "Prime-age (25-54) native-born unemployment rate by education, January-August of each year.\nThe substitution story predicts the largest gains for the least-educated, who compete most directly with immigrant labor.",
       x = NULL, y = "Unemployment rate", caption = CAP) + my_style
ggsave("graphics/fig7_education_annual.png", p7, width = W, height = H, dpi = DPI)

# ---- Fig 8: H11 Hispanic citizens -------------------------------------------
hg <- read_csv("output/h11_native_hispanic.csv", show_col_types = FALSE)
p8 <- hg %>% filter(yr >= 2019) %>%
  ggplot(aes(yr, unrate, color = grp)) +
  geom_vline(xintercept = 2025, linetype = "dashed", color = GREY) +
  geom_line(linewidth = 1) + geom_point(size = 1.8) +
  scale_color_manual(values = c("Native, Hispanic" = RED, "Native, non-Hispanic" = NAVY)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(2019, 2026, 1)) +
  labs(title = "Hispanic U.S. Citizens Absorbed Three Times the Increase in Unemployment",
       subtitle = "Prime-age (25-54) unemployment rate among native-born U.S. citizens, by ethnicity",
       x = NULL, y = "Unemployment rate", caption = CAP) + my_style
ggsave("graphics/fig8_hispanic_citizens.png", p8, width = W, height = H, dpi = DPI)

# ---- Fig 9: H4 rolling dose-response ---------------------------------------
rc <- read_csv("output/h4_rolling_coefficients.csv", show_col_types = FALSE) %>%
  filter(outcome == "d_unrate")
p9 <- rc %>% ggplot(aes(window_end, beta)) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = .4) +
  geom_vline(xintercept = as.Date("2025-01-20"), linetype = "dashed", color = GREY) +
  geom_ribbon(aes(ymin = beta - 1.96*se, ymax = beta + 1.96*se), fill = NAVY, alpha = .15) +
  geom_line(color = NAVY, linewidth = .9) +
  annotate("text", x = as.Date("2021-02-01"), y = 0.135,
           label = "Pandemic: immigrant-heavy\noccupations hit hardest",
           size = 2.9, color = GREY, hjust = .5) +
  labs(title = "No Sign Natives Gained Where Immigrant Labor Was Thickest",
       subtitle = "Coefficient from regressing the 12-month change in the native unemployment rate, by occupation,\non that occupation's pre-2025 foreign-born employment share. Negative = natives did better in\nimmigrant-heavy occupations, which is what the substitution story predicts. Shaded: 95% CI.",
       x = "End of 12-month window", y = "Coefficient on foreign-born share",
       caption = paste0(CAP, "\nRolling windows overlap by 9 months, so the path is descriptive, not a sequence of independent tests.")) +
  my_style
ggsave("graphics/fig9_dose_response.png", p9, width = W, height = H + .4, dpi = DPI)

# ---- Fig 10: H8 involuntary part-time by exposure ---------------------------
hx <- read_csv("output/h8_hours_by_exposure.csv", show_col_types = FALSE)
p10 <- hx %>% filter(yr >= 2019) %>%
  ggplot(aes(yr, share_ptecon, color = exp_grp)) +
  geom_vline(xintercept = 2025, linetype = "dashed", color = GREY) +
  geom_line(linewidth = 1) + geom_point(size = 1.8) +
  scale_color_manual(values = c("Q1 lowest" = NAVY, "Q2" = GREEN, "Q3" = GOLD, "Q4 highest" = RED)) +
  scale_y_continuous(labels = percent_format(accuracy = .5)) +
  scale_x_continuous(breaks = seq(2019, 2026, 1)) +
  labs(title = "Involuntary Part-Time Work Rose Most Where Immigrant Labor Was Thickest",
       subtitle = "Share of employed native-born workers on part time for economic reasons, by quartile of the\noccupation's pre-2025 foreign-born employment share. Scarce labor should push this down.",
       x = NULL, y = "Share part-time for economic reasons", caption = CAP) + my_style
ggsave("graphics/fig10_parttime.png", p10, width = W, height = H, dpi = DPI)

cat("wrote 6 microdata charts\n")

# ---- Fig 11: H6 industry cross-section, nominal wage growth by non-citizen quintile
# blogs_2026-04 methodology: 2024 ACS non-citizen share by NAICS, crosswalked
# onto BLS CES's 249 diffusion industries and quintiled. Wage growth is BLS
# CES production/nonsupervisory average hourly earnings -- no CPS microdata.
# See R/62_industry_wage_exposure.R.
we <- read_csv("output/h6_industry_wage_jan_aug_nominal.csv", show_col_types = FALSE)  # nominal; real version in h6_industry_wage_jan_aug.csv
QCOL <- c("Q1 (lowest)" = NAVY, "Q2" = GREEN, "Q3" = GOLD,
          "Q4" = "#e0762e", "Q5 (highest)" = RED)
p11 <- we %>% filter(yr >= 2023, !is.na(yoy)) %>%
  ggplot(aes(factor(yr), 100*yoy, fill = nc_quintile)) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = .4) +
  geom_col(position = position_dodge(.8), width = .72) +
  scale_fill_manual(values = QCOL) +
  labs(title = "No Wage Acceleration in the Most Non-Citizen-Intensive Industries",
       subtitle = "Nominal hourly wage growth, production/nonsupervisory employees, by quintile of a CES industry's\nnon-citizen worker share (2024 ACS). January-August average of year-over-year growth.",
       x = NULL, y = "Nominal wage growth, % YoY", caption = CAP_CES) +
  my_style + theme(legend.text = element_text(size = 8))
ggsave("graphics/fig11_wages_exposure.png", p11, width = W, height = H, dpi = DPI)

# ---- Fig 12: H6 native real wage levels, p10 and median --------------------
w8 <- read_csv("output/h6_wages_jan_aug.csv", show_col_types = FALSE)
p12 <- w8 %>% filter(grp == "Native-born", yr >= 2019) %>%
  select(yr, `10th percentile` = p10, Median = p50) %>%
  pivot_longer(-yr) %>%
  ggplot(aes(yr, value, color = name)) +
  geom_vline(xintercept = 2025, linetype = "dashed", color = GREY) +
  geom_line(linewidth = 1) + geom_point(size = 1.8) +
  facet_wrap(~name, scales = "free_y") +
  scale_color_manual(values = c(RED, NAVY), guide = "none") +
  scale_x_continuous(breaks = seq(2019, 2026, 1)) +
  scale_y_continuous(labels = dollar_format(accuracy = 0.1)) +
  labs(title = "Real Wages at the Bottom of the Native Distribution Fell",
       subtitle = "Native-born hourly wages, outgoing rotation groups, January-August of each year, 2026 dollars",
       x = NULL, y = NULL, caption = CAP) +
  my_style + theme(strip.text = element_text(face = "bold", color = NAVY))
ggsave("graphics/fig12_wage_levels.png", p12, width = W, height = H, dpi = DPI)
cat("wrote wage charts\n")
