# ==============================================================================
# 42_batch2_published.R
# Batch 2, published-series tests.
#   H14  labor share of income                                    (claim C8)
#   H20  shelter inflation and the real-wage-through-rents claim  (claim C14)
#   plus the CES men-vs-women payroll split, which is the fact the male
#   section of H16/H17 has to explain.
#
# All BLS API. FRED is unreachable from this environment, so housing starts and
# the published Atlanta Fed quartile tracker are not pulled; CES construction
# payrolls stand in as the sector-activity measure and the wage tracker is
# rebuilt from CPS microdata in 41_batch2_wages.R instead.
# ==============================================================================
suppressMessages({
  library(tidyverse); library(lubridate); library(blsR); library(zoo)
})
bls_set_key(Sys.getenv("BLS_KEY"))
dir.create("output", showWarnings = FALSE)

# BLS caps a keyed request at 20 years, so pull in blocks and stack.
pull_m <- function(ids, sy, ey) {
  blocks <- split(sy:ey, ceiling(seq_along(sy:ey) / 19))
  map_dfr(blocks, function(yrs) {
    get_n_series_table(ids, api_key = bls_get_key(), start_year = min(yrs),
                       end_year = max(yrs), tidy = TRUE) %>%
      mutate(across(everything(), as.numeric),
             date = as.Date(sprintf("%d-%02d-01", year, month))) %>%
      select(-year, -month)
  }) %>% distinct(date, .keep_all = TRUE) %>% arrange(date)
}
pull_q <- function(ids, sy, ey) {
  blocks <- split(sy:ey, ceiling(seq_along(sy:ey) / 19))
  map_dfr(blocks, function(yrs) {
    get_n_series_table(ids, api_key = bls_get_key(), start_year = min(yrs),
                       end_year = max(yrs), tidy = TRUE) %>%
      mutate(across(everything(), as.numeric),
             date = as.Date(sprintf("%d-%02d-01", year, (quarter - 1) * 3 + 1))) %>%
      select(-year, -quarter)
  }) %>% distinct(date, .keep_all = TRUE) %>% arrange(date)
}

# ==============================================================================
# H14. The labor share  (claim C8)
# ==============================================================================
# PRS85006173 nonfarm business sector: labor share, index 2017 = 100
# PRS84006173 business sector: labor share, index 2017 = 100
ls_raw <- pull_q(c("PRS85006173", "PRS84006173"), 1947, 2026) %>%
  rename(labor_share_nfb = PRS85006173, labor_share_bus = PRS84006173)

stopifnot(nrow(ls_raw) > 200)
cat("labor share coverage:", format(min(ls_raw$date)), "to",
    format(max(ls_raw$date)), "|", nrow(ls_raw), "quarters\n")

base_q  <- ls_raw %>% filter(date == as.Date("2024-10-01"))   # 2024Q4, last pre-policy quarter
last_q  <- ls_raw %>% slice_tail(n = 1)
h14 <- ls_raw %>%
  mutate(yr = year(date)) %>%
  filter(yr >= 2015) %>%
  group_by(yr) %>%
  summarise(labor_share_nfb = mean(labor_share_nfb, na.rm = TRUE),
            labor_share_bus = mean(labor_share_bus, na.rm = TRUE),
            n_q = n(), .groups = "drop")

h14_head <- tibble(
  base_quarter        = format(base_q$date, "%Y Q4"),
  latest_quarter      = format(last_q$date),
  labor_share_base    = base_q$labor_share_nfb,
  labor_share_latest  = last_q$labor_share_nfb,
  change_index_pts    = last_q$labor_share_nfb - base_q$labor_share_nfb,
  pct_change          = last_q$labor_share_nfb / base_q$labor_share_nfb - 1,
  # where does the latest reading sit in the whole postwar distribution?
  pctile_postwar      = mean(ls_raw$labor_share_nfb <= last_q$labor_share_nfb, na.rm = TRUE),
  n_quarters_postwar  = sum(!is.na(ls_raw$labor_share_nfb)),
  n_quarters_lower    = sum(ls_raw$labor_share_nfb < last_q$labor_share_nfb, na.rm = TRUE),
  min_since_date      = format(max(ls_raw$date[ls_raw$labor_share_nfb < last_q$labor_share_nfb &
                                               ls_raw$date < last_q$date], na.rm = TRUE))
)
cat("\n=== H14 labor share (nonfarm business, 2017 = 100) ===\n")
print(as.data.frame(h14))
print(as.data.frame(t(h14_head)))

write_csv(ls_raw,   "output/b2_h14_labor_share_quarterly.csv")
write_csv(h14,      "output/b2_h14_labor_share_annual.csv")
write_csv(h14_head, "output/b2_h14_labor_share_headline.csv")

# ==============================================================================
# CES: the men-vs-women payroll fact the male section has to explain
# ==============================================================================
# CES0000000001 total nonfarm, all employees;  ...0010 women employees
# CES0500000001 total private, all employees;  ...0010 women employees
# CES2000000001 construction; CES3000000001 manufacturing; CES6562000001 health
ces <- pull_m(c("CES0000000001","CES0000000010","CES0500000001","CES0500000010",
                "CES2000000001","CES3000000001","CES6562000001","CES7000000001",
                "CES4300000001","CES4200000001"), 2007, 2026) %>%
  transmute(date,
            tot_nf      = CES0000000001,
            women_nf    = CES0000000010,
            men_nf      = CES0000000001 - CES0000000010,
            tot_priv    = CES0500000001,
            women_priv  = CES0500000010,
            men_priv    = CES0500000001 - CES0500000010,
            construction = CES2000000001,
            manufacturing = CES3000000001,
            health       = CES6562000001,
            leisure      = CES7000000001,
            transport_wh = CES4300000001,
            retail       = CES4200000001,
            women_share_nf = CES0000000010 / CES0000000001)

d0 <- ces %>% filter(date == as.Date("2024-12-01"))
d1 <- ces %>% slice_tail(n = 1)
ces_chg <- tibble(
  from = format(d0$date), to = format(d1$date),
  months = as.numeric(interval(d0$date, d1$date) %/% months(1)),
  d_total_nf   = d1$tot_nf - d0$tot_nf,
  d_men_nf     = d1$men_nf - d0$men_nf,
  d_women_nf   = d1$women_nf - d0$women_nf,
  men_share_of_gain = (d1$men_nf - d0$men_nf) / (d1$tot_nf - d0$tot_nf),
  d_construction = d1$construction - d0$construction,
  d_manufacturing = d1$manufacturing - d0$manufacturing,
  d_transport_wh = d1$transport_wh - d0$transport_wh,
  d_retail     = d1$retail - d0$retail,
  d_health     = d1$health - d0$health,
  d_leisure    = d1$leisure - d0$leisure,
  women_share_nf_base = d0$women_share_nf,
  women_share_nf_latest = d1$women_share_nf
)
cat("\n=== CES payroll change, Dec 2024 to latest (thousands) ===\n")
print(as.data.frame(t(ces_chg)))

# Same-length prior window, so "men gained nothing" has a benchmark and so the
# construction and manufacturing numbers can be read as a pace rather than a sign.
d_prev0 <- ces %>% filter(date == as.Date("2023-04-01"))
d_prev1 <- ces %>% filter(date == as.Date("2024-12-01"))
bench <- tibble(
  series = c("men_nf","women_nf","tot_nf","construction","manufacturing",
             "transport_wh","health","leisure"),
  prior_20m  = c(d_prev1$men_nf - d_prev0$men_nf, d_prev1$women_nf - d_prev0$women_nf,
                 d_prev1$tot_nf - d_prev0$tot_nf,
                 d_prev1$construction - d_prev0$construction,
                 d_prev1$manufacturing - d_prev0$manufacturing,
                 d_prev1$transport_wh - d_prev0$transport_wh,
                 d_prev1$health - d_prev0$health,
                 d_prev1$leisure - d_prev0$leisure),
  policy_20m = c(ces_chg$d_men_nf, ces_chg$d_women_nf, ces_chg$d_total_nf,
                 ces_chg$d_construction, ces_chg$d_manufacturing,
                 ces_chg$d_transport_wh, ces_chg$d_health, ces_chg$d_leisure)
) %>% mutate(change = policy_20m - prior_20m)
cat("\n--- benchmark: two 20-month windows, thousands of payroll jobs ---\n")
cat("    prior = Apr 2023 to Dec 2024, policy = Dec 2024 to latest\n")
print(as.data.frame(bench))

write_csv(ces,     "output/b2_ces_sex_monthly.csv")
write_csv(ces_chg, "output/b2_ces_sex_change.csv")
write_csv(bench,   "output/b2_ces_benchmark.csv")

# ==============================================================================
# H20. Shelter inflation and the real-wage-through-rents claim  (claim C14)
# ==============================================================================
# CUSR0000SEHA rent of primary residence, SA;  SEHC owners' equivalent rent
# CUSR0000SAH1 shelter;  CUSR0000SA0 all items;  CUSR0000SA0L2 all items less shelter
cpi <- pull_m(c("CUSR0000SEHA","CUSR0000SEHC","CUSR0000SAH1",
                "CUSR0000SA0","CUSR0000SA0L2"), 2007, 2026) %>%
  transmute(date, rent = CUSR0000SEHA, oer = CUSR0000SEHC,
            shelter = CUSR0000SAH1, cpi_all = CUSR0000SA0,
            cpi_less_shelter = CUSR0000SA0L2) %>%
  arrange(date) %>%
  mutate(across(c(rent, oer, shelter, cpi_all, cpi_less_shelter),
                ~ .x / lag(.x, 12) - 1, .names = "yoy_{.col}"))

cat("\n=== shelter inflation, 12-month rates ===\n")
print(cpi %>% filter(date >= as.Date("2022-01-01")) %>%
        mutate(yr = year(date)) %>% group_by(yr) %>%
        summarise(rent = mean(yoy_rent, na.rm=TRUE),
                  oer = mean(yoy_oer, na.rm=TRUE),
                  shelter = mean(yoy_shelter, na.rm=TRUE),
                  all = mean(yoy_cpi_all, na.rm=TRUE)) %>% as.data.frame())

# when did rent inflation peak and start decelerating? This is the whole
# question: a deceleration that starts in 2023 is the multifamily supply wave,
# not 2025 enforcement.
pk <- cpi %>% filter(date >= as.Date("2021-01-01")) %>%
  slice_max(yoy_rent, n = 1) %>% select(date, yoy_rent)
cat("\nrent-of-primary-residence 12m peak:", format(pk$date),
    "at", sprintf("%.2f%%", 100*pk$yoy_rent), "\n")
cat("rent 12m at Dec 2024:",
    sprintf("%.2f%%", 100*cpi$yoy_rent[cpi$date == as.Date("2024-12-01")]), "\n")
cat("rent 12m latest:", sprintf("%.2f%%", 100*tail(cpi$yoy_rent,1)), "\n")

# regional shelter, NSA (regional CPI is published NSA only)
reg <- pull_m(c("CUUR0100SAH1","CUUR0200SAH1","CUUR0300SAH1","CUUR0400SAH1"),
              2007, 2026) %>%
  transmute(date, Northeast = CUUR0100SAH1, Midwest = CUUR0200SAH1,
            South = CUUR0300SAH1, West = CUUR0400SAH1) %>%
  arrange(date) %>%
  mutate(across(-date, ~ .x / lag(.x, 12) - 1))

# 2023 ACS-era foreign-born population shares by census region, for ordering the
# regions by exposure. Hardcoded because this is a four-number ordering, not an
# estimate; the point is only that West and Northeast are the high-immigrant
# regions and the Midwest is the low one.
reg_long <- reg %>% pivot_longer(-date, names_to = "region", values_to = "shelter_yoy") %>%
  mutate(foreign_share_pop = recode(region, Northeast = .165, West = .190,
                                    South = .130, Midwest = .075))
reg_sum <- reg_long %>% filter(!is.na(shelter_yoy)) %>%
  mutate(period = case_when(date >= as.Date("2022-01-01") & date <= as.Date("2022-12-01") ~ "2022",
                            date >= as.Date("2023-01-01") & date <= as.Date("2023-12-01") ~ "2023",
                            date >= as.Date("2024-01-01") & date <= as.Date("2024-12-01") ~ "2024",
                            date >= as.Date("2025-01-01") & date <= as.Date("2025-12-01") ~ "2025",
                            date >= as.Date("2026-01-01") ~ "2026 YTD", TRUE ~ NA_character_)) %>%
  filter(!is.na(period)) %>%
  group_by(region, foreign_share_pop, period) %>%
  summarise(shelter_yoy = mean(shelter_yoy), .groups = "drop") %>%
  pivot_wider(names_from = period, values_from = shelter_yoy) %>%
  mutate(decel_25_26 = `2026 YTD` - `2024`,
         decel_23_24 = `2024` - `2022`) %>%
  arrange(desc(foreign_share_pop))
cat("\n=== H20 regional shelter inflation, ordered by foreign-born population share ===\n")
print(as.data.frame(reg_sum))

# Does the rent story rescue the real wage? Native nominal wage growth comes from
# the CPS ORG work in 05_wages.R / 41_batch2_wages.R; here just supply the two
# deflators so the memo can do the arithmetic with matched windows.
defl <- cpi %>% filter(month(date) %in% 1:8) %>% mutate(yr = year(date)) %>%
  group_by(yr) %>%
  summarise(cpi_all = mean(cpi_all), cpi_less_shelter = mean(cpi_less_shelter),
            shelter = mean(shelter), .groups = "drop") %>%
  filter(yr >= 2019) %>%
  mutate(across(c(cpi_all, cpi_less_shelter, shelter), ~ .x/lag(.x) - 1,
                .names = "infl_{.col}"))
cat("\n=== Jan-Aug average price level, and inflation on matched windows ===\n")
print(as.data.frame(defl))

write_csv(cpi,     "output/b2_h20_cpi_shelter.csv")
write_csv(reg_sum, "output/b2_h20_shelter_by_region.csv")
write_csv(defl,    "output/b2_h20_deflators.csv")

cat("\nDONE 42_batch2_published.R\n")
