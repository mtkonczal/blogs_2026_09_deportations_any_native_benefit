# ==============================================================================
# 03_aggregates_published.R
# H1/H2/H3 (published-series version) + H12 tightness.
# Uses BLS published CPS nativity series. NSA only, so rates are computed from
# levels and seasonally adjusted here.
# ==============================================================================
suppressMessages({
  library(tidyverse); library(lubridate); library(blsR); library(scales)
  library(zoo); library(seasonal)
})
bls_set_key(Sys.getenv("BLS_KEY"))
dir.create("output", showWarnings = FALSE); dir.create("data", showWarnings = FALSE)

pull <- function(ids, sy = 2000, ey = 2026) {
  get_n_series_table(ids, api_key = bls_get_key(), start_year = sy,
                     end_year = ey, tidy = TRUE) %>%
    mutate(across(everything(), as.numeric),
           date = as.Date(paste0(year, "/", month, "/1"))) %>%
    select(-year, -month) %>% arrange(date)
}

# --- nativity levels, NSA (BLS publishes nativity NSA only) -------------------
# native:  LF 01073413, emp 02073413, unemp 03073413, pop 00073413
# foreign: LF 01073395, emp 02073395, unemp 03073395, pop 00073395
nat_ids <- c("LNU00073413","LNU01073413","LNU02073413","LNU03073413",
             "LNU00073395","LNU01073395","LNU02073395","LNU03073395")
# prime-age (25-54) native and foreign pop/emp, summed from 10-yr bands
prime_ids <- c("LNU00073417","LNU00073418","LNU00073419",   # native pop 25-34,35-44,45-54
               "LNU02073417","LNU02073418","LNU02073419",   # native emp
               "LNU00073399","LNU00073400","LNU00073401",   # foreign pop
               "LNU02073399","LNU02073400","LNU02073401")   # foreign emp

raw  <- pull(nat_ids, 2007, 2026)
prim <- pull(prime_ids, 2007, 2026)

nat <- raw %>% transmute(
  date,
  pop_nat = LNU00073413, lf_nat = LNU01073413,
  emp_nat = LNU02073413, un_nat  = LNU03073413,
  pop_for = LNU00073395, lf_for = LNU01073395,
  emp_for = LNU02073395, un_for  = LNU03073395
) %>%
  left_join(prim %>% transmute(
    date,
    ppop_nat = LNU00073417 + LNU00073418 + LNU00073419,
    pemp_nat = LNU02073417 + LNU02073418 + LNU02073419,
    ppop_for = LNU00073399 + LNU00073400 + LNU00073401,
    pemp_for = LNU02073399 + LNU02073400 + LNU02073401
  ), by = "date") %>%
  mutate(
    unrate_nat  = un_nat / lf_nat,
    unrate_for  = un_for / lf_for,
    epop_nat    = emp_nat / pop_nat,
    epop_for    = emp_for / pop_for,
    lfpr_nat    = lf_nat / pop_nat,
    prime_epop_nat = pemp_nat / ppop_nat,
    prime_epop_for = pemp_for / ppop_for,
    foreign_share_lf  = lf_for / (lf_for + lf_nat),
    foreign_share_emp = emp_for / (emp_for + emp_nat)
  )

stopifnot(nrow(nat) > 100)
cat("date range:", format(min(nat$date)), "to", format(max(nat$date)), "\n")
cat("months present:", nrow(nat), "| Oct 2025 present?",
    as.Date("2025-10-01") %in% nat$date, "\n")

# --- seasonal adjustment ------------------------------------------------------
# X-13 needs a regular monthly ts. October 2025 (no household survey) is a true
# hole; interpolate ONLY to feed seas(), then blank the imputed month back out
# so no chart or statistic ever reports an invented October.
sa_with_gap <- function(df, col) {
  full <- tibble(date = seq(min(df$date), max(df$date), by = "month")) %>%
    left_join(df %>% select(date, v = all_of(col)), by = "date")
  missing_dates <- full$date[is.na(full$v)]
  filled <- zoo::na.approx(full$v, na.rm = FALSE)
  filled <- zoo::na.locf(filled, na.rm = FALSE)
  ts_x <- ts(filled, start = c(year(min(full$date)), month(min(full$date))),
             frequency = 12)
  out <- tryCatch(as.numeric(seasonal::final(seasonal::seas(ts_x))),
                  error = function(e) { message("seas failed for ", col, ": ", e$message); rep(NA_real_, length(filled)) })
  tibble(date = full$date, !!paste0(col, "_sa") := out) %>%
    mutate(across(ends_with("_sa"), ~ if_else(date %in% missing_dates, NA_real_, .x)))
}

sa_cols <- c("unrate_nat","unrate_for","epop_nat","prime_epop_nat",
             "prime_epop_for","lfpr_nat")
sa <- reduce(map(sa_cols, ~ sa_with_gap(nat, .x)), left_join, by = "date")
nat <- left_join(nat, sa, by = "date")

# --- H1: did native outcomes improve? ----------------------------------------
# Compare 12-month averages to avoid both the seasonality and the single-month
# noise, and to sidestep the January population-control discontinuity.
yr_avg <- nat %>%
  mutate(period = case_when(
    date >= as.Date("2024-01-01") & date <= as.Date("2024-12-01") ~ "2024",
    date >= as.Date("2025-01-01") & date <= as.Date("2025-12-01") ~ "2025",
    date >= as.Date("2026-01-01") ~ "2026 YTD",
    date >= as.Date("2019-01-01") & date <= as.Date("2019-12-01") ~ "2019",
    TRUE ~ NA_character_)) %>%
  filter(!is.na(period)) %>%
  group_by(period) %>%
  summarise(across(c(unrate_nat, unrate_for, epop_nat, prime_epop_nat,
                     prime_epop_for, lfpr_nat, foreign_share_lf),
                   ~ mean(.x, na.rm = TRUE)), n_months = n(), .groups = "drop")

print(as.data.frame(yr_avg))
write_csv(yr_avg, "output/h1_native_annual_averages.csv")

# latest 6m vs Jul-Dec 2024 (pre-inauguration), same months to control seasonality
h1_recent <- nat %>%
  filter(date >= as.Date("2024-01-01")) %>%
  select(date, unrate_nat, unrate_for, prime_epop_nat, prime_epop_for, lfpr_nat,
         unrate_nat_sa, prime_epop_nat_sa, prime_epop_for_sa)
write_csv(h1_recent, "output/h1_native_monthly.csv")
write_csv(nat, "data/nativity_published.csv")

cat("\n--- H1 headline (SA) ---\n")
h1 <- nat %>% filter(date %in% c(as.Date("2024-12-01"), max(nat$date)))
print(as.data.frame(h1 %>% select(date, unrate_nat_sa, prime_epop_nat_sa,
                                  prime_epop_for_sa, lfpr_nat_sa)))

# --- H3: decompose the rise in the native share of employment ----------------
# Native share of employment rises if (a) natives are more likely to work, or
# (b) foreign-born employment/population simply shrank. Only (a) is a benefit.
d0 <- nat %>% filter(date == as.Date("2024-12-01"))
d1 <- nat %>% filter(date == max(nat$date))
share <- function(d) d$emp_nat / (d$emp_nat + d$emp_for)
# counterfactual: hold native EPOP at its Dec-2024 value, let populations move
cf_emp_nat <- d1$pop_nat * d0$epop_nat
h3 <- tibble(
  actual_share_dec2024 = share(d0),
  actual_share_latest  = share(d1),
  share_if_native_epop_unchanged = cf_emp_nat / (cf_emp_nat + d1$emp_for),
  native_epop_dec2024 = d0$epop_nat,
  native_epop_latest  = d1$epop_nat,
  foreign_emp_change  = d1$emp_for - d0$emp_for,
  native_emp_change   = d1$emp_nat - d0$emp_nat,
  native_pop_change   = d1$pop_nat - d0$pop_nat,
  foreign_pop_change  = d1$pop_for - d0$pop_for
)
print(as.data.frame(t(h3)))
write_csv(h3, "output/h3_share_decomposition.csv")

# --- H2: native unrate vs its historical relationship to the overall rate -----
# Two right-hand sides. (a) the overall unemployment rate, which is contaminated
# because natives are a rising share of it; (b) a demand-side variable that is
# not a composite of native and foreign outcomes.
overall <- pull(c("LNS14000000","LNS13000000","LNS11000000"), 1994, 2026) %>%
  transmute(date, overall_unrate = LNS14000000 / 100)

jolts <- pull(c("JTS000000000000000JOR","JTS000000000000000QUR",
                "JTS000000000000000HIR","JTS000000000000000LDR"), 2001, 2026) %>%
  transmute(date,
            openings_rate = JTS000000000000000JOR / 100,
            quits_rate    = JTS000000000000000QUR / 100,
            hires_rate    = JTS000000000000000HIR / 100,
            layoffs_rate  = JTS000000000000000LDR / 100)

reg_df <- nat %>%
  select(date, unrate_nat_sa, lf_nat, lf_for, unrate_for) %>%
  left_join(overall, by = "date") %>%
  left_join(jolts, by = "date") %>%
  filter(!is.na(unrate_nat_sa), !is.na(overall_unrate))

# (a) naive log-log, trained pre-2020
m_naive <- lm(log(unrate_nat_sa) ~ log(overall_unrate),
              data = reg_df %>% filter(year(date) < 2020))
# (b) composition-held-fixed overall rate: reweight native and foreign unrates
#     using the Dec-2024 labor force shares, so the RHS cannot drift simply
#     because the foreign-born labor force shrank.
w0 <- nat %>% filter(date == as.Date("2024-12-01")) %>%
  transmute(w_nat = lf_nat/(lf_nat+lf_for), w_for = lf_for/(lf_nat+lf_for))
reg_df <- reg_df %>%
  mutate(overall_fixedcomp = w0$w_nat * unrate_nat_sa + w0$w_for * unrate_for)
# (c) demand-side RHS: JOLTS openings rate
m_jolts <- lm(log(unrate_nat_sa) ~ log(openings_rate),
              data = reg_df %>% filter(year(date) < 2020, !is.na(openings_rate)))

reg_df <- reg_df %>%
  mutate(pred_naive = exp(predict(m_naive, newdata = .)),
         pred_jolts = exp(predict(m_jolts, newdata = .)),
         gap_naive  = unrate_nat_sa - pred_naive,
         gap_jolts  = unrate_nat_sa - pred_jolts)

cat("\n--- H2 log-log coefficients ---\n")
print(coef(summary(m_naive))); print(coef(summary(m_jolts)))
cat("\n--- H2 gaps, 12m averages ---\n")
print(reg_df %>% mutate(yr = year(date)) %>% filter(yr >= 2023) %>%
        group_by(yr) %>%
        summarise(actual = mean(unrate_nat_sa, na.rm=TRUE),
                  pred_naive = mean(pred_naive, na.rm=TRUE),
                  gap_naive  = mean(gap_naive, na.rm=TRUE),
                  pred_jolts = mean(pred_jolts, na.rm=TRUE),
                  gap_jolts  = mean(gap_jolts, na.rm=TRUE)) %>% as.data.frame())
write_csv(reg_df, "output/h2_native_vs_prediction.csv")

# --- H12: is the labor market actually tight? --------------------------------
tight <- jolts %>%
  left_join(pull("LNS13000000", 2001, 2026) %>%
              transmute(date, unemployed = LNS13000000), by = "date") %>%
  left_join(pull("JTS000000000000000JOL", 2001, 2026) %>%
              transmute(date, openings = JTS000000000000000JOL), by = "date") %>%
  mutate(vu = openings / unemployed) %>%
  filter(!is.na(vu))
cat("\n--- H12 tightness ---\n")
print(tight %>% mutate(yr = year(date)) %>% filter(yr >= 2019) %>% group_by(yr) %>%
        summarise(vu = mean(vu, na.rm=TRUE), quits = mean(quits_rate, na.rm=TRUE),
                  hires = mean(hires_rate, na.rm=TRUE),
                  openings = mean(openings_rate, na.rm=TRUE)) %>% as.data.frame())
write_csv(tight, "output/h12_tightness.csv")
cat("\nDONE 03_aggregates_published.R\n")
