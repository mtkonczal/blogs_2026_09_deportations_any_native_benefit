# ==============================================================================
# 62_industry_wage_exposure.R   H6, industry cross-section version
#
# This is the blogs_2026_04 (noncitizens-by-industry) methodology, not the
# CPS-microdata dose-response in 05_wages.R: non-citizen share by NAICS
# industry from the 2024 ACS PUMS, crosswalked onto BLS CES's 249 diffusion
# industries and quintiled (data/ces_with_immigration.csv, copied verbatim
# from that project -- it is a static 2024 ACS estimate, so there is nothing
# to re-derive). Wage growth by quintile then comes entirely from BLS-published
# CES establishment data. Zero household-survey microdata anywhere in this
# script.
#
# Headline series: average hourly earnings, production/nonsupervisory
# employees (data_type_code 08) -- per the 2026-04 post, this is the worker
# subset most likely to overlap with the non-citizen workforce in
# high-immigration industries. All-employee AHE (03) kept as a robustness
# check. Deflated by CPI-U so this reports real wage growth, consistent with
# H5/H6 elsewhere in this project (2026-04 used nominal YoY growth).
# ==============================================================================
suppressMessages({
  library(tidyusmacro); library(dplyr); library(tidyr); library(readr)
  library(lubridate); library(blsR)
})
bls_set_key(Sys.getenv("BLS_KEY"))
email <- Sys.getenv("EMAIL_FOR_BLS", unset = "mike@economicsecurityproject.org")

# --- pull full CES flat file (all industries, all data types) ---------------
cache <- "data_raw/ces_full_raw.rds"
if (file.exists(cache)) {
  message("Using cached CES pull: ", cache)
  ces <- readRDS(cache)
} else {
  ces <- getBLSFiles("ces", email)
  saveRDS(ces, cache)
  write_lines(format(Sys.time(), tz = "UTC", usetz = TRUE), "data_raw/ces_download_timestamp.txt")
}
ces <- ces %>% mutate(date = as.Date(paste0(year, "-", substr(period, 2, 3), "-01")))
diff_ind <- tidyusmacro::cesDiffusionIndex$industry_code
cat("CES rows:", nrow(ces), " diffusion industries:", length(diff_ind), "\n")

sub <- function(dtc) ces %>%
  filter(seasonal == "S", data_type_code == dtc, industry_code %in% diff_ind) %>%
  select(industry_code, date, value)

ces_emp         <- sub("01")  # employment (weights)
ces_nonsup_wage <- sub("08")  # AHE, production/nonsupervisory -- headline
ces_all_wage    <- sub("03")  # AHE, all employees -- robustness

# --- non-citizen quintile, identical construction to blogs_2026_04 ----------
imm <- read_csv("data/ces_with_immigration.csv", show_col_types = FALSE) %>%
  mutate(industry_code = as.character(industry_code)) %>%
  select(industry_code, industry_name, acs_pct_foreign_born, acs_pct_noncitizen)
stopifnot(sum(is.na(imm$acs_pct_noncitizen)) == 0)

qb <- quantile(imm$acs_pct_noncitizen, probs = c(.2, .4, .6, .8), na.rm = TRUE)
imm <- imm %>% mutate(nc_quintile = case_when(
  acs_pct_noncitizen <= qb[1] ~ "Q1 (lowest)",
  acs_pct_noncitizen <= qb[2] ~ "Q2",
  acs_pct_noncitizen <= qb[3] ~ "Q3",
  acs_pct_noncitizen <= qb[4] ~ "Q4",
  TRUE ~ "Q5 (highest)"),
  nc_quintile = factor(nc_quintile,
    levels = c("Q1 (lowest)", "Q2", "Q3", "Q4", "Q5 (highest)")))
cat("\n=== quintile boundaries, % non-citizen ===\n"); print(qb)
cat("\n=== industries per quintile ===\n")
print(imm %>% count(nc_quintile))

# --- CPI-U, same series/base convention as 05_wages.R ------------------------
cpi <- get_n_series_table("CUUR0000SA0", api_key = bls_get_key(),
                          start_year = 2014, end_year = 2026, tidy = TRUE) %>%
  transmute(cpi = suppressWarnings(as.numeric(CUUR0000SA0)),
            date = as.Date(paste0(year, "/", month, "/1"))) %>% filter(!is.na(cpi))
cpi_base <- cpi$cpi[which.max(cpi$date)]
cat("\ndeflating to", format(max(cpi$date), "%B %Y"), "dollars\n")

# --- build one quintile x date real-wage series ------------------------------
build_series <- function(wage_dt, min_industries) {
  latest_ok <- wage_dt %>% group_by(date) %>% summarize(n = n(), .groups = "drop") %>%
    filter(n >= min_industries) %>% summarize(max_date = max(date)) %>% pull()
  wage_dt %>%
    filter(date <= latest_ok) %>%
    inner_join(imm, by = "industry_code") %>%
    inner_join(ces_emp %>% rename(employment = value), by = c("industry_code","date")) %>%
    inner_join(cpi, by = "date") %>%
    mutate(real_wage = value * cpi_base / cpi) %>%
    filter(!is.na(real_wage), !is.na(employment)) %>%
    group_by(date, nc_quintile) %>%
    summarize(avg_wage = weighted.mean(real_wage, employment), n_ind = n(), .groups = "drop") %>%
    arrange(nc_quintile, date) %>%
    group_by(nc_quintile) %>%
    mutate(yoy = avg_wage / lag(avg_wage, 12) - 1) %>%
    ungroup()
}

# thresholds mirror blogs_2026_04's "near-full reporting" caps (150/190 of 249)
nonsup_series <- build_series(ces_nonsup_wage, 150)
all_series    <- build_series(ces_all_wage,    190)

cat("\n=== nonsup: latest date used ===\n"); print(max(nonsup_series$date))
cat("\n=== nonsup real wage YoY growth by quintile, last 4 obs each ===\n")
print(as.data.frame(nonsup_series %>% group_by(nc_quintile) %>% slice_tail(n = 4) %>%
                      mutate(yoy = round(100*yoy, 2))))

write_csv(nonsup_series, "output/h6_industry_wage_by_quintile.csv")
write_csv(all_series,    "output/h6_industry_wage_by_quintile_allemp.csv")

# --- January-August like-for-like annual bars, matching the rest of the post -
jan_aug_bars <- function(series_dt) {
  series_dt %>% filter(month(date) <= 8, !is.na(yoy)) %>%
    mutate(yr = year(date)) %>% group_by(nc_quintile, yr) %>%
    summarize(yoy = mean(yoy, na.rm = TRUE), .groups = "drop")
}
nonsup_jan_aug <- jan_aug_bars(nonsup_series)
cat("\n=== H6 industry cross-section: real nonsup wage growth, Jan-Aug avg YoY ===\n")
print(as.data.frame(nonsup_jan_aug %>% mutate(yoy = round(100*yoy, 2)) %>%
                      pivot_wider(names_from = yr, values_from = yoy)))
write_csv(nonsup_jan_aug, "output/h6_industry_wage_jan_aug.csv")

cat("\nDONE 62_industry_wage_exposure.R\n")
