# ==============================================================================
# 54_state_bls_extra.R
# Additional BLS state-level series for the hypothesis scan:
#   - LAUS: state unemployment rate, labor force, employment (household survey,
#     the state analogue of the national U-3 rate).
#   - SAE industry detail: employment growth by supersector (construction,
#     leisure/hospitality, manufacturing, mining/logging, professional and
#     business services, government) -- the first three are the sectors with
#     real immigrant-employment exposure; the last two are near-zero-exposure
#     placebo sectors.
#   - SAE state average hourly earnings by industry. These are NOT seasonally
#     adjusted at the state/industry level (BLS does not publish a SA state
#     industry wage series), so wage growth here is a same-month year-over-year
#     change (Jul 2026 vs Jul 2025), NOT the Jan2025-window cumulative change
#     used for employment/arrests. Comparing NSA levels across different
#     calendar months would contaminate the result with seasonality; this is
#     the CLAUDE.md-mandated SA/NSA check, applied.
# ==============================================================================
suppressMessages({library(dplyr); library(readr); library(tidyr); library(tidyusmacro)})

email <- "mike@economicsecurityproject.org"

# ---- LAUS: state unemployment rate / labor force / employment --------------
laus_cache <- "data_raw/laus_raw.rds"
if (file.exists(laus_cache)) {
  laus <- readRDS(laus_cache)
} else {
  laus <- getBLSFiles("laus", email)
  saveRDS(laus, laus_cache)
}
cat("LAUS rows:", nrow(laus), " columns:", paste(names(laus), collapse=", "), "\n")
cat("measure_text options:\n"); print(unique(laus$measure_text))
cat("areatype_text options:\n"); print(unique(laus$areatype_text))

WIN_START <- as.Date("2025-01-01"); WIN_END <- as.Date("2026-07-01")

laus_state <- laus %>%
  filter(areatype_text == "Statewide",
         date %in% c(WIN_START, WIN_END)) %>%
  select(state = area_text, measure_text, date, value)

cat("\nDistinct states in LAUS statewide/SA slice:", length(unique(laus_state$state)), "\n")

laus_wide <- laus_state %>%
  pivot_wider(names_from = c(measure_text, date), values_from = value)
write_csv(laus_wide, "data/laus_state_raw.csv")
cat("Wrote data/laus_state_raw.csv\n")

# ---- SAE industry detail: state employment growth by supersector -----------
sae <- readRDS("data_raw/sae_raw.rds")

SECTORS <- c("Construction", "Leisure and Hospitality", "Manufacturing",
             "Mining and Logging", "Professional and Business Services", "Government")

ind_emp <- sae %>%
  filter(industry_name %in% SECTORS,
         data_type_text == "All Employees, In Thousands",
         seasonal_text == "Seasonally Adjusted",
         area_code == "00000",
         date %in% c(WIN_START, WIN_END)) %>%
  select(state_name, industry_name, date, value) %>%
  pivot_wider(names_from = date, values_from = value, names_prefix = "emp_") %>%
  rename(emp_start = 3, emp_end = 4) %>%
  filter(!is.na(emp_start), !is.na(emp_end), emp_start > 0) %>%
  mutate(growth = emp_end / emp_start - 1)

cat("\nStates with complete SA employment series by sector:\n")
print(ind_emp %>% count(industry_name))
write_csv(ind_emp, "data/sae_state_sector_growth.csv")

# ---- SAE state average hourly earnings, YoY (NSA, latest common month) -----
WAGE_SECTORS <- c("Total Private", "Construction", "Leisure and Hospitality")
latest_month <- sae %>%
  filter(data_type_text == "Average Hourly Earnings of All Employees, In Dollars",
         industry_name %in% WAGE_SECTORS, area_code == "00000") %>%
  summarise(m = max(date)) %>% pull(m)
prior_year_month <- latest_month - lubridate::years(1)
cat("\nWage YoY window:", format(prior_year_month), "to", format(latest_month), "\n")

wage_yoy <- sae %>%
  filter(data_type_text == "Average Hourly Earnings of All Employees, In Dollars",
         industry_name %in% WAGE_SECTORS, area_code == "00000",
         date %in% c(prior_year_month, latest_month)) %>%
  select(state_name, industry_name, date, value) %>%
  pivot_wider(names_from = date, values_from = value, names_prefix = "wage_") %>%
  rename(wage_start = 3, wage_end = 4) %>%
  filter(!is.na(wage_start), !is.na(wage_end)) %>%
  mutate(wage_growth_yoy = wage_end / wage_start - 1,
         window_start = prior_year_month, window_end = latest_month)

cat("\nStates with complete wage YoY series by sector:\n")
print(wage_yoy %>% count(industry_name))
write_csv(wage_yoy, "data/sae_state_wage_yoy.csv")

cat("\nDONE 54_state_bls_extra.R\n")
