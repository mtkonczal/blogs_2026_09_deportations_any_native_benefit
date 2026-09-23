# ==============================================================================
# 56_state_hypothesis_scan.R
# "Kick the tires": correlate state ICE enforcement intensity (arrests per
# 100k, Jan 2025 - Jul 2026) against every state-level outcome we can build
# from BLS SAE, BLS LAUS, and CPS microdata, plus two confound/placebo checks.
# One outcome per row; rank by |r|; keep the weak ones visible rather than
# deleting them; the report can prune from here.
#
# DC is dropped from every row in this scan, not case-by-case: it is not a
# state, it is a majority-federal-employment city whose economy this window
# is dominated by the federal workforce cuts, and R/52 already showed how much
# a single such outlier can swing a 50-observation correlation. Keeping the
# scan on the same n across hypotheses also makes the |r| ranking comparable.
# ==============================================================================
suppressMessages({library(dplyr); library(readr); library(tidyr); library(purrr)})

base <- read_csv("output/state_enforcement_vs_jobs.csv", show_col_types = FALSE) %>%
  filter(state_name != "District of Columbia") %>%
  select(state_name, arrests, population_2025, arrests_per_100k,
         total_nonfarm_job_growth = job_growth)

sector <- read_csv("data/sae_state_sector_growth.csv", show_col_types = FALSE) %>%
  filter(state_name != "District of Columbia", state_name %in% state.name) %>%
  select(state_name, industry_name, growth) %>%
  pivot_wider(names_from = industry_name, values_from = growth, names_prefix = "growth_") %>%
  rename_with(~gsub(" ", "_", .x))

wage <- read_csv("data/sae_state_wage_yoy.csv", show_col_types = FALSE) %>%
  filter(state_name != "District of Columbia", state_name %in% state.name) %>%
  select(state_name, industry_name, wage_growth_yoy) %>%
  pivot_wider(names_from = industry_name, values_from = wage_growth_yoy, names_prefix = "wage_yoy_") %>%
  rename_with(~gsub(" ", "_", .x))

# Total private wage growth has the same problem as the job-growth level
# comparisons in R/52: high-enforcement states could just have had faster
# wage growth already, before the window started. Fix: difference this
# year's YoY wage growth against last year's YoY wage growth (both NSA, same
# calendar month, so no seasonality contamination), same logic as R/57d for
# employment. Replaces the level version in the scan below (see R/57e for
# the level-vs-diff comparison; the level result does not survive this).
sae_raw <- readRDS("data_raw/sae_raw.rds")
latest_wage_month <- sae_raw %>%
  filter(data_type_text == "Average Hourly Earnings of All Employees, In Dollars",
         industry_name == "Total Private", area_code == "00000") %>%
  summarise(m = max(date)) %>% pull(m)
wage_diff <- sae_raw %>%
  filter(data_type_text == "Average Hourly Earnings of All Employees, In Dollars",
         industry_name == "Total Private", area_code == "00000",
         state_name %in% state.name,
         date %in% c(latest_wage_month - lubridate::years(2),
                     latest_wage_month - lubridate::years(1),
                     latest_wage_month)) %>%
  select(state_name, date, value) %>%
  pivot_wider(names_from = date, values_from = value)
names(wage_diff) <- c("state_name", "w_y2", "w_y1", "w_latest")
wage_diff <- wage_diff %>%
  transmute(state_name,
            wage_yoy_Total_Private = w_latest / w_y1 - 1 - (w_y1 / w_y2 - 1))

wage <- wage %>% select(-wage_yoy_Total_Private)  # replaced by the trend-differenced version above

laus_raw <- read_csv("data/laus_state_raw.csv", show_col_types = FALSE) %>%
  filter(state != "District of Columbia", state %in% state.name)
laus <- laus_raw %>%
  transmute(
    state_name = state,
    d_unrate      = `unemployment rate_2026-07-01` - `unemployment rate_2025-01-01`,
    labor_force_growth = `labor force_2026-07-01` / `labor force_2025-01-01` - 1,
    hh_employment_growth = `employment_2026-07-01` / `employment_2025-01-01` - 1,
    d_epop        = `employment-population ratio_2026-07-01` - `employment-population ratio_2025-01-01`,
    d_lfpr        = `labor force participation rate_2026-07-01` - `labor force participation rate_2025-01-01`,
    unrate_level_jan2025 = `unemployment rate_2025-01-01`
  )

cps <- read_csv("data/cps_state_panel.csv", show_col_types = FALSE) %>%
  filter(state_name != "District of Columbia", state_name %in% state.name) %>%
  select(state_name, d_nat_epop, d_nat_unrate, d_for_epop, d_for_unrate,
         d_log_for_pop, for_share_pre, construction_emp_share_pre = construction_emp_share_pre)

panel <- base %>%
  left_join(sector, by = "state_name") %>%
  left_join(wage, by = "state_name") %>%
  left_join(wage_diff, by = "state_name") %>%
  left_join(laus, by = "state_name") %>%
  left_join(cps, by = "state_name")

write_csv(panel, "data/state_hypothesis_panel.csv")
cat("Master panel: ", nrow(panel), "states x", ncol(panel), "columns\n")

# ---- hypothesis registry ----------------------------------------------------
H <- tribble(
  ~var, ~label, ~category,
  "total_nonfarm_job_growth",        "Total nonfarm payroll job growth (all industries)",         "Labor demand",
  "growth_Construction",             "Construction payroll job growth",                            "Labor demand",
  "growth_Leisure_and_Hospitality",  "Leisure & hospitality payroll job growth",                   "Labor demand",
  "growth_Manufacturing",            "Manufacturing payroll job growth",                           "Labor demand",
  "growth_Mining_and_Logging",       "Mining & logging payroll job growth",                        "Labor demand",
  "growth_Professional_and_Business_Services", "Professional & business services job growth",      "Placebo sector",
  "growth_Government",               "Government payroll job growth",                              "Placebo sector",
  "wage_yoy_Total_Private",          "Total private wage growth, YoY, trend-differenced (NSA)",    "Wages",
  "wage_yoy_Construction",           "Construction wage growth, YoY (NSA)",                        "Wages",
  "wage_yoy_Leisure_and_Hospitality","Leisure & hospitality wage growth, YoY (NSA)",                "Wages",
  "d_unrate",                        "Change in unemployment rate, pp (LAUS, household survey)",   "Labor demand",
  "labor_force_growth",              "Labor force growth (LAUS)",                                  "Labor supply",
  "hh_employment_growth",            "Household-survey employment growth (LAUS)",                  "Labor demand",
  "d_epop",                          "Change in employment-population ratio, pp (LAUS)",           "Labor demand",
  "d_lfpr",                          "Change in labor force participation rate, pp (LAUS)",        "Labor supply",
  "d_nat_epop",                      "Change in NATIVE prime-age EPOP, pp (CPS)",                  "CPS: native outcomes",
  "d_nat_unrate",                    "Change in NATIVE prime-age unemployment rate, pp (CPS)",     "CPS: native outcomes",
  "d_for_epop",                      "Change in FOREIGN-BORN prime-age EPOP, pp (CPS)",            "CPS: foreign-born outcomes",
  "d_for_unrate",                    "Change in FOREIGN-BORN prime-age unemployment rate, pp (CPS)","CPS: foreign-born outcomes",
  "d_log_for_pop",                   "Change in log foreign-born population (CPS, levels)",        "CPS: foreign-born outcomes",
  "for_share_pre",                   "2024 foreign-born share of state population",                "Confound / baseline",
  "construction_emp_share_pre",      "2024 construction share of state employment",                "Confound / baseline",
  "unrate_level_jan2025",            "Jan 2025 unemployment rate LEVEL (pre-existing conditions)", "Confound / baseline"
)

fit_one <- function(var) {
  d <- panel %>% select(arrests_per_100k, val = all_of(var)) %>% drop_na()
  if (nrow(d) < 10) return(tibble(n = nrow(d), r = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_, p_value = NA_real_))
  ct <- cor.test(d$arrests_per_100k, d$val)
  tibble(n = nrow(d), r = unname(ct$estimate), ci_lo = ct$conf.int[1], ci_hi = ct$conf.int[2], p_value = ct$p.value)
}

scan <- H %>% mutate(fit = map(var, fit_one)) %>% unnest(fit) %>%
  arrange(desc(abs(r)))

cat("\n=== Full hypothesis scan, ranked by |r| (arrests per 100k vs. outcome, DC excluded) ===\n")
print(scan %>% mutate(across(c(r, ci_lo, ci_hi, p_value), ~round(.x, 3))) %>%
        select(label, category, n, r, p_value), n = 30)

write_csv(scan, "output/state_hypothesis_scan.csv")
cat("\nWrote output/state_hypothesis_scan.csv (", nrow(scan), "hypotheses )\n")
cat("DONE 56_state_hypothesis_scan.R\n")
