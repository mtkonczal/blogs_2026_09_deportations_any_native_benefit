# ==============================================================================
# 69_state_first_stage.R   Design diagnostic for the state-level native test
#
# The CES payroll test (Hypothesis 10) cannot separate "flat because nobody is
# hiring" from "flat because natives replaced departing immigrants". The native-
# specific version needs (1) an exposure measure that actually predicts where
# immigrant employment fell (a first stage), and (2) a native outcome measured
# as a rate. This script checks candidate exposures against candidate first-
# stage outcomes before any native result is read.
#
# Exposures (predetermined unless noted):
#   E1 noncitizen share of employment, 2022-2024
#   E2 recent-arrival share of employment, 2024 (foreign-born, YRIMMIG >= 56,
#      i.e. arrived 2020 or later: the inflow most exposed to removals and to
#      parole/TPS terminations)
#   E3 ICE arrests per 100k residents, Jan 2025-Jul 2026 (not predetermined)
#   E4 ICE arrests per 1,000 noncitizen residents (CPS 2024 noncitizen pop)
# First-stage outcomes, 2024 vs Sep 2025-Aug 2026 (same windows as 55):
#   F1 change in employed noncitizens as % of state pop 16+, pp
#   F2 change in employed foreign-born as % of state pop 16+, pp
#   F3 change in log unweighted noncitizen respondents (nonresponse diagnostic)
# Native outcomes: N1 native prime-age EPOP, N2 native prime-age men without BA.
# DC excluded, as in 56. WTFINL weights.
# ==============================================================================
suppressMessages({library(ipumsr); library(data.table); library(readr); library(tigris)})

ddi <- read_ipums_ddi(readLines("data_raw/ddi_path.txt")[1])
VARS <- c("YEAR","MONTH","WTFINL","AGE","SEX","CITIZEN","EMPSTAT","STATEFIP","YRIMMIG","EDUC")
acc <- list(); i <- 0
cb <- function(x, pos) {
  i <<- i + 1; setDT(x)
  x <- x[AGE >= 16 & YEAR >= 2022, ..VARS]
  for (j in names(x)) set(x, j = j, value = as.numeric(x[[j]]))
  acc[[i]] <<- x; NULL
}
read_ipums_micro_chunked(ddi, callback = IpumsSideEffectCallback$new(cb),
                         chunk_size = 2e6, verbose = FALSE)
d <- rbindlist(acc); rm(acc); invisible(gc())
d[, `:=`(date = as.Date(sprintf("%d-%02d-01", YEAR, MONTH)),
         native = as.integer(CITIZEN %in% c(1, 2, 3)), foreign = as.integer(CITIZEN %in% c(4, 5)),
         noncit = as.integer(CITIZEN == 5), emp = as.integer(EMPSTAT %in% c(10, 12)),
         prime = as.integer(AGE >= 25 & AGE <= 54), male = as.integer(SEX == 1),
         noba = as.integer(EDUC > 1 & EDUC < 111))]
d[, recent := as.integer(foreign == 1 & YRIMMIG >= 56 & YRIMMIG < 900)]

data(fips_codes)
xw <- unique(data.table(STATEFIP = as.integer(fips_codes$state_code), state_name = fips_codes$state_name))
d <- merge(d, xw, by = "STATEFIP")
d <- d[state_name %in% state.name]

expo <- merge(
  d[date <= as.Date("2024-12-01"), .(E1_noncit_share_emp = sum(WTFINL*emp*noncit)/sum(WTFINL*emp)), by = state_name],
  d[year(date) == 2024, .(E2_recent_share_emp = sum(WTFINL*emp*recent)/sum(WTFINL*emp),
                          noncit_pop_2024 = sum(WTFINL*noncit)/12), by = state_name], by = "state_name")
ice <- as.data.table(read_csv("output/state_enforcement_vs_jobs.csv", show_col_types = FALSE))[
  , .(state_name, arrests, E3_arrests_per_100k = arrests_per_100k)]
expo <- merge(expo, ice, by = "state_name")
expo[, E4_arrests_per_1k_noncit := 1000 * arrests / noncit_pop_2024]

win <- function(a, b) d[date >= a & date <= b, .(
  nc_emp_pop = 100*sum(WTFINL*emp*noncit)/sum(WTFINL),
  fb_emp_pop = 100*sum(WTFINL*emp*foreign)/sum(WTFINL),
  n_noncit   = sum(noncit),
  nat_epop   = 100*sum(WTFINL*emp*native*prime)/sum(WTFINL*native*prime),
  nat_noba_men_epop = 100*sum(WTFINL*emp*native*prime*male*noba)/sum(WTFINL*native*prime*male*noba)
), by = state_name]
pre  <- win(as.Date("2024-01-01"), as.Date("2024-12-01"))
post <- win(as.Date("2025-09-01"), as.Date("2026-08-01"))
m <- merge(pre, post, by = "state_name", suffixes = c("_pre", "_post"))
m[, `:=`(F1_d_noncit_emp_pop = nc_emp_pop_post - nc_emp_pop_pre,
         F2_d_fb_emp_pop     = fb_emp_pop_post - fb_emp_pop_pre,
         F3_dlog_noncit_resp = log(n_noncit_post) - log(n_noncit_pre),
         N1_d_nat_epop       = nat_epop_post - nat_epop_pre,
         N2_d_nat_noba_men   = nat_noba_men_epop_post - nat_noba_men_epop_pre)]
m <- merge(m, expo, by = "state_name")
cat("states:", nrow(m), "\n")
write_csv(m, "output/h10_state_first_stage_panel.csv")

E <- c("E1_noncit_share_emp", "E2_recent_share_emp", "E3_arrests_per_100k", "E4_arrests_per_1k_noncit")
Y <- c("F1_d_noncit_emp_pop", "F2_d_fb_emp_pop", "F3_dlog_noncit_resp", "N1_d_nat_epop", "N2_d_nat_noba_men")
tab <- rbindlist(lapply(E, function(e) rbindlist(lapply(Y, function(y) {
  ct <- cor.test(m[[e]], m[[y]])
  data.table(exposure = e, outcome = y, r = round(ct$estimate, 2), p = round(ct$p.value, 3))
}))))
cat("\n=== Correlations across 50 states (first stage = F rows; native = N rows) ===\n")
print(dcast(tab, outcome ~ exposure, value.var = "r"))
print(dcast(tab, outcome ~ exposure, value.var = "p"))
write_csv(tab, "output/h10_state_first_stage_correlations.csv")
cat("\nNational: F1 mean", round(mean(m$F1_d_noncit_emp_pop), 2), "pp; F3 mean",
    round(mean(m$F3_dlog_noncit_resp), 3), "\n")
cat("DONE 69_state_first_stage.R\n")
