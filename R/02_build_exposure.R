# ==============================================================================
# 02_build_exposure.R
# Pre-period foreign-born share of employment by occupation, industry, and
# state. Computed ONCE here and written to data/, so every dose-response test
# uses an identical exposure measure.
#
# Pre-period = 2022m1-2024m12 (post-pandemic-reallocation, pre-enforcement).
# ==============================================================================
suppressMessages({library(data.table); library(dplyr); library(readr)})
cps <- readRDS("data_raw/cps_analysis.rds")
setDT(cps)

PRE <- cps[date >= as.Date("2022-01-01") & date <= as.Date("2024-12-01") & emp == 1]

mk <- function(dt, key) {
  out <- dt[!is.na(get(key)), .(
    emp_tot     = sum(WTFINL),
    emp_for     = sum(WTFINL * foreign_born),
    emp_nat     = sum(WTFINL * native),
    emp_noncit  = sum(WTFINL * noncitizen),
    n_obs       = .N,
    n_obs_for   = sum(foreign_born)
  ), by = c(key)]
  out[, `:=`(foreign_share  = emp_for / emp_tot,
             noncit_share   = emp_noncit / emp_tot)]
  out[order(-emp_tot)]
}

occ_exp   <- mk(PRE[OCC2010 < 9900], "OCC2010")
ind_exp   <- mk(PRE[IND1990 < 998],  "IND1990")
state_exp <- mk(PRE, "STATEFIP")

# Cell-size floor: dose-response on a cell with 40 foreign-born observations
# across three years is noise. Floor stated in the post.
MIN_OBS <- 400   # unweighted employed person-months in the 36-month pre-period
occ_exp[, usable := n_obs >= MIN_OBS]
ind_exp[, usable := n_obs >= MIN_OBS]
state_exp[, usable := TRUE]

cat("occupations:", nrow(occ_exp), " usable:", sum(occ_exp$usable), "\n")
cat("industries :", nrow(ind_exp), " usable:", sum(ind_exp$usable), "\n")
cat("\n=== highest foreign-born share occupations (usable) ===\n")
print(head(occ_exp[usable == TRUE][order(-foreign_share)], 12))
cat("\n=== highest foreign-born share industries (usable) ===\n")
print(head(ind_exp[usable == TRUE][order(-foreign_share)], 12))
cat("\n=== state exposure, top and bottom ===\n")
print(rbind(head(state_exp[order(-foreign_share)], 6),
            head(state_exp[order(foreign_share)], 6)))

write_csv(occ_exp,   "data/exposure_occ.csv")
write_csv(ind_exp,   "data/exposure_ind.csv")
write_csv(state_exp, "data/exposure_state.csv")
cat("\nDONE 02_build_exposure.R\n")
