# ==============================================================================
# 08_geography.R   H9 and H10.
# H9: did native outcomes improve more in high-immigrant-share states?
# H10: split exposed industries into those that EMPLOYED immigrants and those
#      that SOLD to them. Substitution predicts native gains in the first.
#      A local demand shock predicts native losses in both.
#
# Caveat carried into the writeup: CPS state cells are thin. Everything here is
# 12-month averaged and the standard errors are reported.
# ==============================================================================
suppressMessages({
  library(data.table); library(dplyr); library(readr); library(lmtest); library(sandwich)
})
cps <- readRDS("data_raw/cps_analysis.rds"); setDT(cps)
st_exp <- as.data.table(read_csv("data/exposure_state.csv", show_col_types=FALSE))

win <- function(a, b, lbl) {
  cps[date >= as.Date(a) & date <= as.Date(b) & prime == 1, .(
    nat_epop   = sum(WTFINL*emp*native)/sum(WTFINL*native),
    nat_lfpr   = sum(WTFINL*inlf*native)/sum(WTFINL*native),
    nat_unrate = sum(WTFINL*unemp*native)/sum(WTFINL*inlf*native),
    for_epop   = sum(WTFINL*emp*foreign_born)/sum(WTFINL*foreign_born),
    for_pop    = sum(WTFINL*foreign_born),
    nat_n      = sum(native)
  ), by = STATEFIP][, per := lbl][]
}
mk <- function(pre_a, pre_b, post_a, post_b) {
  m <- merge(win(pre_a, pre_b, "pre"), win(post_a, post_b, "post"),
             by = "STATEFIP", suffixes = c("_0","_1"))
  m <- merge(m, st_exp[, .(STATEFIP, foreign_share, emp_tot)], by = "STATEFIP")
  m[, `:=`(d_epop = nat_epop_1 - nat_epop_0,
           d_lfpr = nat_lfpr_1 - nat_lfpr_0,
           d_unrate = nat_unrate_1 - nat_unrate_0,
           d_log_for_pop = log(for_pop_1) - log(for_pop_0))]
  m
}
treat <- mk("2023-09-01","2024-08-01","2025-09-01","2026-08-01")
plac  <- mk("2017-09-01","2018-08-01","2018-09-01","2019-08-01")

fitst <- function(m, y, lbl) {
  mod <- lm(as.formula(paste(y, "~ foreign_share")), data = m, weights = m$emp_tot)
  ct <- coeftest(mod, vcov = vcovHC(mod, "HC1"))
  data.frame(outcome = y, spec = lbl, beta = ct[2,1], se = ct[2,2], p = ct[2,4],
             n = nobs(mod))
}
res <- bind_rows(lapply(c("d_epop","d_lfpr","d_unrate","d_log_for_pop"),
                        function(y) fitst(treat, y, "2025-26")),
                 lapply(c("d_epop","d_lfpr","d_unrate","d_log_for_pop"),
                        function(y) fitst(plac, y, "PLACEBO 2018-19")))
cat("=== H9: state dose-response (beta = 0%->100% foreign-born share) ===\n")
print(res %>% mutate(across(c(beta,se), ~round(.x,4)), p = round(p,4)))
write_csv(res, "output/h9_state_dose_response.csv")
write_csv(treat, "output/h9_state_cells.csv")

# --- H10: immigrant-EMPLOYING vs immigrant-SERVING industries ----------------
ind_exp <- as.data.table(read_csv("data/exposure_ind.csv", show_col_types=FALSE))
EMPLOYING <- c(60,  # construction
               100,101,102,110,111,112,120,121,122,130,  # food/meat processing
               010,011,012,020,030,031,032,  # agriculture
               721,722,742,750,751,752,  # building/grounds services
               761,762,770)
SERVING   <- c(580,581,590,591,600,601,610,611,612,620,621,622,623,630,631,632,
               633,640,641,642,650,651,652,660,661,662,670,671,  # retail
               641,642,  # eating and drinking
               770,780,781,782,790,791,800,801,802)
cps[, ind_grp := fifelse(IND1990 %in% EMPLOYING, "Immigrant-employing",
                  fifelse(IND1990 %in% SERVING,  "Immigrant-serving", NA_character_))]
# Per-MONTH averages, not sums: 2026 has only 8 months of data and summing the
# monthly weights would make a partial year look like a collapse.
# Levels by nativity are contaminated by the reweighting artifact (Kolko), so
# the headline here is each group's SHARE of employment within the industry
# block and the native unemployment rate, not the level.
i10 <- cps[emp == 1 & !is.na(ind_grp), .(
  nat_emp = sum(WTFINL*native)/1000, for_emp = sum(WTFINL*foreign_born)/1000, n = .N
), by = .(ind_grp, yr = data.table::year(date), mo = data.table::month(date))
][, .(nat_emp = mean(nat_emp), for_emp = mean(for_emp), months = .N, n = sum(n)),
  by = .(ind_grp, yr)][order(ind_grp, yr)]
i10[, `:=`(g_nat = nat_emp/shift(nat_emp)-1, g_for = for_emp/shift(for_emp)-1), by = ind_grp]

# native unemployment rate by last industry, the rate-based version
iu <- cps[!is.na(ind_grp) & inlf == 1 & prime == 1, .(
  nat_unrate = sum(WTFINL*native*unemp)/sum(WTFINL*native*inlf),
  nat_hrs = sum(WTFINL*native*emp*uhrs, na.rm=TRUE)/
            sum(WTFINL*native*emp*!is.na(uhrs), na.rm=TRUE), n = .N
), by = .(ind_grp, yr = data.table::year(date))][order(ind_grp, yr)]
cat("\n=== H10: employment levels by industry role (monthly average, thousands) ===\n")
cat("    [levels are contaminated by the nativity reweighting artifact]\n")
print(as.data.frame(i10[yr >= 2023]))
cat("\n=== H10: NATIVE unemployment rate and hours by industry role (robust) ===\n")
print(as.data.frame(iu[yr >= 2023]))
write_csv(iu, "output/h10_industry_rates.csv")
write_csv(i10, "output/h10_industry_roles.csv")
cat("\nDONE 08_geography.R\n")
