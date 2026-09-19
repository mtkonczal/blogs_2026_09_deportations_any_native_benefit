# ==============================================================================
# 40_batch2_micro.R
# Batch 2, CPS microdata. One load of the 5.3 GB analysis frame, six sections.
#
#   H15  the within-year-consistent-weights claim            (claim C9,  Camarota)
#   H16  young native-born men and teens                     (claim C10)
#   H17  native men as the closest substitutes               (claim C11)
#   H18  construction                                        (claim C12)
#   H21  occupation by occupation                            (claim C15)
#   H22  native self-employment                              (claim C16)
#
# Wage-based versions of H17/H18/H21 and all of H19/H23 live in
# 41_batch2_wages.R, which needs the separate ORG earnings extract; keeping the
# two apart is a memory decision, not a design one.
#
# Outcome discipline is the same as batch 1: rates and per-worker quantities are
# the headline, levels are reported only where the claim under test is itself a
# level (H15), and are labeled as contaminated wherever they appear.
# ==============================================================================
suppressMessages({
  library(data.table); library(dplyr); library(readr); library(tidyr)
  library(lmtest); library(sandwich); library(ipumsr)
})
dir.create("output", showWarnings = FALSE)
cps <- readRDS("data_raw/cps_analysis.rds"); setDT(cps)
cat("rows:", format(nrow(cps), big.mark = ","), "\n")

occ_exp <- as.data.table(read_csv("data/exposure_occ.csv", show_col_types = FALSE))

# SEX: 1 male, 2 female (IPUMS CPS)
cps[, male := as.integer(SEX == 1)]
cps[, female := as.integer(SEX == 2)]
cps[, sexlab := fifelse(SEX == 1, "Men", fifelse(SEX == 2, "Women", NA_character_))]
cps[, yr := data.table::year(date)]
cps[, mo := data.table::month(date)]
LAST <- max(cps$date); LASTMO <- data.table::month(LAST)
cat("last month:", format(LAST), "\n")

# occupation and industry labels, pulled from the DDI so the tables can name
# the occupations rather than print numeric codes
ddi <- read_ipums_ddi(readLines("data_raw/ddi_path.txt")[1])
occ_lab <- as.data.table(ipums_val_labels(ddi, OCC2010))
setnames(occ_lab, c("OCC2010", "occ_name"))

# ==============================================================================
# H15. The within-year consistent weights claim (C9)
#
# Camarota and Zeigler (CIS, 2026-07-09) concede the January population-control
# problem and route around it by comparing January to December of the SAME year,
# where the controls never change. They report native employment gains of ~2.0M
# within 2025 and ~1.9M within 2026 through June, against 758K within 2024 and
# 367K within 2023, and read that as evidence enforcement helped natives.
#
# Step 1 replicate. Steps 2-5 are the tests the replication has to survive.
# ==============================================================================
cat("\n\n############ H15: within-year weights ############\n")

# --- step 1: replicate ---------------------------------------------------------
wy <- cps[, .(
  emp_nat   = sum(WTFINL * native * emp) / 1000,
  emp_for   = sum(WTFINL * foreign_born * emp) / 1000,
  emp_tot   = sum(WTFINL * emp) / 1000,
  pop_nat   = sum(WTFINL * native) / 1000,
  pop_for   = sum(WTFINL * foreign_born) / 1000,
  lf_nat    = sum(WTFINL * native * inlf) / 1000,
  un_nat    = sum(WTFINL * native * unemp) / 1000,
  n_noncit  = sum(noncitizen, na.rm = TRUE),      # UNWEIGHTED respondent counts
  n_native  = sum(native, na.rm = TRUE),
  n_total   = .N
), by = .(yr, mo)][order(yr, mo)]
wy[, `:=`(epop_nat = emp_nat / pop_nat, unrate_nat = un_nat / lf_nat)]

# December, or the last available month for the partial final year. 2025 has no
# October, which does not affect a Jan-to-Dec comparison.
endmo <- wy[, .(endmo = max(mo)), by = yr]
endmo[yr < max(wy$yr), endmo := 12L]

within <- merge(wy[mo == 1], endmo, by = "yr")[
  , .(yr, endmo)][
  wy, on = .(yr, endmo = mo), nomatch = 0][
  , .(yr, endmo,
      emp_nat_end = emp_nat, emp_for_end = emp_for, emp_tot_end = emp_tot,
      pop_nat_end = pop_nat, epop_nat_end = epop_nat, unrate_nat_end = unrate_nat,
      n_noncit_end = n_noncit, n_native_end = n_native, n_total_end = n_total)]
jan <- wy[mo == 1, .(yr, emp_nat_jan = emp_nat, emp_for_jan = emp_for,
                     emp_tot_jan = emp_tot, pop_nat_jan = pop_nat,
                     epop_nat_jan = epop_nat, unrate_nat_jan = unrate_nat,
                     n_noncit_jan = n_noncit, n_native_jan = n_native,
                     n_total_jan = n_total)]
h15 <- merge(jan, within, by = "yr")
h15[, `:=`(
  d_emp_nat      = emp_nat_end - emp_nat_jan,     # THE Camarota number, thousands
  d_emp_for      = emp_for_end - emp_for_jan,
  d_emp_tot      = emp_tot_end - emp_tot_jan,
  d_pop_nat      = pop_nat_end - pop_nat_jan,
  d_epop_nat     = epop_nat_end - epop_nat_jan,
  d_unrate_nat   = unrate_nat_end - unrate_nat_jan,
  d_n_noncit_pct = n_noncit_end / n_noncit_jan - 1,   # unweighted, no weights at all
  d_n_total_pct  = n_total_end / n_total_jan - 1,
  months         = endmo - 1
)]
# What share of the within-year native employment "gain" is just native
# population growth at an unchanged employment rate?
h15[, emp_nat_from_pop := d_pop_nat * epop_nat_jan]
h15[, emp_nat_from_rate := d_emp_nat - emp_nat_from_pop]
# excess nonresponse: how much faster noncitizen respondents left the sample
# than the sample as a whole, within the same year
h15[, excess_noncit_attrition := d_n_noncit_pct - d_n_total_pct]

cat("\n=== H15 step 1: within-year change, January to December, same weights ===\n")
cat("    (thousands; d_emp_nat is the number Camarota reports)\n")
print(as.data.frame(h15[, .(yr, months,
  d_emp_nat = round(d_emp_nat), d_emp_for = round(d_emp_for),
  d_emp_tot = round(d_emp_tot), d_pop_nat = round(d_pop_nat),
  d_epop_nat = round(d_epop_nat, 4), d_unrate_nat = round(d_unrate_nat, 4),
  from_pop = round(emp_nat_from_pop), from_rate = round(emp_nat_from_rate),
  excess_noncit = round(excess_noncit_attrition, 4))]))

cat("\n--- step 2, base rate: distribution of within-year native gains 2015-2024 ---\n")
base <- h15[yr <= 2024 & months == 11, d_emp_nat]
cat(sprintf("mean %.0f  median %.0f  sd %.0f  max %.0f (%d)  min %.0f (%d)\n",
            mean(base), median(base), sd(base), max(base),
            h15[yr <= 2024 & months == 11][which.max(d_emp_nat), yr],
            min(base), h15[yr <= 2024 & months == 11][which.min(d_emp_nat), yr]))
for (y in c(2025, 2026)) {
  v <- h15[yr == y, d_emp_nat]
  cat(sprintf("%d: %.0f  -> z = %.2f, percentile of 2015-2024 = %.2f\n",
              y, v, (v - mean(base))/sd(base), mean(base <= v)))
}

cat("\n--- step 3, the demographic bound ---\n")
cat("Native-born population 16+ grows by births ageing in minus deaths. It is\n")
cat("not a policy variable. Column from_pop is the employment change implied by\n")
cat("measured native population growth at an UNCHANGED January employment rate.\n")
print(as.data.frame(h15[yr >= 2023, .(yr, d_emp_nat = round(d_emp_nat),
  d_pop_nat = round(d_pop_nat), from_pop = round(emp_nat_from_pop),
  from_rate = round(emp_nat_from_rate),
  share_from_pop = round(emp_nat_from_pop / d_emp_nat, 3))]))

cat("\n--- step 4, adding up: CPS within-year total vs the payroll survey ---\n")
print(as.data.frame(h15[yr >= 2023, .(yr, months,
  cps_native = round(d_emp_nat), cps_foreign = round(d_emp_for),
  cps_total = round(d_emp_tot))]))
cat("Compare cps_total to CES payrolls over the identical window\n")
cat("(output/b2_ces_sex_monthly.csv).\n")

cat("\n--- step 5, nonresponse: does the native gain track noncitizen attrition? ---\n")
print(as.data.frame(h15[, .(yr, d_emp_nat = round(d_emp_nat),
  noncit_unwtd_pct = round(100*d_n_noncit_pct, 1),
  all_unwtd_pct = round(100*d_n_total_pct, 1),
  excess_pp = round(100*excess_noncit_attrition, 1))]))
fit15 <- lm(d_emp_nat ~ excess_noncit_attrition, data = h15)
cat("\nregression of within-year native employment gain on excess noncitizen\n")
cat("attrition (both within-year, 2015-2026):\n")
print(coef(summary(fit15)))
cat(sprintf("R-squared %.3f, n = %d\n", summary(fit15)$r.squared, nobs(fit15)))

write_csv(h15, "output/b2_h15_within_year.csv")
write_csv(wy,  "output/b2_h15_monthly_levels.csv")

# ==============================================================================
# H16. Young native-born men and teens (C10)
# ==============================================================================
cat("\n\n############ H16: young native-born men ############\n")
cps[, agegrp := fcase(AGE >= 16 & AGE <= 19, "16-19",
                      AGE >= 20 & AGE <= 24, "20-24",
                      AGE >= 25 & AGE <= 34, "25-34",
                      AGE >= 35 & AGE <= 54, "35-54",
                      AGE >= 55, "55+")]

rate_cells <- function(dt, by) {
  dt[, .(pop = sum(WTFINL), emp_w = sum(WTFINL*emp), lf_w = sum(WTFINL*inlf),
         un_w = sum(WTFINL*unemp), n = .N), by = by][
    , `:=`(epop = emp_w/pop, lfpr = lf_w/pop, unrate = un_w/lf_w)][]
}
# annual averages of monthly rates, computed monthly then averaged, so a partial
# 2026 is not distorted by seasonality within the year
mcell <- rate_cells(cps[native == 1 & !is.na(agegrp) & !is.na(sexlab)],
                    c("yr","mo","agegrp","sexlab"))
h16 <- mcell[, .(epop = mean(epop), lfpr = mean(lfpr), unrate = mean(unrate),
                 n = sum(n), months = .N), by = .(yr, agegrp, sexlab)][order(agegrp, sexlab, yr)]
h16w <- h16[yr %in% c(2019, 2023, 2024, 2025, 2026)]
h16w[, `:=`(d_epop_24_26 = epop - epop[yr == 2024],
            d_lfpr_24_26 = lfpr - lfpr[yr == 2024],
            d_unrate_24_26 = unrate - unrate[yr == 2024]), by = .(agegrp, sexlab)]
cat("\n=== native-born, by age and sex, annual averages of monthly rates ===\n")
print(as.data.frame(h16w[yr == 2026, .(agegrp, sexlab,
  epop = round(epop,4), lfpr = round(lfpr,4), unrate = round(unrate,4),
  d_epop = round(d_epop_24_26,4), d_lfpr = round(d_lfpr_24_26,4),
  d_unrate = round(d_unrate_24_26,4))][order(agegrp, sexlab)]))

# young men without a BA: the group the claim is explicitly about
ymcell <- rate_cells(cps[native == 1 & male == 1 & AGE >= 20 & AGE <= 34 &
                         noncollege == 1], c("yr","mo"))
h16nc <- ymcell[, .(epop = mean(epop), lfpr = mean(lfpr), unrate = mean(unrate),
                    n = sum(n), months = .N), by = yr][order(yr)]
cat("\n=== native men 20-34 without a BA ===\n")
print(as.data.frame(h16nc[yr >= 2019]))

write_csv(h16,   "output/b2_h16_age_sex.csv")
write_csv(h16nc, "output/b2_h16_young_men_nocollege.csv")

# ==============================================================================
# H17. Native men as the closest substitutes (C11)
# ==============================================================================
cat("\n\n############ H17: native men, the closest substitutes ############\n")

# --- 1. how male was the removed workforce? -----------------------------------
PRE <- cps[date >= as.Date("2022-01-01") & date <= as.Date("2024-12-01") & emp == 1]
maleint <- PRE[, .(male_share = sum(WTFINL*male)/sum(WTFINL), emp = sum(WTFINL)/1000/36),
  by = .(grp = fcase(noncitizen == 1, "Noncitizen",
                     naturalized == 1, "Naturalized citizen",
                     native == 1, "Native-born"))][order(-male_share)]
cat("\n=== male share of employment, 2022-2024 average ===\n")
print(as.data.frame(maleint))

# male share within the most immigrant-intensive occupations
occ_q <- occ_exp[usable == TRUE][order(foreign_share)]
occ_q[, exp_q := cut(rank(foreign_share, ties.method="first"), 4,
                     labels = c("Q1 lowest","Q2","Q3","Q4 highest"))]
PRE2 <- merge(PRE, occ_q[, .(OCC2010, foreign_share, exp_q)], by = "OCC2010")
maleq <- PRE2[, .(male_share_all = sum(WTFINL*male)/sum(WTFINL),
                  male_share_noncit = sum(WTFINL*male*noncitizen)/sum(WTFINL*noncitizen),
                  male_share_native = sum(WTFINL*male*native)/sum(WTFINL*native),
                  foreign_share = sum(WTFINL*foreign_born)/sum(WTFINL)),
              by = exp_q][order(exp_q)]
cat("\n=== male share by occupational exposure quartile, 2022-2024 ===\n")
print(as.data.frame(maleq))

# --- 2. the overlap index -----------------------------------------------------
# Overlap_g = sum_o (share of native-g employment in occupation o)
#             x (foreign-born share of occupation o), all pre-period.
overlap_for <- function(dt, label) {
  w <- dt[, .(w = sum(WTFINL)), by = OCC2010]
  w <- merge(w, occ_exp[usable == TRUE, .(OCC2010, foreign_share)], by = "OCC2010")
  w[, sh := w / sum(w)]
  data.table(grp = label, overlap = sum(w$sh * w$foreign_share), n_occ = nrow(w))
}
ov <- rbindlist(list(
  overlap_for(PRE[native == 1 & male == 1], "Native men"),
  overlap_for(PRE[native == 1 & female == 1], "Native women"),
  overlap_for(PRE[native == 1 & male == 1 & noncollege == 1], "Native men, no BA"),
  overlap_for(PRE[native == 1 & female == 1 & noncollege == 1], "Native women, no BA"),
  overlap_for(PRE[native == 1 & male == 1 & prime == 1], "Native men, prime age"),
  overlap_for(PRE[native == 1 & female == 1 & prime == 1], "Native women, prime age"),
  overlap_for(PRE[native == 1], "All native-born"),
  overlap_for(PRE[noncitizen == 1], "Noncitizens (reference)")
))
cat("\n=== overlap index: exposure-weighted competition with immigrant labor ===\n")
print(as.data.frame(ov))

# --- 3. native men vs native women, aggregate ---------------------------------
sexcell <- rate_cells(cps[!is.na(sexlab) & prime == 1],
                      c("yr","mo","sexlab","native"))
h17agg <- sexcell[, .(epop = mean(epop), lfpr = mean(lfpr), unrate = mean(unrate),
                      n = sum(n), months = .N), by = .(yr, sexlab, native)][order(sexlab, native, yr)]
h17agg[, grp := fifelse(native == 1, paste("Native", tolower(sexlab)),
                        paste("Foreign-born", tolower(sexlab)))]
cat("\n=== prime-age, by sex and nativity ===\n")
print(as.data.frame(h17agg[yr >= 2023, .(yr, grp, epop = round(epop,4),
  unrate = round(unrate,4), lfpr = round(lfpr,4))][order(grp, yr)]))

# --- 4. sex-specific occupational dose-response -------------------------------
# Same windows and the same placebo as 04_dose_response.R.
W <- list(
  treat = list(pre = c("2023-09-01","2024-08-01"), post = c("2025-09-01","2026-08-01")),
  plac  = list(pre = c("2017-09-01","2018-08-01"), post = c("2018-09-01","2019-08-01"))
)
cell_sex <- function(a, b, sexval) {
  d <- cps[date >= as.Date(a) & date <= as.Date(b) & !is.na(OCC2010) &
           OCC2010 < 9900 & SEX == sexval]
  d[, .(nat_emp = sum(WTFINL*native*emp),
        nat_lf  = sum(WTFINL*native*inlf),
        nat_un  = sum(WTFINL*native*unemp),
        for_emp = sum(WTFINL*foreign_born*emp),
        nat_hrs = sum(WTFINL*native*emp*uhrs, na.rm=TRUE) /
                  sum(WTFINL*native*emp*!is.na(uhrs), na.rm=TRUE),
        nat_ptecon = sum(WTFINL*native*emp*as.integer(WHYPTLWK %in% c(10, 60)), na.rm=TRUE) /
                     sum(WTFINL*native*emp),
        n_nat = sum(native)), by = OCC2010]
}
run_sex <- function(win, sexval) {
  a <- cell_sex(win$pre[1], win$pre[2], sexval)
  b <- cell_sex(win$post[1], win$post[2], sexval)
  m <- merge(a, b, by = "OCC2010", suffixes = c("_0","_1"))
  m <- merge(m, occ_exp[usable == TRUE, .(OCC2010, foreign_share, emp_tot)], by = "OCC2010")
  m[, `:=`(d_nat_unrate = nat_un_1/nat_lf_1 - nat_un_0/nat_lf_0,
           d_nat_hrs    = nat_hrs_1 - nat_hrs_0,
           d_log_nat_emp = log(nat_emp_1) - log(nat_emp_0))]
  m[nat_lf_0 > 0 & nat_lf_1 > 0 & n_nat_0 >= 100 & n_nat_1 >= 100]
}
fit_sex <- function(m, y, lbl) {
  mm <- m[is.finite(get(y))]
  if (nrow(mm) < 15) return(data.frame(outcome=y, spec=lbl, beta=NA, se=NA, p=NA, n=nrow(mm)))
  mod <- lm(as.formula(paste(y, "~ foreign_share")), data = mm, weights = mm$nat_emp_0)
  ct <- coeftest(mod, vcov = vcovHC(mod, "HC1"))
  data.frame(outcome = y, spec = lbl, beta = ct[2,1], se = ct[2,2], p = ct[2,4], n = nobs(mod))
}
dose_sex <- rbindlist(lapply(c(1,2), function(s) {
  slab <- ifelse(s == 1, "Men", "Women")
  tr <- run_sex(W$treat, s); pl <- run_sex(W$plac, s)
  rbindlist(list(
    rbindlist(lapply(c("d_nat_unrate","d_nat_hrs","d_log_nat_emp"),
                     function(y) as.data.table(fit_sex(tr, y, paste(slab, "2025-26"))))),
    rbindlist(lapply(c("d_nat_unrate","d_nat_hrs","d_log_nat_emp"),
                     function(y) as.data.table(fit_sex(pl, y, paste(slab, "PLACEBO 2018-19")))))
  ))
}))
cat("\n=== sex-specific occupational dose-response ===\n")
cat("beta is the effect of going from a 0% to a 100% foreign-born occupation.\n")
cat("The substitution story predicts NEGATIVE on unemployment, POSITIVE on hours.\n")
print(as.data.frame(dose_sex[, .(outcome, spec, beta = round(beta,4),
                                 se = round(se,4), p = round(p,4), n)]))

write_csv(maleint,  "output/b2_h17_male_intensity.csv")
write_csv(maleq,    "output/b2_h17_male_by_exposure_q.csv")
write_csv(ov,       "output/b2_h17_overlap_index.csv")
write_csv(h17agg,   "output/b2_h17_sex_nativity.csv")
write_csv(dose_sex, "output/b2_h17_dose_by_sex.csv")

# ==============================================================================
# H18. Construction (C12)
# ==============================================================================
cat("\n\n############ H18: construction ############\n")
# IND1990 60 = construction. Construction trades occupations OCC2010 6200-6940.
cps[, constr_ind := as.integer(IND1990 == 60)]
cps[, constr_occ := as.integer(OCC2010 >= 6200 & OCC2010 <= 6940)]

con_m <- cps[constr_ind == 1 & inlf == 1, .(
  lf = sum(WTFINL), emp_w = sum(WTFINL*emp), un_w = sum(WTFINL*unemp),
  nat_lf = sum(WTFINL*native*inlf), nat_un = sum(WTFINL*native*unemp),
  nat_emp = sum(WTFINL*native*emp), for_emp = sum(WTFINL*foreign_born*emp),
  nat_hrs = sum(WTFINL*native*emp*uhrs, na.rm=TRUE)/sum(WTFINL*native*emp*!is.na(uhrs), na.rm=TRUE),
  nat_male_emp = sum(WTFINL*native*male*emp),
  n = .N), by = .(yr, mo)]
con_m[, `:=`(nat_unrate = nat_un/nat_lf, for_share = for_emp/(for_emp+nat_emp))]
h18 <- con_m[, .(nat_unrate = mean(nat_unrate), nat_hrs = mean(nat_hrs),
                 for_share = mean(for_share), n = sum(n), months = .N), by = yr][order(yr)]
cat("\n=== construction industry: native unemployment, hours, foreign share ===\n")
print(as.data.frame(h18[yr >= 2019]))

# native men in construction trades occupations, whatever industry
ctrade <- cps[constr_occ == 1 & native == 1 & male == 1 & inlf == 1,
  .(lf = sum(WTFINL), un = sum(WTFINL*unemp), emp_w = sum(WTFINL*emp),
    hrs = sum(WTFINL*emp*uhrs, na.rm=TRUE)/sum(WTFINL*emp*!is.na(uhrs), na.rm=TRUE),
    ptecon = sum(WTFINL*emp*as.integer(WHYPTLWK %in% c(10, 60)), na.rm=TRUE)/sum(WTFINL*emp),
    n = .N), by = .(yr, mo)]
ctrade[, unrate := un/lf]
h18b <- ctrade[, .(unrate = mean(unrate), hrs = mean(hrs), ptecon = mean(ptecon),
                   n = sum(n), months = .N), by = yr][order(yr)]
cat("\n=== native MEN in construction trades occupations ===\n")
print(as.data.frame(h18b[yr >= 2019]))

write_csv(h18,  "output/b2_h18_construction_industry.csv")
write_csv(h18b, "output/b2_h18_construction_trades_men.csv")

# ==============================================================================
# H21. Occupation by occupation (C15)
# ==============================================================================
cat("\n\n############ H21: the twenty most immigrant-intensive occupations ############\n")
TOP <- occ_exp[usable == TRUE][order(-foreign_share)][1:20]
occ_out <- function(a, b, lbl) {
  d <- cps[date >= as.Date(a) & date <= as.Date(b) & OCC2010 %in% TOP$OCC2010]
  d[, .(nat_emp = sum(WTFINL*native*emp), for_emp = sum(WTFINL*foreign_born*emp),
        nat_lf = sum(WTFINL*native*inlf), nat_un = sum(WTFINL*native*unemp),
        nat_hrs = sum(WTFINL*native*emp*uhrs, na.rm=TRUE)/sum(WTFINL*native*emp*!is.na(uhrs), na.rm=TRUE),
        n_nat = sum(native), period = lbl), by = OCC2010]
}
o24 <- occ_out("2023-09-01","2024-08-01","pre")
o26 <- occ_out("2025-09-01","2026-08-01","post")
h21 <- merge(o24, o26, by = "OCC2010", suffixes = c("_0","_1"))
h21 <- merge(h21, TOP[, .(OCC2010, foreign_share)], by = "OCC2010")
h21 <- merge(h21, occ_lab, by = "OCC2010", all.x = TRUE)
h21[, `:=`(
  nat_share_0 = nat_emp_0/(nat_emp_0+for_emp_0),
  nat_share_1 = nat_emp_1/(nat_emp_1+for_emp_1),
  nat_unrate_0 = nat_un_0/nat_lf_0, nat_unrate_1 = nat_un_1/nat_lf_1
)]
h21[, `:=`(d_nat_share = nat_share_1 - nat_share_0,
           d_nat_unrate = nat_unrate_1 - nat_unrate_0,
           d_nat_hrs = nat_hrs_1 - nat_hrs_0)]
h21 <- h21[order(-foreign_share)]
cat("\n=== native outcomes in the 20 highest foreign-born-share occupations ===\n")
cat("pre = 2023m9-2024m8, post = 2025m9-2026m8\n")
print(as.data.frame(h21[, .(occ_name = substr(occ_name, 1, 44),
  fshare = round(foreign_share,3), nat_share_0 = round(nat_share_0,3),
  d_nat_share = round(d_nat_share,3), d_unrate = round(d_nat_unrate,4),
  d_hrs = round(d_nat_hrs,2), n = n_nat_1)]))
cat(sprintf("\nnative unemployment FELL in %d of %d;  hours ROSE in %d of %d;\n",
            sum(h21$d_nat_unrate < 0, na.rm=TRUE), nrow(h21),
            sum(h21$d_nat_hrs > 0, na.rm=TRUE), nrow(h21)))
cat(sprintf("native share of the occupation ROSE in %d of %d.\n",
            sum(h21$d_nat_share > 0, na.rm=TRUE), nrow(h21)))
cat("(Native share is a level-based ratio inside the occupation and inherits the\n")
cat(" reweighting problem; unemployment and hours do not.)\n")
write_csv(h21, "output/b2_h21_top20_occupations.csv")

# ==============================================================================
# H22. Native self-employment (C16)
# ==============================================================================
cat("\n\n############ H22: native self-employment ############\n")
# CLASSWKR 10/13/14 self-employed; 13 not incorporated, 14 incorporated
cps[, selfemp := as.integer(CLASSWKR %in% c(10, 13, 14))]
cps[, selfemp_uninc := as.integer(CLASSWKR == 13)]
cps[, selfemp_inc := as.integer(CLASSWKR == 14)]

se_m <- cps[emp == 1 & native == 1, .(
  emp_w = sum(WTFINL),
  se_w = sum(WTFINL*selfemp, na.rm=TRUE),
  se_uninc_w = sum(WTFINL*selfemp_uninc, na.rm=TRUE),
  se_inc_w = sum(WTFINL*selfemp_inc, na.rm=TRUE), n = .N), by = .(yr, mo)]
se_m[, `:=`(se_rate = se_w/emp_w, se_uninc_rate = se_uninc_w/emp_w,
            se_inc_rate = se_inc_w/emp_w)]
h22 <- se_m[, .(se_rate = mean(se_rate), se_uninc_rate = mean(se_uninc_rate),
                se_inc_rate = mean(se_inc_rate), n = sum(n), months = .N), by = yr][order(yr)]
cat("\n=== native self-employment rate, share of employed native-born ===\n")
print(as.data.frame(h22[yr >= 2019]))

# and in the immigrant-intensive trades where the claim is sharpest
se_q <- merge(cps[emp == 1 & native == 1 & OCC2010 < 9900],
              occ_q[, .(OCC2010, exp_q)], by = "OCC2010")
se_qm <- se_q[, .(emp_w = sum(WTFINL), se_w = sum(WTFINL*selfemp, na.rm=TRUE), n=.N),
              by = .(yr, mo, exp_q)][, se_rate := se_w/emp_w][]
h22b <- se_qm[, .(se_rate = mean(se_rate), n = sum(n)), by = .(yr, exp_q)][order(exp_q, yr)]
cat("\n=== native self-employment rate by occupational exposure quartile ===\n")
print(as.data.frame(dcast(h22b[yr >= 2022], exp_q ~ yr, value.var = "se_rate")))

# construction specifically
se_c <- cps[emp == 1 & native == 1 & constr_ind == 1,
  .(emp_w = sum(WTFINL), se_w = sum(WTFINL*selfemp, na.rm=TRUE), n=.N), by = .(yr, mo)][
  , se_rate := se_w/emp_w][]
h22c <- se_c[, .(se_rate = mean(se_rate), n = sum(n)), by = yr][order(yr)]
cat("\n=== native self-employment rate within construction ===\n")
print(as.data.frame(h22c[yr >= 2019]))

write_csv(h22,  "output/b2_h22_selfemp.csv")
write_csv(h22b, "output/b2_h22_selfemp_by_exposure.csv")
write_csv(h22c, "output/b2_h22_selfemp_construction.csv")

cat("\nDONE 40_batch2_micro.R\n")
