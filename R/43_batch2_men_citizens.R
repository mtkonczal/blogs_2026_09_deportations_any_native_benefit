# ==============================================================================
# 43_batch2_men_citizens.R
# The two hardest versions of the restrictionist case.
#
#   H24  "The zero IS the proof."  (claim C18)
#        Male payroll growth has been about zero since December 2024. The
#        strongest reading of that is not that men did badly: it is that a
#        heavily male immigrant workforce left and native men stepped into the
#        jobs one for one, so the two cancel and the net looks flat. If true,
#        this is exactly what the substitution model predicts and it would be
#        the best evidence anyone has produced for it. Test it properly.
#
#   H25  "The real winners should be naturalized citizens."  (claim C19)
#        The economics literature that finds little or no native wage effect
#        from immigration generally does find one on PREVIOUSLY ARRIVED
#        IMMIGRANTS, because new and prior immigrants are close substitutes in
#        production while natives specialise into communication-intensive
#        tasks (Ottaviano and Peri 2012; Card 2009; Borjas 2003 for the
#        education-experience cell version). VERIFY ALL THREE CITES AND THEIR
#        POINT ESTIMATES BEFORE PUBLICATION.
#        The symmetric prediction of a large negative shock to new-immigrant
#        labor supply is therefore a GAIN for prior immigrants, above all
#        naturalized citizens, who cannot be removed and who are voters. This
#        is the substitution model's own best case, and it is untested.
#
# MEASUREMENT NOTE that makes H25 unusually strong: batch 1 found unweighted
# CPS respondent counts fell 17.4% for noncitizens, 5.6% for the native-born,
# and only 2.5% for naturalized citizens. The group the theory says should gain
# most is also the group whose survey measurement held up best. Their rates are
# the most trustworthy numbers in this project.
# ==============================================================================
suppressMessages({
  library(data.table); library(dplyr); library(readr); library(tidyr)
  library(lmtest); library(sandwich)
})
dir.create("output", showWarnings = FALSE)
cps <- readRDS("data_raw/cps_analysis.rds"); setDT(cps)
occ_exp <- as.data.table(read_csv("data/exposure_occ.csv", show_col_types = FALSE))

cps[, male := as.integer(SEX == 1)]
cps[, yr := data.table::year(date)]
cps[, mo := data.table::month(date)]
cps[, citgrp := fcase(native == 1, "Native-born",
                      naturalized == 1, "Naturalized citizen",
                      noncitizen == 1, "Noncitizen")]
cps[, citizen_any := as.integer(native == 1 | naturalized == 1)]

occ_q <- occ_exp[usable == TRUE][order(foreign_share)]
occ_q[, exp_q := cut(rank(foreign_share, ties.method = "first"), 4,
                     labels = c("Q1 lowest","Q2","Q3","Q4 highest"))]

rate_cells <- function(dt, by) {
  dt[, .(pop = sum(WTFINL), emp_w = sum(WTFINL*emp), lf_w = sum(WTFINL*inlf),
         un_w = sum(WTFINL*unemp), n = .N), by = by][
    , `:=`(epop = emp_w/pop, lfpr = lf_w/pop, unrate = un_w/lf_w)][]
}
annual <- function(mc, by) mc[, .(epop = mean(epop), lfpr = mean(lfpr),
                                  unrate = mean(unrate), n = sum(n), months = .N),
                              by = by]

# ==============================================================================
# H24. "The zero is the proof": did native men fill the vacated male jobs? (C18)
# ==============================================================================
cat("\n############ H24: the male-replacement hypothesis ############\n")

# --- 1. the arithmetic the claim requires -------------------------------------
# Male employment by nativity, December 2024 vs the latest month. These are
# LEVELS and are contaminated by the reweighting artifact in the direction that
# FAVORS the claim: when foreign-born respondents leave, weight moves onto
# native respondents in the same age-sex-race cell, so measured native male
# employment is biased UP. If native men still do not show a gain here, the
# claim is dead on its own most favourable measurement.
mlv <- cps[male == 1, .(
  emp_nat = sum(WTFINL*native*emp)/1000,
  emp_nzd = sum(WTFINL*naturalized*emp)/1000,
  emp_ncz = sum(WTFINL*noncitizen*emp)/1000,
  emp_for = sum(WTFINL*foreign_born*emp)/1000,
  emp_all = sum(WTFINL*emp)/1000,
  pop_nat = sum(WTFINL*native)/1000,
  pop_all = sum(WTFINL)/1000,
  un_nat  = sum(WTFINL*native*unemp)/1000,
  lf_nat  = sum(WTFINL*native*inlf)/1000,
  # prime-age (25-54) native men, for the monthly EPOP chart in blog_post.md.
  # The columns above are all ages 16+ and drift down with population aging.
  emp_nat_prime = sum(WTFINL*native*prime*emp)/1000,
  pop_nat_prime = sum(WTFINL*native*prime)/1000
), by = date][order(date)]
# epop_nat_men is ages 16+ (used in the H24 levels arithmetic below);
# epop_nat_men_prime is ages 25-54.
mlv[, `:=`(epop_nat_men = emp_nat/pop_nat, unrate_nat_men = un_nat/lf_nat,
           epop_nat_men_prime = emp_nat_prime/pop_nat_prime)]

d0 <- mlv[date == as.Date("2024-12-01")]; d1 <- mlv[date == max(mlv$date)]
h24a <- data.table(
  from = format(d0$date), to = format(d1$date),
  d_emp_men_all      = d1$emp_all - d0$emp_all,
  d_emp_men_native   = d1$emp_nat - d0$emp_nat,
  d_emp_men_foreign  = d1$emp_for - d0$emp_for,
  d_emp_men_noncit   = d1$emp_ncz - d0$emp_ncz,
  d_pop_men_native   = d1$pop_nat - d0$pop_nat,
  epop_nat_men_0     = d0$epop_nat_men, epop_nat_men_1 = d1$epop_nat_men,
  d_epop_nat_men     = d1$epop_nat_men - d0$epop_nat_men,
  unrate_nat_men_0   = d0$unrate_nat_men, unrate_nat_men_1 = d1$unrate_nat_men,
  d_unrate_nat_men   = d1$unrate_nat_men - d0$unrate_nat_men
)
# The replacement requirement. Native male employment can rise for two reasons:
# the native male population grew, or a larger share of it works. Only the
# second is "filling the vacated jobs". So:
#   counterfactual employment   = epop_0 x pop_1      (population growth alone)
#   full-replacement employment = epop_0 x pop_1 + (foreign-born male decline)
# and the EPOP increase the claim requires is just the decline divided by the
# native male population.
h24a[, emp_nat_counterfactual := d0$epop_nat_men * d1$pop_nat]
h24a[, emp_gain_beyond_population := d1$emp_nat - d0$epop_nat_men * d1$pop_nat]
h24a[, foreign_male_decline := -(d1$emp_for - d0$emp_for)]
h24a[, epop_required_change := foreign_male_decline / d1$pop_nat]
h24a[, epop_gap_pp := 100 * (d_epop_nat_men - epop_required_change)]
h24a[, share_of_decline_absorbed := emp_gain_beyond_population / foreign_male_decline]
cat("\n=== 1. male employment change by nativity, thousands (LEVELS, contaminated) ===\n")
print(as.data.frame(t(h24a)))
cat("\nFull replacement requires native male EPOP to RISE by",
    sprintf("%+.2f pp.\n", 100*h24a$epop_required_change))
cat("It actually changed by", sprintf("%+.2f pp", 100*h24a$d_epop_nat_men),
    sprintf("-- a gap of %.2f pp,\n", -h24a$epop_gap_pp))
cat(sprintf("about %.0f thousand jobs. Share of the foreign-born male decline\n",
            abs(h24a$epop_gap_pp/100 * d1$pop_nat)))
cat(sprintf("actually absorbed by more native men working: %.0f%%.\n",
            100*h24a$share_of_decline_absorbed))
cat("\nNote the two surveys disagree on the male total: CPS has male employment\n")
cat("up because its native levels are inflated by reweighting, while CES payrolls,\n")
cat("which are never weighted to nativity at all, show men flat. See\n")
cat("output/b2_ces_sex_change.csv.\n")

# --- 2. the clean version: native male rates, no levels anywhere ---------------
mm <- rate_cells(cps[male == 1 & prime == 1 & !is.na(citgrp)], c("yr","mo","citgrp"))
h24b <- annual(mm, c("yr","citgrp"))[order(citgrp, yr)]
cat("\n=== 2. prime-age MEN by citizenship, annual averages of monthly rates ===\n")
print(as.data.frame(h24b[yr >= 2022, .(citgrp, yr, epop = round(epop,4),
  lfpr = round(lfpr,4), unrate = round(unrate,4), months)]))

# --- 3. the occupational-mix test ---------------------------------------------
# THE clean test of "natives took the vacated jobs". If native men moved into
# the work immigrants left, the share of native male employment sitting in
# high-foreign-share occupations must RISE. This is a distribution of native
# employment across occupations, weighted by a FIXED pre-period exposure
# measure, so it is invariant to any proportional reweighting of the native
# sample: multiply every native weight by the same factor and the index is
# unchanged.
overlap_ts <- function(dt, label) {
  w <- dt[!is.na(OCC2010) & OCC2010 < 9900, .(w = sum(WTFINL)), by = .(yr, OCC2010)]
  w <- merge(w, occ_exp[usable == TRUE, .(OCC2010, foreign_share)], by = "OCC2010")
  w[, sh := w / sum(w), by = yr]
  w[, .(overlap = sum(sh * foreign_share), n_occ = .N), by = yr][, grp := label][]
}
ovt <- rbindlist(list(
  overlap_ts(cps[emp == 1 & native == 1 & male == 1], "Native men"),
  overlap_ts(cps[emp == 1 & native == 1 & SEX == 2], "Native women"),
  overlap_ts(cps[emp == 1 & native == 1 & male == 1 & noncollege == 1], "Native men, no BA"),
  overlap_ts(cps[emp == 1 & naturalized == 1], "Naturalized citizens"),
  overlap_ts(cps[emp == 1 & noncitizen == 1], "Noncitizens")
))[order(grp, yr)]
cat("\n=== 3. occupational overlap index over time (fixed pre-period exposure) ===\n")
cat("Rising = the group is moving INTO immigrant-intensive work.\n")
print(as.data.frame(dcast(ovt[yr >= 2019], grp ~ yr, value.var = "overlap")))

# share of native male employment in the top exposure quartile, same logic
shq <- merge(cps[emp == 1 & OCC2010 < 9900], occ_q[, .(OCC2010, exp_q)], by = "OCC2010")
shq_out <- shq[, .(w = sum(WTFINL)), by = .(yr, exp_q, grp = fcase(
    native == 1 & male == 1, "Native men",
    native == 1 & SEX == 2, "Native women",
    naturalized == 1, "Naturalized citizens",
    noncitizen == 1, "Noncitizens"))][!is.na(grp)]
shq_out[, sh := w/sum(w), by = .(yr, grp)]
cat("\n=== share of each group's employment in the TOP exposure quartile ===\n")
print(as.data.frame(dcast(shq_out[exp_q == "Q4 highest" & yr >= 2019],
                          grp ~ yr, value.var = "sh")))

# --- 4. inside the exposed occupations: how are native men doing? -------------
inq <- merge(cps[!is.na(OCC2010) & OCC2010 < 9900 & inlf == 1],
             occ_q[, .(OCC2010, exp_q)], by = "OCC2010")
h24d <- inq[native == 1 & male == 1, .(
  lf = sum(WTFINL*inlf), un = sum(WTFINL*unemp),
  hrs = sum(WTFINL*emp*uhrs, na.rm=TRUE)/sum(WTFINL*emp*!is.na(uhrs), na.rm=TRUE),
  ptecon = sum(WTFINL*emp*as.integer(WHYPTLWK %in% c(10,60)), na.rm=TRUE)/sum(WTFINL*emp),
  n = .N), by = .(yr, mo, exp_q)][, unrate := un/lf][]
h24d <- h24d[, .(unrate = mean(unrate), hrs = mean(hrs), ptecon = mean(ptecon),
                 n = sum(n)), by = .(yr, exp_q)][order(exp_q, yr)]
cat("\n=== 4. native MEN inside each exposure quartile: unemployment rate ===\n")
print(as.data.frame(dcast(h24d[yr >= 2022], exp_q ~ yr, value.var = "unrate")))
cat("\n=== native MEN inside each exposure quartile: usual hours ===\n")
print(as.data.frame(dcast(h24d[yr >= 2022], exp_q ~ yr, value.var = "hrs")))

write_csv(h24a, "output/b2_h24_male_replacement_arithmetic.csv")
write_csv(h24b, "output/b2_h24_men_by_citizenship.csv")
write_csv(ovt,  "output/b2_h24_overlap_over_time.csv")
write_csv(shq_out, "output/b2_h24_share_top_exposure_q.csv")
write_csv(h24d, "output/b2_h24_native_men_by_exposure.csv")
write_csv(mlv,  "output/b2_h24_male_levels_monthly.csv")

# ==============================================================================
# H25. Naturalized citizens: the substitution model's own best case (C19)
# ==============================================================================
cat("\n\n############ H25: naturalized citizens ############\n")

nzm <- rate_cells(cps[prime == 1 & !is.na(citgrp)], c("yr","mo","citgrp"))
h25a <- annual(nzm, c("yr","citgrp"))[order(citgrp, yr)]
cat("\n=== prime-age by citizenship, annual averages of monthly rates ===\n")
print(as.data.frame(h25a[yr >= 2019, .(citgrp, yr, epop = round(epop,4),
  lfpr = round(lfpr,4), unrate = round(unrate,4), n, months)]))

# by sex, since the male story and the citizenship story interact
nzs <- rate_cells(cps[prime == 1 & !is.na(citgrp)], c("yr","mo","citgrp","male"))
h25b <- annual(nzs, c("yr","citgrp","male"))[order(citgrp, male, yr)]
h25b[, sexlab := fifelse(male == 1, "Men", "Women")]
cat("\n=== prime-age by citizenship and sex ===\n")
print(as.data.frame(h25b[yr >= 2023, .(citgrp, sexlab, yr, epop = round(epop,4),
  unrate = round(unrate,4))]))

# Hispanic split: separates the substitution channel from the enforcement
# chilling channel. Under substitution, naturalized citizens gain regardless of
# ethnicity. Under chilling, Hispanic naturalized citizens do worse.
nzh <- rate_cells(cps[prime == 1 & naturalized == 1], c("yr","mo","hispanic"))
h25c <- annual(nzh, c("yr","hispanic"))[order(hispanic, yr)]
h25c[, grp := fifelse(hispanic == 1, "Naturalized, Hispanic", "Naturalized, non-Hispanic")]
cat("\n=== naturalized citizens by ethnicity ===\n")
print(as.data.frame(h25c[yr >= 2022, .(grp, yr, epop = round(epop,4),
  lfpr = round(lfpr,4), unrate = round(unrate,4), n)]))

# years since arrival. Long-settled naturalized citizens are the purest test:
# maximally substitutable with the removed workforce, minimally exposed to
# enforcement risk themselves.
# YRIMMIG in this extract is a bracketed code, not a year: 0 is NIU, 1 is 1949
# or earlier, and the codes run up to 62 for 2024-2026. Collapse to three
# arrival eras rather than inventing a precision the variable does not have.
cps[, arrgrp := fcase(naturalized != 1 | is.na(YRIMMIG) | YRIMMIG == 0, NA_character_,
                      YRIMMIG >= 50, "Arrived 2016 or later",
                      YRIMMIG >= 26, "Arrived 2000-2015",
                      default = "Arrived before 2000")]
nza <- rate_cells(cps[prime == 1 & naturalized == 1 & !is.na(arrgrp)],
                  c("yr","mo","arrgrp"))
h25d <- annual(nza, c("yr","arrgrp"))[order(arrgrp, yr)]
cat("\n=== naturalized citizens by time since arrival ===\n")
print(as.data.frame(h25d[yr >= 2022, .(arrgrp, yr, epop = round(epop,4),
  unrate = round(unrate,4), n)]))

# dose-response: do naturalized citizens do better where immigrant labor was
# thickest? This is the single sharpest test of the substitution model in the
# whole project, because naturalized citizens are the group the literature
# actually identifies as the close substitute.
W <- list(treat = list(pre = c("2023-09-01","2024-08-01"), post = c("2025-09-01","2026-08-01")),
          plac  = list(pre = c("2017-09-01","2018-08-01"), post = c("2018-09-01","2019-08-01")))
cell_cit <- function(a, b, flagcol) {
  d <- cps[date >= as.Date(a) & date <= as.Date(b) & !is.na(OCC2010) & OCC2010 < 9900]
  d[, .(g_lf  = sum(WTFINL*get(flagcol)*inlf),
        g_un  = sum(WTFINL*get(flagcol)*unemp),
        g_emp = sum(WTFINL*get(flagcol)*emp),
        g_hrs = sum(WTFINL*get(flagcol)*emp*uhrs, na.rm=TRUE) /
                sum(WTFINL*get(flagcol)*emp*!is.na(uhrs), na.rm=TRUE),
        n_g   = sum(get(flagcol))), by = OCC2010]
}
run_cit <- function(win, flagcol) {
  a <- cell_cit(win$pre[1], win$pre[2], flagcol)
  b <- cell_cit(win$post[1], win$post[2], flagcol)
  m <- merge(a, b, by = "OCC2010", suffixes = c("_0","_1"))
  m <- merge(m, occ_exp[usable == TRUE, .(OCC2010, foreign_share)], by = "OCC2010")
  m[, `:=`(d_unrate = g_un_1/g_lf_1 - g_un_0/g_lf_0, d_hrs = g_hrs_1 - g_hrs_0)]
  m[n_g_0 >= 100 & n_g_1 >= 100 & g_lf_0 > 0 & g_lf_1 > 0]
}
fit_cit <- function(m, y, lbl) {
  mm <- m[is.finite(get(y))]
  if (nrow(mm) < 12) return(data.table(outcome=y, spec=lbl, beta=NA_real_,
                                       se=NA_real_, p=NA_real_, n=nrow(mm)))
  mod <- lm(as.formula(paste(y, "~ foreign_share")), data = mm, weights = mm$g_emp_0)
  ct <- coeftest(mod, vcov = vcovHC(mod, "HC1"))
  data.table(outcome = y, spec = lbl, beta = ct[2,1], se = ct[2,2], p = ct[2,4], n = nobs(mod))
}
dose_cit <- rbindlist(lapply(c("naturalized","native"), function(fc) {
  lbl <- ifelse(fc == "naturalized", "Naturalized citizens", "Native-born")
  tr <- run_cit(W$treat, fc); pl <- run_cit(W$plac, fc)
  rbindlist(list(
    rbindlist(lapply(c("d_unrate","d_hrs"), function(y) fit_cit(tr, y, paste(lbl, "2025-26")))),
    rbindlist(lapply(c("d_unrate","d_hrs"), function(y) fit_cit(pl, y, paste(lbl, "PLACEBO 2018-19"))))
  ))
}))
cat("\n=== dose-response for naturalized citizens vs native-born ===\n")
cat("The substitution story predicts a NEGATIVE unemployment coefficient, and\n")
cat("predicts it should be LARGER for naturalized citizens than for natives.\n")
print(as.data.frame(dose_cit[, .(outcome, spec, beta = round(beta,4),
                                 se = round(se,4), p = round(p,4), n)]))

# measurement: unweighted respondent counts by citizenship, the check that makes
# the naturalized-citizen result the most trustworthy in the project
unw <- cps[, .(n_native = sum(native), n_nzd = sum(naturalized),
               n_ncz = sum(noncitizen), n_tot = .N), by = date][order(date)]
b0 <- unw[date >= as.Date("2024-01-01") & date <= as.Date("2024-12-01"),
          lapply(.SD, mean), .SDcols = c("n_native","n_nzd","n_ncz","n_tot")]
b1 <- unw[date >= as.Date("2025-09-01"), lapply(.SD, mean),
          .SDcols = c("n_native","n_nzd","n_ncz","n_tot")]
h25e <- data.table(group = c("Native-born","Naturalized citizens","Noncitizens","All"),
                   avg_2024 = as.numeric(b0), avg_recent = as.numeric(b1))
h25e[, pct_change := 100*(avg_recent/avg_2024 - 1)]
cat("\n=== unweighted CPS respondents by citizenship, 2024 vs Sep-2025 onward ===\n")
print(as.data.frame(h25e))

write_csv(h25a, "output/b2_h25_by_citizenship.csv")
write_csv(h25b, "output/b2_h25_by_citizenship_sex.csv")
write_csv(h25c, "output/b2_h25_naturalized_hispanic.csv")
write_csv(h25d, "output/b2_h25_naturalized_by_arrival.csv")
write_csv(dose_cit, "output/b2_h25_dose_response_citizenship.csv")
write_csv(h25e, "output/b2_h25_unweighted_counts.csv")

cat("\nDONE 43_batch2_men_citizens.R\n")
