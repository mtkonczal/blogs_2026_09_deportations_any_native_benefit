# ==============================================================================
# 04_dose_response.R   H4 and H5: the core of the post.
#
# The aggregate results invite "it would have been worse otherwise." These tests
# difference the aggregate out. If immigrant and native labor are substitutes,
# native outcomes should improve MORE where immigrant labor was thickest, and
# most of all for less-educated natives in those cells.
#
# OUTCOME CHOICE (important). Following Kolko, employment LEVELS by nativity
# are not trustworthy: nativity is not a population control, so when foreign-
# born respondents leave the sample their weight is redistributed onto natives
# in the same age-sex-race cell and measured native employment rises
# mechanically. Cell-level log employment changes inherit that problem. So the
# HEADLINE outcomes here are rates and per-worker quantities (unemployment
# rate, usual hours, real wages), which are robust to proportional reweighting.
# The log-employment specifications are still reported, flagged as contaminated.
#
# Design: collapse to occupation (and industry) cells, regress the change in a
# native outcome from the pre-period to the post-period on the pre-period
# foreign-born employment share, weighted by pre-period native employment.
# Every treatment regression is run identically on a 2018->2019 placebo window.
# If the placebo coefficient is nonzero the exposure measure is picking up a
# secular trend and the design is dead: report it either way.
# ==============================================================================
suppressMessages({
  library(data.table); library(dplyr); library(readr); library(tidyr)
  library(lmtest); library(sandwich)
})
cps <- readRDS("data_raw/cps_analysis.rds"); setDT(cps)
occ_exp <- as.data.table(read_csv("data/exposure_occ.csv", show_col_types = FALSE))
ind_exp <- as.data.table(read_csv("data/exposure_ind.csv", show_col_types = FALSE))

# Windows. Post is 2025m9-2026m8 (a full 12 months clear of the Jan-2025 start,
# with the missing Oct-2025 simply absent). Pre is the same 12 calendar months
# a year earlier so seasonality differences out.
W <- list(
  treat = list(pre = c("2023-09-01","2024-08-01"), post = c("2025-09-01","2026-08-01")),
  plac  = list(pre = c("2017-09-01","2018-08-01"), post = c("2018-09-01","2019-08-01"))
)

cell_stats <- function(dt, key, a, b) {
  d <- dt[date >= as.Date(a) & date <= as.Date(b) & !is.na(get(key))]
  d[, .(
    nat_emp      = sum(WTFINL * native * emp),
    nat_lf       = sum(WTFINL * native * inlf),
    nat_unemp    = sum(WTFINL * native * unemp),
    nat_emp_nc   = sum(WTFINL * native * emp * noncollege, na.rm = TRUE),
    nat_lf_nc    = sum(WTFINL * native * inlf * noncollege, na.rm = TRUE),
    nat_unemp_nc = sum(WTFINL * native * unemp * noncollege, na.rm = TRUE),
    for_emp      = sum(WTFINL * foreign_born * emp),
    nat_hrs      = sum(WTFINL * native * emp * uhrs, na.rm = TRUE) /
                   sum(WTFINL * native * emp * !is.na(uhrs), na.rm = TRUE),
    n_nat        = sum(native, na.rm = TRUE)
  ), by = c(key)]
}

run_window <- function(key, exp_dt, win) {
  a <- cell_stats(cps, key, win$pre[1],  win$pre[2])
  b <- cell_stats(cps, key, win$post[1], win$post[2])
  m <- merge(a, b, by = key, suffixes = c("_0","_1"))
  m <- merge(m, exp_dt[usable == TRUE, c(key, "foreign_share","emp_tot"), with = FALSE],
             by = key)
  m[, `:=`(
    d_log_nat_emp    = log(nat_emp_1) - log(nat_emp_0),
    d_log_nat_emp_nc = log(nat_emp_nc_1) - log(nat_emp_nc_0),
    d_log_for_emp    = log(for_emp_1) - log(for_emp_0),
    d_nat_unrate     = nat_unemp_1/nat_lf_1 - nat_unemp_0/nat_lf_0,
    d_nat_unrate_nc  = nat_unemp_nc_1/nat_lf_nc_1 - nat_unemp_nc_0/nat_lf_nc_0,
    d_nat_hrs        = nat_hrs_1 - nat_hrs_0
  )]
  m[is.finite(d_log_nat_emp)]
}

fit <- function(m, y, label) {
  # cells with an empty subgroup (e.g. no non-college native unemployed) give
  # NaN differences; drop them here rather than letting lm fail silently later
  mm <- m[is.finite(get(y)) & is.finite(nat_emp_0) & nat_emp_0 > 0]
  if (nrow(mm) < 10) return(data.frame(outcome=y, spec=label, beta=NA, se=NA,
                                       t=NA, p=NA, n_cells=nrow(mm)))
  mod <- lm(as.formula(paste(y, "~ foreign_share")), data = mm, weights = mm$nat_emp_0)
  ct <- coeftest(mod, vcov = vcovHC(mod, type = "HC1"))
  data.frame(outcome = y, spec = label, beta = ct[2,1], se = ct[2,2],
             t = ct[2,3], p = ct[2,4], n_cells = nobs(mod))
}

# rates first (robust), levels last (contaminated, reported for completeness)
OUTS <- c("d_nat_unrate","d_nat_unrate_nc","d_nat_hrs",
          "d_log_nat_emp","d_log_nat_emp_nc","d_log_for_emp")
ROBUST <- c("d_nat_unrate","d_nat_unrate_nc","d_nat_hrs")
res <- list()
for (key in c("OCC2010","IND1990")) {
  exp_dt <- if (key == "OCC2010") occ_exp else ind_exp
  for (wn in names(W)) {
    m <- run_window(key, exp_dt, W[[wn]])
    for (y in OUTS) {
      r <- fit(m, y, paste0(key, " / ", ifelse(wn == "treat", "2025-26", "PLACEBO 2018-19")))
      res[[length(res)+1]] <- r
    }
    if (key == "OCC2010" && wn == "treat") write_csv(m, "output/h4_occ_cells.csv")
    if (key == "IND1990" && wn == "treat") write_csv(m, "output/h4_ind_cells.csv")
  }
}
res <- bind_rows(res)
res$stars <- cut(res$p, c(-Inf,.01,.05,.1,Inf), c("***","**","*",""))

res$robust_to_reweighting <- res$outcome %in% ROBUST
cat("\n================ H4 / H5 DOSE-RESPONSE ================\n")
cat("beta = effect of moving a cell from 0% to 100% pre-period foreign-born share.\n")
cat("Restrictionist prediction: unemployment beta < 0, hours beta > 0, employment beta > 0.\n")
cat("robust_to_reweighting = FALSE means the outcome is an employment LEVEL and\n")
cat("is contaminated by the nativity reweighting artifact (Kolko). Read the\n")
cat("rate outcomes as the finding.\n\n")
print(res %>% mutate(across(c(beta,se,t), ~round(.x,4)), p = round(p,4)) %>% as.data.frame())
write_csv(res, "output/h4_h5_dose_response.csv")

# --- H5 alternative cut: native outcomes by education, nationally ------------
edu <- cps[!is.na(educ_grp) & prime == 1, .(
  unrate = sum(WTFINL*unemp*native)/sum(WTFINL*inlf*native),
  epop   = sum(WTFINL*emp*native)/sum(WTFINL*native),
  lfpr   = sum(WTFINL*inlf*native)/sum(WTFINL*native)
), by = .(educ_grp, yr = data.table::year(date))][order(educ_grp, yr)]
cat("\n=== H5: prime-age NATIVE-BORN outcomes by education ===\n")
print(dcast(edu[yr >= 2023], educ_grp ~ yr, value.var = "unrate"))
cat("\n(EPOP)\n")
print(dcast(edu[yr >= 2023], educ_grp ~ yr, value.var = "epop"))
write_csv(edu, "output/h5_native_by_education.csv")

# Like-for-like: 2026 has only Jan-Aug, and unemployment is seasonal, so the
# chart compares January-August of every year.
edu8 <- cps[!is.na(educ_grp) & prime == 1 & native == 1 & data.table::month(date) <= 8, .(
  unrate = sum(WTFINL*unemp)/sum(WTFINL*inlf),
  epop   = sum(WTFINL*emp)/sum(WTFINL),
  n_lf   = sum(inlf)
), by = .(educ_grp, yr = data.table::year(date))][order(educ_grp, yr)]
cat("\n=== H5: prime-age NATIVE unemployment by education, January-August ===\n")
print(dcast(edu8[yr >= 2023], educ_grp ~ yr, value.var = "unrate"))
write_csv(edu8, "output/h5_native_by_education_jan_aug.csv")
cat("\nDONE 04_dose_response.R\n")
