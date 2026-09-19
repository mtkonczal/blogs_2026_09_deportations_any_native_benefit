# ==============================================================================
# 04b_rolling_dose.R
# The single 2018-19 placebo in 04_dose_response.R is itself significant, which
# means the occupational exposure measure is correlated with something that
# moves over the cycle. A single placebo cannot tell us whether 2025-26 is
# unusual. So run the identical regression on EVERY rolling 12-month-over-
# 12-month window from 2016 onward and look at where 2025-26 sits in the
# distribution of coefficients.
#
# Outcome: change in the native unemployment rate in the cell (a rate, so
# robust to the nativity reweighting artifact).
# ==============================================================================
suppressMessages({
  library(data.table); library(dplyr); library(readr); library(lmtest); library(sandwich)
})
cps <- readRDS("data_raw/cps_analysis.rds"); setDT(cps)
occ_exp <- as.data.table(read_csv("data/exposure_occ.csv", show_col_types=FALSE))
occ_exp <- occ_exp[usable == TRUE]

cell <- function(a, b) {
  cps[date >= a & date <= b & OCC2010 < 9900 & !is.na(OCC2010), .(
    nat_lf    = sum(WTFINL*native*inlf),
    nat_unemp = sum(WTFINL*native*unemp),
    nat_emp   = sum(WTFINL*native*emp),
    nat_hrs   = sum(WTFINL*native*emp*uhrs, na.rm=TRUE)/
                sum(WTFINL*native*emp*!is.na(uhrs), na.rm=TRUE)
  ), by = OCC2010]
}
anchors <- seq(as.Date("2017-09-01"), as.Date("2025-09-01"), by = "3 months")
out <- list()
for (a in anchors) {
  a <- as.Date(a, origin = "1970-01-01")
  post_a <- a; post_b <- seq(a, by = "11 months", length.out = 2)[2]
  pre_a  <- seq(a, by = "-12 months", length.out = 2)[2]
  pre_b  <- seq(pre_a, by = "11 months", length.out = 2)[2]
  if (post_b > max(cps$date)) next
  m <- merge(cell(pre_a, pre_b), cell(post_a, post_b), by = "OCC2010",
             suffixes = c("_0","_1"))
  m <- merge(m, occ_exp[, .(OCC2010, foreign_share)], by = "OCC2010")
  m[, d_unrate := nat_unemp_1/nat_lf_1 - nat_unemp_0/nat_lf_0]
  m[, d_hrs := nat_hrs_1 - nat_hrs_0]
  m <- m[is.finite(d_unrate) & nat_emp_0 > 0]
  for (y in c("d_unrate","d_hrs")) {
    mm <- m[is.finite(get(y))]
    mod <- lm(as.formula(paste(y, "~ foreign_share")), data = mm, weights = mm$nat_emp_0)
    ct <- coeftest(mod, vcov = vcovHC(mod, "HC1"))
    out[[length(out)+1]] <- data.frame(
      window_end = post_b, outcome = y,
      beta = ct[2,1], se = ct[2,2], p = ct[2,4], n = nobs(mod))
  }
}
res <- bind_rows(out)
write_csv(res, "output/h4_rolling_coefficients.csv")

u <- res %>% filter(outcome == "d_unrate") %>% arrange(window_end)
cat("=== rolling dose-response: change in NATIVE unemployment rate on exposure ===\n")
cat("beta > 0 means native unemployment rose MORE in immigrant-heavy occupations.\n")
cat("The restrictionist prediction is beta < 0 in the post-2025 windows.\n\n")
print(u %>% mutate(beta = round(beta,4), se = round(se,4), p = round(p,3)) %>% as.data.frame())
post <- u %>% filter(window_end >= as.Date("2025-12-01"))
pre  <- u %>% filter(window_end < as.Date("2020-01-01"))
cat("\npre-2020 windows: mean beta =", round(mean(pre$beta),4),
    " sd =", round(sd(pre$beta),4), " n =", nrow(pre), "\n")
cat("2025-26 windows : mean beta =", round(mean(post$beta),4), " n =", nrow(post), "\n")
# Deliberately NOT reporting a z-statistic against the pre-2020 windows: those
# windows overlap by 9 months each, so they are not independent draws and any
# standard error built from their spread is spuriously small. The path is
# descriptive evidence, read as such.
cat("Note: rolling windows overlap; treat the path descriptively, not as a test.\n")
cat("\nDONE 04b_rolling_dose.R\n")
