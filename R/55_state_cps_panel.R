# ==============================================================================
# 55_state_cps_panel.R
# Bridges the CPS microdata directly to state ICE enforcement intensity,
# instead of the pre-period foreign-born-share exposure measure used in
# R/08_geography.R (H9). Same caveat carries over unchanged: CPS state cells
# are thin, so everything here is a 12-month-averaged state panel and every
# correlation should be read as suggestive, not decisive (see R/08_geography.R
# header). This is what lets us test native AND foreign-born CPS outcomes
# against the actual measured enforcement rate, not just a static exposure
# proxy.
#
# pre  = calendar year 2024 (last full pre-policy year)
# post = most recent 12 available CPS months (2025-09 through 2026-08;
#        October 2025 has no household survey and is simply absent from the
#        average, not interpolated)
# ==============================================================================
suppressMessages({library(data.table); library(dplyr); library(readr); library(tigris)})

cps <- readRDS("data_raw/cps_analysis.rds"); setDT(cps)
data(fips_codes)
xwalk <- unique(fips_codes[, c("state_code", "state_name")])
xwalk$STATEFIP <- as.integer(xwalk$state_code)

PRE_A  <- as.Date("2024-01-01"); PRE_B  <- as.Date("2024-12-01")
POST_A <- as.Date("2025-09-01"); POST_B <- as.Date("2026-08-01")

win <- function(a, b) {
  cps[date >= a & date <= b, .(
    nat_epop_prime   = sum(WTFINL*emp*native*prime)/sum(WTFINL*native*prime),
    nat_unrate_prime = sum(WTFINL*unemp*native*prime)/sum(WTFINL*inlf*native*prime),
    for_epop_prime   = sum(WTFINL*emp*foreign_born*prime)/sum(WTFINL*foreign_born*prime),
    for_unrate_prime = sum(WTFINL*unemp*foreign_born*prime)/sum(WTFINL*inlf*foreign_born*prime),
    for_pop          = sum(WTFINL*foreign_born),
    tot_pop          = sum(WTFINL),
    construction_emp_share = sum(WTFINL*emp*(IND1990==60))/sum(WTFINL*emp),
    n_obs = .N, n_for_obs = sum(foreign_born)
  ), by = STATEFIP]
}

pre  <- win(PRE_A, PRE_B)
post <- win(POST_A, POST_B)

m <- merge(pre, post, by = "STATEFIP", suffixes = c("_pre", "_post"))
m <- merge(m, xwalk[, c("STATEFIP","state_name")], by = "STATEFIP")

m[, `:=`(
  d_nat_epop     = nat_epop_prime_post - nat_epop_prime_pre,
  d_nat_unrate   = nat_unrate_prime_post - nat_unrate_prime_pre,
  d_for_epop     = for_epop_prime_post - for_epop_prime_pre,
  d_for_unrate   = for_unrate_prime_post - for_unrate_prime_pre,
  d_log_for_pop  = log(for_pop_post) - log(for_pop_pre),
  for_share_pre  = for_pop_pre / tot_pop_pre
)]

cat("Cell sizes (unweighted obs), pre / post -- flag any state below 100:\n")
print(m[order(n_obs_pre)][, .(state_name, n_obs_pre, n_obs_post, n_for_obs_pre, n_for_obs_post)][1:10])

thin <- m[n_obs_pre < 100 | n_obs_post < 100 | n_for_obs_pre < 30 | n_for_obs_post < 30]
cat("\nStates/cells dropped for insufficient sample (native obs <100 or foreign-born obs <30 in either period):",
    nrow(thin), "\n")
if (nrow(thin) > 0) print(thin$state_name)

m_ok <- m[!STATEFIP %in% thin$STATEFIP]
cat("\nUsable state panel N =", nrow(m_ok), "\n")

write_csv(m_ok, "data/cps_state_panel.csv")
cat("Wrote data/cps_state_panel.csv\n")
cat("DONE 55_state_cps_panel.R\n")
