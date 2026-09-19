# ==============================================================================
# 07_hours.R   H8: did native hours rise and involuntary part-time fall?
# Scarce labor should show up in hours before it shows up in wages, so this is
# a fast-moving margin. Rising economic part-time is a demand-shock signature,
# not a supply-shock one.
# WHYPTLWK economic reasons: slack work / business conditions, could only find
# part-time work, seasonal slack. Non-economic: childcare, school, health, etc.
# ==============================================================================
suppressMessages({library(data.table); library(dplyr); library(readr); library(lubridate)})
cps <- readRDS("data_raw/cps_analysis.rds"); setDT(cps)

# Part-time for economic reasons, BLS definition: slack work / business
# conditions (010) and could only find part-time work (060). Codes 011, 012,
# 020 and 030 (material shortage, repairs, seasonal, weather) are also
# demand-side; included in the wider measure, reported separately.
ECON       <- c(10, 60)
ECON_WIDE  <- c(10, 11, 12, 20, 30, 60)
cat("=== WHYPTLWK value distribution (employed) ===\n")
print(cps[emp == 1 & WHYPTLWK > 0, .N, by = WHYPTLWK][order(WHYPTLWK)])
cps[, ptecon := as.integer(emp == 1 & WHYPTLWK %in% ECON)]
cps[, ptecon_wide := as.integer(emp == 1 & WHYPTLWK %in% ECON_WIDE)]
cps[, pt := as.integer(emp == 1 & !is.na(uhrs) & uhrs < 35)]

h <- cps[emp == 1 & prime == 1, .(
  mean_uhrs  = sum(WTFINL*uhrs, na.rm=TRUE)/sum(WTFINL*!is.na(uhrs)),
  share_pt   = sum(WTFINL*pt, na.rm=TRUE)/sum(WTFINL),
  share_ptecon = sum(WTFINL*ptecon, na.rm=TRUE)/sum(WTFINL),
  share_ptecon_wide = sum(WTFINL*ptecon_wide, na.rm=TRUE)/sum(WTFINL),
  n = .N
), by = .(yr = year(date), grp = fifelse(native == 1, "Native", "Foreign-born"))
][order(grp, yr)]
cat("\n=== H8: prime-age hours and part-time ===\n")
print(as.data.frame(h[yr >= 2022]))
write_csv(h, "output/h8_hours.csv")

# dose-response version: native hours by occupational exposure quartile
occ_exp <- as.data.table(read_csv("data/exposure_occ.csv", show_col_types=FALSE))
m <- merge(cps[emp == 1 & native == 1], occ_exp[usable == TRUE, .(OCC2010, foreign_share)],
           by = "OCC2010")
qs <- quantile(occ_exp[usable==TRUE]$foreign_share, c(0,.25,.5,.75,1), na.rm=TRUE)
m[, exp_grp := cut(foreign_share, qs, labels=c("Q1 lowest","Q2","Q3","Q4 highest"),
                   include.lowest=TRUE)]
hx <- m[!is.na(exp_grp), .(
  mean_uhrs = sum(WTFINL*uhrs, na.rm=TRUE)/sum(WTFINL*!is.na(uhrs)),
  share_ptecon = sum(WTFINL*ptecon, na.rm=TRUE)/sum(WTFINL), n = .N
), by = .(exp_grp, yr = year(date))][order(exp_grp, yr)]
cat("\n=== H8 dose-response: NATIVE hours by occupation exposure quartile ===\n")
print(as.data.frame(hx[yr >= 2023]))
write_csv(hx, "output/h8_hours_by_exposure.csv")
cat("\nDONE 07_hours.R\n")
