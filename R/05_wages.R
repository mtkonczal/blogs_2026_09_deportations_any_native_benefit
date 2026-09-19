# ==============================================================================
# 05_wages.R   H6: did native wage growth accelerate, especially at the bottom?
#
# CPS outgoing rotation groups (MISH 4 and 8), weighted by EARNWT, deflated by
# CPI-U into the latest month's dollars.
#
# Earnings come from the supplementary extract: IPUMS replaced EARNWEEK and
# HOURWAGE with EARNWEEK2 and HOURWAGE2 in 2023 when BLS changed earnings
# topcoding, and the originals are empty from 2024 on. NIU codes are
# 999999.99 (EARNWEEK2) and 999.99 (HOURWAGE2).
#
# Hourly wage, standard construction: HOURWAGE2 for workers paid by the hour,
# EARNWEEK2 / usual hours for everyone else.
#
# Imputation: CPS earnings imputation rates run above 40% and are not random
# with respect to nativity, so the headline drops imputed records and a
# robustness column keeps them. Flags come from the main extract, which still
# carries QEARNWEE / QHOURWAG even where the old value columns are empty.
# ==============================================================================
suppressMessages({
  library(data.table); library(dplyr); library(readr); library(blsR); library(lubridate)
})
bls_set_key(Sys.getenv("BLS_KEY"))
cps <- readRDS("data_raw/cps_analysis.rds"); setDT(cps)
e   <- readRDS("data_raw/cps_earn.rds");     setDT(e)

e <- e[MISH %in% c(4, 8)]
e[, `:=`(ew = fifelse(EARNWEEK2 > 0 & EARNWEEK2 < 999999, EARNWEEK2, NA_real_),
         hw = fifelse(HOURWAGE2 > 0 & HOURWAGE2 < 999,    HOURWAGE2, NA_real_),
         uh = fifelse(UHRSWORKORG > 0 & UHRSWORKORG < 997, UHRSWORKORG, NA_real_))]
e <- e[!is.na(ew) | !is.na(hw)]
setkey(e, CPSIDP, date)

m <- merge(cps[org == 1 & emp == 1, .(CPSIDP, date, native, foreign_born, noncollege,
                                      educ_grp, prime, AGE, SEX, OCC2010, IND1990,
                                      earn_imputed, EARNWT_main = EARNWT, uhrs)],
           e[, .(CPSIDP, date, ew, hw, uh, EARNWT, PAIDHOUR, UNION)],
           by = c("CPSIDP","date"))
cat("merged ORG records:", format(nrow(m), big.mark=","), "\n")

# hourly wage: reported hourly rate where available, else weekly / usual hours
m[, hours := fifelse(!is.na(uh), uh, uhrs)]
m[, wage := fifelse(!is.na(hw), hw, fifelse(hours >= 1, ew / hours, NA_real_))]
m <- m[!is.na(wage) & wage >= 2 & wage < 500 & EARNWT > 0]
cat("usable hourly-wage records:", format(nrow(m), big.mark=","),
    "| share imputed:", round(mean(m$earn_imputed, na.rm=TRUE), 3), "\n")

cpi <- get_n_series_table("CUUR0000SA0", api_key = bls_get_key(),
                          start_year = 2014, end_year = 2026, tidy = TRUE) %>%
  transmute(cpi = suppressWarnings(as.numeric(CUUR0000SA0)),
            date = as.Date(paste0(year, "/", month, "/1"))) %>% filter(!is.na(cpi))
base <- cpi$cpi[which.max(cpi$date)]
m <- merge(m, as.data.table(cpi)[, .(date, defl = base/cpi)], by = "date")
m[, real_wage := wage * defl]
cat("deflated to", format(max(cpi$date), "%B %Y"), "dollars\n")

wq <- function(x, wt, p) { o <- order(x); x <- x[o]; wt <- wt[o]
  x[which(cumsum(wt)/sum(wt) >= p)[1]] }
summ <- function(dt, lbl) {
  out <- dt[, .(p10 = wq(real_wage, EARNWT, .10), p25 = wq(real_wage, EARNWT, .25),
                p50 = wq(real_wage, EARNWT, .50), p75 = wq(real_wage, EARNWT, .75),
                mean = sum(real_wage*EARNWT)/sum(EARNWT), n = .N),
            by = .(yr = year(date))]
  out[, grp := lbl][] }

main <- m[earn_imputed == 0]
res <- rbind(summ(main[native == 1], "Native-born"),
             summ(main[foreign_born == 1], "Foreign-born"),
             summ(main[native == 1 & noncollege == 1], "Native, no BA"),
             summ(main[native == 1 & educ_grp == "BA+"], "Native, BA+"))[order(grp, yr)]
res[, `:=`(g_p10 = p10/shift(p10)-1, g_p25 = p25/shift(p25)-1,
           g_p50 = p50/shift(p50)-1, g_mean = mean/shift(mean)-1), by = grp]
cat("\n=== H6: real hourly wages, imputed records dropped ===\n")
print(as.data.frame(res[yr >= 2022, .(grp, yr, p10=round(p10,2), p25=round(p25,2),
                                      p50=round(p50,2), mean=round(mean,2), n)]))
cat("\n=== H6: real wage growth, percent per year ===\n")
print(as.data.frame(res[yr >= 2023, .(grp, yr, g_p10=round(100*g_p10,2),
                                      g_p25=round(100*g_p25,2), g_p50=round(100*g_p50,2),
                                      g_mean=round(100*g_mean,2))]))
write_csv(res, "output/h6_wages.csv")

# robustness: keep imputed records
res_i <- rbind(summ(m[native == 1], "Native-born (incl. imputed)"),
               summ(m[native == 1 & noncollege == 1], "Native, no BA (incl. imputed)"))[order(grp, yr)]
res_i[, `:=`(g_p10 = p10/shift(p10)-1, g_p50 = p50/shift(p50)-1), by = grp]
cat("\n=== H6 robustness: imputed records retained ===\n")
print(as.data.frame(res_i[yr >= 2023, .(grp, yr, g_p10=round(100*g_p10,2),
                                        g_p50=round(100*g_p50,2))]))
write_csv(res_i, "output/h6_wages_with_imputed.csv")

# --- dose-response: native wage growth by occupational exposure --------------
occ_exp <- as.data.table(read_csv("data/exposure_occ.csv", show_col_types=FALSE))[usable == TRUE]
mw <- merge(main[native == 1], occ_exp[, .(OCC2010, foreign_share)], by = "OCC2010")
qs <- quantile(occ_exp$foreign_share, c(0,.25,.5,.75,1), na.rm=TRUE)
mw[, exp_grp := cut(foreign_share, qs, include.lowest = TRUE,
                    labels = c("Q1 lowest immigrant share","Q2","Q3","Q4 highest immigrant share"))]
byexp <- mw[!is.na(exp_grp), .(p10 = wq(real_wage, EARNWT, .1),
                               p50 = wq(real_wage, EARNWT, .5),
                               mean = sum(real_wage*EARNWT)/sum(EARNWT), n = .N),
            by = .(exp_grp, yr = year(date))][order(exp_grp, yr)]
byexp[, `:=`(g_p10 = p10/shift(p10)-1, g_p50 = p50/shift(p50)-1,
             g_mean = mean/shift(mean)-1), by = exp_grp]
cat("\n=== H6 dose-response: NATIVE real wage growth by occupation exposure quartile ===\n")
print(as.data.frame(byexp[yr >= 2023, .(exp_grp, yr, p50=round(p50,2),
                                        g_p10=round(100*g_p10,2), g_p50=round(100*g_p50,2),
                                        g_mean=round(100*g_mean,2), n)]))
write_csv(byexp, "output/h6_wages_by_exposure.csv")
# --- like-for-like: 2026 has only Jan-Aug, so redo on months 1-8 of every year
m8 <- main[month(date) <= 8]
res8 <- rbind(summ(m8[native == 1], "Native-born"),
              summ(m8[native == 1 & noncollege == 1], "Native, no BA"),
              summ(m8[foreign_born == 1], "Foreign-born"))[order(grp, yr)]
res8[, `:=`(g_p10 = p10/shift(p10)-1, g_p25 = p25/shift(p25)-1,
            g_p50 = p50/shift(p50)-1, g_mean = mean/shift(mean)-1), by = grp]
cat("\n=== H6 like-for-like: January-August of each year only ===\n")
print(as.data.frame(res8[yr >= 2023, .(grp, yr, p10=round(p10,2), p50=round(p50,2),
                                       g_p10=round(100*g_p10,2), g_p25=round(100*g_p25,2),
                                       g_p50=round(100*g_p50,2), g_mean=round(100*g_mean,2))]))
write_csv(res8, "output/h6_wages_jan_aug.csv")

mw8 <- mw[month(date) <= 8 & !is.na(exp_grp)]
byexp8 <- mw8[, .(p10 = wq(real_wage, EARNWT, .1), p50 = wq(real_wage, EARNWT, .5),
                  mean = sum(real_wage*EARNWT)/sum(EARNWT), n = .N),
              by = .(exp_grp, yr = year(date))][order(exp_grp, yr)]
byexp8[, `:=`(g_p50 = p50/shift(p50)-1, g_mean = mean/shift(mean)-1), by = exp_grp]
cat("\n=== H6 dose-response, January-August only ===\n")
print(as.data.frame(byexp8[yr >= 2024, .(exp_grp, yr, p50=round(p50,2),
                                         g_p50=round(100*g_p50,2), g_mean=round(100*g_mean,2), n)]))
write_csv(byexp8, "output/h6_wages_by_exposure_jan_aug.csv")
cat("\nDONE 05_wages.R\n")
