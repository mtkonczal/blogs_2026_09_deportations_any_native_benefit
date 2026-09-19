# ==============================================================================
# 06_flows.R   H7: did unemployed natives find work faster?
# Month-to-month links on CPSIDP. A valid link requires the person to appear in
# consecutive calendar months with MISH advancing by exactly 1.
#
# DATA BREAK: there is no October 2025 household survey. The 2025m9->m10 and
# 2025m10->m11 links do not exist and are never interpolated.
# ==============================================================================
suppressMessages({library(data.table); library(dplyr); library(readr); library(lubridate)})
cps <- readRDS("data_raw/cps_analysis.rds"); setDT(cps)

f <- cps[CPSIDP > 0, .(CPSIDP, date, MISH, emp, unemp, inlf, nilf, native,
                       foreign_born, noncollege, educ_grp, prime, AGE,
                       LNKFW1MWT, WTFINL, gen, hispanic)]
setorder(f, CPSIDP, date)
f[, `:=`(next_date = shift(date, -1), next_mish = shift(MISH, -1),
         next_emp = shift(emp, -1), next_unemp = shift(unemp, -1),
         next_nilf = shift(nilf, -1)), by = CPSIDP]
# valid 1-month link: consecutive calendar month AND MISH advances by one
f[, valid := !is.na(next_date) &
    next_date == (date %m+% months(1)) & next_mish == MISH + 1]
lk <- f[valid == TRUE]
lk[, wt := fifelse(!is.na(LNKFW1MWT) & LNKFW1MWT > 0, LNKFW1MWT, WTFINL)]
cat("valid 1-month links:", format(nrow(lk), big.mark=","), "\n")
cat("months with zero links (expect the Oct-2025 gap):\n")
allm <- seq(min(lk$date), max(lk$date), by = "month")
print(allm[!allm %in% unique(lk$date)])

rates <- function(dt, lbl) {
  dt[, .(
    UE = sum(wt*unemp*next_emp)/sum(wt*unemp),      # job-finding rate
    UN = sum(wt*unemp*next_nilf)/sum(wt*unemp),
    EU = sum(wt*emp*next_unemp)/sum(wt*emp),        # separation into unemployment
    EN = sum(wt*emp*next_nilf)/sum(wt*emp),
    NE = sum(wt*nilf*next_emp)/sum(wt*nilf),
    n_u = sum(unemp)
  ), by = .(date)][, grp := lbl][]
}
r <- rbind(rates(lk[native == 1 & prime == 1], "Native prime-age"),
           rates(lk[foreign_born == 1 & prime == 1], "Foreign-born prime-age"),
           rates(lk[native == 1 & prime == 1 & noncollege == 1], "Native prime, no BA"))
write_csv(r, "output/h7_flows_monthly.csv")

ann <- r[, .(UE = mean(UE, na.rm=TRUE), EU = mean(EU, na.rm=TRUE),
             EN = mean(EN, na.rm=TRUE), NE = mean(NE, na.rm=TRUE),
             months = .N), by = .(grp, yr = year(date))][order(grp, yr)]
cat("\n=== H7: monthly transition rates, annual averages ===\n")
print(as.data.frame(ann[yr >= 2022]))
write_csv(ann, "output/h7_flows_annual.csv")

# --- unemployment duration ---------------------------------------------------
d <- cps[unemp == 1 & DURUNEMP < 999 & prime == 1]
dur <- d[, .(mean_dur = sum(WTFINL*DURUNEMP)/sum(WTFINL),
             share_27plus = sum(WTFINL*(DURUNEMP >= 27))/sum(WTFINL), n = .N),
         by = .(yr = year(date), grp = fifelse(native == 1, "Native", "Foreign-born"))
         ][order(grp, yr)]
cat("\n=== H7: prime-age unemployment duration (weeks) ===\n")
print(as.data.frame(dur[yr >= 2022]))
write_csv(dur, "output/h7_duration.csv")
cat("\nDONE 06_flows.R\n")
