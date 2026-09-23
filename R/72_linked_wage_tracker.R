# ==============================================================================
# 72_linked_wage_tracker.R   Hypothesis 6 robustness (text only, no chart)
#
# The percentile chart (67) compares two cross-sections, so its level moves with
# who is in the ORG sample (e.g. including vs. excluding imputed earners shifts
# the 2026 native median by ~1 pp). This follows the SAME native-born worker
# across the two ORG interviews 12 months apart (MISH 4 -> MISH 8), in the
# spirit of the Atlanta Fed Wage Growth Tracker:
#   - hourly wage built exactly as in 67 (HOURWAGE2, else EARNWEEK2 / hours)
#   - both interviews non-imputed, wage in [2, 500), native-born at MISH 4
#   - monthly unweighted median of the 12-month log wage change (tracker practice),
#     overall and for the bottom quartile of the MISH-4 wage within each month
#   - 6-month moving average of the monthly medians, to match 67's window.
#     October 2025 has no survey, so windows spanning it average 5 months.
# Deterministic: quartile ties broken by CPSIDP order, no random draws.
# ==============================================================================
suppressMessages({library(ipumsr); library(data.table); library(readr); library(lubridate)})

ddi <- read_ipums_ddi(readLines("data_raw/ddi_path.txt")[1])
VARS <- c("YEAR","MONTH","CPSIDP","MISH","AGE","CITIZEN","EMPSTAT","UHRSWORKT",
          "QEARNWEE","QHOURWAG")
acc <- list(); i <- 0
cb <- function(x, pos) {
  i <<- i + 1; setDT(x)
  x <- x[YEAR >= 2021 & MISH %in% c(4, 8) & AGE >= 16 & EMPSTAT %in% c(10, 12), ..VARS]
  for (j in names(x)) set(x, j = j, value = as.numeric(x[[j]]))
  acc[[i]] <<- x; NULL
}
read_ipums_micro_chunked(ddi, callback = IpumsSideEffectCallback$new(cb),
                         chunk_size = 2e6, verbose = FALSE)
cps <- rbindlist(acc); rm(acc); invisible(gc())
cps[, `:=`(date = as.Date(sprintf("%d-%02d-01", YEAR, MONTH)),
           native = as.integer(CITIZEN %in% c(1, 2, 3)),
           uhrs = fifelse(UHRSWORKT < 997, UHRSWORKT, NA_real_),
           earn_imputed = as.integer((!is.na(QEARNWEE) & QEARNWEE > 0) |
                                     (!is.na(QHOURWAG) & QHOURWAG > 0)))]

e <- readRDS("data_raw/cps_earn.rds"); setDT(e)
e <- e[MISH %in% c(4, 8) & YEAR >= 2021]
e[, `:=`(ew = fifelse(EARNWEEK2 > 0 & EARNWEEK2 < 999999, EARNWEEK2, NA_real_),
         hw = fifelse(HOURWAGE2 > 0 & HOURWAGE2 < 999, HOURWAGE2, NA_real_),
         uh = fifelse(UHRSWORKORG > 0 & UHRSWORKORG < 997, UHRSWORKORG, NA_real_))]
m <- merge(cps[earn_imputed == 0, .(CPSIDP, date, MISH, uhrs, native)],
           e[, .(CPSIDP, date, ew, hw, uh)], by = c("CPSIDP", "date"))
m[, hours := fifelse(!is.na(uh), uh, uhrs)]
m[, wage := fifelse(!is.na(hw), hw, fifelse(hours >= 1, ew / hours, NA_real_))]
m <- m[!is.na(wage) & wage >= 2 & wage < 500]

# link MISH 4 at t-12 to MISH 8 at t, same person
lk <- merge(m[MISH == 4, .(CPSIDP, date0 = date, w0 = wage, nat0 = native)],
            m[MISH == 8, .(CPSIDP, date1 = date, w1 = wage)], by = "CPSIDP")
lk <- lk[date1 == date0 %m+% months(12) & nat0 == 1]
lk[, dl := log(w1 / w0)]
setorder(lk, date0, w0, CPSIDP)
lk[, q0 := cut(frank(w0, ties.method = "first") / .N, c(0, .25, .5, .75, 1),
               labels = c("Q1", "Q2", "Q3", "Q4")), by = date0]
cat("linked native wage pairs:", format(nrow(lk), big.mark = ","), "\n")

tr <- merge(lk[, .(med_all = median(dl), n_all = .N), by = .(date = date1)],
            lk[q0 == "Q1", .(med_q1 = median(dl), n_q1 = .N), by = .(date = date1)], by = "date")
setorder(tr, date)
# 6-month calendar window (months present, so the Oct 2025 gap averages 5)
tr[, `:=`(ma6_all = sapply(date, function(t) mean(tr[date > t %m-% months(6) & date <= t]$med_all)),
          ma6_q1  = sapply(date, function(t) mean(tr[date > t %m-% months(6) & date <= t]$med_q1)))]
write_csv(tr, "output/h5_native_linked_wage_growth.csv")

cat("\n=== Native linked 12-month wage growth, 6-month MA of monthly medians (%) ===\n")
sel <- as.Date(c("2024-06-01","2024-09-01","2024-12-01","2025-03-01","2025-06-01",
                 "2025-09-01","2025-12-01","2026-03-01","2026-06-01","2026-08-01"))
print(tr[date %in% sel, .(date, all = round(100*ma6_all, 1), bottom_quartile = round(100*ma6_q1, 1), n_all)])
cat("DONE 72_linked_wage_tracker.R\n")
