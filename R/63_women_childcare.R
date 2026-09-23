# ==============================================================================
# 63_women_childcare.R   Follow-up to Hypothesis 3 (not yet its own hypothesis)
#
# Q1. Native prime-age EPOP fell 2024 -> 2026 but native prime-age men's fell
#     only a little. Did women's fall more? Monthly EPOP by sex, 2024-2026.
# Q2. Is any of it childcare? Three direct CPS basic-monthly questions:
#       WNLOOK   6 can't arrange childcare, 7 family responsibilities
#                (asked of NILF who want a job but are not looking)
#       WHYABSNT 7 child care problems, 9 maternity/paternity leave
#                (employed, absent from work last week)
#       WHYPTLWK 121 child care problems (worked <35 hours last week)
#     plus EPOP split by presence of an own child under 5 (NCHLT5).
#
# WNLOOK and WHYABSNT were not kept by 01_load_cps_micro.R, so this reads the
# raw extract (cps_00055) directly, keeping only the columns it needs.
# Definitions match 01: native = CITIZEN 1-3; employed = EMPSTAT 10/12;
# prime age = 25-54; weights WTFINL. NSA throughout; annual comparisons use
# January-August of each year because 2026 ends in August.
# ==============================================================================
suppressMessages({library(ipumsr); library(data.table); library(readr)})

ddi <- read_ipums_ddi(readLines("data_raw/ddi_path.txt")[1])
VARS <- c("YEAR","MONTH","WTFINL","AGE","SEX","CITIZEN","EMPSTAT","LABFORCE",
          "NCHLT5","NCHILD","WNLOOK","WHYABSNT","WHYPTLWK")
acc <- list(); i <- 0
cb <- function(x, pos) {
  i <<- i + 1; setDT(x)
  x <- x[AGE >= 25 & AGE <= 54 & YEAR >= 2023 & CITIZEN %in% c(1, 2, 3), ..VARS]
  for (j in names(x)) set(x, j = j, value = as.numeric(x[[j]]))
  acc[[i]] <<- x; NULL
}
read_ipums_micro_chunked(ddi, callback = IpumsSideEffectCallback$new(cb),
                         chunk_size = 2e6, verbose = FALSE)
d <- rbindlist(acc); rm(acc); gc()
cat("native prime-age person-months, 2023+:", format(nrow(d), big.mark = ","), "\n")

d[, `:=`(yr = YEAR, mo = MONTH, sex = fifelse(SEX == 1, "Men", "Women"),
         emp = as.integer(EMPSTAT %in% c(10, 12)),
         inlf = as.integer(LABFORCE == 2),
         kid5 = fifelse(NCHLT5 > 0, "Own child under 5", "No child under 5"))]
d[, nilf := 1L - inlf]
# childcare / family flags, each as a share of the whole group's population
d[, `:=`(nilf_childcare = as.integer(inlf == 0 & WNLOOK == 6),
         nilf_family    = as.integer(inlf == 0 & WNLOOK %in% c(6, 7)),
         abs_childcare  = as.integer(EMPSTAT == 12 & WHYABSNT == 7),
         abs_parental   = as.integer(EMPSTAT == 12 & WHYABSNT == 9),
         pt_childcare   = as.integer(emp == 1 & WHYPTLWK == 121))]

# sanity: native prime-age EPOP from this read should match BLS published
# (LNU0207341[7-9] / LNU0007341[7-9]) to within sampling/weighting rounding.
chk <- d[, .(epop = sum(WTFINL*emp)/sum(WTFINL)), by = .(yr, mo)][order(yr, mo)]
bls <- read_csv("data/fig2_prime_epop_native_official.csv", show_col_types = FALSE)
bls$yr <- as.integer(format(bls$date, "%Y")); bls$mo <- as.integer(format(bls$date, "%m"))
chk <- merge(chk, as.data.table(bls)[, .(yr, mo, bls = prime_epop_nat)], by = c("yr","mo"))
cat("max |micro - BLS| native prime EPOP, pp:",
    round(100*max(abs(chk$epop - chk$bls), na.rm = TRUE), 3), "\n")

# ---- Q1: EPOP by sex, monthly and Jan-Aug --------------------------------
monthly <- d[, .(epop = sum(WTFINL*emp)/sum(WTFINL), pop_k = sum(WTFINL)/1000),
             by = .(sex, yr, mo)][order(sex, yr, mo)]
write_csv(monthly, "output/h3_native_prime_epop_by_sex_monthly.csv")

ja <- d[mo <= 8, .(epop = sum(WTFINL*emp)/sum(WTFINL), lfpr = sum(WTFINL*inlf)/sum(WTFINL),
                   pop_k = sum(WTFINL)/1000/8, n = .N), by = .(sex, yr)][order(sex, yr)]
all_ja <- d[mo <= 8, .(sex = "All", epop = sum(WTFINL*emp)/sum(WTFINL),
                       lfpr = sum(WTFINL*inlf)/sum(WTFINL), pop_k = sum(WTFINL)/1000/8, n = .N), by = yr]
ja <- rbind(ja, all_ja)
cat("\n=== Native prime-age EPOP and LFPR, January-August ===\n")
print(dcast(ja, sex ~ yr, value.var = "epop"), digits = 4)
print(dcast(ja, sex ~ yr, value.var = "lfpr"), digits = 4)

# Shift-share: total change = sum_s popshare_s * d(epop_s) + composition
w <- ja[sex != "All" & yr %in% c(2024, 2026)]
w[, share := pop_k / sum(pop_k), by = yr]
ww <- dcast(w, sex ~ yr, value.var = c("epop","share"))
ww[, contrib_pp := 100 * share_2024 * (epop_2026 - epop_2024)]
cat("\n=== Contribution to the 2024->2026 change in native prime-age EPOP (pp, Jan-Aug) ===\n")
print(ww[, .(sex, d_epop_pp = round(100*(epop_2026 - epop_2024), 2), contrib_pp = round(contrib_pp, 2))])
cat("total change, pp:", round(100*diff(ja[sex == "All" & yr %in% c(2024, 2026), epop]), 2), "\n")
write_csv(ja, "output/h3_native_prime_epop_by_sex_jan_aug.csv")

# ---- Q2: children and childcare, Jan-Aug ------------------------------------
kid <- d[mo <= 8, .(epop = sum(WTFINL*emp)/sum(WTFINL), pop_k = sum(WTFINL)/1000/8, n = .N),
         by = .(sex, kid5, yr)][order(sex, kid5, yr)]
cat("\n=== Native prime-age EPOP by presence of own child under 5, January-August ===\n")
print(dcast(kid, sex + kid5 ~ yr, value.var = "epop"), digits = 4)
write_csv(kid, "output/h3_native_prime_epop_by_child5_jan_aug.csv")

cc <- d[mo <= 8, lapply(.SD, function(v) 100 * sum(WTFINL*v)/sum(WTFINL)),
        by = .(sex, yr),
        .SDcols = c("nilf_childcare","nilf_family","abs_childcare","abs_parental","pt_childcare")][order(sex, yr)]
ccn <- d[mo <= 8, .(n_nilf_childcare = sum(nilf_childcare), n_abs_childcare = sum(abs_childcare),
                    n_pt_childcare = sum(pt_childcare)), by = .(sex, yr)]
cc <- merge(cc, ccn, by = c("sex","yr"))
cat("\n=== Childcare/family measures, % of native prime-age population, January-August ===\n")
cat("(nilf_childcare: NILF, wants job, not looking because can't arrange childcare;\n",
    " nilf_family adds 'family responsibilities'; abs_* = employed but absent; pt = part-time)\n")
print(cc, digits = 3)
write_csv(cc, "output/h3_native_prime_childcare_jan_aug.csv")

# mothers of young children are where childcare would bite
ccm <- d[mo <= 8 & sex == "Women" & kid5 == "Own child under 5",
         lapply(.SD, function(v) 100 * sum(WTFINL*v)/sum(WTFINL)), by = yr,
         .SDcols = c("nilf_childcare","nilf_family","abs_childcare","pt_childcare")][order(yr)]
cat("\n=== Same, native prime-age women with own child under 5 ===\n")
print(ccm, digits = 3)
write_csv(ccm, "output/h3_native_prime_childcare_mothers5_jan_aug.csv")
cat("\nDONE 63_women_childcare.R\n")
