# ==============================================================================
# 41_batch2_wages.R
# Batch 2, earnings.
#   H19  a matched-worker wage tracker, built here            (claim C13, Bessent)
#   H23  union density and the native union wage premium      (claim C17)
#   plus the wage columns for H17 (sex), H18 (construction) and H21 (top-20
#   occupations), which need the ORG earnings extract.
#
# WHY A MATCHED TRACKER. Batch 1 measured wages as cross-sectional percentiles,
# which are composition-contaminated in both directions: when low-wage
# employment falls the measured p10 rises for a bad reason. Bessent's claim that
# "the bottom 25 percent of wage earners" saw wages rise "almost three and a
# half times more than the top 25 percent" is plausibly the Atlanta Fed Wage
# Growth Tracker by quartile, which is matched at the person level and is
# therefore immune to that. So rebuild it here.
#
# Construction: the CPS rotation puts a household in sample for months 1-4, out
# for eight months, then back for months 5-8. So a person's MIS-4 record and
# their MIS-8 record are exactly twelve months apart. Match on CPSIDP, take the
# median percent change in the hourly wage. That is the Atlanta Fed measure, cut
# by nativity, education, wage quartile and occupational exposure, which the
# published tracker cannot do.
#
# CAVEAT, stated everywhere this appears: a matched tracker conditions on being
# employed with a measured wage in both periods. It removes composition bias and
# adds survivorship bias, because it drops exactly the workers who lost jobs. It
# belongs next to the cross-sectional numbers, not instead of them.
# ==============================================================================
suppressMessages({
  library(data.table); library(dplyr); library(readr); library(blsR); library(lubridate)
})
bls_set_key(Sys.getenv("BLS_KEY"))
dir.create("output", showWarnings = FALSE)

cps <- readRDS("data_raw/cps_analysis.rds"); setDT(cps)
e   <- readRDS("data_raw/cps_earn.rds");     setDT(e)
occ_exp <- as.data.table(read_csv("data/exposure_occ.csv", show_col_types = FALSE))

e <- e[MISH %in% c(4, 8)]
e[, `:=`(ew = fifelse(EARNWEEK2 > 0 & EARNWEEK2 < 999999, EARNWEEK2, NA_real_),
         hw = fifelse(HOURWAGE2 > 0 & HOURWAGE2 < 999,    HOURWAGE2, NA_real_),
         uh = fifelse(UHRSWORKORG > 0 & UHRSWORKORG < 997, UHRSWORKORG, NA_real_))]
e <- e[!is.na(ew) | !is.na(hw)]

m <- merge(cps[org == 1 & emp == 1,
               .(CPSIDP, date, MISH, native, foreign_born, noncitizen, naturalized, noncollege,
                 educ_grp, prime, AGE, SEX, OCC2010, IND1990, earn_imputed, uhrs)],
           e[, .(CPSIDP, date, ew, hw, uh, EARNWT, UNION)],
           by = c("CPSIDP", "date"))
rm(cps, e); gc()

m[, hours := fifelse(!is.na(uh), uh, uhrs)]
m[, wage  := fifelse(!is.na(hw), hw, fifelse(hours >= 1, ew / hours, NA_real_))]
m <- m[!is.na(wage) & wage >= 2 & wage < 500 & EARNWT > 0]
m[, yr := year(date)]
cat("usable ORG hourly-wage records:", format(nrow(m), big.mark = ","), "\n")

cpi <- get_n_series_table("CUUR0000SA0", api_key = bls_get_key(),
                          start_year = 2014, end_year = 2026, tidy = TRUE) %>%
  transmute(cpi = suppressWarnings(as.numeric(CUUR0000SA0)),
            date = as.Date(paste0(year, "/", month, "/1"))) %>% filter(!is.na(cpi))
base <- cpi$cpi[which.max(cpi$date)]
m <- merge(m, as.data.table(cpi)[, .(date, cpi, defl = base / cpi)], by = "date")
m[, real_wage := wage * defl]
cat("deflated to", format(max(cpi$date), "%B %Y"), "dollars\n")

# ==============================================================================
# H19. The matched-worker wage tracker  (C13)
# ==============================================================================
cat("\n\n############ H19: matched-worker wage tracker ############\n")

a <- m[MISH == 4, .(CPSIDP, date_0 = date, wage_0 = wage, real_wage_0 = real_wage,
                    cpi_0 = cpi, OCC2010_0 = OCC2010, IND1990_0 = IND1990,
                    imputed_0 = earn_imputed, EARNWT_0 = EARNWT)]
b <- m[MISH == 8, .(CPSIDP, date_1 = date, wage_1 = wage, real_wage_1 = real_wage,
                    native, foreign_born, naturalized, noncitizen, noncollege,
                    educ_grp, prime, AGE, SEX,
                    OCC2010_1 = OCC2010, imputed_1 = earn_imputed, EARNWT_1 = EARNWT,
                    UNION)]
a[, join_date := date_0 %m+% months(12)]
mt <- merge(a, b, by.x = c("CPSIDP", "join_date"), by.y = c("CPSIDP", "date_1"))
setnames(mt, "join_date", "date_1")
stopifnot(all(mt$date_1 == mt$date_0 %m+% months(12)))
cat("matched 12-month wage pairs:", format(nrow(mt), big.mark = ","), "\n")

# the October 2025 hole: no MIS-4 in Oct 2025 and no survey to match Oct 2024
# forward into, so two cohorts are structurally absent. Never interpolated.
cat("\nmatched pairs per ending year (the Oct-2025 gap costs one cohort):\n")
print(mt[, .(pairs = .N), by = .(yr_1 = year(date_1))][order(yr_1)])
cat("pairs with an Oct 2025 endpoint:", sum(mt$date_1 == as.Date("2025-10-01")), "\n")

mt[, g := wage_1 / wage_0 - 1]                      # nominal, Atlanta Fed convention
mt[, g_real := real_wage_1 / real_wage_0 - 1]
mt <- mt[is.finite(g) & g > -0.90 & g < 4]          # trim the worst coding errors
mt[, avg_real := (real_wage_0 + real_wage_1) / 2]

# Quartiles on the AVERAGE of the two wages, which is the Atlanta Fed
# convention; ranking on the initial wage alone would import mean reversion and
# inflate bottom-quartile growth on its own.
wq_cut <- function(x, wt, p) { o <- order(x); x <- x[o]; wt <- wt[o]
  x[which(cumsum(wt)/sum(wt) >= p)[1]] }
mt[, qtile := {
  c1 <- wq_cut(avg_real, EARNWT_1, .25); c2 <- wq_cut(avg_real, EARNWT_1, .50)
  c3 <- wq_cut(avg_real, EARNWT_1, .75)
  fcase(avg_real <= c1, "Q1 lowest", avg_real <= c2, "Q2",
        avg_real <= c3, "Q3", default = "Q4 highest")
}, by = .(yr_1 = year(date_1))]

wmed <- function(x, wt) { o <- order(x); x <- x[o]; wt <- wt[o]
  x[which(cumsum(wt)/sum(wt) >= 0.5)[1]] }

trk <- function(dt, lbl) dt[, .(tracker = wmed(g, EARNWT_1),
                                tracker_real = wmed(g_real, EARNWT_1), n = .N),
                            by = .(yr = year(date_1))][, grp := lbl][]

h19 <- rbindlist(list(
  trk(mt, "All workers"),
  trk(mt[native == 1], "Native-born"),
  trk(mt[foreign_born == 1], "Foreign-born"),
  trk(mt[native == 1 & noncollege == 1], "Native, no BA"),
  trk(mt[native == 1 & educ_grp == "BA+"], "Native, BA+"),
  trk(mt[native == 1 & SEX == 1], "Native men"),
  trk(mt[native == 1 & SEX == 2], "Native women"),
  # H25: the group the literature says actually competes with new arrivals
  trk(mt[naturalized == 1], "Naturalized citizens"),
  trk(mt[noncitizen == 1], "Noncitizens"),
  trk(mt[naturalized == 1 & noncollege == 1], "Naturalized, no BA")
))[order(grp, yr)]
cat("\n=== matched 12-month wage growth, median, by group (nominal / real) ===\n")
print(as.data.frame(h19[yr >= 2020, .(grp, yr, tracker = round(100*tracker, 2),
                                      real = round(100*tracker_real, 2), n)]))

h19q <- rbindlist(list(
  mt[, .(tracker = wmed(g, EARNWT_1), tracker_real = wmed(g_real, EARNWT_1), n = .N),
     by = .(yr = year(date_1), qtile)][, grp := "All workers"],
  mt[native == 1, .(tracker = wmed(g, EARNWT_1), tracker_real = wmed(g_real, EARNWT_1),
                    n = .N), by = .(yr = year(date_1), qtile)][, grp := "Native-born"]
))[order(grp, qtile, yr)]
cat("\n=== by wage quartile: THE Bessent test ===\n")
cat("His claim is that the bottom quartile is growing about 3.5x the top.\n")
print(as.data.frame(dcast(h19q[yr >= 2022 & grp == "Native-born"],
                          qtile ~ yr, value.var = "tracker")))
rat <- h19q[grp == "Native-born" & yr >= 2022]
rat <- dcast(rat, yr ~ qtile, value.var = "tracker")
rat[, q1_over_q4 := `Q1 lowest` / `Q4 highest`]
cat("\nratio of bottom-quartile to top-quartile median wage growth, native-born:\n")
print(as.data.frame(rat))

# THE discriminating test: if the mechanism is less competition from immigrant
# labor, any bottom-quartile acceleration has to be concentrated in the
# occupations where that labor was thickest. If it is flat across exposure it is
# a macro wage-floor story with nothing to do with enforcement.
occ_q <- occ_exp[usable == TRUE][order(foreign_share)]
occ_q[, exp_q := cut(rank(foreign_share, ties.method = "first"), 4,
                     labels = c("Q1 lowest","Q2","Q3","Q4 highest"))]
mtx <- merge(mt, occ_q[, .(OCC2010_0 = OCC2010, exp_q)], by = "OCC2010_0")
h19x <- mtx[native == 1, .(tracker = wmed(g, EARNWT_1),
                           tracker_real = wmed(g_real, EARNWT_1), n = .N),
            by = .(yr = year(date_1), exp_q)][order(exp_q, yr)]
cat("\n=== native matched wage growth by OCCUPATIONAL EXPOSURE quartile ===\n")
print(as.data.frame(dcast(h19x[yr >= 2022], exp_q ~ yr, value.var = "tracker")))

h19xb <- mtx[native == 1 & qtile == "Q1 lowest",
             .(tracker = wmed(g, EARNWT_1), n = .N),
             by = .(yr = year(date_1), exp_q)][order(exp_q, yr)]
cat("\n=== bottom-quartile native workers only, by exposure quartile ===\n")
print(as.data.frame(dcast(h19xb[yr >= 2022], exp_q ~ yr, value.var = "tracker")))

h19n <- mtx[naturalized == 1, .(tracker = wmed(g, EARNWT_1), n = .N),
            by = .(yr = year(date_1), exp_q)][order(exp_q, yr)]
cat("\n=== naturalized-citizen matched wage growth by exposure quartile ===\n")
cat("The substitution model's own best case: prior immigrants are the close\n")
cat("substitutes for new arrivals, so THIS is where a gain should appear.\n")
print(as.data.frame(dcast(h19n[yr >= 2022], exp_q ~ yr, value.var = "tracker")))
write_csv(h19n, "output/b2_h25_tracker_naturalized_by_exposure.csv")

write_csv(h19,   "output/b2_h19_tracker_by_group.csv")
write_csv(h19q,  "output/b2_h19_tracker_by_quartile.csv")
write_csv(h19x,  "output/b2_h19_tracker_by_exposure.csv")
write_csv(h19xb, "output/b2_h19_tracker_bottomq_by_exposure.csv")

# ==============================================================================
# H23. Union density and the native union wage premium  (C17)
# ==============================================================================
cat("\n\n############ H23: union density and the union wage premium ############\n")
cat("UNION value distribution (ORG wage records):\n")
print(m[, .N, by = UNION][order(UNION)])
# IPUMS UNION: 0 NIU, 1 no union coverage, 2 member of a labor union,
# 3 covered by a union but not a member.
m[, union_member := fifelse(UNION %in% c(1,2,3), as.integer(UNION == 2), NA_integer_)]
m[, union_cov    := fifelse(UNION %in% c(1,2,3), as.integer(UNION %in% c(2,3)), NA_integer_)]

h23 <- m[!is.na(union_cov), .(
  member_rate = sum(EARNWT*union_member)/sum(EARNWT),
  cov_rate    = sum(EARNWT*union_cov)/sum(EARNWT), n = .N),
  by = .(yr, grp = fcase(native == 1, "Native-born",
                         naturalized == 1, "Naturalized citizen",
                         noncitizen == 1, "Noncitizen"))][order(grp, yr)]
cat("\n=== union membership and coverage rate, ORG wage earners ===\n")
print(as.data.frame(h23))

h23nc <- m[!is.na(union_cov) & native == 1 & noncollege == 1, .(
  member_rate = sum(EARNWT*union_member)/sum(EARNWT),
  cov_rate = sum(EARNWT*union_cov)/sum(EARNWT), n = .N), by = yr][order(yr)]
cat("\n=== native workers without a BA ===\n")
print(as.data.frame(h23nc))

# union wage premium for native workers, controlling for the obvious things
prem <- rbindlist(lapply(sort(unique(m$yr)), function(y) {
  d <- m[yr == y & native == 1 & !is.na(union_cov) & earn_imputed == 0 &
         !is.na(OCC2010) & !is.na(IND1990)]
  if (nrow(d) < 5000) return(NULL)
  d[, `:=`(age2 = AGE^2, occ = factor(OCC2010 %/% 100), ind = factor(IND1990 %/% 100))]
  mod <- lm(log(real_wage) ~ union_cov + factor(educ_grp) + AGE + age2 +
              factor(SEX) + occ + ind, data = d, weights = d$EARNWT)
  ct <- summary(mod)$coefficients
  data.table(yr = y, premium = ct["union_cov", 1], se = ct["union_cov", 2], n = nrow(d))
}))
cat("\n=== native union wage premium, log points ===\n")
cat("controls: education, age, age^2, sex, 1-digit occupation, 1-digit industry\n")
print(as.data.frame(prem[, .(yr, premium = round(premium, 4),
                             se = round(se, 4), n)]))

write_csv(h23,   "output/b2_h23_union.csv")
write_csv(h23nc, "output/b2_h23_union_nocollege.csv")
write_csv(prem,  "output/b2_h23_union_premium.csv")

# ==============================================================================
# Wage columns for H17 (sex), H18 (construction), H21 (top-20 occupations)
# ==============================================================================
cat("\n\n############ wage cuts for H17, H18, H21 ############\n")
wq <- function(x, wt, p) { o <- order(x); x <- x[o]; wt <- wt[o]
  x[which(cumsum(wt)/sum(wt) >= p)[1]] }
main <- m[earn_imputed == 0]

# Jan-August of each year throughout, so a partial 2026 has the same seasonal
# composition as every other year it is compared to.
j8 <- main[month(date) %in% 1:8]

w_sex <- j8[native == 1, .(p10 = wq(real_wage, EARNWT, .10),
                           p50 = wq(real_wage, EARNWT, .50),
                           mean = sum(real_wage*EARNWT)/sum(EARNWT), n = .N),
            by = .(yr, grp = fifelse(SEX == 1, "Native men", "Native women"))][order(grp, yr)]
w_sex[, `:=`(g_p10 = p10/shift(p10)-1, g_p50 = p50/shift(p50)-1), by = grp]
cat("\n=== native real hourly wages by sex, Jan-Aug ===\n")
print(as.data.frame(w_sex[yr >= 2022, .(grp, yr, p10 = round(p10,2), p50 = round(p50,2),
                                        g_p50 = round(100*g_p50,2), n)]))

w_con <- j8[native == 1 & IND1990 == 60, .(p50 = wq(real_wage, EARNWT, .50),
                                           p25 = wq(real_wage, EARNWT, .25),
                                           mean = sum(real_wage*EARNWT)/sum(EARNWT),
                                           n = .N), by = yr][order(yr)]
w_con[, g_p50 := p50/shift(p50)-1]
cat("\n=== native real hourly wages in construction, Jan-Aug ===\n")
print(as.data.frame(w_con[yr >= 2022]))

TOP <- occ_exp[usable == TRUE][order(-foreign_share)][1:20]
w_occ <- j8[native == 1 & OCC2010 %in% TOP$OCC2010 &
            yr %in% c(2024, 2026), .(p50 = wq(real_wage, EARNWT, .50), n = .N),
            by = .(OCC2010, yr)]
w_occ <- dcast(w_occ, OCC2010 ~ yr, value.var = c("p50", "n"))
w_occ[, g_p50_24_26 := p50_2026 / p50_2024 - 1]
# ORG wage cells at the occupation level are thin. A median on fifteen records
# is not a statistic, so flag which rows are usable and count only those.
w_occ[, usable := n_2024 >= 50 & n_2026 >= 50]
cat("\n=== native real median wage in the top-20 occupations, 2024 vs 2026 ===\n")
print(as.data.frame(w_occ[order(-g_p50_24_26)]))
cat(sprintf("all 20 cells:      native real median wage ROSE in %d of %d\n",
            sum(w_occ$g_p50_24_26 > 0, na.rm = TRUE), sum(!is.na(w_occ$g_p50_24_26))))
cat(sprintf("cells with n>=50:  native real median wage ROSE in %d of %d\n",
            sum(w_occ$usable & w_occ$g_p50_24_26 > 0, na.rm = TRUE), sum(w_occ$usable)))

write_csv(w_sex, "output/b2_h17_wages_by_sex.csv")
write_csv(w_con, "output/b2_h18_wages_construction.csv")
write_csv(w_occ, "output/b2_h21_wages_top20.csv")

cat("\nDONE 41_batch2_wages.R\n")
