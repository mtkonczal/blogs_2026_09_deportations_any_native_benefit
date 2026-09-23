# ==============================================================================
# 64_parents_childcare.R   Is the drop in parents-of-young-children EPOP real,
# and do the CPS childcare questions show it?
#
# Follow-up to 63_women_childcare.R, which found native prime-age EPOP for
# people with an own child under 5 (NCHLT5 > 0) fell ~1 pp for both mothers
# and fathers from 2025 to 2026. Before leaning on a subgroup number:
#   1. Standard errors, clustered on household (CPSID): the same households
#      are interviewed in up to 8 months, so person-months are not independent.
#   2. Composition: reweight each year to the 2024 mix of age x education x
#      marital status x Hispanic, so a shift in WHO the survey reaches (non-
#      response rose) is not read as a change in behavior.
#   3. Parents vs. non-parents difference-in-differences within sex.
#   4. Every childcare / family code in the three CPS questions, for parents:
#        WNLOOK   6 can't arrange childcare, 7 family responsibilities
#                 (NILF, want a job, not looking)            -> % of population
#        WHYABSNT 7 child care problems, 8 other family/personal obligation,
#                 9 maternity/paternity leave (employed, absent) -> % of employed
#        WHYPTLWK 121 child care problems, 122 other family/personal
#                 obligations, 120 too busy with house/school
#                 (worked <35 hours last week)               -> % of employed
# Definitions match 01/63. NSA; January-August of each year.
# ==============================================================================
suppressMessages({library(ipumsr); library(data.table); library(readr); library(fixest)})

ddi <- read_ipums_ddi(readLines("data_raw/ddi_path.txt")[1])
VARS <- c("YEAR","MONTH","CPSID","WTFINL","AGE","SEX","CITIZEN","EMPSTAT","LABFORCE",
          "NCHLT5","EDUC","MARST","HISPAN","WNLOOK","WHYABSNT","WHYPTLWK")
acc <- list(); i <- 0
cb <- function(x, pos) {
  i <<- i + 1; setDT(x)
  x <- x[AGE >= 25 & AGE <= 54 & YEAR >= 2023 & MONTH <= 8 & CITIZEN %in% c(1, 2, 3), ..VARS]
  for (j in names(x)) set(x, j = j, value = as.numeric(x[[j]]))
  acc[[i]] <<- x; NULL
}
read_ipums_micro_chunked(ddi, callback = IpumsSideEffectCallback$new(cb),
                         chunk_size = 2e6, verbose = FALSE)
d <- rbindlist(acc); rm(acc); invisible(gc())

d[, `:=`(yr = YEAR, sex = fifelse(SEX == 1, "Men", "Women"),
         kid5 = as.integer(NCHLT5 > 0),
         emp  = as.integer(EMPSTAT %in% c(10, 12)),
         inlf = as.integer(LABFORCE == 2))]
d[, grp := paste(sex, fifelse(kid5 == 1, "child<5", "no child<5"))]
# composition cells (EDUC cuts as in 01_load_cps_micro.R)
d[, `:=`(agec = cut(AGE, c(24, 34, 44, 54), labels = c("25-34","35-44","45-54")),
         educ = fifelse(EDUC >= 111, "BA+", fifelse(EDUC >= 73, "HS-some col", "<HS")),
         married = as.integer(MARST %in% c(1, 2)),
         hisp = as.integer(HISPAN > 0 & HISPAN < 900))]
d[, cell := paste(agec, educ, married, hisp)]

# ---- 1. EPOP by group and year, with clustered SEs on the change vs 2024 -----
epop <- d[, .(epop = sum(WTFINL*emp)/sum(WTFINL), lfpr = sum(WTFINL*inlf)/sum(WTFINL),
              pop_k = sum(WTFINL)/8000, n = .N, n_hh = uniqueN(CPSID)), by = .(grp, yr)][order(grp, yr)]
cat("\n=== EPOP, native prime-age, January-August ===\n")
print(dcast(epop, grp ~ yr, value.var = "epop"), digits = 4)
cat("\n=== LFPR ===\n"); print(dcast(epop, grp ~ yr, value.var = "lfpr"), digits = 4)
cat("\n=== unweighted person-months / households ===\n")
print(dcast(epop, grp ~ yr, value.var = "n")); print(dcast(epop, grp ~ yr, value.var = "n_hh"))

se_tab <- rbindlist(lapply(unique(d$grp), function(g) {
  f <- feols(emp ~ i(yr, ref = 2024), data = d[grp == g], weights = ~WTFINL, cluster = ~CPSID)
  ct <- coeftable(f); ct <- ct[grepl("yr::", rownames(ct)), , drop = FALSE]
  data.table(grp = g, yr = sub("yr::", "", rownames(ct)),
             d_pp = 100*ct[, 1], se_pp = 100*ct[, 2], p = ct[, 4])
}))
cat("\n=== Change in EPOP vs 2024, pp, household-clustered SEs ===\n")
print(se_tab[, .(grp, yr, d_pp = round(d_pp, 2), se_pp = round(se_pp, 2), p = round(p, 3))])

# 2025 -> 2026 specifically
se_2526 <- rbindlist(lapply(unique(d$grp), function(g) {
  f <- feols(emp ~ i(yr, ref = 2025), data = d[grp == g & yr %in% c(2025, 2026)],
             weights = ~WTFINL, cluster = ~CPSID)
  ct <- coeftable(f)
  data.table(grp = g, d_pp = 100*ct[2, 1], se_pp = 100*ct[2, 2], p = ct[2, 4])
}))
cat("\n=== Change in EPOP 2025 -> 2026, pp, household-clustered SEs ===\n")
print(se_2526[, .(grp, d_pp = round(d_pp, 2), se_pp = round(se_pp, 2), p = round(p, 3))])

# ---- 2. Parents vs non-parents, difference-in-differences within sex --------
did <- rbindlist(lapply(c("Men","Women"), function(s) {
  f <- feols(emp ~ kid5 * i(yr, ref = 2024), data = d[sex == s],
             weights = ~WTFINL, cluster = ~CPSID)
  ct <- coeftable(f); ct <- ct[grepl("^kid5:yr::", rownames(ct)), , drop = FALSE]
  data.table(sex = s, yr = sub("kid5:yr::", "", rownames(ct)),
             did_pp = 100*ct[, 1], se_pp = 100*ct[, 2], p = ct[, 4])
}))
cat("\n=== Parents-of-child<5 minus non-parents, change vs 2024, pp ===\n")
print(did[, .(sex, yr, did_pp = round(did_pp, 2), se_pp = round(se_pp, 2), p = round(p, 3))])

# ---- 3. Composition: reweight each year to the 2024 cell mix ----------------
base <- d[yr == 2024, .(w24 = sum(WTFINL)), by = .(grp, cell)][, w24 := w24/sum(w24), by = grp]
cr <- d[, .(e = sum(WTFINL*emp)/sum(WTFINL)), by = .(grp, yr, cell)]
cr <- merge(cr, base, by = c("grp","cell"))          # cells absent in 2024 drop out
adj <- cr[, .(epop_adj = sum(w24*e)/sum(w24)), by = .(grp, yr)]
cat("\n=== EPOP reweighted to 2024 age x educ x married x Hispanic mix ===\n")
print(dcast(adj, grp ~ yr, value.var = "epop_adj"), digits = 4)
mix <- d[, .(share_BA = sum(WTFINL*(educ=="BA+"))/sum(WTFINL),
             share_married = sum(WTFINL*married)/sum(WTFINL),
             share_hisp = sum(WTFINL*hisp)/sum(WTFINL),
             mean_age = sum(WTFINL*AGE)/sum(WTFINL)), by = .(grp, yr)][order(grp, yr)]
cat("\n=== Who is in each group (weighted) ===\n"); print(mix, digits = 3)
share_kid <- d[, .(share_with_child5 = sum(WTFINL*kid5)/sum(WTFINL)), by = .(sex, yr)][order(sex, yr)]
cat("\n=== Share of native prime-age with an own child under 5 ===\n"); print(share_kid, digits = 3)

# ---- 4. Every childcare / family code, by group --------------------------
d[, `:=`(
  nl_childcare = as.integer(inlf == 0 & WNLOOK == 6),
  nl_family    = as.integer(inlf == 0 & WNLOOK == 7),
  ab_childcare = as.integer(EMPSTAT == 12 & WHYABSNT == 7),
  ab_family    = as.integer(EMPSTAT == 12 & WHYABSNT == 8),
  ab_parental  = as.integer(EMPSTAT == 12 & WHYABSNT == 9),
  pt_childcare = as.integer(emp == 1 & WHYPTLWK == 121),
  pt_family    = as.integer(emp == 1 & WHYPTLWK == 122),
  pt_house     = as.integer(emp == 1 & WHYPTLWK == 120))]
nl <- d[, .(nl_childcare = 100*sum(WTFINL*nl_childcare)/sum(WTFINL),
            nl_family    = 100*sum(WTFINL*nl_family)/sum(WTFINL),
            n_nl_cc = sum(nl_childcare), n_nl_fam = sum(nl_family)), by = .(grp, yr)]
wk <- d[emp == 1, .(ab_childcare = 100*sum(WTFINL*ab_childcare)/sum(WTFINL),
                    ab_family    = 100*sum(WTFINL*ab_family)/sum(WTFINL),
                    ab_parental  = 100*sum(WTFINL*ab_parental)/sum(WTFINL),
                    pt_childcare = 100*sum(WTFINL*pt_childcare)/sum(WTFINL),
                    pt_family    = 100*sum(WTFINL*pt_family)/sum(WTFINL),
                    pt_house     = 100*sum(WTFINL*pt_house)/sum(WTFINL),
                    n_ab_cc = sum(ab_childcare), n_ab_par = sum(ab_parental),
                    n_pt_cc = sum(pt_childcare), n_pt_fam = sum(pt_family)), by = .(grp, yr)]
cc <- merge(nl, wk, by = c("grp","yr"))[order(grp, yr)]
cat("\n=== Not looking for work (WNLOOK), % of group population ===\n")
print(cc[, .(grp, yr, nl_childcare, nl_family, n_nl_cc, n_nl_fam)], digits = 3)
cat("\n=== Absent from job (WHYABSNT), % of employed ===\n")
print(cc[, .(grp, yr, ab_childcare, ab_family, ab_parental, n_ab_cc, n_ab_par)], digits = 3)
cat("\n=== Part-time last week (WHYPTLWK), % of employed ===\n")
print(cc[, .(grp, yr, pt_childcare, pt_family, pt_house, n_pt_cc, n_pt_fam)], digits = 3)

# Could the childcare codes account for the EPOP drop? The NILF-childcare
# share would have to rise by about as much as EPOP fell.
cat("\n=== Change 2024 -> 2026: EPOP vs. NILF-for-childcare/family, pp of population ===\n")
cmp <- merge(epop[yr %in% c(2024, 2026), .(grp, yr, epop = 100*epop)],
             cc[yr %in% c(2024, 2026), .(grp, yr, nl_cf = nl_childcare + nl_family)], by = c("grp","yr"))
print(dcast(cmp, grp ~ yr, value.var = c("epop","nl_cf"))[
  , .(grp, d_epop = round(epop_2026 - epop_2024, 2), d_nilf_childcare_family = round(nl_cf_2026 - nl_cf_2024, 2))])

write_csv(epop, "output/h3b_parents_epop_jan_aug.csv")
write_csv(se_tab, "output/h3b_parents_epop_change_se.csv")
write_csv(did, "output/h3b_parents_did.csv")
write_csv(adj, "output/h3b_parents_epop_reweighted.csv")
write_csv(cc, "output/h3b_parents_childcare_codes.csv")
cat("\nDONE 64_parents_childcare.R\n")
