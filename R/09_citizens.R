# ==============================================================================
# 09_citizens.R   H11: did enforcement spill onto U.S. citizens?
# NATIVITY separates native-born with foreign-born parents (2nd generation,
# disproportionately living in mixed-status households) from native-born with
# native-born parents. Both groups are U.S. citizens. Under the substitution
# story there is no reason for them to diverge.
# ==============================================================================
suppressMessages({library(data.table); library(dplyr); library(readr); library(lubridate)})
cps <- readRDS("data_raw/cps_analysis.rds"); setDT(cps)

g <- cps[prime == 1 & !is.na(gen), .(
  epop   = sum(WTFINL*emp)/sum(WTFINL),
  lfpr   = sum(WTFINL*inlf)/sum(WTFINL),
  unrate = sum(WTFINL*unemp)/sum(WTFINL*inlf),
  pop    = sum(WTFINL)/1000, n = .N
), by = .(gen, yr = year(date))][order(gen, yr)]
cat("=== H11: prime-age outcomes by generation (all U.S. citizens except the last) ===\n")
print(as.data.frame(g[yr >= 2022]))
write_csv(g, "output/h11_by_generation.csv")

cat("\n=== change from 2024 to 2026 YTD ===\n")
ch <- dcast(g[yr %in% c(2024, 2026)], gen ~ yr, value.var = c("epop","lfpr","unrate"))
print(as.data.frame(ch))

# Hispanic vs non-Hispanic among the native-born
hg <- cps[prime == 1 & native == 1, .(
  epop = sum(WTFINL*emp)/sum(WTFINL),
  lfpr = sum(WTFINL*inlf)/sum(WTFINL),
  unrate = sum(WTFINL*unemp)/sum(WTFINL*inlf), n = .N
), by = .(grp = fifelse(hispanic == 1, "Native, Hispanic", "Native, non-Hispanic"),
          yr = year(date))][order(grp, yr)]
cat("\n=== H11: NATIVE-BORN prime-age, Hispanic vs non-Hispanic ===\n")
print(as.data.frame(hg[yr >= 2022]))
write_csv(hg, "output/h11_native_hispanic.csv")

# 2nd-generation Hispanic citizens: the most exposed citizen group
hg2 <- cps[prime == 1 & native == 1, .(
  epop = sum(WTFINL*emp)/sum(WTFINL), lfpr = sum(WTFINL*inlf)/sum(WTFINL),
  unrate = sum(WTFINL*unemp)/sum(WTFINL*inlf), n = .N
), by = .(grp = paste0(fifelse(gen == "2nd gen native", "2nd gen", "3rd+ gen"), ", ",
                       fifelse(hispanic == 1, "Hispanic", "non-Hispanic")),
          yr = year(date))][order(grp, yr)]
cat("\n=== H11: native-born citizens by generation x ethnicity ===\n")
print(as.data.frame(hg2[yr >= 2023]))
write_csv(hg2, "output/h11_gen_by_ethnicity.csv")
cat("\nDONE 09_citizens.R\n")
