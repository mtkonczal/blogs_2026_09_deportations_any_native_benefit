# ==============================================================================
# 01_load_cps_micro.R
# Read the IPUMS CPS basic monthly extract in chunks, keep age >= 16, build the
# analysis frame, and cache it as an RDS. Chunked because the full extract is
# ~18M person-months and will not fit comfortably in memory as a tibble.
#
# Definitions locked here and used by every downstream script:
#   native-born  = CITIZEN in {1,2,3}  (born in US, US outlying, or abroad of
#                  American parents). This matches the BLS nativity definition.
#   prime age    = 25-54
#   education    = EDUC >= 111 BA+, 73-110 HS through some college, <73 < HS
#                  (same cuts as 01_education_young_unrate for comparability)
# ==============================================================================
suppressMessages({library(ipumsr); library(data.table); library(dplyr)})

ddi_path <- readLines("data_raw/ddi_path.txt")[1]
stopifnot(file.exists(ddi_path))
ddi <- read_ipums_ddi(ddi_path)
cat("reading:", ddi_path, "\n")

KEEP <- c("YEAR","MONTH","CPSID","CPSIDP","MISH","WTFINL","EARNWT","LNKFW1MWT",
          "AGE","SEX","RACE","HISPAN","MARST","NCHLT5",
          "EDUC","EMPSTAT","LABFORCE","CLASSWKR","OCC2010","IND1990",
          "UHRSWORKT","AHRSWORKT","WKSTAT","WHYPTLWK","WHYUNEMP","DURUNEMP",
          "EARNWEEK","HOURWAGE","QEARNWEE","QHOURWAG","PAIDHOUR",
          "NATIVITY","CITIZEN","YRIMMIG","STATEFIP","METFIPS")

if (file.exists("data_raw/cps_raw.rds")) {
  cps <- readRDS("data_raw/cps_raw.rds"); setDT(cps)
  cat("restored cached raw read:", format(nrow(cps), big.mark=","), "rows\n")
} else {
acc <- list(); i <- 0
cb <- function(x, pos) {
  i <<- i + 1
  setDT(x)
  keep <- intersect(KEEP, names(x))
  x <- x[AGE >= 16, ..keep]
  # zap_labels equivalent: strip haven labels, keep the integer codes
  for (j in names(x)) set(x, i = NULL, j = j, value = as.numeric(x[[j]]))
  acc[[i]] <<- x
  NULL
}
read_ipums_micro_chunked(ddi, callback = IpumsSideEffectCallback$new(cb),
                         chunk_size = 2e6, verbose = FALSE)
cps <- rbindlist(acc, use.names = TRUE, fill = TRUE); rm(acc); gc()
cat("rows (age 16+):", format(nrow(cps), big.mark=","), "\n")
# cache the raw read so a downstream error never costs another full parse
saveRDS(cps, "data_raw/cps_raw.rds", compress = FALSE)
}

cps[, date := as.Date(sprintf("%d-%02d-01", YEAR, MONTH))]

# --- labor force status (IPUMS codes) ----------------------------------------
# EMPSTAT: 10,12 at work / has job; 20,21,22 unemployed; 30+ NILF
cps[, emp   := as.integer(EMPSTAT %in% c(10, 12))]
cps[, unemp := as.integer(EMPSTAT %in% c(20, 21, 22))]
cps[, inlf  := as.integer(LABFORCE == 2)]
cps[, nilf  := as.integer(inlf == 0 & !is.na(LABFORCE) & LABFORCE != 0)]

# --- nativity ----------------------------------------------------------------
# CITIZEN 1 born in US / 2 born in US outlying / 3 born abroad of American
# parents / 4 naturalized / 5 not a citizen.
cps[, native := as.integer(CITIZEN %in% c(1, 2, 3))]
cps[, foreign_born := as.integer(CITIZEN %in% c(4, 5))]
cps[, noncitizen  := as.integer(CITIZEN == 5)]
cps[, naturalized := as.integer(CITIZEN == 4)]
# NATIVITY 1 both parents native ... 4 both parents foreign, 5 foreign born.
# Among the native-born this separates 2nd-generation citizens (mixed-status
# households) from 3rd-generation-plus. Used for H11.
cps[, gen := fifelse(native == 1 & NATIVITY == 1, "3rd+ gen native",
              fifelse(native == 1 & NATIVITY %in% c(2,3,4), "2nd gen native",
               fifelse(foreign_born == 1, "Foreign born", NA_character_)))]
cps[, hispanic := as.integer(HISPAN > 0 & HISPAN < 900)]

# --- education ---------------------------------------------------------------
cps[, educ_grp := fifelse(EDUC >= 111, "BA+",
                   fifelse(EDUC >= 73, "HS to some college",
                    fifelse(EDUC > 1 & EDUC < 73, "Less than HS", NA_character_)))]
cps[, noncollege := as.integer(educ_grp %in% c("Less than HS","HS to some college"))]

cps[, prime := as.integer(AGE >= 25 & AGE <= 54)]

# --- hours and part-time -----------------------------------------------------
# UHRSWORKT 997 = hours vary, 999 = NIU. WHYPTLWK: economic reasons are the
# "slack work / could only find part-time" codes (see 07_hours.R).
cps[, uhrs := fifelse(UHRSWORKT < 997, UHRSWORKT, NA_real_)]
cps[, ahrs := fifelse(AHRSWORKT < 997, AHRSWORKT, NA_real_)]

# --- earnings (outgoing rotation groups only) --------------------------------
cps[, org := as.integer(MISH %in% c(4, 8))]
cps[, earnwk := fifelse(EARNWEEK < 9999.99 & EARNWEEK > 0, EARNWEEK, NA_real_)]
cps[, hrwage := fifelse(HOURWAGE < 99.99 & HOURWAGE > 0, HOURWAGE, NA_real_)]
cps[, earn_imputed := as.integer((!is.na(QEARNWEE) & QEARNWEE > 0) |
                                 (!is.na(QHOURWAG) & QHOURWAG > 0))]

stopifnot(all(c("native","foreign_born","gen","educ_grp") %in% names(cps)))
saveRDS(cps, "data_raw/cps_analysis.rds", compress = FALSE)

# --- sanity: weighted totals must match published BLS -------------------------
chk <- cps[, .(emp = sum(WTFINL * emp)/1000, unemp = sum(WTFINL * unemp)/1000,
               lf = sum(WTFINL * inlf)/1000), by = .(date, native)][order(date)]
cat("\n=== weighted employment by nativity, latest 4 months (thousands) ===\n")
print(chk[date >= max(date) - 120])
cat("\nsaved data_raw/cps_analysis.rds\n")
