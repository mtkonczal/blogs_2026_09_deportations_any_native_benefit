# Load the supplementary ORG earnings extract (EARNWEEK2 / HOURWAGE2) and merge
# onto the main analysis frame by CPSIDP + date. IPUMS introduced these two
# variables in 2023 when BLS changed earnings topcoding; the original
# EARNWEEK / HOURWAGE are empty from 2024 on.
suppressMessages({library(ipumsr); library(data.table)})
ddi <- read_ipums_ddi(readLines("data_raw/ddi_path_earn.txt")[1])
KEEP <- c("YEAR","MONTH","CPSIDP","PERNUM","SERIAL","MISH","EARNWT",
          "EARNWEEK2","HOURWAGE2","PAIDHOUR","UNION","UHRSWORKORG")
acc <- list(); i <- 0
cb <- function(x, pos) {
  i <<- i + 1; setDT(x)
  k <- intersect(KEEP, names(x)); x <- x[, ..k]
  x <- x[!is.na(EARNWEEK2) | !is.na(HOURWAGE2)]
  for (j in names(x)) set(x, i = NULL, j = j, value = as.numeric(x[[j]]))
  acc[[i]] <<- x; NULL
}
read_ipums_micro_chunked(ddi, callback = IpumsSideEffectCallback$new(cb),
                         chunk_size = 2e6, verbose = FALSE)
e <- rbindlist(acc, use.names = TRUE, fill = TRUE); rm(acc); gc()
e[, date := as.Date(sprintf("%d-%02d-01", YEAR, MONTH))]
cat("earnings records:", format(nrow(e), big.mark=","), "\n")
cat("by year, non-missing HOURWAGE2 / EARNWEEK2:\n")
print(e[, .(hw = sum(!is.na(HOURWAGE2) & HOURWAGE2 > 0 & HOURWAGE2 < 99.99),
            ew = sum(!is.na(EARNWEEK2) & EARNWEEK2 > 0), n = .N),
        by = .(yr = YEAR)][order(yr)])
saveRDS(e, "data_raw/cps_earn.rds", compress = FALSE)
cat("saved data_raw/cps_earn.rds\n")
