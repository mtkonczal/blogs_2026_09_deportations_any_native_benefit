# ==============================================================================
# 50_load_state_jobs_bls.R
# Pull State and Area Employment, Hours, and Earnings (SAE) via
# tidyusmacro::getBLSFiles("sae", ...). This is the establishment-survey
# analogue of state CES: near-census payroll counts by state, so unlike CPS
# state cells (thin, see R/08_geography.R) there is no small-sample problem.
#
# We keep total nonfarm payroll employment, seasonally adjusted, by state.
# Caches the raw pull to data_raw/ so re-runs don't re-download 330MB.
# ==============================================================================
suppressMessages({library(tidyusmacro); library(dplyr); library(readr)})

email <- Sys.getenv("EMAIL_FOR_BLS", unset = "mike@economicsecurityproject.org")
cache <- "data_raw/sae_raw.rds"

if (file.exists(cache)) {
  message("Using cached SAE pull: ", cache)
  sae <- readRDS(cache)
} else {
  sae <- getBLSFiles("sae", email)
  saveRDS(sae, cache)
  write_lines(format(Sys.time(), tz = "UTC", usetz = TRUE), "data_raw/sae_download_timestamp.txt")
}

cat("SAE rows:", nrow(sae), "  columns:", paste(names(sae), collapse=", "), "\n")
cat("Date range:", format(min(sae$date)), "to", format(max(sae$date)), "\n")

# Sanity check: confirm the state lookup joined (area_code alone is not a
# state identifier -- it is state FIPS + area sequence, so we need the
# decoded state name/abbreviation column from the pinned "state" lookup).
state_cols <- grep("state", names(sae), value = TRUE, ignore.case = TRUE)
cat("State-related columns found:", paste(state_cols, collapse=", "), "\n")
print(head(sae, 3))
saveRDS(sae, "data_raw/sae_raw.rds")
cat("DONE 50_load_state_jobs_bls.R\n")
