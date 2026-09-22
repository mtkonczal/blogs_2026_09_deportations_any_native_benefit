# ==============================================================================
# 51_load_ice_state.R
# State-level ICE administrative arrest counts, from the Deportation Data
# Project (UC Berkeley Law / UCLA), FOIA microdata processed and posted at
# github.com/deportationdata/ice (data/arrests-latest.parquet).
#
# Source facts, verified 2026-09-19:
#   - Underlying data: individual-level ICE arrest records, FOIA litigation.
#   - Processed file covers 2022-10-01 through 2026-08-06 (partial last month).
#   - apprehension_state_filled_in backfills missing apprehension_state using
#     toa_current_duty_site / event_landmark (see codebook). ~1.5% still
#     missing in our analysis window and are dropped, not imputed further.
#   - duplicate_drop_row flags rows the Deportation Data Project itself
#     identifies as likely duplicates; we drop them per their own guidance.
#   - "Arrests" here means ICE administrative arrests, NOT deportations/
#     removals. The two diverge (an arrest can end in release, a court case,
#     or removal). Keep this distinction explicit in every chart/label.
#
# Window: 2025-01-01 through 2026-07-31 (last full month in the release),
# matching the reference chart's Jan 2025 - Jul 2026 window.
# ==============================================================================
suppressMessages({library(nanoparquet); library(dplyr); library(readr); library(lubridate); library(stringr)})

ice_path <- "data_raw/ice/arrests-latest.parquet"
stopifnot(file.exists(ice_path))

ice <- read_parquet(ice_path) %>% as_tibble()
cat("Raw rows:", nrow(ice), "\n")
cat("Date range:", format(min(ice$apprehension_date, na.rm=TRUE)), "to",
    format(max(ice$apprehension_date, na.rm=TRUE)), "\n")

WIN_START <- as.Date("2025-01-01")
WIN_END   <- as.Date("2026-07-31")

# US states + DC only. The state field also contains Mexican states (border
# crossers processed under a US state's AOR but landmarked to origin), US
# territories, and "Armed Forces" / "Other" -- all dropped here because they
# have no matching Census population estimate or BLS SAE employment series.
us_states <- c(state.name, "District of Columbia")

ice_win <- ice %>%
  filter(!duplicate_drop_row,
         apprehension_date >= WIN_START, apprehension_date <= WIN_END) %>%
  mutate(state_name = str_to_title(apprehension_state_filled_in),
         state_name = recode(state_name, "District Of Columbia" = "District of Columbia"))

n_total <- nrow(ice_win)
n_matched <- sum(ice_win$state_name %in% us_states, na.rm = TRUE)
cat(sprintf("Window rows: %s | matched to a US state/DC: %s (%.1f%%)\n",
            format(n_total, big.mark=","), format(n_matched, big.mark=","),
            100 * n_matched / n_total))

by_state <- ice_win %>%
  filter(state_name %in% us_states) %>%
  count(state_name, name = "arrests") %>%
  arrange(desc(arrests))

cat("\n=== Top 10 states by raw arrest count, Jan 2025 - Jul 2026 ===\n")
print(head(by_state, 10))

write_csv(by_state, "data/ice_arrests_by_state.csv")
cat("\nWrote data/ice_arrests_by_state.csv (", nrow(by_state), "states/DC )\n")
cat("DONE 51_load_ice_state.R\n")
