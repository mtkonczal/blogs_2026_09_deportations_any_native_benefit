# ==============================================================================
# 99_measurement.R   H13: what the CPS can and cannot tell us.
#
# Following Jed Kolko ("No, Native-Born Employment Has Not Soared," Sept 2025;
# "Your Guide to Immigration and the Jobs Report") and Ahn, Kim and Santacreu
# (St. Louis Fed, Dec 2025). Three separate problems with CPS nativity LEVELS:
#
#   (1) Small sample. Foreign-born employment has a confidence interval of
#       roughly +/- 700k, wider than a typical year of net immigration.
#   (2) January population controls are not applied retroactively, so the level
#       series jumps each January. The Jan-2025 jump was unusually large because
#       Census changed its net-international-migration methodology.
#   (3) THE BINDING ONE: Census sets population controls by age, sex, and
#       race/ethnicity. It does NOT set them by nativity. Nativity comes from
#       the survey response. So when foreign-born respondents leave the sample,
#       for whatever reason, their weight is redistributed inside their
#       age-sex-race cell, much of it onto native-born respondents. Measured
#       native-born population and employment rise mechanically. The two series
#       are forced to sum to a predetermined total.
#
# Consequence (3) means our own chained/control-adjusted level series, which
# removes only problem (2), is still not a clean estimate of departures. We
# report it only to show the size of problem (2), and we do NOT use levels to
# measure the size of the labor supply shock anywhere in this post.
#
# What IS robust: rates. Weight redistribution inside an age-sex-race cell
# scales up employed and unemployed natives roughly proportionally, so it
# leaves the native unemployment rate largely intact. Every headline result in
# this post is a rate.
#
# Original diagnostic added here: UNWEIGHTED respondent counts. Raw sample
# counts carry no weights at all, so they are immune to the reweighting
# artifact and speak directly to the survey-participation channel.
# ==============================================================================
suppressMessages({library(tidyverse); library(lubridate); library(data.table)})
d <- read_csv("data/nativity_published.csv", show_col_types = FALSE)

# --- (2) size of the January population-control artifact ---------------------
jumps <- d %>% arrange(date) %>%
  mutate(d_for = pop_for - lag(pop_for), d_nat = pop_nat - lag(pop_nat)) %>%
  filter(month(date) == 1, year(date) >= 2019) %>% select(date, d_for, d_nat)
cat("=== January population-control revisions, thousands ===\n")
cat("(not behavior: the CPS does not revise prior months)\n")
print(as.data.frame(jumps %>% mutate(across(-date, round))))
write_csv(jumps, "output/h13_january_control_revisions.csv")

chain <- function(df, col) {
  df %>% arrange(date) %>%
    mutate(chg = .data[[col]] - lag(.data[[col]]),
           chg = if_else(month(date) == 1, 0, chg), chg = replace_na(chg, 0)) %>%
    filter(date >= as.Date("2019-01-01")) %>%
    mutate(!!paste0(col,"_chained") := .data[[col]][1] + cumsum(chg)) %>%
    select(date, ends_with("_chained"))
}
ch <- reduce(map(c("pop_for","emp_for","pop_nat","emp_nat"), ~chain(d,.x)),
             left_join, by = "date")
out <- d %>% select(date, pop_for, emp_for, pop_nat, emp_nat) %>% right_join(ch, by="date")
d0 <- out %>% filter(date == as.Date("2024-12-01"))
d1 <- out %>% filter(date == max(out$date))
cat("\n=== Foreign-born EMPLOYMENT change, Dec-2024 to", format(d1$date), "===\n")
cat("  as published                     :", round(d1$emp_for - d0$emp_for), "k\n")
cat("  removing Jan control steps only  :", round(d1$emp_for_chained - d0$emp_for_chained), "k\n")
cat("  size of the Jan control steps    :", round(sum(jumps$d_for[year(jumps$date)>=2025])), "k\n")
cat("  NEITHER number is a credible estimate of how many workers left.\n")
cat("\n=== Native-born EMPLOYMENT change, same window ===\n")
cat("  as published                     :", round(d1$emp_nat - d0$emp_nat), "k\n")
cat("  removing Jan control steps only  :", round(d1$emp_nat_chained - d0$emp_nat_chained), "k\n")
write_csv(out, "output/h13_control_adjusted_levels.csv")

# --- (3) the participation channel: UNWEIGHTED respondent counts -------------
cps <- readRDS("data_raw/cps_analysis.rds"); setDT(cps)
raw <- cps[, .(n_noncit = sum(noncitizen, na.rm=TRUE),
               n_natzd  = sum(naturalized, na.rm=TRUE),
               n_native = sum(native, na.rm=TRUE),
               n_total  = .N), by = date][order(date)]
raw[, `:=`(sh_noncit = n_noncit/n_total, sh_natzd = n_natzd/n_total)]
write_csv(raw, "output/h13_unweighted_counts.csv")

base <- raw[date >= as.Date("2024-01-01") & date <= as.Date("2024-12-01"),
            .(b_noncit = mean(n_noncit), b_natzd = mean(n_natzd),
              b_native = mean(n_native), b_total = mean(n_total))]
recent <- raw[date >= as.Date("2025-09-01"), .(r_noncit = mean(n_noncit),
              r_natzd = mean(n_natzd), r_native = mean(n_native), r_total = mean(n_total))]
cat("\n=== UNWEIGHTED CPS respondents, 2024 average vs 2025m9-2026m8 average ===\n")
cat("These are raw interview counts. No weights, so no reweighting artifact.\n")
cmp <- data.frame(
  group = c("Non-citizens","Naturalized citizens","Native-born","All respondents 16+"),
  avg_2024 = round(c(base$b_noncit, base$b_natzd, base$b_native, base$b_total)),
  avg_recent = round(c(recent$r_noncit, recent$r_natzd, recent$r_native, recent$r_total)))
cmp$pct_change <- round(100*(cmp$avg_recent/cmp$avg_2024 - 1), 1)
print(cmp)
write_csv(cmp, "output/h13_unweighted_change.csv")
cat("\nIf non-citizen respondent counts fell far more than the overall sample,\n")
cat("part of the measured foreign-born decline is people declining to be\n")
cat("surveyed, not people leaving the country. St. Louis Fed (Dec 2025) puts\n")
cat("most of the CPS decline in this category.\n")

# by month-in-sample: a first-interview drop is a contact/participation signal
mish <- cps[, .(n_noncit = sum(noncitizen, na.rm=TRUE), n = .N),
            by = .(MISH, yr = year(date))][order(MISH, yr)]
mish[, sh := n_noncit/n]
cat("\n=== non-citizen share of respondents by month-in-sample ===\n")
print(dcast(mish[yr >= 2023], MISH ~ yr, value.var = "sh"))
write_csv(mish, "output/h13_mish.csv")
cat("\nDONE 99_measurement.R\n")
