# ==============================================================================
# 57e_state_wage_trend_break.R -> output/state_wage_trend_break.csv
# Same fix as R/57d, applied to the one wage outcome that cleared p<0.05 in
# the 23-outcome scan (R/56): total private average hourly earnings, YoY.
# The scan's version is a LEVEL (this year's YoY wage growth) correlated with
# enforcement intensity, which has the same problem as the job-growth level
# comparison in R/52 -- high-enforcement states could just have had faster
# wage growth already. Fix: difference this year's YoY growth against last
# year's YoY growth (both NSA, same calendar month, so no seasonality
# contamination) and correlate that CHANGE with enforcement intensity.
# ==============================================================================
suppressMessages({library(dplyr); library(readr); library(lubridate); library(tidyr)})

sae <- readRDS("data_raw/sae_raw.rds")

latest_month <- sae %>%
  filter(data_type_text == "Average Hourly Earnings of All Employees, In Dollars",
         industry_name == "Total Private", area_code == "00000") %>%
  summarise(m = max(date)) %>% pull(m)
y1 <- latest_month - years(1)   # start of "post" YoY window
y2 <- latest_month - years(2)   # start of "pre" YoY window
cat("Post YoY window:", format(y1), "-", format(latest_month), "\n")
cat("Pre YoY window: ", format(y2), "-", format(y1), "\n")

w <- sae %>%
  filter(data_type_text == "Average Hourly Earnings of All Employees, In Dollars",
         industry_name == "Total Private", area_code == "00000",
         state_name %in% state.name, date %in% c(y2, y1, latest_month)) %>%
  select(state_name, date, value) %>%
  pivot_wider(names_from = date, values_from = value)
names(w) <- c("state_name", "w_y2", "w_y1", "w_latest")

w <- w %>% mutate(post_yoy = w_latest / w_y1 - 1,
                   pre_yoy  = w_y1 / w_y2 - 1,
                   diff     = post_yoy - pre_yoy)

ice <- read_csv("output/state_enforcement_vs_jobs.csv", show_col_types = FALSE) %>%
  select(state_name, arrests_per_100k)

m <- ice %>% inner_join(w, by = "state_name")
cat("Merged N =", nrow(m), "states\n")

fit <- function(df, y, lbl) {
  ct <- cor.test(df$arrests_per_100k, df[[y]])
  tibble(spec = lbl, n = nrow(df), r = unname(ct$estimate),
         ci_lo = ct$conf.int[1], ci_hi = ct$conf.int[2], p_value = ct$p.value)
}
res <- bind_rows(
  fit(m, "post_yoy", "Level (post-period YoY only, matches R/56 scan)"),
  fit(m, "diff",     "Trend-differenced (post YoY minus pre YoY)"),
  fit(filter(m, state_name != "Texas"), "diff", "Trend-differenced, excl. Texas")
)
cat("\n=== Total private wage growth vs. ICE arrests per 100k ===\n")
print(res %>% mutate(across(c(r, ci_lo, ci_hi, p_value), ~round(.x, 4))))
write_csv(res, "output/state_wage_trend_break.csv")
cat("Wrote output/state_wage_trend_break.csv\n")
cat("DONE 57e_state_wage_trend_break.R\n")
