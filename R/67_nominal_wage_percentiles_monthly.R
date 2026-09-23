# ==============================================================================
# 67_nominal_wage_percentiles_monthly.R   Hypothesis 5, monthly nominal version
#
# Monthly version of 66_nominal_wage_percentiles.R. For each month t from
# January 2023 on, pool native-born ORG wage records over a trailing window
# (6 months headline, 12 months robustness), compute band-smoothed weighted
# percentiles (p10, p20, p40, p50), and compare with the same window ending
# 12 months earlier. Year-over-year on matched calendar windows, so no seasonal
# adjustment is needed.
#
# Why 6 months: the question is timing (did growth turn as enforcement ramped
# up in 2025?). A 12-month window lags turning points by ~6 months; a 6-month
# window by ~3, with ~30k records per window.
# October 2025 has no survey, so windows spanning it pool one fewer month.
# Wage definition, sample, weights and smoothing identical to 66 (and 05).
# ==============================================================================
suppressMessages({library(ipumsr); library(data.table); library(tidyverse); library(scales)})

ddi <- read_ipums_ddi(readLines("data_raw/ddi_path.txt")[1])
VARS <- c("YEAR","MONTH","CPSIDP","MISH","AGE","CITIZEN","EMPSTAT","UHRSWORKT",
          "QEARNWEE","QHOURWAG")
acc <- list(); i <- 0
cb <- function(x, pos) {
  i <<- i + 1; setDT(x)
  x <- x[YEAR >= 2020 & MISH %in% c(4, 8) & AGE >= 16 &
         CITIZEN %in% c(1, 2, 3) & EMPSTAT %in% c(10, 12), ..VARS]
  for (j in names(x)) set(x, j = j, value = as.numeric(x[[j]]))
  acc[[i]] <<- x; NULL
}
read_ipums_micro_chunked(ddi, callback = IpumsSideEffectCallback$new(cb),
                         chunk_size = 2e6, verbose = FALSE)
cps <- rbindlist(acc); rm(acc); invisible(gc())
cps[, `:=`(date = as.Date(sprintf("%d-%02d-01", YEAR, MONTH)),
           uhrs = fifelse(UHRSWORKT < 997, UHRSWORKT, NA_real_),
           earn_imputed = as.integer((!is.na(QEARNWEE) & QEARNWEE > 0) |
                                     (!is.na(QHOURWAG) & QHOURWAG > 0)))]

e <- readRDS("data_raw/cps_earn.rds"); setDT(e)
e <- e[MISH %in% c(4, 8) & YEAR >= 2020]
e[, `:=`(ew = fifelse(EARNWEEK2 > 0 & EARNWEEK2 < 999999, EARNWEEK2, NA_real_),
         hw = fifelse(HOURWAGE2 > 0 & HOURWAGE2 < 999, HOURWAGE2, NA_real_),
         uh = fifelse(UHRSWORKORG > 0 & UHRSWORKORG < 997, UHRSWORKORG, NA_real_))]
e <- e[!is.na(ew) | !is.na(hw)]
m <- merge(cps[earn_imputed == 0, .(CPSIDP, date, uhrs)],
           e[, .(CPSIDP, date, ew, hw, uh, EARNWT)], by = c("CPSIDP", "date"))
m[, hours := fifelse(!is.na(uh), uh, uhrs)]
m[, wage := fifelse(!is.na(hw), hw, fifelse(hours >= 1, ew / hours, NA_real_))]
m <- m[!is.na(wage) & wage >= 2 & wage < 500 & EARNWT > 0]
cat("native ORG wage records (imputed dropped), 2020+:", format(nrow(m), big.mark = ","), "\n")

PCTS <- c(.10, .20, .40, .50)
# band-smoothed weighted percentiles, one sort per window
band_pcts <- function(x, w) {
  o <- order(x); x <- x[o]; cw <- cumsum(w[o]) / sum(w)
  sapply(PCTS, function(p) mean(sapply(seq(p - .02, p + .02, by = .005),
                                       function(q) x[which(cw >= q)[1]])))
}
ends <- seq(as.Date("2021-06-01"), max(m$date), by = "month")
roll <- function(W) {
  rbindlist(lapply(ends, function(t) {
    lo <- seq(t, by = "-1 month", length.out = W)[W]
    w <- m[date >= lo & date <= t]
    data.table(date = t, window = W, pctile = paste0("p", 100*PCTS),
               wage = band_pcts(w$wage, w$EARNWT), n = nrow(w),
               months = uniqueN(w$date))
  }))
}
r <- rbind(roll(6), roll(12))
setorder(r, window, pctile, date)
r[, wage_lag12 := r[.(window, pctile, date %m-% months(12)), on = .(window, pctile, date), x.wage]]
r[, yoy := wage / wage_lag12 - 1]
r <- r[date >= as.Date("2023-01-01")]
write_csv(r, "output/h5_native_nominal_wage_percentiles_monthly.csv")

cat("\n=== YoY nominal growth, 6-month window, selected months ===\n")
sel <- as.Date(c("2023-06-01","2023-12-01","2024-06-01","2024-12-01","2025-06-01",
                 "2025-12-01","2026-03-01","2026-06-01","2026-08-01"))
print(dcast(r[window == 6 & date %in% sel], pctile ~ date, value.var = "yoy",
            fun.aggregate = function(x) round(100*x, 1)))
cat("\n=== Same, 12-month window ===\n")
print(dcast(r[window == 12 & date %in% sel], pctile ~ date, value.var = "yoy",
            fun.aggregate = function(x) round(100*x, 1)))
cat("\nrecords per 6-month window, range:", range(r[window == 6]$n), "\n")

# ---- chart: 6-month window (shared blog style) -------------------------------
source("R/00_blog_style.R")
PCT_COLORS <- c("10th percentile" = BLOG_RED, "20th percentile" = BLOG_GOLD,
                "40th percentile" = BLOG_GREEN, "Median" = BLOG_NAVY)
pd <- as_tibble(r) %>% filter(window == 6) %>%
  mutate(pctile = factor(pctile, levels = c("p10","p20","p40","p50"),
                         labels = names(PCT_COLORS)))
# end labels placed directly, spaced at least 0.45 pp apart so none overlap
lab <- pd %>% filter(date == max(date)) %>% arrange(yoy) %>% mutate(y_lab = yoy)
for (k in 2:nrow(lab)) lab$y_lab[k] <- max(lab$y_lab[k], lab$y_lab[k - 1] + 0.0045)
p <- ggplot(pd, aes(date, yoy, color = pctile)) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = .3) +
  geom_vline(xintercept = as.Date("2025-01-20"), linetype = "dashed", color = BLOG_GREY) +
  geom_line(linewidth = .9) +
  geom_text(data = lab, aes(x = date + 25, y = y_lab,
                            label = paste0(pctile, ": ", percent(yoy, accuracy = 0.1))),
            hjust = 0, size = 2.9, show.legend = FALSE) +
  scale_color_manual(values = PCT_COLORS, guide = "none") +
  scale_x_date(breaks = seq(as.Date("2023-01-01"), as.Date("2026-07-01"), by = "6 months"),
               date_labels = "%b\n%Y", limits = c(as.Date("2023-01-01"), as.Date("2027-06-01"))) +
  scale_y_continuous(labels = function(x) paste0(sub("\\.?0+$", "", sprintf("%.1f", 100*x)), "%")) +
  labs(title = hyp_title(6, "Nominal Wage Growth at the Bottom Slowed"),
       subtitle = paste0("Native-born hourly wages, year-over-year nominal growth at selected percentiles, trailing 6-month\n",
                         "window vs. the same window a year earlier. Dashed line: January 20, 2025."),
       x = NULL, y = "Nominal wage growth, year over year",
       caption = paste0("Source: IPUMS CPS outgoing rotation groups, author's calculations; imputed earnings excluded. Each percentile\n",
                        "averages the +/-2 percentile band to offset heaping at round-dollar wages. Mike Konczal.")) +
  blog_theme + coord_cartesian(clip = "off")
blog_save("graphics/fig14c_nominal_wage_percentiles_monthly.png", p)
cat("DONE 67_nominal_wage_percentiles_monthly.R\n")
