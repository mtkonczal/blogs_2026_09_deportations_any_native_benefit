# ==============================================================================
# 66_nominal_wage_percentiles.R   Hypothesis 5, nominal version
#
# Did nominal wage growth for native-born workers at the bottom of the
# distribution pick up or slow? Real growth in 2026 is dominated by the
# inflation shock, so this looks at the nominal side first.
#
# Wage definition identical to 05_wages.R: CPS outgoing rotation groups
# (MISH 4/8), EARNWEEK2/HOURWAGE2 supplementary extract, hourly rate for hourly
# workers else weekly earnings / usual hours, $2-$500, EARNWT weights, imputed
# earnings dropped (robustness keeps them). Native = CITIZEN 1-3, age 16+,
# employed wage and salary workers (the only ones with ORG earnings).
# January-August of each year, since 2026 ends in August.
#
# Heaping: ~70% of hourly rates are whole dollars, so a raw weighted quantile
# sits on a round number for years at a time (native p10 was exactly $15.00 in
# both 2025 and 2026). Each percentile is therefore the average of weighted
# quantiles over a band of +/-2 percentiles in 0.5-point steps (p10 = mean of
# p8, p8.5, ..., p12). The raw quantile is kept for comparison.
# ==============================================================================
suppressMessages({library(ipumsr); library(data.table); library(tidyverse); library(scales)})

# --- main extract: nativity, employment, imputation flags, usual hours -------
ddi <- read_ipums_ddi(readLines("data_raw/ddi_path.txt")[1])
VARS <- c("YEAR","MONTH","CPSIDP","MISH","AGE","CITIZEN","EMPSTAT","UHRSWORKT",
          "QEARNWEE","QHOURWAG")
acc <- list(); i <- 0
cb <- function(x, pos) {
  i <<- i + 1; setDT(x)
  x <- x[YEAR >= 2018 & MONTH <= 8 & MISH %in% c(4, 8) & AGE >= 16 &
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

# --- earnings extract, same cleaning as 05_wages.R ---------------------------
e <- readRDS("data_raw/cps_earn.rds"); setDT(e)
e <- e[MISH %in% c(4, 8) & MONTH <= 8]
e[, `:=`(ew = fifelse(EARNWEEK2 > 0 & EARNWEEK2 < 999999, EARNWEEK2, NA_real_),
         hw = fifelse(HOURWAGE2 > 0 & HOURWAGE2 < 999, HOURWAGE2, NA_real_),
         uh = fifelse(UHRSWORKORG > 0 & UHRSWORKORG < 997, UHRSWORKORG, NA_real_))]
e <- e[!is.na(ew) | !is.na(hw)]

m <- merge(cps[, .(CPSIDP, date, uhrs, earn_imputed)],
           e[, .(CPSIDP, date, ew, hw, uh, EARNWT)], by = c("CPSIDP", "date"))
m[, hours := fifelse(!is.na(uh), uh, uhrs)]
m[, wage := fifelse(!is.na(hw), hw, fifelse(hours >= 1, ew / hours, NA_real_))]
m <- m[!is.na(wage) & wage >= 2 & wage < 500 & EARNWT > 0]
m[, yr := year(date)]
cat("native ORG wage records, Jan-Aug 2018+:", format(nrow(m), big.mark = ","),
    "| share imputed:", round(mean(m$earn_imputed), 3), "\n")

wq <- function(x, w, p) { o <- order(x); x <- x[o]; w <- w[o]
  x[which(cumsum(w)/sum(w) >= p)[1]] }
band <- function(x, w, p) mean(sapply(seq(p - .02, p + .02, by = .005), function(q) wq(x, w, q)))
PCTS <- c(.10, .20, .40, .50)

pct_table <- function(dt, lbl) {
  rbindlist(lapply(PCTS, function(p)
    dt[, .(pctile = paste0("p", 100*p), wage_smooth = band(wage, EARNWT, p),
           wage_raw = wq(wage, EARNWT, p), n = .N), by = yr]))[, sample := lbl][]
}
res <- rbind(pct_table(m[earn_imputed == 0], "Imputed dropped (headline)"),
             pct_table(m, "Imputed kept"))
setorder(res, sample, pctile, yr)
res[, `:=`(g_smooth = wage_smooth/shift(wage_smooth) - 1,
           g_raw    = wage_raw/shift(wage_raw) - 1), by = .(sample, pctile)]
write_csv(res, "output/h5_native_nominal_wage_percentiles_jan_aug.csv")

cat("\n=== Native-born NOMINAL hourly wage, smoothed percentile, Jan-Aug, imputed dropped ===\n")
print(dcast(res[sample == "Imputed dropped (headline)" & yr >= 2019], pctile ~ yr,
            value.var = "wage_smooth"), digits = 4)
cat("\n=== Nominal growth, % YoY (smoothed), imputed dropped ===\n")
print(dcast(res[sample == "Imputed dropped (headline)" & yr >= 2020], pctile ~ yr,
            value.var = "g_smooth", fun.aggregate = function(x) round(100*x, 2)))
cat("\n=== Same, RAW quantile (heaping visible) ===\n")
print(dcast(res[sample == "Imputed dropped (headline)" & yr >= 2020], pctile ~ yr,
            value.var = "g_raw", fun.aggregate = function(x) round(100*x, 2)))
cat("\n=== Robustness: imputed kept, smoothed growth ===\n")
print(dcast(res[sample == "Imputed kept" & yr >= 2020], pctile ~ yr,
            value.var = "g_smooth", fun.aggregate = function(x) round(100*x, 2)))

# ---- chart: nominal growth by percentile, one line per percentile ----------
BG <- "#faf6ec"; NAVY <- "#1c2340"
pd <- as_tibble(res) %>% filter(sample == "Imputed dropped (headline)", yr >= 2020) %>%
  mutate(pctile = factor(pctile, levels = c("p10","p20","p40","p50"),
                         labels = c("10th percentile","20th percentile","40th percentile","Median")))
lab <- pd %>% filter(yr == max(yr))
p <- ggplot(pd, aes(yr, g_smooth, color = pctile)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = .3) +
  geom_line(linewidth = 1.1) + geom_point(size = 2) +
  ggrepel::geom_text_repel(data = lab, aes(label = paste0(pctile, ": ", percent(g_smooth, accuracy = 0.1))),
                           nudge_x = .25, direction = "y", hjust = 0, size = 3.4,
                           segment.color = NA, show.legend = FALSE, seed = 1) +
  scale_color_brewer(palette = "Dark2", guide = "none") +
  scale_x_continuous(breaks = 2020:2026, limits = c(2020, 2027.4)) +
  scale_y_continuous(labels = function(x) paste0(sub("\\.?0+$", "", sprintf("%.1f", 100*x)), "%")) +
  labs(title = "Nominal Wage Growth at the Bottom Slowed",
       subtitle = paste0("Native-born hourly wages, year-over-year growth at selected percentiles, ",
                         "January-August of each year, nominal"),
       caption = paste0("Author's calculation from IPUMS CPS outgoing rotation groups (EARNWT-weighted), imputed earnings excluded.\n",
                        "Each percentile averages the +/-2 percentile band to offset heaping at round-dollar wages. Mike Konczal, ESP.")) +
  theme_minimal(base_size = 13) +
  theme(plot.background = element_rect(fill = BG, color = NA),
        panel.background = element_rect(fill = BG, color = NA),
        panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey82", linewidth = .35),
        plot.title = element_text(face = "bold", color = NAVY, size = 16),
        plot.title.position = "plot",
        plot.subtitle = element_text(color = NAVY, size = 11),
        plot.caption = element_text(color = "grey45", size = 8.5),
        axis.title = element_blank(), axis.text = element_text(color = NAVY, size = 10)) +
  coord_cartesian(clip = "off")
ggsave("graphics/fig14b_nominal_wage_percentiles.png", p, width = 8, height = 5, dpi = 200, bg = BG)
cat("DONE 66_nominal_wage_percentiles.R\n")
