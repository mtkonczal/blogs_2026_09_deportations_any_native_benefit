# ==============================================================================
# 65_education_monthly.R -> graphics/fig7_education.png (Hypothesis 4)
# Native prime-age unemployment rate by education, 3-month moving average,
# NSA, one line per year (2024, 2026) with month on the x-axis, in the
# same style as Figs 1-3 (R/60_bls_official_monthly.R).
# The 3-month average pools the current and two prior calendar months (sum of
# weighted unemployed / sum of weighted labor force), the way BLS smooths thin
# demographic cells. January uses the prior November-December, so 2023 is read.

# Replaces the annual version in R/21_charts_micro.R, which now writes to
# fig7_education_annual.png so the two never clobber each other.
#
# Education (IPUMS EDUC): < HS = 2-72; HS diploma/equivalent = 73;
# some college / associate = 80-110. BA+ (111+) not shown. This splits the
# "HS to some college" group used in 01_load_cps_micro.R into its two parts.
# Native = CITIZEN 1-3, prime age 25-54, WTFINL weights, as in 01.
# Monthly < HS cells are ~650 labor-force respondents (SE roughly 1 pp), which
# is why the chart smooths.
# ==============================================================================
suppressMessages({library(ipumsr); library(data.table); library(tidyverse); library(scales)})

ddi <- read_ipums_ddi(readLines("data_raw/ddi_path.txt")[1])
VARS <- c("YEAR","MONTH","WTFINL","AGE","CITIZEN","EMPSTAT","LABFORCE","EDUC")
acc <- list(); i <- 0
cb <- function(x, pos) {
  i <<- i + 1; setDT(x)
  x <- x[AGE >= 25 & AGE <= 54 & YEAR >= 2023 &
         CITIZEN %in% c(1, 2, 3) & LABFORCE == 2, ..VARS]
  for (j in names(x)) set(x, j = j, value = as.numeric(x[[j]]))
  acc[[i]] <<- x; NULL
}
read_ipums_micro_chunked(ddi, callback = IpumsSideEffectCallback$new(cb),
                         chunk_size = 2e6, verbose = FALSE)
d <- rbindlist(acc); rm(acc); invisible(gc())

d[, educ3 := fcase(EDUC > 1 & EDUC < 73, "Less than high school",
                   EDUC == 73,              "High school diploma",
                   EDUC > 73 & EDUC < 111,  "Some college or associate")]
d <- d[!is.na(educ3)]
d[, unemp := as.integer(EMPSTAT %in% c(20, 21, 22))]

m <- d[, .(un_w = sum(WTFINL*unemp), lf_w = sum(WTFINL), n_lf = .N),
       by = .(educ3, year = YEAR, month = MONTH)][order(educ3, year, month)]
m[, `:=`(unrate = un_w/lf_w, date = as.Date(sprintf("%d-%02d-01", year, month)))]
# pooled trailing 3-calendar-month rate
ma <- rbindlist(lapply(split(m, m$educ3), function(g) {
  g[, {
    rbindlist(lapply(date, function(t) {
      w <- g[date >= seq(t, by = "-2 month", length.out = 2)[2] & date <= t]
      data.table(date = t, unrate_3m = sum(w$un_w)/sum(w$lf_w), months_in_window = nrow(w))
    }))
  }][, educ3 := g$educ3[1]][]
}))
m <- merge(m, ma, by = c("educ3", "date"))
write_csv(m[, .(educ3, year, month, unrate, unrate_3m, months_in_window, n_lf)],
          "output/h5_native_unrate_educ3_monthly.csv")

ja <- m[month <= 8, .(unrate_avg_monthly = mean(unrate), n_lf_per_month = mean(n_lf)),
        by = .(educ3, year)]
cat("=== Jan-Aug average of monthly rates ===\n")
print(dcast(ja, educ3 ~ year, value.var = "unrate_avg_monthly"), digits = 3)
print(dcast(ja, educ3 ~ year, value.var = "n_lf_per_month"), digits = 4)

# ---- chart: shared blog style (R/00_blog_style.R) ---------------------------
source("R/00_blog_style.R")
pd <- as_tibble(m) %>% filter(year %in% c(2024, 2026)) %>%
  mutate(unrate = unrate_3m) %>%
  mutate(year = factor(year),
         educ3 = factor(educ3, levels = c("Less than high school", "High school diploma",
                                          "Some college or associate")))
lab <- pd %>% group_by(educ3, year) %>% filter(month == max(month)) %>% ungroup()

p <- ggplot(pd, aes(month, unrate, color = year, group = year)) +
  geom_line(linewidth = .9) + geom_point(size = 1.3) +
  ggrepel::geom_text_repel(data = lab, aes(label = percent(unrate, accuracy = 0.1)),
                           nudge_x = .7, direction = "y", hjust = 0, size = 2.7,
                           segment.color = NA, show.legend = FALSE, seed = 1) +
  facet_wrap(~educ3, nrow = 1, scales = "free_y") +
  scale_color_manual(values = BLOG_YEAR_COLORS) +
  scale_y_continuous(labels = function(x) paste0(sub("\\.?0+$", "", sprintf("%.2f", 100*x)), "%")) +
  scale_x_continuous(breaks = c(1, 4, 7, 10), labels = month.abb[c(1, 4, 7, 10)],
                     limits = c(1, 14)) +
  labs(title = hyp_title(4, "The Least-Educated Native Workers Did Not Gain"),
       subtitle = paste0("Native-born prime-age (25-54) unemployment rate by education, 3-month moving average,\n",
                         "not seasonally adjusted, 2024 vs. 2026. Each panel has its own scale."),
       x = NULL, y = "Unemployment rate",
       caption = paste0("Source: IPUMS CPS microdata, author's calculations. Each point pools the month and the two before it.\n",
                        "Mike Konczal.")) +
  blog_theme + theme(panel.spacing.x = unit(1.2, "lines"), axis.text.x = element_text(size = 8.5)) +
  coord_cartesian(clip = "off")
blog_save("graphics/fig7_education.png", p)
cat("DONE 65_education_monthly.R\n")
