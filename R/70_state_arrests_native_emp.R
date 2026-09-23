# ==============================================================================
# 70_state_arrests_native_emp.R -> graphics/fig10_arrests_native_emp.png
# Hypothesis 10, modeled on Steven Rattner's chart: by state, ICE arrests per
# 100,000 residents (Jan 2025-Jul 2026) vs. native-born employment growth,
# 3-month averages, Jan-Mar 2025 vs. May-Jul 2026, from CPS microdata.
#
# Rattner's y-axis is growth in the native-born employment LEVEL. Levels move
# with native population growth and with CPS reweighting (the January 2026
# population-control reset), so a rate version is computed alongside: the
# change in the native prime-age (25-54) employment-population ratio, same
# windows. If the two agree, the level chart is safe to publish.
#
# Headline is TREND-DIFFERENCED, mirroring the payroll trend-break test
# (57d): post-period growth (Jan-Mar 2025 -> May-Jul 2026) minus the same-shape
# pre-period growth (Jan-Mar 2023 -> May-Jul 2024). Same calendar months in
# both, so the NSA seasonal pattern cancels; each window crosses one January
# population-control reset; the windows do not overlap. This nets out states
# that were already growing fast (TX, FL, GA) before enforcement began. The
# Rattner-style level correlation is still reported for comparison.
# Arrests and population: output/state_enforcement_vs_jobs.csv (51, 52).
# DC excluded, as in 56. Native = CITIZEN 1-3, employed = EMPSTAT 10/12, WTFINL.
# ==============================================================================
suppressMessages({library(ipumsr); library(data.table); library(tidyverse); library(tigris)})

ddi <- read_ipums_ddi(readLines("data_raw/ddi_path.txt")[1])
VARS <- c("YEAR","MONTH","WTFINL","AGE","CITIZEN","EMPSTAT","STATEFIP")
acc <- list(); i <- 0
cb <- function(x, pos) {
  i <<- i + 1; setDT(x)
  x <- x[AGE >= 16 & YEAR >= 2023 & CITIZEN %in% c(1, 2, 3), ..VARS]
  for (j in names(x)) set(x, j = j, value = as.numeric(x[[j]]))
  acc[[i]] <<- x; NULL
}
read_ipums_micro_chunked(ddi, callback = IpumsSideEffectCallback$new(cb),
                         chunk_size = 2e6, verbose = FALSE)
d <- rbindlist(acc); rm(acc); invisible(gc())
d[, `:=`(ym = YEAR*100 + MONTH, emp = as.integer(EMPSTAT %in% c(10, 12)),
         prime = as.integer(AGE >= 25 & AGE <= 54))]
data(fips_codes)
xw <- unique(data.table(STATEFIP = as.integer(fips_codes$state_code), state_name = fips_codes$state_name))
d <- merge(d, xw, by = "STATEFIP")[state_name %in% state.name]

win <- function(months) d[ym %in% months, .(
  nat_emp_k   = sum(WTFINL*emp) / 1000 / uniqueN(ym),
  nat_prime_epop = sum(WTFINL*emp*prime) / sum(WTFINL*prime),
  n = .N), by = state_name]
growth <- function(a, b, sfx) {
  x <- merge(win(a), win(b), by = "state_name", suffixes = c("_a", "_b"))
  x[, .(state_name, g = nat_emp_k_b / nat_emp_k_a - 1,
        de = nat_prime_epop_b - nat_prime_epop_a, n = n_a)][
    , setnames(.SD, c("g", "de", "n"), paste0(c("nat_emp_growth", "d_nat_prime_epop", "n"), sfx))]
}
post <- growth(c(202501, 202502, 202503), c(202605, 202606, 202607), "")
pre  <- growth(c(202301, 202302, 202303), c(202405, 202406, 202407), "_prior")
m <- merge(post, pre, by = "state_name")
setnames(m, "n", "n_pre")
m[, `:=`(diff_growth = nat_emp_growth - nat_emp_growth_prior,
         diff_epop   = d_nat_prime_epop - d_nat_prime_epop_prior)]
ice <- as.data.table(read_csv("output/state_enforcement_vs_jobs.csv", show_col_types = FALSE))[
  , .(state_name, arrests, population_2025, arrests_per_100k)]
m <- merge(m, ice, by = "state_name")
m[, abb := state.abb[match(state_name, state.name)]]
write_csv(m, "output/h10_state_arrests_native_emp.csv")

ct_lvl  <- cor.test(m$arrests_per_100k, m$nat_emp_growth)
ct_rate <- cor.test(m$arrests_per_100k, m$d_nat_prime_epop)
ct_lvl_tx  <- with(m[abb != "TX"], cor.test(arrests_per_100k, nat_emp_growth))
ct_rate_tx <- with(m[abb != "TX"], cor.test(arrests_per_100k, d_nat_prime_epop))
ct_diff    <- cor.test(m$arrests_per_100k, m$diff_growth)
ct_diff_tx <- with(m[abb != "TX"], cor.test(arrests_per_100k, diff_growth))
ct_diffe   <- cor.test(m$arrests_per_100k, m$diff_epop)
ct_prior   <- cor.test(m$arrests_per_100k, m$nat_emp_growth_prior)
cat("states:", nrow(m), "\n")
cat(sprintf("Level (Rattner): r = %+.3f, p = %.2f | excl. TX r = %+.3f, p = %.2f\n",
            ct_lvl$estimate, ct_lvl$p.value, ct_lvl_tx$estimate, ct_lvl_tx$p.value))
cat(sprintf("Rate (prime EPOP): r = %+.3f, p = %.2f | excl. TX r = %+.3f, p = %.2f\n",
            ct_rate$estimate, ct_rate$p.value, ct_rate_tx$estimate, ct_rate_tx$p.value))
cat(sprintf("Prior growth (2023->24) vs arrests: r = %+.3f, p = %.2f  [pre-existing trend?]\n",
            ct_prior$estimate, ct_prior$p.value))
cat(sprintf("TREND-DIFFERENCED growth: r = %+.3f, p = %.2f | excl. TX r = %+.3f, p = %.2f\n",
            ct_diff$estimate, ct_diff$p.value, ct_diff_tx$estimate, ct_diff_tx$p.value))
cat(sprintf("Trend-differenced prime EPOP change: r = %+.3f, p = %.2f\n", ct_diffe$estimate, ct_diffe$p.value))
cat("unweighted native obs per state-window, min / median:", min(m$n_pre), "/", median(m$n_pre), "\n")
print(m[order(-arrests_per_100k)][1:5, .(state_name, arrests_per_100k = round(arrests_per_100k),
                                         growth = round(100*nat_emp_growth, 1),
                                         prior = round(100*nat_emp_growth_prior, 1),
                                         diff = round(100*diff_growth, 1))])

# ---- chart ---------------------------------------------------------------------
NAVY <- "#1c2e4a"; RED <- "#c0392b"; GREY <- "#555555"
lab_states <- unique(c(m[order(-arrests_per_100k)][1:3, abb],
                       m[order(-diff_growth)][1:1, abb], m[order(diff_growth)][1:2, abb],
                       "CA", "NY"))
m[, lbl := fifelse(abb %in% lab_states, abb, NA_character_)]
fit <- lm(diff_growth ~ arrests_per_100k, data = m)
xr <- range(m$arrests_per_100k)
ann_y <- predict(fit, newdata = data.frame(arrests_per_100k = xr[2])) - 0.03

p <- ggplot(m, aes(arrests_per_100k, diff_growth)) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = .4) +
  geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], color = RED,
              linewidth = 1.1, linetype = "dashed") +
  geom_point(color = NAVY, size = 3, alpha = .8) +
  ggrepel::geom_text_repel(aes(label = lbl), size = 3.6, color = GREY, na.rm = TRUE,
                           min.segment.length = Inf, point.padding = .3, seed = 1) +
  annotate("text", x = xr[2], y = ann_y, hjust = 1, color = RED, fontface = "bold.italic", size = 4.2,
           label = sprintf("trend: flat\n(correlation %.2f)", ct_diff$estimate)) +
  scale_y_continuous(labels = function(x) ifelse(abs(x) < 1e-9, "0", sprintf("%+.0f pp", 100*x))) +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, .03))) +
  labs(title = "States With More ICE Arrests Didn't See Native-Born Job Growth Speed Up",
       subtitle = paste0("By state: ICE arrests per 100,000 residents vs. change in native-born employment growth,\n",
                         "Jan.-Mar. 2025 to May-July 2026, minus the same months two years earlier (2023 to 2024), pp"),
       x = "ICE arrests per 100,000 residents, January 2025-July 2026", y = NULL,
       caption = paste0("Source: Deportation Data Project (ICE arrests); Census Bureau population estimates; IPUMS CPS microdata ",
                        "(native-born employment, 3-month averages).\n",
                        sprintf("Correlation: %.2f; without the trend adjustment, %.2f. ", ct_diff$estimate, ct_lvl$estimate),
                        "Chart modeled on one by Steven Rattner. Mike Konczal, ESP.")) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey88", linewidth = .35),
        plot.title = element_text(face = "bold", color = NAVY, size = 17),
        plot.title.position = "plot",
        plot.subtitle = element_text(color = GREY, face = "italic", size = 11.5),
        plot.caption = element_text(color = GREY, face = "italic", size = 8.5, hjust = 0),
        axis.line = element_line(color = "grey40", linewidth = .4),
        axis.title.x = element_text(color = GREY, size = 11),
        axis.text = element_text(color = GREY, size = 11)) +
  coord_cartesian(clip = "off")
ggsave("graphics/fig10_arrests_native_emp.png", p, width = 9, height = 5.4, dpi = 200, bg = "white")
cat("DONE 70_state_arrests_native_emp.R\n")
