# ==============================================================================
# 52_state_enforcement_jobs.R
# First-pass state cross-section: does ICE enforcement intensity line up with
# state-level PAYROLL job growth (BLS SAE, total nonfarm, SA) over the same
# window, Jan 2025 - Jul 2026?
#
# This deliberately uses the establishment survey (SAE), not CPS microdata:
# SAE is a near-census of payrolls, so state cells are not thin the way CPS
# state cells are (see the caveat in R/08_geography.R). The tradeoff is that
# SAE cannot distinguish native vs. foreign-born workers -- it is a pure
# aggregate labor-demand check, not a substitution/complementarity test.
#
# Denominator for the enforcement rate: Census Vintage 2025 state population
# estimate (POPESTIMATE2025, as of 2025-07-01), the midpoint of the window.
# ==============================================================================
suppressMessages({library(dplyr); library(readr); library(tidyr); library(stringr)
  library(ggplot2); library(scales)})

# House style, lifted from R/47_charts_batch2.R so this figure matches the
# rest of the series exactly.
NAVY <- "#2c3254"; RED <- "#ff8361"; GREY <- "#8d8b7f"
my_style <- list(
  theme_minimal(base_size = 12),
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey88", linewidth = .3),
        plot.title = element_text(face = "bold", color = NAVY, size = 13),
        plot.title.position = "plot",
        plot.subtitle = element_text(color = NAVY, size = 9.5),
        plot.caption = element_text(color = "grey40", size = 8),
        legend.position = "none",
        axis.title = element_text(size = 9)),
  coord_cartesian(clip = "off"))
W <- 8.6; H <- 5.0; DPI <- 200

WIN_START <- as.Date("2025-01-01")
WIN_END   <- as.Date("2026-07-01")   # last full month common to SAE + ICE window

# ---- 1. ICE arrests by state (from R/51_load_ice_state.R) -------------------
ice <- read_csv("data/ice_arrests_by_state.csv", show_col_types = FALSE)

# ---- 2. Census population, Vintage 2025, state-level (SUMLEV 040) ----------
pop <- read_csv("data_raw/census/NST-EST2025-ALLDATA.csv", show_col_types = FALSE) %>%
  filter(SUMLEV == "040", NAME != "Puerto Rico") %>%
  transmute(state_name = NAME, population_2025 = POPESTIMATE2025)

stopifnot(nrow(pop) == 51)  # 50 states + DC

# ---- 3. BLS SAE, total nonfarm, seasonally adjusted, statewide -------------
sae <- readRDS("data_raw/sae_raw.rds")

nf <- sae %>%
  filter(industry_name == "Total Nonfarm",
         data_type_text == "All Employees, In Thousands",
         seasonal_text == "Seasonally Adjusted",
         area_code == "00000",
         date %in% c(WIN_START, WIN_END)) %>%
  select(state_name, date, value) %>%
  pivot_wider(names_from = date, values_from = value, names_prefix = "emp_")

names(nf) <- gsub("emp_2025-01-01", "emp_start", names(nf), fixed = TRUE)
names(nf) <- gsub("emp_2026-07-01", "emp_end",   names(nf), fixed = TRUE)

nf <- nf %>%
  filter(!is.na(emp_start), !is.na(emp_end)) %>%
  mutate(job_growth = emp_end / emp_start - 1)

cat("States with complete SAE total-nonfarm series for the window:", nrow(nf), "\n")

# ---- 4. Merge and compute the enforcement rate ------------------------------
m <- ice %>%
  inner_join(pop, by = "state_name") %>%
  inner_join(nf %>% select(state_name, emp_start, emp_end, job_growth), by = "state_name") %>%
  mutate(arrests_per_100k = arrests / population_2025 * 1e5)

n_dropped <- nrow(ice) - nrow(m)
cat("Merged N =", nrow(m), "states/DC. Dropped for missing pop/SAE match:", n_dropped, "\n")
if (n_dropped > 0) {
  print(anti_join(ice, m, by = "state_name") %>% select(state_name))
}

write_csv(m, "output/state_enforcement_vs_jobs.csv")

# ---- 5. Correlation: all 51, and robustness excluding DC / DC+TX -----------
# DC is a documented outlier on BOTH axes: its arrest rate is inflated by
# processing tied to federal immigration courts/facilities relative to a tiny
# resident population, and its job losses over this window are the federal
# workforce reduction story (RIFs/DOGE), not an immigrant-labor-market effect.
# Texas is the single largest state by arrest count and has the highest
# arrests-per-100k by a wide margin, so it is checked separately for leverage.
# Report all three; treat "excl. DC" (n=50) as primary.
fit_spec <- function(df, lbl) {
  ct  <- cor.test(df$arrests_per_100k, df$job_growth)
  mod <- lm(job_growth ~ arrests_per_100k, data = df)
  tibble(spec = lbl, n = nrow(df), r = unname(ct$estimate),
         ci_lo = ct$conf.int[1], ci_hi = ct$conf.int[2], p_value = ct$p.value,
         beta = coef(mod)[2], beta_se = summary(mod)$coefficients[2, 2])
}
specs <- bind_rows(
  fit_spec(m, "all 51 (50 states + DC)"),
  fit_spec(filter(m, state_name != "District of Columbia"), "excl. DC (primary, n=50)"),
  fit_spec(filter(m, !state_name %in% c("District of Columbia", "Texas")), "excl. DC and TX (n=49)")
) %>% mutate(window_start = WIN_START, window_end = WIN_END)

cat("\n=== Correlation across specifications ===\n")
print(specs %>% mutate(across(c(r, ci_lo, ci_hi, p_value, beta, beta_se), ~round(.x, 4))))
write_csv(specs, "output/state_enforcement_vs_jobs_correlation.csv")

ct_primary <- specs %>% filter(spec == "excl. DC (primary, n=50)")

# ---- 6. Chart ----------------------------------------------------------------
# All 51 points are plotted (never hide data), but the fit line is estimated
# on the primary n=50 sample (DC excluded) -- see note above and in the caption.
label_states <- c("Texas","Florida","California","New York","Louisiana","West Virginia",
                   "Nevada","Michigan","District of Columbia")
m <- m %>% mutate(
  lbl = if_else(state_name %in% label_states,
                if_else(state_name == "District of Columbia", "DC", state.abb[match(state_name, state.name)]),
                NA_character_),
  is_dc = state_name == "District of Columbia"
)

p <- ggplot(m, aes(arrests_per_100k, job_growth)) +
  geom_smooth(data = filter(m, !is_dc), method = "lm", se = TRUE,
              color = RED, fill = RED, alpha = 0.15, linewidth = 1) +
  geom_point(aes(alpha = is_dc), color = NAVY, size = 2.4) +
  scale_alpha_manual(values = c(`TRUE` = 0.35, `FALSE` = 0.85)) +
  ggrepel::geom_text_repel(aes(label = lbl), size = 3, color = NAVY,
                            min.segment.length = 0, seed = 1) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    title = "State ICE Enforcement vs. Total Payroll Job Growth",
    subtitle = sprintf("ICE arrests per 100,000 residents vs. total nonfarm employment growth, %s - %s. r = %.2f, n = %d, p = %.2f (DC excluded, see note).",
                        format(WIN_START, "%b %Y"), format(WIN_END, "%b %Y"),
                        ct_primary$r, ct_primary$n, ct_primary$p_value),
    x = "ICE arrests per 100,000 residents, cumulative over window",
    y = "Total nonfarm payroll employment growth (SA)",
    caption = paste0(
      "Source: Deportation Data Project (FOIA'd ICE arrest records); BLS State and Area Employment (SAE), seasonally adjusted;\n",
      "Census Bureau Vintage 2025 population estimates. Arrests are not deportations; correlation is not causation.\n",
      "DC (faint point) dropped from the trend line: its job losses this window are the federal-workforce-cut story, not immigration enforcement. Mike Konczal."
    )
  ) +
  my_style

ggsave("graphics/fig_state_enforcement_vs_jobs.png", p, width = W, height = H + 0.4, dpi = DPI)
cat("\nWrote graphics/fig_state_enforcement_vs_jobs.png\n")
cat("DONE 52_state_enforcement_jobs.R\n")
