# ==============================================================================
# 71_state_exposure_native_design.R -> graphics/fig11_exposure_native.png
#                                      (Hypothesis 5 in blog_post.md)
#
# BLOG CHART (Hypothesis 5): a direct state scatter, no instrument. x = noncitizen
# jobs lost, y = native-born jobs gained, both per 100 prime-age residents,
# 2024 vs Sep 2025-Aug 2026. Full replacement is the 45-degree line; the fitted
# OLS line is unweighted, matching the unweighted dots. This replaced the
# two-panel exposure/2SLS chart: its first stage was weak (F = 3.8), so the
# 2SLS replacement ratio below is kept only as a printed diagnostic.
# Caveats on the direct scatter: state CPS changes are noisy (split-sample
# reliability of the noncitizen change ~0.5), which pulls the slope toward 0;
# and common local demand shocks push both series the same way (positive
# co-movement, i.e. a flatter line). Full replacement would still show up as a
# clearly positive slope (~+0.5 after attenuation); the estimate is ~-0.16.
#
# Diagnostics retained below (exposure design, see 69_state_first_stage.R):
#
#   Exposure  E  = noncitizen share of state employment, 2022-2024 (ages 16+)
#   1. First stage: change in employed prime-age noncitizens as % of state
#      prime-age population, 2024 vs Sep 2025-Aug 2026.
#   2. Native outcome: change in native prime-age EPOP, same windows; also
#      native prime-age men without a BA.
#   3. Replacement ratio: 2SLS of native jobs gained beyond population growth
#      (d native EPOP x native share of prime pop, in pp of prime pop) on the
#      change in noncitizen employment (pp of prime pop), instrumented by E.
#      Full substitution = 1 native job per departed noncitizen job.
#   4. Pre-trends: monthly state panel 2022m1-2026m8, outcome on E x quarter
#      (ref 2024Q4), state and month fixed effects, weighted by state prime-age
#      population, SEs clustered by state.
# Prime age 25-54 throughout the outcomes so both sides share one denominator.
# Native = CITIZEN 1-3, noncitizen = 5. WTFINL. DC excluded, as in 56.
# ==============================================================================
suppressMessages({library(ipumsr); library(data.table); library(tidyverse); library(tigris); library(fixest)})

ddi <- read_ipums_ddi(readLines("data_raw/ddi_path.txt")[1])
VARS <- c("YEAR","MONTH","WTFINL","AGE","SEX","CITIZEN","EMPSTAT","STATEFIP","EDUC")
acc <- list(); i <- 0
cb <- function(x, pos) {
  i <<- i + 1; setDT(x)
  x <- x[AGE >= 16 & YEAR >= 2022, ..VARS]
  for (j in names(x)) set(x, j = j, value = as.numeric(x[[j]]))
  acc[[i]] <<- x; NULL
}
read_ipums_micro_chunked(ddi, callback = IpumsSideEffectCallback$new(cb),
                         chunk_size = 2e6, verbose = FALSE)
d <- rbindlist(acc); rm(acc); invisible(gc())
d[, `:=`(date = as.Date(sprintf("%d-%02d-01", YEAR, MONTH)),
         native = as.integer(CITIZEN %in% c(1, 2, 3)), noncit = as.integer(CITIZEN == 5),
         emp = as.integer(EMPSTAT %in% c(10, 12)), prime = as.integer(AGE >= 25 & AGE <= 54),
         menoba = as.integer(SEX == 1 & EDUC > 1 & EDUC < 111))]
data(fips_codes)
xw <- unique(data.table(STATEFIP = as.integer(fips_codes$state_code), state_name = fips_codes$state_name))
d <- merge(d, xw, by = "STATEFIP")[state_name %in% state.name]

# ---- exposure ------------------------------------------------------------------
ex <- d[date <= as.Date("2024-12-01"), .(E = sum(WTFINL*emp*noncit)/sum(WTFINL*emp)), by = state_name]

# ---- two-window state panel -----------------------------------------------------
win <- function(a, b) d[date >= a & date <= b & prime == 1, .(
  nc_emp_pp   = 100*sum(WTFINL*emp*noncit)/sum(WTFINL),
  nat_epop    = 100*sum(WTFINL*emp*native)/sum(WTFINL*native),
  nat_share   = sum(WTFINL*native)/sum(WTFINL),
  nat_menoba_epop = 100*sum(WTFINL*emp*native*menoba)/sum(WTFINL*native*menoba),
  pop = sum(WTFINL)/uniqueN(date)), by = state_name]
pre  <- win(as.Date("2024-01-01"), as.Date("2024-12-01"))
post <- win(as.Date("2025-09-01"), as.Date("2026-08-01"))
s <- merge(merge(pre, post, by = "state_name", suffixes = c("_pre", "_post")), ex, by = "state_name")
s[, `:=`(d_nc = nc_emp_pp_post - nc_emp_pp_pre,
         d_nat_epop = nat_epop_post - nat_epop_pre,
         d_nat_menoba = nat_menoba_epop_post - nat_menoba_epop_pre,
         nat_jobs_beyond_pop = (nat_epop_post - nat_epop_pre) * nat_share_pre,
         E10 = 10 * E,                           # per 10 pp of exposure
         abb = state.abb[match(state_name, state.name)])]
write_csv(s, "output/h11_state_exposure_panel.csv")
cat("states:", nrow(s), "| exposure range:", round(100*range(s$E), 1), "% noncitizen share of employment\n")

rep_ols <- function(y, w = NULL) {
  f <- feols(as.formula(paste(y, "~ E10")), data = s, weights = w, vcov = "hetero")
  c(b = unname(coef(f)["E10"]), se = unname(se(f)["E10"]), p = unname(pvalue(f)["E10"]))
}
cat("\n=== Slopes per 10 pp higher 2022-24 noncitizen employment share (robust SEs) ===\n")
for (w in list(NULL, ~pop_pre)) {
  lab <- if (is.null(w)) "unweighted" else "weighted by prime-age pop"
  cat("--", lab, "--\n")
  for (y in c("d_nc", "d_nat_epop", "d_nat_menoba", "nat_jobs_beyond_pop"))
    cat(sprintf("  %-20s b = %+.2f pp  (se %.2f, p = %.3f)\n", y, rep_ols(y, w)[1], rep_ols(y, w)[2], rep_ols(y, w)[3]))
}

# ---- replacement ratio (2SLS) ---------------------------------------------------
iv_u <- feols(nat_jobs_beyond_pop ~ 1 | d_nc ~ E10, data = s, vcov = "hetero")
iv_w <- feols(nat_jobs_beyond_pop ~ 1 | d_nc ~ E10, data = s, weights = ~pop_pre, vcov = "hetero")
iv_x <- feols(nat_jobs_beyond_pop ~ 1 | d_nc ~ E10, data = s[!abb %in% c("CA","TX","FL","NY")], vcov = "hetero")
rr <- function(f, lab) {
  b <- -coef(f)["fit_d_nc"]; se <- se(f)["fit_d_nc"]
  fs <- fitstat(f, "ivf")[[1]]$stat
  cat(sprintf("%-32s replacement = %+.2f native jobs per departed noncitizen job, 95%% CI [%+.2f, %+.2f], first-stage F = %.1f\n",
              lab, b, b - 1.96*se, b + 1.96*se, fs))
  data.table(spec = lab, replacement = b, ci_lo = b - 1.96*se, ci_hi = b + 1.96*se, first_stage_F = fs)
}
cat("\n=== Replacement ratio, 2SLS (full substitution = +1) ===\n")
rrt <- rbind(rr(iv_u, "unweighted"), rr(iv_w, "weighted by prime-age pop"), rr(iv_x, "unweighted, excl. CA/TX/FL/NY"))
write_csv(rrt, "output/h11_replacement_ratio.csv")

# ---- event study -----------------------------------------------------------------
pm <- d[prime == 1, .(nat_epop = 100*sum(WTFINL*emp*native)/sum(WTFINL*native),
                      nc_emp_pp = 100*sum(WTFINL*emp*noncit)/sum(WTFINL),
                      pop = sum(WTFINL)), by = .(state_name, date)]
pm <- merge(pm, ex, by = "state_name")
pm[, `:=`(E10 = 10*E, q = paste0(year(date), "Q", quarter(date)))]
es <- rbindlist(lapply(c("nat_epop", "nc_emp_pp"), function(y) {
  f <- feols(as.formula(paste(y, "~ i(q, E10, ref = '2024Q4') | state_name + date")),
             data = pm, weights = ~pop, cluster = ~state_name)
  ct <- coeftable(f)
  data.table(outcome = y, q = sub("q::(.*):E10", "\\1", rownames(ct)), b = ct[, 1], se = ct[, 2])
}))
write_csv(es, "output/h11_event_study.csv")
cat("\n=== Event study: effect of +10 pp exposure, pp, by quarter (ref 2024Q4) ===\n")
print(dcast(es[, .(outcome, q, v = sprintf("%+.2f (%.2f)", b, se))], q ~ outcome, value.var = "v"))
pre_q <- es[outcome == "nat_epop" & q < "2025Q1"]
cat("pre-2025 native EPOP coefficients: mean", round(mean(pre_q$b), 2), "| max |b|", round(max(abs(pre_q$b)), 2), "\n")

# ---- chart (Hypothesis 5): noncitizen jobs lost vs native jobs gained --------
# Both axes in jobs per 100 prime-age residents, so full replacement = 45-degree
# line. y = change in native prime-age EPOP x native share of prime pop, which
# excludes native gains that come only from population growth.
source("R/00_blog_style.R")
NAVY <- BLOG_NAVY; GREY <- "#555555"   # used by the event-study chart below
dfit <- lm(nat_jobs_beyond_pop ~ I(-d_nc), data = s)
cat(sprintf("\nBlog chart: native jobs gained per noncitizen job lost (OLS, unweighted) = %+.2f (se %.2f)\n",
            coef(dfit)[2], summary(dfit)$coef[2, 2]))
lab_st <- c("NC","NV","TX","CA","MD","NY","NJ","IL","FL")
pd <- as_tibble(s) %>%
  transmute(abb, x = -d_nc, y = nat_jobs_beyond_pop,
            # "" (not NA) keeps unlabeled states in the repel set, so no label sits on a dot
            lbl = if_else(abb %in% lab_st, abb, ""))
p <- ggplot(pd, aes(x, y)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = .3) +
  geom_vline(xintercept = 0, color = "grey60", linewidth = .3) +
  geom_abline(slope = 1, intercept = 0, color = BLOG_GREY, linetype = "dashed", linewidth = .8) +
  annotate("text", x = 1.55, y = 3.1, label = "If native-born workers\nfilled every lost job",
           hjust = 0, size = 3, color = BLOG_GREY, lineheight = .95) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = BLOG_RED, fill = BLOG_RED,
              alpha = .15, linewidth = .9) +
  annotate("text", x = 3.75, y = -1.3, label = "What happened", hjust = 1, size = 3,
           color = BLOG_RED, fontface = "bold") +
  geom_point(color = BLOG_NAVY, size = 1.8, alpha = .8) +
  ggrepel::geom_text_repel(aes(label = lbl), size = 2.7, color = "grey30",
                           point.padding = 0.25, box.padding = 0.3, max.overlaps = Inf,
                           min.segment.length = Inf, seed = 1) +
  scale_x_continuous(labels = function(v) ifelse(abs(v) < 1e-9, "0", sprintf("%+.0f", v))) +
  scale_y_continuous(labels = function(v) ifelse(abs(v) < 1e-9, "0", sprintf("%+.0f", v))) +
  coord_cartesian(xlim = c(-2.5, 3.8), ylim = c(-3.5, 3.5)) +
  labs(title = hyp_title(5, "Where Noncitizen Workers Left, Native-Born Workers Didn't Fill In"),
       subtitle = "By state, 2024 vs. September 2025-August 2026. Both axes: jobs per 100 prime-age (25-54) residents.",
       x = "Noncitizen jobs lost (negative = gained)", y = "Native-born jobs gained",
       caption = paste0("Source: IPUMS CPS microdata, author's calculations. Native-born jobs gained excludes population growth.\n",
                        "DC excluded. Mike Konczal.")) +
  blog_theme
blog_save("graphics/fig11_exposure_native.png", p)

# event-study chart saved for review, not yet in the post
pe <- as_tibble(es) %>% filter(outcome == "nat_epop") %>%
  bind_rows(tibble(outcome = "nat_epop", q = "2024Q4", b = 0, se = 0)) %>%
  mutate(t = as.numeric(substr(q, 1, 4)) + (as.numeric(substr(q, 6, 6)) - 1)/4) %>%
  ggplot(aes(t, b)) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_vline(xintercept = 2025, linetype = "dashed", color = GREY) +
  geom_ribbon(aes(ymin = b - 1.96*se, ymax = b + 1.96*se), fill = NAVY, alpha = .15) +
  geom_line(color = NAVY, linewidth = 1) + geom_point(color = NAVY) +
  labs(title = "Native Employment in High-Exposure States, Before and After 2025",
       subtitle = "Effect of a 10-point higher 2022-24 noncitizen employment share on native prime-age EPOP (pp), by quarter, ref. 2024Q4.\nState and month fixed effects, weighted by prime-age population, 95% CI clustered by state.",
       x = NULL, y = NULL, caption = "Source: IPUMS CPS microdata, author's calculations. Mike Konczal, ESP.") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", color = NAVY), plot.title.position = "plot",
        plot.subtitle = element_text(color = GREY, size = 9.5), panel.grid.minor = element_blank())
ggsave("graphics/fig11b_exposure_event_study.png", pe, width = 8.5, height = 4.8, dpi = 200, bg = "white")
cat("DONE 71_state_exposure_native_design.R\n")
