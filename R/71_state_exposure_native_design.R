# ==============================================================================
# 71_state_exposure_native_design.R -> Hypothesis 11
#
# The state test with a working first stage (see 69_state_first_stage.R: ICE
# arrests do not predict where noncitizen employment fell; the predetermined
# 2022-2024 noncitizen share of employment does).
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

# ---- chart: first stage and native outcome ------------------------------------
NAVY <- "#1c2e4a"; RED <- "#c0392b"; GREY <- "#555555"
lab_st <- c("CA","TX","FL","NY","NJ","NV","WV","ME")
pd <- bind_rows(
  as_tibble(s) %>% transmute(abb, E, y = d_nc,
    panel = "1. Where immigrant workers left\nChange in employed noncitizens, % of prime-age pop."),
  as_tibble(s) %>% transmute(abb, E, y = d_nat_epop,
    panel = "2. Native employment didn't rise there\nChange in native prime-age employment rate, pp")) %>%
  mutate(lbl = if_else(abb %in% lab_st, abb, NA_character_))
p <- ggplot(pd, aes(E, y)) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = .4) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = RED, fill = RED,
              alpha = .12, linewidth = 1, linetype = "dashed") +
  geom_point(color = NAVY, size = 2.4, alpha = .8) +
  ggrepel::geom_text_repel(aes(label = lbl), size = 3.1, color = GREY, na.rm = TRUE,
                           min.segment.length = Inf, seed = 1) +
  facet_wrap(~panel, scales = "free_y") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = function(x) ifelse(x == 0, "0", sprintf("%+.1f", x))) +
  labs(title = "Where Immigrant Workers Left, Native-Born Workers Didn't Fill In",
       subtitle = "By state, against each state's 2022-24 noncitizen share of employment. 2024 vs. September 2025-August 2026.",
       x = "Noncitizen share of state employment, 2022-24", y = NULL,
       caption = paste0("Source: IPUMS CPS microdata, author's calculations. Prime age = 25-54. DC excluded. ",
                        sprintf("Replacement ratio (2SLS): %.2f native jobs per departed noncitizen job, 95%% CI [%.2f, %.2f].",
                                rrt$replacement[1], rrt$ci_lo[1], rrt$ci_hi[1]),
                        "\nMike Konczal, ESP.")) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey88", linewidth = .35),
        plot.title = element_text(face = "bold", color = NAVY, size = 15),
        plot.title.position = "plot",
        plot.subtitle = element_text(color = GREY, face = "italic", size = 10),
        plot.caption = element_text(color = GREY, face = "italic", size = 8, hjust = 0),
        strip.text = element_text(color = NAVY, face = "bold", size = 9.5, hjust = 0),
        axis.title.x = element_text(color = GREY, size = 10), axis.text = element_text(color = GREY)) +
  coord_cartesian(clip = "off")
ggsave("graphics/fig11_exposure_native.png", p, width = 10, height = 5.2, dpi = 200, bg = "white")

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
