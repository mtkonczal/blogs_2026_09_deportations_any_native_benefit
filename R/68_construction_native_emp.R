# ==============================================================================
# 68_construction_native_emp.R   Hypothesis 9 (construction), employment side
#
# The industry unemployment rate is a weak test: unemployed workers are coded
# to their last job's industry, and anyone who left construction for another
# industry drops out of it. The direct question is whether natives moved INTO
# construction as the foreign-born share fell. Candidate measures, all native-
# born, January-August of every year (construction is seasonal; 2026 ends Aug):
#   A. share of native prime-age (25-54) men employed in construction
#   B. same, native prime-age men without a BA
#   C. same, all native prime-age adults
#   D. native construction employment level, millions (context only: levels
#      move with the January population-control resets)
#   E. usual weekly hours of native construction workers
#   F. foreign-born share of construction employment (the first panel)
# Construction = IND1990 60, as in 40_batch2_micro.R. Employed = EMPSTAT 10/12,
# native = CITIZEN 1-3, WTFINL weights.
# ==============================================================================
suppressMessages({library(ipumsr); library(data.table); library(readr)})

ddi <- read_ipums_ddi(readLines("data_raw/ddi_path.txt")[1])
VARS <- c("YEAR","MONTH","CPSID","WTFINL","AGE","SEX","CITIZEN","EMPSTAT","IND1990",
          "EDUC","UHRSWORKT")
acc <- list(); i <- 0
cb <- function(x, pos) {
  i <<- i + 1; setDT(x)
  x <- x[AGE >= 16 & YEAR >= 2015 & MONTH <= 8, ..VARS]
  for (j in names(x)) set(x, j = j, value = as.numeric(x[[j]]))
  acc[[i]] <<- x; NULL
}
read_ipums_micro_chunked(ddi, callback = IpumsSideEffectCallback$new(cb),
                         chunk_size = 2e6, verbose = FALSE)
d <- rbindlist(acc); rm(acc); invisible(gc())

d[, `:=`(yr = YEAR, native = as.integer(CITIZEN %in% c(1, 2, 3)),
         foreign = as.integer(CITIZEN %in% c(4, 5)),
         emp = as.integer(EMPSTAT %in% c(10, 12)),
         con = as.integer(EMPSTAT %in% c(10, 12) & IND1990 == 60),
         prime = as.integer(AGE >= 25 & AGE <= 54), male = as.integer(SEX == 1),
         noba = as.integer(EDUC > 1 & EDUC < 111),
         uhrs = fifelse(UHRSWORKT < 997, UHRSWORKT, NA_real_))]

res <- d[, .(
  A_nat_prime_men_con_rate   = sum(WTFINL*con*(native*prime*male)) / sum(WTFINL*native*prime*male),
  B_nat_prime_men_noba_rate  = sum(WTFINL*con*(native*prime*male*noba)) / sum(WTFINL*native*prime*male*noba),
  C_nat_prime_all_con_rate   = sum(WTFINL*con*(native*prime)) / sum(WTFINL*native*prime),
  C2_nat_prime_women_con_rate = sum(WTFINL*con*(native*prime*(1-male))) / sum(WTFINL*native*prime*(1-male)),
  # ages 16-24: the one age group where native construction work rose (see text)
  G_nat_youth_con_rate       = sum(WTFINL*con*native*(AGE <= 24)) / sum(WTFINL*native*(AGE <= 24)),
  D_nat_con_emp_mil          = sum(WTFINL*con*native) / 1e6 / 8,
  D_for_con_emp_mil          = sum(WTFINL*con*foreign) / 1e6 / 8,
  E_nat_con_hours            = sum(WTFINL*con*native*uhrs, na.rm = TRUE) /
                               sum(WTFINL*con*native*!is.na(uhrs)),
  F_for_share_con            = sum(WTFINL*con*foreign) / sum(WTFINL*con),
  n_nat_prime_men_con        = sum(con*native*prime*male)
), by = yr][order(yr)]
cat("=== Construction, native-born, January-August ===\n")
print(res, digits = 4)
write_csv(res, "output/h9_construction_native_emp_jan_aug.csv")

# Did the rate for native prime-age men change 2024 -> 2026? household-clustered SE
suppressMessages(library(fixest))
for (g in list(list("men", d$male == 1), list("women", d$male == 0), list("all", TRUE))) {
  f <- feols(con ~ i(yr, ref = 2024), data = d[native == 1 & prime == 1 & g[[2]] & yr >= 2022],
             weights = ~WTFINL, cluster = ~CPSID)
  cat("\n=== native prime-age", g[[1]], ": change in share working in construction vs 2024, pp ===\n")
  ct <- coeftable(f); ct <- ct[grepl("yr::", rownames(ct)), ]
  print(round(cbind(d_pp = 100*ct[, 1], se_pp = 100*ct[, 2], p = ct[, 4]), 3))
}

# Native 16-24: their construction rate rose in 2026. Test against both 2024
# (a low year for them) and 2023, household-clustered SE.
for (ref in c(2024, 2023)) {
  f <- feols(as.formula(paste0("con ~ i(yr, ref = ", ref, ")")), data = d[native == 1 & AGE <= 24 & yr >= 2022],
             weights = ~WTFINL, cluster = ~CPSID)
  cat("\n=== native 16-24: change in share working in construction vs", ref, ", pp ===\n")
  ct <- coeftable(f); ct <- ct[grepl("yr::", rownames(ct)), ]
  print(round(cbind(d_pp = 100*ct[, 1], se_pp = 100*ct[, 2], p = ct[, 4]), 3))
}

# ---- chart: replaces the native-unemployment panel of b2_fig12 -------------
# Written to fig9_construction.png so 47_charts_batch2.R cannot overwrite it.
suppressMessages({library(tidyverse); library(scales)})
source("R/00_blog_style.R")
NAVY <- BLOG_NAVY; RED <- BLOG_RED; GREY <- BLOG_GREY
pd <- as_tibble(res) %>%
  select(yr, `Foreign-born share of construction employment` = F_for_share_con,
         `Share of native-born prime-age adults working in construction` = C_nat_prime_all_con_rate) %>%
  pivot_longer(-yr) %>%
  mutate(name = factor(name, levels = c("Foreign-born share of construction employment",
                                        "Share of native-born prime-age adults working in construction")))
p <- ggplot(pd, aes(yr, value, color = name)) +
  geom_vline(xintercept = 2025, linetype = "dashed", color = GREY) +
  geom_line(linewidth = .9) + geom_point(size = 1.4) +
  facet_wrap(~name, scales = "free_y", labeller = label_wrap_gen(34)) +
  scale_color_manual(values = c(RED, NAVY), guide = "none") +
  scale_x_continuous(breaks = seq(2015, 2026, 2)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(title = hyp_title(10, "Construction's Immigrant Share Fell, and Prime-Age Native Workers Didn't Move In"),
       subtitle = "January-August of each year. Each panel has its own scale.",
       x = NULL, y = NULL,
       caption = paste0("Source: IPUMS CPS microdata, author's calculations. Construction = CPS industry (IND1990 60). ",
                        "Prime age = 25-54,\nmen and women. Mike Konczal.")) +
  blog_theme
blog_save("graphics/fig9_construction.png", p)
cat("DONE 68_construction_native_emp.R\n")
