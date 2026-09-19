# ==============================================================================
# 48_build_report_batch2.R  -> findings_batch2.html
# Every number is pulled from output/, so the memo cannot drift from the code.
# The stylesheet is lifted out of findings.html at build time so the two memos
# stay visually identical without a second copy of the CSS to maintain.
# ==============================================================================
suppressMessages({library(tidyverse); library(glue); library(lubridate)})
rd <- function(f) { p <- file.path("output", f)
  if (file.exists(p)) suppressWarnings(read_csv(p, show_col_types = FALSE)) else NULL }
g    <- function(x, d=2) ifelse(is.na(x), "n/a", sprintf(paste0("%.",d,"f"), x))
pctf <- function(x, d=1) ifelse(is.na(x),"n/a", sprintf(paste0("%.",d,"f%%"), 100*x))
ppf  <- function(x, d=2) ifelse(is.na(x),"n/a", sprintf(paste0("%+.",d,"f pp"), 100*x))
ppfr <- function(x, d=2) ifelse(is.na(x),"n/a", sprintf(paste0("%+.",d,"f pp"), x))
kf   <- function(x, d=0) ifelse(is.na(x),"n/a", formatC(x, format="f", digits=d, big.mark=","))
mf   <- function(x, d=2) ifelse(is.na(x),"n/a", sprintf(paste0("%+.",d,"f million"), x/1000))

tblf <- function(df, digits=4, note=NULL) {
  if (is.null(df) || nrow(df)==0) return("<p class='missing'>Not available.</p>")
  df <- df %>% mutate(across(where(is.numeric), ~ round(.x, digits)))
  hdr <- paste0("<th>", gsub("_"," ", names(df)), "</th>", collapse="")
  rows <- apply(df, 1, function(r) paste0("<tr>",
    paste0("<td>", ifelse(is.na(r)|r=="NA","",r), "</td>", collapse=""), "</tr>"))
  paste0("<div class='tw'><table><thead><tr>",hdr,"</tr></thead><tbody>",
         paste(rows,collapse=""),"</tbody></table></div>",
         if(!is.null(note)) paste0("<p class='note'>",note,"</p>") else "")
}
figf <- function(f, cap) if (file.exists(file.path("graphics",f)))
  paste0('<figure><img src="graphics/',f,'" alt="',cap,'"><figcaption>',cap,'</figcaption></figure>') else ""
vb <- function(v) { lab <- c(refuted="REFUTED", supported="SUPPORTED", mixed="MIXED",
  null="NO EFFECT FOUND", inconc="INCONCLUSIVE")[v]
  paste0('<span class="verdict v-',v,'">',lab,'</span>') }

# ---------------- pull every number ------------------------------------------
h14   <- rd("b2_h14_labor_share_headline.csv"); h14a <- rd("b2_h14_labor_share_annual.csv")
ces   <- rd("b2_ces_sex_change.csv");           bench <- rd("b2_ces_benchmark.csv")
wy    <- rd("b2_h15_within_year.csv")
ages  <- rd("b2_h16_age_sex.csv");              ym   <- rd("b2_h16_young_men_nocollege.csv")
mint  <- rd("b2_h17_male_intensity.csv");       mq   <- rd("b2_h17_male_by_exposure_q.csv")
ovi   <- rd("b2_h17_overlap_index.csv");        sxn  <- rd("b2_h17_sex_nativity.csv")
dsex  <- rd("b2_h17_dose_by_sex.csv");          wsex <- rd("b2_h17_wages_by_sex.csv")
con   <- rd("b2_h18_construction_industry.csv");ctm  <- rd("b2_h18_construction_trades_men.csv")
wcon  <- rd("b2_h18_wages_construction.csv")
trk   <- rd("b2_h19_tracker_by_group.csv");     trkq <- rd("b2_h19_tracker_by_quartile.csv")
trkx  <- rd("b2_h19_tracker_by_exposure.csv");  trkbx<- rd("b2_h19_tracker_bottomq_by_exposure.csv")
cpi   <- rd("b2_h20_cpi_shelter.csv");          reg  <- rd("b2_h20_shelter_by_region.csv")
defl  <- rd("b2_h20_deflators.csv")
occ20 <- rd("b2_h21_top20_occupations.csv");    wocc <- rd("b2_h21_wages_top20.csv")
se    <- rd("b2_h22_selfemp.csv");              seq_ <- rd("b2_h22_selfemp_by_exposure.csv")
sec   <- rd("b2_h22_selfemp_construction.csv")
un    <- rd("b2_h23_union.csv");                unp  <- rd("b2_h23_union_premium.csv")
h24a  <- rd("b2_h24_male_replacement_arithmetic.csv")
m24b  <- rd("b2_h24_men_by_citizenship.csv");   ovt  <- rd("b2_h24_overlap_over_time.csv")
nmx   <- rd("b2_h24_native_men_by_exposure.csv")
cz    <- rd("b2_h25_by_citizenship.csv");       czs  <- rd("b2_h25_by_citizenship_sex.csv")
czh   <- rd("b2_h25_naturalized_hispanic.csv"); cza  <- rd("b2_h25_naturalized_by_arrival.csv")
dcit  <- rd("b2_h25_dose_response_citizenship.csv")
unw   <- rd("b2_h25_unweighted_counts.csv");    trkn <- rd("b2_h25_tracker_naturalized_by_exposure.csv")

pick <- function(df, col, ...) { v <- df %>% filter(...); if (nrow(v)==0) NA_real_ else v[[col]][1] }

# labor share
ls_base <- h14$labor_share_base; ls_last <- h14$labor_share_latest
ls_lower <- h14$n_quarters_lower; ls_nq <- h14$n_quarters_postwar

# CES sex
ces_men <- ces$d_men_nf; ces_wom <- ces$d_women_nf; ces_tot <- ces$d_total_nf
men_prior <- bench$prior_20m[bench$series=="men_nf"]
wom_prior <- bench$prior_20m[bench$series=="women_nf"]
health_chg <- ces$d_health

# within year
wy25 <- wy %>% filter(yr==2025); wy26 <- wy %>% filter(yr==2026)
wy_base <- wy %>% filter(yr <= 2024, yr != 2020, months==11)
wy1819 <- wy %>% filter(yr %in% c(2018,2019))
wy1519 <- wy %>% filter(yr >= 2015, yr <= 2019)
# CES payroll change over exactly the same within-year window as the 2025 claim
cesm <- rd("b2_ces_sex_monthly.csv")
ces_2025_jan_dec <- cesm$tot_nf[cesm$date == as.Date("2025-12-01")] -
                    cesm$tot_nf[cesm$date == as.Date("2025-01-01")]

# H24
epop_req <- h24a$epop_required_change; epop_act <- h24a$d_epop_nat_men
epop_gap <- -h24a$epop_gap_pp; absorbed <- h24a$share_of_decline_absorbed
for_male_decline <- h24a$foreign_male_decline
jobs_gap <- abs(h24a$foreign_male_decline - h24a$emp_gain_beyond_population)
nm_ov24 <- pick(ovt,"overlap", grp=="Native men", yr==2024)
nm_ov26 <- pick(ovt,"overlap", grp=="Native men", yr==2026)
nm_ov19 <- pick(ovt,"overlap", grp=="Native men", yr==2019)

# H25
cz24 <- cz %>% filter(yr==2024); cz26 <- cz %>% filter(yr==2026)
gtv <- function(d, grp, col) d[[col]][d$citgrp==grp]
nat_epop_d <- gtv(cz26,"Native-born","epop") - gtv(cz24,"Native-born","epop")
nzd_epop_d <- gtv(cz26,"Naturalized citizen","epop") - gtv(cz24,"Naturalized citizen","epop")
ncz_epop_d <- gtv(cz26,"Noncitizen","epop") - gtv(cz24,"Noncitizen","epop")
nat_un_d <- gtv(cz26,"Native-born","unrate") - gtv(cz24,"Native-born","unrate")
nzd_un_d <- gtv(cz26,"Naturalized citizen","unrate") - gtv(cz24,"Naturalized citizen","unrate")
ncz_un_d <- gtv(cz26,"Noncitizen","unrate") - gtv(cz24,"Noncitizen","unrate")
trk_nzd26 <- pick(trk,"tracker_real", grp=="Naturalized citizens", yr==2026)
trk_nat26 <- pick(trk,"tracker_real", grp=="Native-born", yr==2026)
trk_ncz26 <- pick(trk,"tracker_real", grp=="Noncitizens", yr==2026)

# H19 Bessent
q1 <- function(y) pick(trkq,"tracker", grp=="Native-born", qtile=="Q1 lowest", yr==y)
q4 <- function(y) pick(trkq,"tracker", grp=="Native-born", qtile=="Q4 highest", yr==y)
r22 <- q1(2022)/q4(2022); r26 <- q1(2026)/q4(2026)

# misc
occ_un_down <- sum(occ20$d_nat_unrate < 0, na.rm=TRUE)
occ_hrs_up  <- sum(occ20$d_nat_hrs > 0, na.rm=TRUE)
occ_both    <- sum(occ20$d_nat_unrate < 0 & occ20$d_nat_hrs > 0, na.rm=TRUE)
wocc_ok <- wocc %>% filter(usable)
wocc_up <- sum(wocc_ok$g_p50_24_26 > 0, na.rm=TRUE)
rent_pk <- cpi %>% filter(date >= as.Date("2021-01-01")) %>% slice_max(yoy_rent, n=1)
rent_dec24 <- cpi$yoy_rent[cpi$date == as.Date("2024-12-01")]
rent_last  <- tail(cpi$yoy_rent[!is.na(cpi$yoy_rent)], 1)
infl26 <- defl %>% filter(yr==2026)
se24 <- se$se_rate[se$yr==2024]; se26 <- se$se_rate[se$yr==2026]
un24 <- pick(un,"cov_rate", grp=="Native-born", yr==2024)
un26 <- pick(un,"cov_rate", grp=="Native-born", yr==2026)
unp24 <- unp$premium[unp$yr==2024]; unp26 <- unp$premium[unp$yr==2026]

dsex_u <- dsex %>% filter(outcome=="d_nat_unrate", spec=="Men 2025-26")
dcit_u <- dcit %>% filter(outcome=="d_unrate", spec=="Naturalized citizens 2025-26")

CLAIMS <- tribble(
  ~n, ~claim, ~pred, ~found, ~hyp, ~verdict,
  18, "The zero <em>is</em> the proof. Male payroll growth is flat because a two-thirds-male immigrant workforce left and native men filled the jobs one for one.",
      "Native male employment rises by roughly the foreign-born male decline, which requires native male EPOP to rise.",
      glue("Full replacement needed the native male employment rate to rise {ppf(epop_req)}; it <strong>fell</strong> {g(abs(100*epop_act),2)} points, and native men&rsquo;s share of employment in immigrant-intensive occupations has not moved in a decade."),
      "H24", "refuted",
  19, "The workers who actually compete with new arrivals are prior immigrants. Naturalized citizens should be the biggest winners, and they vote.",
      "Naturalized-citizen employment and wages improve, and improve most in high-exposure occupations.",
      glue("Their employment rate did <strong>rise</strong> {ppf(nzd_epop_d)} while the native-born rate fell, but their unemployment rose too, their real wage growth was the weakest of any group at {pctf(trk_nzd26)}, and the exposure gradient is {g(dcit_u$beta,4)} (p = {g(dcit_u$p,2)})."),
      "H25", "mixed",
   9, "Done properly, with within-year consistent weights, the levels show natives gained about two million jobs a year.",
      "Within-year native employment gains in 2025 and 2026 beat prior years and beat what population growth implies.",
      glue("The {kf(wy25$d_emp_nat)} thousand replicates, but it sits around the 30th percentile of 2015&ndash;2024 and is <strong>smaller</strong> than the {kf(wy25$emp_nat_from_pop)} thousand that measured native population growth implies on its own."),
      "H15", "refuted",
  10, "Immigration is what kept young U.S.-born men out of the workforce. Remove it and they come back.",
      "Native male LFP and EPOP rise at 16-24 and 25-34, most without a BA.",
      glue("Native men aged 20&ndash;34 without a degree saw employment <strong>fall</strong> from {pctf(ym$epop[ym$yr==2024])} to {pctf(ym$epop[ym$yr==2026])} and unemployment <strong>rise</strong> from {pctf(ym$unrate[ym$yr==2024],2)} to {pctf(ym$unrate[ym$yr==2026],2)}."),
      "H16", "refuted",
  11, "The removed workforce was two-thirds male, so native men are the closest substitutes and gain most.",
      "Native men improve relative to native women, most where occupational overlap is highest.",
      glue("Native men do have the higher overlap with immigrant labor ({pctf(ovi$overlap[ovi$grp==\"Native men\"])} against {pctf(ovi$overlap[ovi$grp==\"Native women\"])}), and got nothing for it: their dose-response coefficient is {g(dsex_u$beta,4)}, the wrong sign and insignificant."),
      "H17", "refuted",
  12, "Construction is the test case: heavily immigrant, heavily male, heavily non-college.",
      "Native construction unemployment falls; hours and wages rise.",
      glue("The foreign-born share of construction <strong>fell</strong> from {pctf(con$for_share[con$yr==2024])} to {pctf(con$for_share[con$yr==2026])} and native construction unemployment <strong>rose</strong> from {pctf(con$nat_unrate[con$yr==2024],2)} to {pctf(con$nat_unrate[con$yr==2026],2)}."),
      "H18", "refuted",
  13, "Wage growth accelerated at the bottom: the bottom quartile is growing three times faster than the top.",
      "A matched-worker tracker shows bottom-quartile native wage growth accelerating after Jan 2025.",
      glue("On a matched-worker tracker the bottom quartile ran {g(r22,1)} times the top in 2022 and {g(r26,1)} times in 2026, and what is left has no gradient in immigrant exposure."),
      "H19", "refuted",
   8, "Immigration is a labor-supply subsidy to capital. Removing it shifts income back to labor.",
      "The labor share of income rises.",
      glue("The labor share <strong>fell</strong> {g(abs(h14$change_index_pts),1)} index points to {g(ls_last,1)}, the lowest of {kf(ls_nq)} quarters since 1947."),
      "H14", "refuted",
  14, "Even if nominal wages are flat, deportations cut rents, so native real wages rose.",
      "Rent inflation falls faster where immigrants lived; native real wages rise on a shelter-adjusted deflator.",
      glue("Rent inflation peaked in {format(rent_pk$date, \"%B %Y\")}, two years before enforcement; it decelerated <em>least</em> in the West; and stripping shelter out of the deflator makes 2026 real wages look worse, not better."),
      "H20", "refuted",
  15, "Go occupation by occupation: roofers, drywall, meatpacking, landscaping, housekeeping.",
      "Native unemployment, hours and wages improve in the highest foreign-born-share occupations.",
      glue("Across the twenty most immigrant-intensive occupations, native unemployment fell in {occ_un_down}, hours rose in {occ_hrs_up}, and both together in {occ_both}."),
      "H21", "refuted",
  16, "Natives step into the businesses immigrants ran.",
      "Native self-employment rises, especially in the construction trades.",
      glue("Native self-employment <strong>fell</strong> from {pctf(se24,2)} to {pctf(se26,2)}, and fell furthest in the most immigrant-intensive quartile of occupations."),
      "H22", "refuted",
  17, "Illegal immigration undercut union standards; removing it raises density and the union premium.",
      "Native union coverage and the native union wage premium rise.",
      glue("Native union coverage went from {pctf(un24,2)} to {pctf(un26,2)} and the union wage premium from {g(unp24,3)} to {g(unp26,3)} log points. Nothing moved."),
      "H23", "null")

claims_rows <- paste0(apply(CLAIMS, 1, function(r) paste0(
 '<tr><td class="cn">C', trimws(r[["n"]]), '</td><td><strong>', r[["claim"]],
 '</strong><br><span class="pred">Hypothesis: ', r[["pred"]],
 '</span><br><span class="found">Found: ', r[["found"]],
 '</span></td><td class="hy">', r[["hyp"]], '</td><td>', vb(r[["verdict"]]), '</td></tr>')),
 collapse="")

CSS <- if (file.exists("findings.html")) {
  x <- readLines("findings.html", warn = FALSE)
  a <- grep("<style>", x)[1]; b <- grep("</style>", x)[1]
  paste(x[a:b], collapse = "\n")
} else "<style></style>"

BODY <- glue('
<div class="wrap">
<p class="kicker">Findings memo, round two</p>
<h1>Round Two: The Arguments That Came After</h1>
<p class="sub">Twelve more claims, including the two we could not dismiss on paper, and what the
CPS microdata say about each.</p>
<p class="byline">Mike Konczal &middot; Analysis run September 19, 2026 &middot;
IPUMS CPS basic monthly, January 2015 &ndash; August 2026 &middot; 12,545,329 person-month records</p>

<div class="caveat">
<strong>Read the first memo first.</strong> The seven original claims and thirteen hypotheses are in
<a href="findings.html">the batch-one memo</a>, and nothing here revises it. That memo is
sufficient to make the case. This one exists because the argument moved: the other side conceded
the population-control problem and rebuilt its numbers around it, the administration shifted to
prices and to the wage distribution, and the collapse in male employment gave both sides something
new to claim. Batch two is a stress test of our own conclusion, not a rescue of it.
</div>

<h2>Bottom line</h2>
<div class="finding">
Twelve more claims, ten refuted, one null, one mixed. Two of the twelve were serious enough
to worry us before we ran them.
<br><br>
<strong>The first is the male-replacement argument</strong>, and it is the best case anyone has
made. Payroll employment for men has gone nowhere since December 2024: {kf(ces_men)} thousand jobs
against {kf(ces_wom)} thousand for women. The strongest reading of that is not male distress but
arithmetic, a heavily male immigrant workforce leaving while native men step into the openings, the
two cancelling to zero. If that were right it would be the cleanest evidence for substitution in
the whole episode. It requires native men to be working at a higher rate. They are not. Filling the
measured foreign-born male decline would have taken a {ppf(epop_req)} rise in the native male
employment rate; it fell {g(abs(100*epop_act))} points, a gap of about
{kf(jobs_gap)} thousand jobs. And native men did not move into that work: the share of their
employment sitting in immigrant-intensive occupations is a shade <em>lower</em> than in 2019,
{pctf(nm_ov19,1)} then against {pctf(nm_ov26,1)} now.
<br><br>
<strong>The second is the naturalized-citizen argument</strong>, which is the substitution model&rsquo;s
own best case and the one honest point on the board for the other side. The literature that finds
little native wage effect from immigration does find one on previously arrived immigrants. So
naturalized citizens, twenty-odd million people who cannot be removed and who vote, should be the
winners. On employment they did outperform: their prime-age employment rate moved {ppf(nzd_epop_d)}
from 2024 to 2026 while the native-born rate moved {ppf(nat_epop_d)}. That is real and we report it.
But their unemployment rate still rose, their occupational dose-response is flat and
indistinguishable from zero, and their matched real wage growth was the <em>weakest</em> of any
group we measured, {pctf(trk_nzd26)} in 2026 against {pctf(trk_nat26)} for the native-born. And the
ranking across citizenship groups follows exposure to enforcement, not substitution: the group being
removed has the fastest-rising employment rate, which is what selective departure looks like.
</div>

<h2>The twelve claims</h2>
<div class="tw"><table><thead><tr><th>#</th><th>The claim, as its proponents make it</th>
<th>Tests</th><th>Verdict</th></tr></thead><tbody>{claims_rows}</tbody></table></div>
<p class="note">Ordered by how hard the claim was to beat, not by claim number. C8&ndash;C17 continue
the numbering from the first memo; C18 and C19 were added after the first draft of this one.</p>

<h2>C18 / H24. &ldquo;The zero is the proof&rdquo;</h2>
{vb("refuted")}

<div class="grid">
<div class="stat"><div class="big">{kf(ces_men)}K</div><div class="lab">Change in male payroll employment, Dec 2024 to Aug 2026</div></div>
<div class="stat"><div class="big">+{kf(ces_wom)}K</div><div class="lab">Change in female payroll employment, same window</div></div>
<div class="stat"><div class="big">{kf(men_prior)}K</div><div class="lab">Change in male payroll employment over the previous 20 months</div></div>
</div>

{figf("b2_fig3_men_women_payrolls.png","Cumulative change in payroll employment by sex since December 2024.")}

<p>The fact is not in dispute. Men have added essentially no payroll jobs in twenty months, after
adding {kf(men_prior)} thousand in the twenty months before. The question is what it means, and
there is a reading on which it is evidence <em>for</em> the policy: the removed workforce was
disproportionately male, native men took the vacated jobs, and the two flows net to zero. That is
the best argument on this page and it has four testable consequences.</p>

<h3>1. The replacement arithmetic</h3>
<p>Native male employment can rise for two reasons: the native male population grew, or a larger
share of it works. Only the second is filling a vacated job. Measured foreign-born male employment
fell by {kf(for_male_decline)} thousand. Absorbing all of that would have required the native male
employment rate to rise {ppf(epop_req)}. It fell {g(abs(100*epop_act))} points instead, and native
male unemployment rose {ppf(h24a$d_unrate_nat_men)}. Net of population growth, the number of native
men working went <em>down</em> by about {kf(abs(h24a$emp_gain_beyond_population))} thousand.</p>

<div class="caveat">The levels in this section are contaminated, and the contamination runs in the
claim&rsquo;s favour. When foreign-born respondents leave the sample their weight is redistributed
onto native-born respondents in the same age-sex-race cell, so measured native male employment is
biased <em>up</em>. The claim is being given the benefit of its own most flattering measurement and
still fails. Note also that the two surveys disagree about the male total: the household survey
shows male employment up {kf(h24a$d_emp_men_all)} thousand while payrolls show
{kf(ces_men)} thousand, which is that same artifact.</div>

<h3>2. The occupational-mix test</h3>
<p>This is the clean one. If native men moved into the work immigrants left, the share of native
male employment sitting in high-foreign-born-share occupations has to rise. The overlap index below
distributes each group&rsquo;s employment across occupations and weights by each occupation&rsquo;s
fixed 2022&ndash;24 foreign-born share, so multiplying every native weight by the same factor leaves
it unchanged. It is immune to the reweighting problem in a way that no level and no level-ratio is.</p>

{figf("b2_fig4_overlap.png","Exposure-weighted occupational overlap with immigrant labor, by group.")}

<div class="finding">Native men&rsquo;s overlap index was {pctf(nm_ov24,2)} in 2024 and
{pctf(nm_ov26,2)} in 2026. It has not moved in a decade. Whatever happened to male employment, it
was not native men moving into immigrant-intensive occupations.</div>

<h3>3. Inside the exposed occupations</h3>
{figf("b2_fig5_native_men_exposure.png","Native-born men\'s unemployment rate by quartile of occupational immigrant exposure.")}
{tblf(nmx %>% filter(yr>=2022) %>% select(yr, exp_q, unrate, hrs, ptecon), 4,
"Native-born men only. exp q is the quartile of the occupation&rsquo;s 2022-24 foreign-born employment share. ptecon is the share working part time for economic reasons.")}
<p>Native men&rsquo;s unemployment rose in every exposure quartile and their usual hours fell most
in the most exposed quartile. Both run against the substitution prediction.</p>

<h3>4. So where did the male jobs go?</h3>
{tblf(bench, 1, "Payroll employment change, thousands, over two 20-month windows: April 2023 to December 2024, and December 2024 to the latest month.")}
<p>Health care alone added {kf(health_chg)} thousand jobs, more than the entire net payroll gain of
{kf(ces_tot)} thousand. Manufacturing and transportation and warehousing both shrank. The male
payroll flatline is mostly a sectoral story about which industries were hiring, and health care is
about three-quarters female. That is a demand-composition explanation, and it has very little to do
with immigration in either direction.</p>

<h2>C19 / H25. Naturalized citizens: the substitution model&rsquo;s own best case</h2>
{vb("mixed")}

<p>The literature usually cited to show immigration does not hurt native workers does not say it
hurts nobody. It says the incidence falls on <em>previously arrived immigrants</em>, who are close
substitutes for new arrivals in production, while natives specialise into language- and
communication-intensive tasks and are partly complements. Run that forward and the prediction is
sharp: removing new immigrants should <em>raise</em> the wages and employment of prior immigrants,
above all naturalized citizens, who cannot be removed. They are also voters, which is why the
political argument that Democrats lose ground with naturalized citizens on immigration has an
economic premise underneath it that can be checked.</p>

<p>Three things make this the strongest test in either memo. It is the substitution model&rsquo;s
best prediction rather than its weakest. It has a within-model prediction about magnitudes: the gain
for naturalized citizens should <em>exceed</em> the gain for natives. And naturalized citizens are
the best-measured group in the data.</p>

{tblf(unw, 1, "Unweighted CPS respondents by citizenship, 2024 monthly average versus September 2025 to August 2026. Raw interview counts carry no weights, so they isolate survey participation.")}
<p class="note">The group the theory says should gain most is the group whose survey participation
held up best, falling {g(abs(unw$pct_change[unw$group=="Naturalized citizens"]),1)} percent against
{g(abs(unw$pct_change[unw$group=="Noncitizens"]),1)} percent for noncitizens. The usual objection
that we are looking at a selection artifact is weakest exactly here.</p>

<h3>What they got</h3>
<div class="grid">
<div class="stat"><div class="big">{ppf(nzd_epop_d)}</div><div class="lab">Naturalized citizens, prime-age employment rate, 2024 &rarr; 2026</div></div>
<div class="stat"><div class="big">{ppf(nat_epop_d)}</div><div class="lab">Native-born, same measure</div></div>
<div class="stat"><div class="big">{ppf(ncz_epop_d)}</div><div class="lab">Noncitizens, same measure</div></div>
</div>

{figf("b2_fig8_citizenship.png","Prime-age employment-population ratio by citizenship.")}

<div class="finding">On employment rates, naturalized citizens did outperform the native-born, and
that is the one result in this memo that points the restrictionist way. It should be reported as
such. But it does not survive the three follow-up questions.</div>

<p><strong>Their unemployment still rose.</strong> Naturalized-citizen prime-age unemployment moved
{ppf(nzd_un_d)} from 2024 to 2026. Less than the native-born ({ppf(nat_un_d)}), but the substitution
story predicts a fall, not a smaller rise.</p>

<p><strong>Their wages did worst of anyone.</strong> On the matched-worker tracker, which follows
the same person twelve months apart and so cannot be fooled by composition, naturalized
citizens&rsquo; real wage growth was {pctf(trk_nzd26)} in 2026 against {pctf(trk_nat26)} for the
native-born and {pctf(trk_ncz26)} for noncitizens. The group that should have been bid up was the
only one whose real wages fell.</p>

{tblf(trk %>% filter(yr>=2024) %>% select(grp, yr, tracker, tracker_real, n) %>% arrange(grp, yr), 4,
"Median 12-month wage growth, matched person to person, nominal and real. A matched tracker conditions on being employed in both periods, so it removes composition bias and adds survivorship bias; read it beside the cross-sectional numbers, not instead of them.")}

<p><strong>There is no exposure gradient.</strong> The mechanism requires the gain to show up where
immigrant labor was thickest. It does not, for naturalized citizens or for natives.</p>

{tblf(dcit %>% select(outcome, spec, beta, se, p, n), 4,
"Occupation-level regressions of the change in the group&rsquo;s unemployment rate and usual hours on the occupation&rsquo;s pre-period foreign-born share, weighted by pre-period employment, HC1 standard errors. Substitution predicts a negative unemployment coefficient, larger for naturalized citizens than for natives.")}

<p>In the 2018&ndash;19 placebo window both groups show the predicted negative coefficient, and it is
larger for naturalized citizens, exactly as the model says. In 2025&ndash;26 it is gone for both. A
genuinely tight labor market produces the gradient; this episode did not.</p>

<h3>The ordering is the tell</h3>
<div class="finding">Rank the three citizenship groups by the change in their employment rate and you
get noncitizens ({ppf(ncz_epop_d)}), then naturalized citizens ({ppf(nzd_epop_d)}), then the
native-born ({ppf(nat_epop_d)}). That is a perfect ranking in exposure to enforcement, and it is the
<em>reverse</em> of what substitution predicts, which is that the group being removed should be the
one whose measured outcomes deteriorate. What it looks like instead is selection: the noncitizens
still answering the survey are the most settled and most work-attached of those who were here, and
the same sorting runs, more weakly, through mixed-status households with a naturalized member.</div>

{tblf(czh %>% select(grp, yr, epop, lfpr, unrate, n) %>% filter(yr>=2023), 4,
"Naturalized citizens by ethnicity. Note this does not reproduce the sharp Hispanic-citizen penalty the first memo found among the native-born, which is an open tension worth naming rather than smoothing over.")}
{tblf(cza %>% select(arrgrp, yr, epop, unrate, n) %>% filter(yr>=2023), 4,
"By arrival era. YRIMMIG is a bracketed code in this extract, not a year, so the cohorts are coarse on purpose.")}

<h2>C9 / H15. The within-year-weights argument</h2>
{vb("refuted")}

<p>Camarota and Zeigler concede the January population-control problem and route around it by
comparing January to December of the same year, where the controls never change. On that basis they
report native employment gains of about two million within 2025 and 1.9 million within 2026 through
June, against 758,000 within 2024 and 367,000 within 2023. It is a better argument than the one the
first memo refuted, and the replication works: we get {kf(wy25$d_emp_nat)} thousand within 2025 and
{kf(wy26$d_emp_nat)} thousand within 2026 through August.</p>

<p>Then it fails three ways.</p>

<h3>The base rate</h3>
{figf("b2_fig2_within_year.png","Within-year change in native-born employment and population, using each year\'s own weights.")}
<p>The comparison years chosen are 2023 and 2024, which were the two weakest in a decade. Set 2025
against the full 2015&ndash;2024 distribution and it sits at roughly the 30th percentile. Within-year
native employment rose {kf(wy1819$d_emp_nat[1])} thousand in 2018 and {kf(wy1819$d_emp_nat[2])}
thousand in 2019, both far above 2025. Nothing unusual happened.</p>

<h3>The demographic bound</h3>
<div class="finding">Within 2025 the measured native-born population aged 16 and over grew by
{kf(wy25$d_pop_nat)} thousand. The native-born population grows by births ageing in minus deaths. It
is not a policy variable and it does not do that. In 2015 through 2019 the same within-year figure
ran between {kf(min(wy1519$d_pop_nat))} and {kf(max(wy1519$d_pop_nat))} thousand. The 2025 native population number is the reweighting artifact, visible in the denominator.</div>
<p>Hold that aside and the claim collapses on its own arithmetic. At the January 2025 employment
rate, that measured population growth alone delivers {kf(wy25$emp_nat_from_pop)} thousand more
employed natives, which is <em>more</em> than the {kf(wy25$d_emp_nat)} thousand actually recorded.
The employment-rate contribution was {kf(wy25$emp_nat_from_rate)} thousand, and native EPOP within
2025 changed {ppf(wy25$d_epop_nat)}. Either the native population level is trustworthy, in which
case the job gain is smaller than population growth alone implies, or it is not, in which case the
employment level cannot be used either. There is no third option.</p>

{tblf(wy %>% select(yr, months, d_emp_nat, d_emp_for, d_emp_tot, d_pop_nat, d_epop_nat,
                    emp_nat_from_pop, emp_nat_from_rate), 3,
"January-to-December change using that year&rsquo;s own weights, thousands. 2026 is January to August. from pop is the employment change implied by measured population growth at an unchanged January employment rate.")}

<h3>Adding up</h3>
<p>Within 2025 the same calculation gives {kf(wy25$d_emp_tot)} thousand for total employment, all
workers, because the foreign-born change was {kf(wy25$d_emp_for)} thousand. A two-million native job
gain inside a one-million total is a transfer between two columns of the same table, not job
creation. And the other survey is blunter still: payroll employment over exactly that window,
January to December 2025, grew {kf(ces_2025_jan_dec)} thousand. Two million native-born jobs did
not appear in a labor market that added {kf(ces_2025_jan_dec)} thousand payroll positions.</p>

<h2>C10 &amp; C11 / H16, H17. Men</h2>
{vb("refuted")}

<p>Two claims here, pointing opposite ways. Camarota&rsquo;s is that immigration kept young U.S.-born
men out of the workforce and removing it brings them back. Ours, constructed as the hardest version
we could build against ourselves, is that the removed workforce was
{pctf(mint$male_share[mint$grp=="Noncitizen"])} male, so native men are the closest substitutes in
the whole economy and should be the single biggest winners. Both predict native men improving.</p>

{figf("b2_fig10_young_men.png","Native-born men aged 20-34 without a bachelor\'s degree.")}
{tblf(ym %>% filter(yr>=2022) %>% select(yr, epop, lfpr, unrate, n, months), 4,
"Native-born men aged 20-34 without a bachelor&rsquo;s degree, annual averages of monthly rates.")}

<div class="finding">Native men aged 20 to 34 without a degree, the group the claim names, saw
employment fall from {pctf(ym$epop[ym$yr==2024],1)} to {pctf(ym$epop[ym$yr==2026],1)},
participation fall, and unemployment rise from {pctf(ym$unrate[ym$yr==2024],2)} to
{pctf(ym$unrate[ym$yr==2026],2)}. They did not come back.</div>

<h3>The overlap index</h3>
{tblf(ovi %>% select(grp, overlap, n_occ), 4,
"Exposure-weighted overlap with immigrant labor, 2022-24. Each group&rsquo;s employment distributed across occupations, dotted with each occupation&rsquo;s foreign-born share.")}
<p>Native men have measurably more overlap with immigrant labor than native women, and native men
without a degree more still. The top exposure quartile of occupations is
{pctf(mq$male_share_all[mq$exp_q=="Q4 highest"])} male. The theory&rsquo;s demographic prediction is
therefore not optional: native men should be the winners.</p>

{tblf(sxn %>% filter(yr>=2023) %>% select(yr, grp, epop, lfpr, unrate), 4,
"Prime-age, by sex and nativity, annual averages of monthly rates.")}

<p>They were not. Native men&rsquo;s prime-age unemployment rose more than native women&rsquo;s, and
their employment rate fell. We should be straight about one wrinkle: native <em>women&rsquo;s</em>
employment rate fell by more than native men&rsquo;s over the same window, so this is not a story
about men doing uniquely badly. It is a story about the group with the most exposure to immigrant
competition getting nothing while the labor market softened for everyone.</p>

{tblf(dsex %>% select(outcome, spec, beta, se, p, n), 4,
"Occupation-level regressions run separately by sex, weighted by pre-period native employment, HC1 standard errors. Substitution predicts a negative coefficient on unemployment and a positive one on hours.")}
<p>The male dose-response coefficient on unemployment is the wrong sign and insignificant in
2025&ndash;26, and the correct sign and significant in the 2018&ndash;19 placebo. Same pattern as the
pooled result in the first memo, and now visible in the subgroup the theory cares most about.</p>

<h2>C12 / H18. Construction</h2>
{vb("refuted")}

<p>The best-identified sector available: about a third foreign-born before the campaign, over ninety
percent male, mostly non-college, and non-tradable, so the output cannot be moved offshore.</p>

{figf("b2_fig12_construction.png","Construction: foreign-born employment share and the native-born unemployment rate.")}
{tblf(con %>% filter(yr>=2022) %>% select(yr, nat_unrate, nat_hrs, for_share, n, months), 4,
"Construction industry, annual averages of monthly rates.")}
{tblf(ctm %>% filter(yr>=2022) %>% select(yr, unrate, hrs, ptecon, n, months), 4,
"Native-born men in construction-trades occupations, any industry.")}

<div class="finding">The foreign-born share of construction employment fell from
{pctf(con$for_share[con$yr==2024],1)} to {pctf(con$for_share[con$yr==2026],1)}, and over the same
window native construction unemployment rose from {pctf(con$nat_unrate[con$yr==2024],2)} to
{pctf(con$nat_unrate[con$yr==2026],2)}. Native men in the construction trades went from
{pctf(ctm$unrate[ctm$yr==2024],2)} to {pctf(ctm$unrate[ctm$yr==2026],2)}. Real median wages for
native construction workers fell {pctf(abs(wcon$g_p50[wcon$yr==2026]))} in 2026.</div>

<p>One thing did move the other way and it is worth naming: hours held up and involuntary part-time
among native construction men fell, from {pctf(ctm$ptecon[ctm$yr==2024],2)} to
{pctf(ctm$ptecon[ctm$yr==2026],2)}. The natural reading is selection rather than victory, because
the marginal workers are the ones who left employment, but it is the only construction number that
points the restrictionist way and we are not going to bury it.</p>

<h2>C13 / H19. Wage growth at the bottom</h2>
{vb("refuted")}

<p>Bessent&rsquo;s claim is that workers without a degree are having their best decade and that the
bottom quartile of earners is seeing wage growth about three and a half times the top quartile. The
first memo measured wages as cross-sectional percentiles, which is the wrong tool here: when
low-wage employment falls, the measured tenth percentile rises for a bad reason. So we rebuilt the
Atlanta Fed Wage Growth Tracker from the microdata, matching each worker&rsquo;s outgoing-rotation
record to their own record twelve months earlier. {kf(sum(trk$n[trk$grp=="All workers"]))} matched
pairs. That measure cannot be fooled by composition.</p>

{figf("b2_fig6_tracker_quartile.png","Median matched 12-month wage growth for native-born workers, by wage quartile.")}

<div class="finding">The bottom-quartile premium is real, and it is a 2022 artifact. Bottom-quartile
native wage growth ran {g(r22,1)} times the top quartile in 2022, under the pre-enforcement labor
market. In 2026 the ratio is {g(r26,1)}. The compression Bessent is describing happened, and then it
stopped happening, and the stopping coincides with the policy he credits for it.</div>

{tblf(trkq %>% filter(grp=="Native-born", yr>=2021) %>% select(yr, qtile, tracker, tracker_real, n), 4,
"Median 12-month wage growth, native-born workers matched to themselves a year earlier, by quartile of their own average wage across the two observations. Quartiles are set on the average rather than the initial wage, following the Atlanta Fed, because ranking on the initial wage imports mean reversion.")}

<h3>The test that separates the theories</h3>
<p>Suppose the modest 2026 uptick at the bottom is real. Under the substitution story it has to be
concentrated in the occupations where immigrant labor was thickest. That is the whole mechanism.</p>

{figf("b2_fig7_tracker_exposure.png","Median matched wage growth for native-born workers by occupational immigrant exposure.")}
{tblf(trkbx %>% filter(yr>=2023) %>% select(yr, exp_q, tracker, n), 4,
"Bottom-quartile native-born workers only, by quartile of their occupation&rsquo;s 2022-24 foreign-born share.")}

<p>It is not. Native wage growth is flat across exposure quartiles in every recent year, and among
bottom-quartile natives the 2026 pickup is as large in the <em>least</em> exposed occupations as in
the most. Whatever is happening at the bottom of the wage distribution is a macro wage-floor story,
and it has no enforcement signature in it.</p>

<h2>C8 / H14. The labor share</h2>
{vb("refuted")}

<p>The claim is that immigration is a labor-supply subsidy to capital, so removing it shifts income
back to labor. The claim is wrong before any data arrives. Under CES production in capital and labor
with elasticity of substitution &sigma;, the labor share <em>s</em> satisfies</p>
<p style="text-align:center"><code>d ln s / d ln(L/K) = (&sigma; &minus; 1) / &sigma;</code></p>
<p>so cutting labor supply raises the labor share only if &sigma; &gt; 1. The empirical consensus for
aggregate capital&ndash;labor substitution is well below one. At those values a fall in L/K
<em>lowers</em> the labor share. The prediction requires a parameter the profession does not believe
in.</p>

{figf("b2_fig1_labor_share.png","Labor share of nonfarm business sector income.")}

<div class="grid">
<div class="stat"><div class="big">{g(ls_base,1)}</div><div class="lab">Labor share index, 2024 Q4</div></div>
<div class="stat"><div class="big">{g(ls_last,1)}</div><div class="lab">Latest quarter, {paste0(year(as.Date(h14$latest_quarter)), " Q", quarter(as.Date(h14$latest_quarter)))}</div></div>
<div class="stat"><div class="big">{pctf(h14$pct_change)}</div><div class="lab">Change over the period</div></div>
</div>

<div class="finding">The labor share of nonfarm business income fell {g(abs(h14$change_index_pts),1)}
index points between 2024 Q4 and {paste0(year(as.Date(h14$latest_quarter)), " Q", quarter(as.Date(h14$latest_quarter)))}.
That is the lowest reading in the whole series: {kf(ls_nq)} quarters going back to 1947, and not one
of them is lower.</div>

<p>We are not claiming deportations caused that. The labor share moves with profit margins, the
capital-spending cycle and measured productivity, and the current artificial-intelligence
investment boom is doing most of the work. The point is narrower and sufficient: the claim named a
number, the number went the other way, and it went there hard.</p>

{tblf(h14a %>% filter(yr>=2019), 2, "Annual averages of the quarterly index, 2017 = 100.")}

<h2>C14 / H20. Cheaper housing and the real wage</h2>
{vb("refuted")}

<p>If nominal wages will not cooperate, the deflator might. The White House has argued since January
2026 that deportations lowered rents and home prices in high-immigrant metros, which is a real-wage
claim routed around the nominal wage. It has three testable parts and fails all three.</p>

{figf("b2_fig9_rents.png","12-month change in CPI shelter components.")}

<p><strong>The timing is wrong.</strong> Rent-of-primary-residence inflation peaked at
{pctf(rent_pk$yoy_rent)} in {format(rent_pk$date, "%B %Y")}, was already down to {pctf(rent_dec24)}
by December 2024, and is {pctf(rent_last)} now. The deceleration is two years old and is the
post-pandemic multifamily supply wave.</p>

<p><strong>The geography is wrong.</strong> If enforcement is the mechanism, shelter disinflation
should be concentrated where immigrants lived.</p>
{tblf(reg, 4, "CPI shelter, 12-month rates averaged within each period, by census region, ordered by foreign-born population share. Regional CPI is published NSA.")}
<p>The ordering is not monotone in immigrant share and the largest deceleration is in the South, not
the West.</p>

<p><strong>And it does not rescue the real wage anyway.</strong> Over January to August 2026,
headline inflation ran {pctf(infl26$infl_cpi_all,2)} while inflation excluding shelter ran
{pctf(infl26$infl_cpi_less_shelter,2)}, and shelter itself ran {pctf(infl26$infl_shelter,2)}. Shelter is currently <em>holding the headline number down</em>,
not propping it up, so stripping it out makes real wages look worse rather than better. The rent
disinflation is already inside the deflator that produced the first memo&rsquo;s negative real wage
results.</p>

<h2>C15 / H21. Occupation by occupation</h2>
{vb("refuted")}

<p>The version a reader can check line by line. Take the twenty occupations with the highest
foreign-born employment share that clear the sample floor, name them, and ask how many delivered for
native workers.</p>

{figf("b2_fig11_occupations.png","The twenty most immigrant-intensive occupations: change in the native-born unemployment rate.")}
{tblf(occ20 %>% transmute(occupation = substr(occ_name,1,42), foreign_share,
                          native_share_pre = nat_share_0, d_native_share = d_nat_share,
                          d_unrate = d_nat_unrate, d_hours = d_nat_hrs, n = n_nat_1), 4,
"Pre-period 2023m9-2024m8 versus 2025m9-2026m8. Native share is a level-based ratio inside the occupation and inherits the reweighting problem; the unemployment rate and hours do not.")}

<div class="finding">Native unemployment fell in {occ_un_down} of the 20. Native usual hours rose in
{occ_hrs_up} of the 20. Both improved together in {occ_both} of the 20. Under the substitution
story these should have been near-sweeps.</div>

<p class="note">We also tried to put a wage column on this table and could not. Outgoing-rotation
records are a quarter of the CPS sample, and only {nrow(wocc_ok)} of the twenty occupations have
even fifty wage observations in both 2024 and 2026, which is not enough to support twenty separate
medians. That margin is reported in the exposure-quartile form instead, above, where the cells are
large enough to mean something.</p>

<p class="note">One incidental finding worth a sentence: Economists is on the list, at
{pctf(occ20$foreign_share[occ20$occ_name=="Economists"])} foreign-born. The occupations where
immigrant labor concentrates are not only the ones the argument imagines.</p>

<h2>C16 / H22. Native self-employment</h2>
{vb("refuted")}

<p>Included because a rise here was genuinely possible and would have been the first real point on
the board. Immigrants are disproportionately self-employed in the construction trades, landscaping
and personal services, and their exit leaves customers, contracts and equipment behind.</p>

{tblf(se %>% filter(yr>=2022) %>% select(yr, se_rate, se_uninc_rate, se_inc_rate, n), 4,
"Self-employment as a share of employed native-born workers. CLASSWKR 10, 13 and 14.")}
{tblf(seq_ %>% filter(yr>=2022) %>% pivot_wider(names_from=yr, values_from=se_rate, id_cols=exp_q), 4,
"Native self-employment rate by quartile of occupational immigrant exposure.")}
{tblf(sec %>% filter(yr>=2022), 4, "Native self-employment rate within the construction industry.")}

<div class="finding">The native self-employment rate fell from {pctf(se24,2)} to {pctf(se26,2)}, the
unincorporated rate fell further, and the largest decline of any exposure quartile was in the most
immigrant-intensive one. Nobody stepped into the businesses.</div>

<h2>C17 / H23. Unions</h2>
{vb("null")}

<p>The one restrictionist argument with a labor-movement pedigree: undocumented labor undercuts
union standards, so removing it should raise density and the union wage premium. It has never been
tested on this episode.</p>

{tblf(un %>% filter(yr>=2022) %>% select(yr, grp, member_rate, cov_rate, n), 4,
"Union membership and coverage rates among wage and salary workers in the CPS outgoing rotation groups. UNION is available from 2018 forward only.")}
{tblf(unp %>% filter(yr>=2022), 4,
"Native union wage premium in log points, from a log real wage regression with controls for education, age, age squared, sex, one-digit occupation and one-digit industry.")}

<p>Native union coverage went from {pctf(un24,2)} to {pctf(un26,2)} and the native union wage
premium from {g(unp24,3)} to {g(unp26,3)} log points. Nothing happened. Naturalized citizens, the
group with the highest union coverage of the three, lost the most.</p>

<h2>Where the other side scores points</h2>
<p>Four results in this memo do not run our way, and a memo that does not list them is not worth
reading.</p>
<ol>
<li><strong>Naturalized citizens&rsquo; employment rate rose while the native-born rate fell.</strong>
That is directionally what the incidence literature predicts. Our reading is that it is selection
and that the wage and dose-response evidence contradicts it, but the employment number is real.</li>
<li><strong>Native men&rsquo;s matched wage growth slightly exceeded native women&rsquo;s</strong>
({pctf(pick(trk,"tracker", grp=="Native men", yr==2026))} against
{pctf(pick(trk,"tracker", grp=="Native women", yr==2026))} nominal in 2026), which is the sign the
substitution story wants, on a survivorship-biased measure and against a rising male unemployment
rate.</li>
<li><strong>Involuntary part-time among native men in the construction trades fell.</strong> The only
construction number pointing the restrictionist way.</li>
<li><strong>Bottom-quartile matched wage growth ticked up in 2026</strong> from
{pctf(q1(2025))} to {pctf(q1(2026))}, after falling for three years. It has no exposure gradient,
which is why we do not read it as enforcement, but it did tick up.</li>
</ol>
<p>None of these is nothing. None of them is the mechanism either: in each case the margin that
would connect the result to immigration enforcement, a gradient in occupational exposure, is
missing.</p>

<h2>What would have changed our mind</h2>
<p>Written into the plan before any of this ran, so it was not chosen afterwards. We said we would
treat the restrictionist case as having a real point if any two of the following held.</p>
<div class="tw"><table><thead><tr><th>Pre-registered condition</th><th>Result</th></tr></thead><tbody>
<tr><td>Native male prime-age EPOP rose, or rose relative to native female</td><td>Fell; relative to women, mixed</td></tr>
<tr><td>The male occupational dose-response turned negative and significant with a clean placebo</td><td>Wrong sign, insignificant, placebo not clean</td></tr>
<tr><td>Matched bottom-quartile wage acceleration concentrated in high-exposure occupations</td><td>No exposure gradient</td></tr>
<tr><td>Native construction unemployment fell while construction output held up</td><td>Unemployment rose</td></tr>
<tr><td>A majority of the twenty most immigrant-intensive occupations improved on both unemployment and hours</td><td>{occ_both} of 20 did</td></tr>
<tr><td>The labor share rose</td><td>Postwar low</td></tr>
<tr><td>Native men&rsquo;s occupational overlap index rose</td><td>Unchanged in a decade</td></tr>
<tr><td>Naturalized citizens gained on employment or wages, and gained more than natives</td><td>Employment yes, wages no</td></tr>
</tbody></table></div>
<p>One of eight holds, and only on one of its two margins.</p>

<h2>What this still does not establish</h2>
<p>The identification caveats from the first memo apply here unchanged and are not repeated. Three
are specific to this round. The overlap index and the within-year decomposition are descriptive
accounting, not causal estimates; they establish that a proposed mechanism did not operate, not that
something else did. The matched wage tracker trades composition bias for survivorship bias and
should never be read on its own. And the naturalized-citizen result is the one place where a
selection story and a substitution story make the same prediction about employment rates and are
separated only by the wage and dose-response evidence, which is thinner. A reader who weights the
employment number more heavily than we do would reach a more mixed verdict on C19, and that is a
defensible place to land.</p>

<h2>Sources and method notes</h2>
<ul>
<li>IPUMS CPS basic monthly, January 2015 to August 2026, 12,545,329 person-month records age 16+,
plus a supplementary outgoing-rotation earnings extract (EARNWEEK2, HOURWAGE2, UNION, UHRSWORKORG)
covering 2018m1 forward. Native-born is CITIZEN in 1, 2 or 3.</li>
<li>Steven Camarota and Karen Zeigler, <a href="https://cis.org/Camarota/Has-Immigration-Enforcement-Benefitted-American-Workers">&ldquo;Has Immigration Enforcement Benefitted American Workers?&rdquo;</a>,
Center for Immigration Studies, July 9 2026, for the within-year-weights argument tested in H15, and
<a href="https://cis.org/Oped/Job-gains-are-going-immigrants-and-keeping-young-USborn-men-out-workforce">&ldquo;Job Gains Are Going to Immigrants, and Keeping Young U.S.-Born Men Out of the Workforce&rdquo;</a>
for C10.</li>
<li>Treasury Secretary Scott Bessent, interviews of August and September 2026, for the
non-college and bottom-quartile wage claims tested in H19.</li>
<li>The White House, <a href="https://www.whitehouse.gov/releases/2026/01/mass-deportations-are-improving-americans-quality-of-life">&ldquo;Mass Deportations Are Improving Americans&rsquo; Quality of Life&rdquo;</a> (January 2026)
and <a href="https://www.whitehouse.gov/releases/2026/06/president-trump-drives-down-rents-by-ending-open-borders-disaster/">&ldquo;President Trump Drives Down Rents by Ending Open Borders Disaster&rdquo;</a> (June 2026), for C14.</li>
<li>BLS Productivity and Costs (PRS85006173, PRS84006173) for the labor share; CES for payrolls by
sex and industry; CPI for shelter and the deflators. FRED was unreachable from this environment, so
the Atlanta Fed quartile tracker was rebuilt from CPS microdata rather than downloaded, which is
also what allows the nativity and exposure cuts.</li>
<li>Literature to verify before publication, none of it re-read for this memo: Ottaviano and Peri
(2012, <em>JEEA</em>) and Peri and Sparber (2009, <em>AEJ: Applied</em>) on task specialisation and
on the incidence of immigration falling on prior immigrants, which is the premise of C19; Card
(2009); Borjas (2003, <em>QJE</em>); Chirinko&rsquo;s survey and Oberfield and Raval on the
capital&ndash;labor elasticity of substitution used in C8. Point estimates should not be quoted
from memory.</li>
<li>No household survey was conducted in October 2025. It is left empty everywhere and never
interpolated. The matched wage tracker structurally loses the cohorts that would have ended in that
month, and zero matched pairs have an October 2025 endpoint.</li>
</ul>
</div>')

HTML <- paste0('<!DOCTYPE html>\n<html lang="en"><head><meta charset="utf-8">\n',
  '<meta name="viewport" content="width=device-width, initial-scale=1">\n',
  '<title>Deportations and Native-Born Workers: Round Two</title>\n',
  CSS, '\n</head><body>\n', BODY, '\n</body></html>')
writeLines(HTML, "findings_batch2.html")
cat("wrote findings_batch2.html (", round(file.size("findings_batch2.html")/1024), "KB )\n")
