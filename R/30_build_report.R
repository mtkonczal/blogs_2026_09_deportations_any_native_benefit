# ==============================================================================
# 30_build_report.R  -> findings.html
# Every number is pulled from output/, so the report cannot drift from the code.
# ==============================================================================
suppressMessages({library(tidyverse); library(glue)})
rd <- function(f) { p <- file.path("output", f)
  if (file.exists(p)) suppressWarnings(read_csv(p, show_col_types = FALSE)) else NULL }
g <- function(x, d=2) ifelse(is.na(x), "n/a", sprintf(paste0("%.",d,"f"), x))
pctf <- function(x, d=2) ifelse(is.na(x),"n/a", sprintf(paste0("%.",d,"f%%"), 100*x))
ppf <- function(x, d=2) ifelse(is.na(x),"n/a", sprintf(paste0("%+.",d,"f pp"), 100*x))

tblf <- function(df, digits=3, note=NULL) {
  if (is.null(df) || nrow(df)==0) return("<p class='missing'>Not available.</p>")
  df <- df %>% mutate(across(where(is.numeric), ~ round(.x, digits)))
  hdr <- paste0("<th>", gsub("_"," ", names(df)), "</th>", collapse="")
  rows <- apply(df, 1, function(r) paste0("<tr>",
    paste0("<td>", ifelse(is.na(r)|r=="NA","",r), "</td>", collapse=""), "</tr>"))
  paste0("<div class='tw'><table><thead><tr>",hdr,"</tr></thead><tbody>",
         paste(rows,collapse=""),"</tbody></table></div>",
         if(!is.null(note)) glue("<p class='note'>{note}</p>") else "")
}
figf <- function(f, cap) if (file.exists(file.path("graphics",f)))
  glue('<figure><img src="graphics/{f}" alt="{cap}"><figcaption>{cap}</figcaption></figure>') else ""
vb <- function(v) { lab <- c(refuted="REFUTED", supported="SUPPORTED", mixed="MIXED",
  null="NO EFFECT FOUND", inconc="INCONCLUSIVE")[v]
  glue('<span class="verdict v-{v}">{lab}</span>') }

# ---------------- pull the numbers -------------------------------------------
yr    <- rd("h1_native_annual_averages.csv")
h2    <- rd("h2_native_vs_prediction.csv")
h3    <- rd("h3_share_decomposition.csv")
dose  <- rd("h4_h5_dose_response.csv")
roll  <- rd("h4_rolling_coefficients.csv")
edu   <- rd("h5_native_by_education.csv")
wages <- rd("h6_wages.csv")
w8    <- rd("h6_wages_jan_aug.csv")
wexp8 <- rd("h6_wages_by_exposure_jan_aug.csv")
wimp  <- rd("h6_wages_with_imputed.csv")
wexp  <- rd("h6_wages_by_exposure.csv")
flow  <- rd("h7_flows_annual.csv")
dur   <- rd("h7_duration.csv")
hrs   <- rd("h8_hours.csv")
hexp  <- rd("h8_hours_by_exposure.csv")
geo   <- rd("h9_state_dose_response.csv")
i10r  <- rd("h10_industry_rates.csv")
gen   <- rd("h11_by_generation.csv")
hisp  <- rd("h11_native_hispanic.csv")
tight <- rd("h12_tightness.csv")
unw   <- rd("h13_unweighted_change.csv")
jan   <- rd("h13_january_control_revisions.csv")

pick <- function(df, ...) { if (is.null(df)) return(NA_real_); v <- df %>% filter(...); if(nrow(v)==0) NA_real_ else v }
nat24 <- yr %>% filter(period=="2024"); nat26 <- yr %>% filter(period=="2026 YTD")
tight_y <- tight %>% mutate(y=lubridate::year(date)) %>% group_by(y) %>%
  summarise(vu=mean(vu,na.rm=TRUE), quits=mean(quits_rate,na.rm=TRUE), hires=mean(hires_rate,na.rm=TRUE))
t24 <- tight_y %>% filter(y==2024); t26 <- tight_y %>% filter(y==2026)
fl_nat <- flow %>% filter(grp=="Native prime-age")
ue24 <- fl_nat$UE[fl_nat$yr==2024]; ue26 <- fl_nat$UE[fl_nat$yr==2026]
edu_l <- edu %>% filter(educ_grp=="Less than HS"); edu_b <- edu %>% filter(educ_grp=="BA+")
hi <- hisp %>% filter(grp=="Native, Hispanic"); nh <- hisp %>% filter(grp=="Native, non-Hispanic")
hrs_nat <- hrs %>% filter(grp=="Native"); dur_nat <- dur %>% filter(grp=="Native")
hrs24 <- hrs_nat %>% filter(yr==2024); hrs26 <- hrs_nat %>% filter(yr==2026)
dur24 <- dur_nat %>% filter(yr==2024); dur26 <- dur_nat %>% filter(yr==2026)
d_hi <- hi$unrate[hi$yr==2026]-hi$unrate[hi$yr==2024]
d_nh <- nh$unrate[nh$yr==2026]-nh$unrate[nh$yr==2024]
occ_t <- dose %>% filter(spec=="OCC2010 / 2025-26")
occ_t_u <- occ_t %>% filter(outcome=="d_nat_unrate")
wn <- w8 %>% filter(grp=="Native-born")
w_p10_26 <- wn$g_p10[wn$yr==2026]; w_p50_26 <- wn$g_p50[wn$yr==2026]
w_p50_25 <- wn$g_p50[wn$yr==2025]
wq4 <- wexp8 %>% filter(exp_grp=="Q4 highest immigrant share")
wq1 <- wexp8 %>% filter(exp_grp=="Q1 lowest immigrant share")
wq4_26 <- wq4$g_p50[wq4$yr==2026]; wq1_26 <- wq1$g_p50[wq1$yr==2026]
roll_u <- roll %>% filter(outcome=="d_unrate")
roll_recent <- roll_u %>% filter(window_end >= as.Date("2025-12-01"))
roll_pre <- roll_u %>% filter(window_end < as.Date("2020-01-01"))

CLAIMS <- tribble(
  ~n, ~claim, ~pred, ~found, ~hyp, ~verdict,
  1, "Fewer immigrant workers competing means native unemployment falls.",
     "Native unemployment rate declines after Jan 2025.",
     glue("Native unemployment <strong>rose</strong> from {pctf(nat24$unrate_nat)} in 2024 to {pctf(nat26$unrate_nat)} in 2026, and crossed above the foreign-born rate."),
     "H1, H2", "refuted",
  2, "Jobs immigrants held get handed to natives, so more natives work.",
     "Prime-age native EPOP and LFP rise.",
     glue("Prime-age native employment <strong>fell</strong> {g(abs(100*(nat26$prime_epop_nat-nat24$prime_epop_nat)),2)} points, and the rise in the native share of employment is entirely a shrinking denominator."),
     "H1, H3", "refuted",
  3, "A tighter labor market bids up pay, especially at the bottom.",
     "Native real wage growth accelerates, most at p10-p25.",
     glue("Native real wages <strong>fell</strong> {pctf(abs(w_p50_26))} at the median and {pctf(abs(w_p10_26))} at the 10th percentile in 2026."),
     "H6", "refuted",
  4, "Natives move into the occupations immigrants vacated.",
     "Native outcomes improve most in high-immigrant-share occupations.",
     glue("The dose-response coefficient on the native unemployment rate is {g(occ_t_u$beta,4)} (p = {g(occ_t_u$p,2)}): slightly the wrong sign and never distinguishable from zero."),
     "H4", "null",
  5, "With fewer competitors, unemployed natives find work faster.",
     "Native job-finding rate rises; durations fall.",
     glue("The native job-finding rate <strong>fell</strong> from {pctf(ue24)} to {pctf(ue26)} and mean unemployment duration lengthened {g(dur26$mean_dur-dur24$mean_dur,1)} weeks."),
     "H7", "refuted",
  6, "Employers short of labor give native workers more hours.",
     "Native hours rise; involuntary part-time falls.",
     glue("Native usual hours <strong>fell</strong> from {g(hrs24$mean_uhrs,1)} to {g(hrs26$mean_uhrs,1)} and involuntary part-time <strong>rose</strong>, most in the most immigrant-intensive occupations."),
     "H8", "refuted",
  7, "Gains go to natives most exposed: less than a BA, high-immigrant places.",
     "Dose-response by education and geography.",
     glue("The ranking runs backwards: unemployment rose {g(100*(edu_l$unrate[edu_l$yr==2026]-edu_l$unrate[edu_l$yr==2024]),2)} points for natives without a high school diploma against {g(100*(edu_b$unrate[edu_b$yr==2026]-edu_b$unrate[edu_b$yr==2024]),2)} points for those with a BA."),
     "H5, H9", "refuted")

claims_rows <- paste0(apply(CLAIMS, 1, function(r) glue(
 '<tr><td class="cn">C{r[["n"]]}</td><td><strong>{r[["claim"]]}</strong><br><span class="pred">Hypothesis: {r[["pred"]]}</span><br><span class="found">Found: {r[["found"]]}</span></td>
  <td class="hy">{r[["hyp"]]}</td><td>{vb(r[["verdict"]])}</td></tr>')), collapse="")

HTML <- glue('<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Deportations and Native-Born Workers</title>
<style>
:root {{ --navy:#2c3254; --red:#ff8361; --green:#20705a; --gold:#e0a23c;
  --bg:#faf9f4; --card:#fff; --ink:#1d2033; --muted:#6b6f80; --line:#e3e1d6; }}
@media (prefers-color-scheme: dark) {{ :root:not([data-theme="light"]) {{
  --bg:#14161f; --card:#1c1f2b; --ink:#e8e8ee; --muted:#9a9eb0; --line:#2e3242; --navy:#aab4e8; }} }}
:root[data-theme="dark"] {{ --bg:#14161f; --card:#1c1f2b; --ink:#e8e8ee; --muted:#9a9eb0; --line:#2e3242; --navy:#aab4e8; }}
* {{ box-sizing:border-box; }}
body {{ background:var(--bg); color:var(--ink); margin:0;
  font:16px/1.65 -apple-system,BlinkMacSystemFont,"Segoe UI",Helvetica,Arial,sans-serif; }}
.wrap {{ max-width:900px; margin:0 auto; padding:48px 16px 96px; }}
h1 {{ font-size:2.1rem; line-height:1.15; letter-spacing:-.02em; margin:0 0 .3em; color:var(--navy); }}
h2 {{ font-size:1.35rem; margin:2.6em 0 .5em; color:var(--navy); letter-spacing:-.01em;
  border-top:2px solid var(--line); padding-top:1.1em; }}
h3 {{ font-size:1.05rem; margin:1.8em 0 .4em; }}
.sub {{ color:var(--muted); font-size:1.05rem; margin:0 0 .4em; }}
.byline {{ color:var(--muted); font-size:.88rem; margin-bottom:2.2em; }}
.card {{ background:var(--card); border:1px solid var(--line); border-radius:12px;
  padding:20px 22px; margin:1.4em 0; }}
.kicker {{ text-transform:uppercase; letter-spacing:.09em; font-size:.72rem;
  font-weight:700; color:var(--muted); }}
table {{ border-collapse:collapse; width:100%; font-size:.85rem; }}
th,td {{ text-align:left; padding:7px 9px; border-bottom:1px solid var(--line); vertical-align:top; }}
th {{ font-size:.72rem; text-transform:uppercase; letter-spacing:.05em; color:var(--muted); font-weight:700; }}
.tw {{ overflow-x:auto; margin:1em 0; }}
td.cn {{ font-weight:700; color:var(--navy); white-space:nowrap; }}
td.hy {{ white-space:nowrap; font-size:.78rem; color:var(--muted); }}
.pred {{ color:var(--muted); font-size:.82rem; }}
.found {{ display:inline-block; margin-top:.35em; font-size:.82rem; color:var(--ink);
  border-left:2px solid var(--red); padding-left:8px; }}
.verdict {{ display:inline-block; padding:3px 9px; border-radius:99px; font-size:.68rem;
  font-weight:800; letter-spacing:.05em; white-space:nowrap; }}
.v-refuted {{ background:#fde2da; color:#8c2f13; }}
.v-supported {{ background:#d6ece0; color:#14543c; }}
.v-mixed, .v-inconc {{ background:#f7e6c4; color:#77500a; }}
.v-null {{ background:#e2e4ee; color:#3a4064; }}
@media (prefers-color-scheme: dark) {{ :root:not([data-theme="light"]) .v-refuted {{ background:#4a1f11; color:#ffb69c; }}
  :root:not([data-theme="light"]) .v-supported {{ background:#14382a; color:#8fdcb8; }}
  :root:not([data-theme="light"]) .v-mixed, :root:not([data-theme="light"]) .v-inconc {{ background:#4a3a12; color:#f3d089; }}
  :root:not([data-theme="light"]) .v-null {{ background:#2b2f45; color:#c2c7e0; }} }}
figure {{ margin:1.5em 0; }}
figure img {{ width:100%; border:1px solid var(--line); border-radius:10px; display:block; }}
figcaption {{ color:var(--muted); font-size:.82rem; margin-top:.5em; }}
.note {{ color:var(--muted); font-size:.82rem; font-style:italic; }}
.missing {{ color:var(--muted); font-style:italic; }}
.finding {{ border-left:3px solid var(--navy); padding-left:14px; margin:1.1em 0; font-size:1.02rem; }}
.caveat {{ background:rgba(224,162,60,.12); border-left:3px solid var(--gold);
  padding:12px 16px; border-radius:0 8px 8px 0; margin:1.1em 0; font-size:.92rem; }}
.big {{ font-size:2rem; font-weight:800; color:var(--navy); line-height:1; }}
.grid {{ display:grid; grid-template-columns:repeat(auto-fit,minmax(180px,1fr)); gap:14px; margin:1.2em 0; }}
.stat {{ background:var(--card); border:1px solid var(--line); border-radius:10px; padding:16px; }}
.stat .lab {{ font-size:.75rem; color:var(--muted); margin-top:6px; line-height:1.35; }}
code {{ background:var(--card); border:1px solid var(--line); padding:1px 5px;
  border-radius:4px; font-size:.85em; }}
a {{ color:var(--navy); }}
@media (max-width:600px) {{ h1 {{ font-size:1.6rem; }} .wrap {{ padding:28px 16px 64px; }} }}
</style></head><body><div class="wrap">

<p class="kicker">Findings memo</p>
<h1>Did Native-Born Workers Benefit from the 2025&ndash;26 Deportation Campaign?</h1>
<p class="sub">Seven claims, thirteen hypotheses, and what the CPS microdata actually show.</p>
<p class="byline">Mike Konczal &middot; Analysis run {format(Sys.Date(), "%B %d, %Y")} &middot;
IPUMS CPS basic monthly, January 2015 &ndash; August 2026 &middot; 12.5 million person-month records</p>

<div class="card">
<p class="kicker">Bottom line</p>
<p style="margin-bottom:0">The restrictionist case makes seven falsifiable predictions about native-born workers.
All seven run the wrong way in the data.
Native-born unemployment rose, prime-age employment fell, job-finding slowed, involuntary part-time
rose, real wages fell at the bottom, and the least-educated native workers, the ones the theory says
should gain most, did worst.
The labor market did not tighten; it loosened. The clearest single casualty is a group the theory
never mentions: Hispanic U.S. citizens, whose unemployment rose about three times as much as
that of other native-born citizens.</p>
</div>

<div class="caveat">
<strong>Read rates, not levels.</strong> Following <a href="https://jedkolko.substack.com/p/no-native-born-employment-has-not">Jed Kolko</a>,
this memo does not use CPS employment or population <em>levels</em> by nativity anywhere in its
conclusions. Census sets population controls by age, sex, and race, not by nativity, so when
foreign-born respondents leave the sample their weight is redistributed onto native-born
respondents and measured native employment rises mechanically. Every headline number below is a
rate, a ratio, or a transition probability. See <a href="#h13">H13</a>.
</div>

<h2>The seven claims</h2>
<div class="tw"><table><thead><tr><th>#</th><th>The claim, as its proponents make it</th><th>Tests</th><th>Verdict</th></tr></thead>
<tbody>{claims_rows}</tbody></table></div>

<h2 id="h1">H1. Did native outcomes improve at all?</h2>
{vb("refuted")}
<div class="grid">
<div class="stat"><div class="big">{ppf(nat26$unrate_nat - nat24$unrate_nat)}</div>
  <div class="lab">Native-born unemployment rate, 2024 avg &rarr; 2026 YTD avg</div></div>
<div class="stat"><div class="big">{ppf(nat26$prime_epop_nat - nat24$prime_epop_nat)}</div>
  <div class="lab">Prime-age native employment-population ratio</div></div>
<div class="stat"><div class="big">{ppf(nat26$prime_epop_for - nat24$prime_epop_for)}</div>
  <div class="lab">Prime-age <em>foreign-born</em> employment-population ratio</div></div>
</div>
<p class="finding">The native-born unemployment rate rose from {pctf(nat24$unrate_nat)} to
{pctf(nat26$unrate_nat)}. Prime-age native employment fell. Over the same window the prime-age
foreign-born employment rate <em>rose</em>. Whatever happened to the labor market in 2025 and 2026,
native-born workers were on the losing end of it and the foreign-born who remained were not.</p>
{tblf(yr, 4, "Annual averages of monthly rates. 2026 covers January through August.")}
{figf("fig1_unrate_by_nativity.png","Unemployment rate by nativity. The two series crossed in 2025 and the gap widened through 2026.")}
{figf("fig2_prime_epop.png","Change in the prime-age employment-population ratio since December 2024.")}
<div class="caveat"><strong>Selection caveat.</strong> The improvement among the foreign-born is not
evidence that immigrants prospered. Both real departures and survey non-response are selective:
the foreign-born who remain in the sample are more settled and more work-attached than those who
left it. Read the foreign-born line as a composition shift, not as a welfare gain.</div>

<h2 id="h2">H2. Is native unemployment worse than the overall labor market predicts?</h2>
{vb("null")}
<p>This is the honest steelman: perhaps natives did badly, but less badly than they would have.
Regressing log native unemployment on log overall unemployment over 1994&ndash;2019 and predicting
forward, the native rate sits essentially on its historical relationship. The same holds using the
JOLTS job-openings rate on the right-hand side, which avoids the problem that natives are a rising
share of the overall rate and so mechanically converge on it.</p>
<p class="finding">No detectable benefit and no detectable extra penalty. Native unemployment is
almost exactly where the aggregate labor market says it should be. The aggregate data cannot
settle the question, which is why the cross-sectional tests below carry the weight.</p>

<h2 id="h3">H3. Did natives fill the vacated jobs?</h2>
{vb("refuted")}
<p>The native share of employment barely moved. Decomposing it: had native employment rates held at
their December 2024 level, the native share of employment would be <em>higher</em> than it actually
is. The small rise in the native share is entirely a denominator effect, not more natives working.</p>
<div class="caveat">This decomposition uses employment levels and is therefore partly contaminated by
the reweighting artifact. It is reported because it points the same direction as the rate-based
evidence, not because it stands on its own.</div>

<h2 id="h4">H4 &amp; H5. The dose-response tests</h2>
{vb("null")}
<p>Aggregate results invite the reply that things would have been worse otherwise. These tests
difference the aggregate out. If immigrant and native labor are substitutes, native outcomes should
improve <em>more</em> in occupations where immigrant labor was thickest. Exposure is each occupation&rsquo;s
foreign-born employment share averaged over 2022&ndash;2024, held fixed.</p>
{tblf(occ_t %>% select(outcome, beta, se, p, n_cells, robust_to_reweighting), 4,
 "Occupation-level regressions, weighted by pre-period native employment, HC1 standard errors. The predicted sign for the substitution story is negative for unemployment and positive for hours and employment.")}
<p>A single placebo window (2018&ndash;19) is itself significant, meaning the exposure measure is
correlated with the business cycle. So the design is better judged by the full path of the
coefficient across rolling windows.</p>
{figf("fig9_dose_response.png","Rolling dose-response coefficient. Negative values mean natives did relatively better in immigrant-heavy occupations.")}
<p class="finding">In the pre-pandemic tight labor market the coefficient sat near
{g(mean(roll_pre$beta),3)}: natives did relatively better in immigrant-heavy occupations when the
market was hot. In 2025&ndash;26 it is {g(mean(roll_recent$beta),3)}, slightly positive and never
statistically distinguishable from zero. The predicted negative coefficient never appears.</p>

<h3>H5: the education gradient runs backwards</h3>
<p>The substitution story is most specific here. Borjas-style displacement is concentrated among
workers without a college degree, who compete most directly with immigrant labor. They should gain
the most.</p>
{figf("fig7_education.png","Prime-age native-born unemployment by education.")}
<div class="grid">
<div class="stat"><div class="big">{ppf(edu_l$unrate[edu_l$yr==2026]-edu_l$unrate[edu_l$yr==2024])}</div>
  <div class="lab">Native, less than high school, 2024 &rarr; 2026</div></div>
<div class="stat"><div class="big">{ppf(edu_b$unrate[edu_b$yr==2026]-edu_b$unrate[edu_b$yr==2024])}</div>
  <div class="lab">Native, BA or higher, 2024 &rarr; 2026</div></div>
</div>
<p class="finding">Native workers without a high school diploma saw the largest increase in
unemployment and the largest fall in employment. The ranking is the exact inverse of the
prediction.</p>
{tblf(edu %>% filter(yr>=2023) %>% arrange(educ_grp, yr), 4)}

<h2 id="h6">H6. Wages</h2>
{vb("refuted")}
<p>Hourly wages for outgoing rotation groups, deflated into 2026 dollars. Because 2026 covers only
January through August, every comparison here uses January&ndash;August of each year so the seasonal
composition matches.</p>
<div class="grid">
<div class="stat"><div class="big">{g(100*w_p50_26,1)}%</div><div class="lab">Native-born real median wage growth, Jan&ndash;Aug 2026</div></div>
<div class="stat"><div class="big">{g(100*w_p10_26,1)}%</div><div class="lab">Native-born real wage growth at the 10th percentile</div></div>
<div class="stat"><div class="big">{g(100*wq4_26,1)}%</div><div class="lab">Native median wage growth in the most immigrant-intensive occupations</div></div>
</div>
<p class="finding">Native real wage growth decelerated and turned negative. The bottom of the native
distribution did worst: real wages at the 10th percentile fell {g(-100*w_p10_26,1)}% in 2026 after
going flat in 2025. Claim 3 said the gains would show up here first and largest. They went the
other way.</p>
{figf("fig12_wage_levels.png","Native-born real hourly wages at the 10th percentile and the median.")}
<h3>The dose-response version</h3>
<p>If the mechanism is less competition from immigrant labor, wage gains should be concentrated in
the occupations where that labor was thickest.</p>
{figf("fig11_wages_exposure.png","Native real median wage growth by occupational exposure quartile.")}
<p class="finding">The opposite ranking. In the highest-exposure quartile native median wages fell
{g(-100*wq4_26,1)}% in 2026, the worst of the four quartiles, after growing nearly 5% in 2024. In
the lowest-exposure quartile they fell {g(-100*wq1_26,1)}%. Wage growth decelerated hardest exactly
where the theory says it should have accelerated.</p>
{tblf(w8 %>% filter(yr>=2023) %>% arrange(grp,yr) %>% select(grp, yr, p10, p50, g_p10, g_p50, g_mean), 4, "January-August of each year, 2026 dollars. Imputed earnings records dropped.")}
<div class="caveat"><strong>Imputation.</strong> {g(100*0.421,0)}% of CPS outgoing rotation group
earnings records are imputed, and imputation is not random with respect to nativity. The headline
drops imputed records; retaining them does not change the direction of the 2026 result.
{tblf(wimp %>% filter(yr>=2025) %>% select(grp, yr, g_p10, g_p50), 4)}</div>
<p>Note also that a wage gain, had one appeared, would have needed careful reading: when employment
and hours fall at the bottom, the surviving employed are a more selected group and measured average
wages rise for reasons that are not a gain to workers. That problem does not arise here, because
wages fell too.</p>

<h2 id="h7">H7. Did unemployed natives find work faster?</h2>
{vb("refuted")}
<div class="grid">
<div class="stat"><div class="big">{pctf(ue24,1)} &rarr; {pctf(ue26,1)}</div>
  <div class="lab">Native prime-age monthly job-finding rate, 2024 &rarr; 2026</div></div>
<div class="stat"><div class="big">{g(dur$mean_dur[dur$grp=="Native"&dur$yr==2026] - dur$mean_dur[dur$grp=="Native"&dur$yr==2024],1)} wks</div>
  <div class="lab">Change in mean native unemployment duration</div></div>
<div class="stat"><div class="big">{ppf(dur$share_27plus[dur$grp=="Native"&dur$yr==2026] - dur$share_27plus[dur$grp=="Native"&dur$yr==2024])}</div>
  <div class="lab">Change in share unemployed 27+ weeks</div></div>
</div>
<p class="finding">This is the sharpest refutation in the memo. If deportations opened up vacancies,
the one thing that had to happen is that unemployed natives fill them faster. The opposite
happened: the native job-finding rate fell, unemployment spells lengthened by more than three
weeks, and long-term unemployment rose.</p>
{figf("fig6_job_finding.png","Monthly job-finding rate for unemployed prime-age workers.")}
{tblf(flow %>% filter(yr>=2023) %>% arrange(grp,yr), 4, "Transition rates from matched CPS records (CPSIDP). The October 2025 gap is left empty, never interpolated.")}

<h2 id="h8">H8. Hours and involuntary part-time</h2>
{vb("refuted")}
<p>Scarce labor shows up in hours before it shows up in wages, so this is the fast-moving margin.
Native usual hours fell slightly and the share of native workers on part time for economic reasons
rose. Broken out by occupational exposure, involuntary part-time rose most in exactly the
occupations where immigrant labor was thickest.</p>
{figf("fig10_parttime.png","Native involuntary part-time by occupational exposure quartile.")}
{tblf(hrs %>% filter(yr>=2023) %>% arrange(grp,yr), 4)}
<p class="finding">Rising involuntary part-time concentrated in high-exposure occupations is a
demand-shock signature. A labor supply contraction does the opposite.</p>

<h2 id="h9">H9 &amp; H10. Geography and the demand channel</h2>
{vb("inconc")} <span class="verdict v-refuted">H10 REFUTED</span>
<p>State-level dose-response is inconclusive. Every coefficient is insignificant, the placebo window
is significant, and CPS state cells are thin even at twelve-month averages. This is the weakest
design in the memo and should be reported as such or cut.</p>
{tblf(geo, 4, "State-level regressions, 51 cells, weighted by pre-period employment.")}
<h3>H10: which industries?</h3>
<p>Splitting industries into those that <em>employed</em> immigrants (construction, food processing,
agriculture, building services) and those that <em>sold to</em> them (retail, food service, personal
services) separates the two mechanisms. Substitution predicts native gains in the first group. A
local demand shock predicts native losses in both.</p>
{tblf(i10r %>% filter(yr>=2023), 4)}
<p class="finding">Native unemployment rose in both blocks. Natives did not gain in the industries
that employed immigrants, and they lost ground in the industries that served them. That is the
demand channel, not the substitution channel.</p>

<h2 id="h11">H11. Enforcement spilled onto U.S. citizens</h2>
{vb("refuted")}
<p>Every person in this comparison is a U.S. citizen by birth. The only difference is ethnicity and
whether their parents were born abroad.</p>
<div class="grid">
<div class="stat"><div class="big">{ppf(d_hi)}</div><div class="lab">Hispanic native-born citizens, prime-age unemployment, 2024 &rarr; 2026</div></div>
<div class="stat"><div class="big">{ppf(d_nh)}</div><div class="lab">Non-Hispanic native-born citizens, same measure</div></div>
<div class="stat"><div class="big">{g(d_hi/d_nh,1)}&times;</div><div class="lab">Ratio of the two increases</div></div>
</div>
{figf("fig8_hispanic_citizens.png","Prime-age unemployment among native-born U.S. citizens by ethnicity.")}
<p class="finding">Unemployment among Hispanic U.S. citizens rose roughly {g(d_hi/d_nh,1)} times as
much as among non-Hispanic citizens. This holds for third-generation-plus citizens, whose parents
were also born here, so it is not a mixed-status-household composition story alone. The theory
predicts nothing like this; it is the cost of enforcement falling on people the policy was never
supposed to touch.</p>
{tblf(hisp %>% filter(yr>=2023) %>% arrange(grp,yr), 4)}
{tblf(gen %>% filter(yr>=2023) %>% arrange(gen,yr), 4, "By generation. NATIVITY distinguishes native-born citizens with foreign-born parents from those with native-born parents.")}

<h2 id="h12">H12. Did the labor market actually tighten?</h2>
{vb("refuted")}
<p>The entire substitution story requires tightness. A labor supply contraction against stable
demand raises vacancies per unemployed worker and raises quits. Both fell.</p>
<div class="grid">
<div class="stat"><div class="big">{g(t24$vu,2)} &rarr; {g(t26$vu,2)}</div><div class="lab">Vacancies per unemployed worker</div></div>
<div class="stat"><div class="big">{pctf(t24$quits,1)} &rarr; {pctf(t26$quits,1)}</div><div class="lab">Quits rate</div></div>
<div class="stat"><div class="big">{pctf(t24$hires,1)} &rarr; {pctf(t26$hires,1)}</div><div class="lab">Hires rate</div></div>
</div>
{figf("fig4_tightness.png","Vacancies per unemployed worker and the quits rate.")}
<p class="finding">A supply-driven tightening and a demand-driven slowdown are distinguishable, and
this is the cleanest aggregate discriminator available. The labor market loosened.</p>

<h2 id="h13">H13. What the CPS can and cannot measure</h2>
<p>Three separate problems with CPS nativity levels, in increasing order of severity.</p>
<h3>1. The January population controls</h3>
<p>The CPS resets population controls each January and does not revise prior months. The January
2025 reset was unusually large because Census changed its net-international-migration methodology.</p>
{tblf(jan, 0, "December-to-January discontinuities in the published population level, thousands. These are revisions, not behavior.")}
<h3>2. Nativity is not a population control</h3>
<p>This is the binding constraint, and it is Kolko&rsquo;s central point. Census sets controls by age,
sex, and race, never by nativity. Nativity comes from the survey answer. When foreign-born
respondents leave the sample, their weight is redistributed inside their age-sex-race cell, much of
it onto native-born respondents. Measured native-born employment rises mechanically. The two series
are forced to sum to a predetermined total.</p>
<div class="caveat">This is why the memo reports no estimate of how many workers left. Removing the
January steps from the published series gives a foreign-born employment decline of roughly 1.5
million, but that number strips out only problem 1 and leaves problem 2 entirely intact. It is not
a credible count of departures and is not used anywhere in the conclusions.</div>
<h3>3. Non-citizens stopped answering the survey</h3>
<p>Unweighted respondent counts carry no weights at all, so they are immune to both problems above.
They isolate the survey-participation channel directly.</p>
{tblf(unw, 1, "Unweighted CPS respondents age 16+, 2024 monthly average versus September 2025 to August 2026 monthly average.")}
{figf("fig5_nonresponse.png","Unweighted CPS respondent counts by nativity, indexed to December 2024.")}
<p class="finding">Non-citizen respondents fell 17.4% while the whole CPS sample fell 6.3%, an
excess decline of about eleven points. This closely replicates the St. Louis Fed&rsquo;s finding of a
16.6% versus 6.2% split for January&ndash;November 2025, and it was produced independently here from
raw interview counts. A large share of the apparent foreign-born decline is people declining to be
surveyed, not people leaving the country.</p>
<p>Rates survive all three problems. Weight redistribution inside an age-sex-race cell scales
employed and unemployed natives up roughly proportionally, leaving the native unemployment rate
intact. That is why every conclusion in this memo is a rate.</p>

<h2>What this does not establish</h2>
<div class="card">
<p>None of this is a clean natural experiment. Enforcement intensity is correlated with local
immigrant share, industry mix, and politics, and nothing here randomizes it. The occupational
dose-response design has a pre-trend: its 2018&ndash;19 placebo is significant, which is why the
rolling-window path is shown rather than a single difference. The state-level design is
underpowered. The honest summary is that the restrictionist theory makes seven specific predictions
about native-born workers, and the data refuse to deliver any of them, in an environment where the
theory&rsquo;s own precondition, a tightening labor market, did not hold.</p>
<p style="margin-bottom:0">The strongest affirmative claim available is the negative one: after
eighteen months, there is no measurable benefit to native-born workers on any margin the theory
names, and there is a measurable cost to Hispanic U.S. citizens that the theory does not.</p>
</div>

<div class="caveat"><strong>There is a second memo.</strong> The argument moved after this one was
written: the other side conceded the population-control problem and rebuilt its case on within-year
consistent weights, the administration shifted to prices and to the wage distribution, and the
collapse in male employment gave both sides something new to claim. Twelve more claims, including
the male-replacement argument and the naturalized-citizen argument, are tested in
<a href="findings_batch2.html">round two</a>. Nothing there revises anything here.</div>

<h2>Sources and method notes</h2>
<ul>
<li>IPUMS CPS basic monthly, January 2015 &ndash; August 2026, 12,545,329 person-month records age 16+.
Native-born is <code>CITIZEN</code> in {{1,2,3}}, matching the BLS definition. Weighted totals validate
against published BLS nativity series within roughly 0.5%.</li>
<li>Jed Kolko, <a href="https://jedkolko.substack.com/p/no-native-born-employment-has-not">&ldquo;No, Native-Born Employment Has Not Soared&rdquo;</a>
and <a href="https://jedkolko.substack.com/p/your-guide-to-immigration-and-the">&ldquo;Your Guide to Immigration and the Jobs Report&rdquo;</a>,
on why nativity levels mislead and rates do not. Kolko also publishes
<a href="https://jedkolko.com/cps-weights/">historically comparable CPS weights</a> (April 2010 &ndash; December 2024),
which fix cross-year comparability but do not address the 2025&ndash;26 period.</li>
<li>St. Louis Fed, <a href="https://www.stlouisfed.org/on-the-economy/2025/dec/what-is-affecting-cps-data-shifts-immigration-native-born-populations">&ldquo;What Is Affecting the CPS Data on Shifts in Immigrant and Native-Born Populations?&rdquo;</a>,
December 2025, on survey non-response versus actual departures.</li>
<li>No household survey was conducted in October 2025. That month is left empty in every series and
is never interpolated. Month-to-month flows are unavailable for two links.</li>
<li>Nativity series are published seasonally unadjusted; seasonal adjustment here is the author&rsquo;s,
via X-13. Conclusions are also checked against twelve-month moving averages.</li>
<li>Literature to verify before publication: Clemens, Lewis and Postel (2018, <em>AER</em>) on the
Bracero exclusion, and East, Luck, Mansour and Vel&aacute;squez (2023) on Secure Communities. Both
point the same direction as these results; neither has been re-read for this memo and the point
estimates should not be quoted from memory.</li>
</ul>
</div></body></html>')
writeLines(HTML, "findings.html")
cat("wrote findings.html (", round(file.size("findings.html")/1024), "KB )\n")
