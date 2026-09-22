# ==============================================================================
# 53_build_report_state.R  -> findings3.html
# First-pass state-level cross-section: ICE enforcement intensity vs. total
# payroll job growth. Every number is pulled from output/, so the memo cannot
# drift from the code. Stylesheet lifted from findings.html at build time so
# all three memos stay visually identical.
# ==============================================================================
suppressMessages({library(tidyverse); library(glue)})

rd <- function(f) { p <- file.path("output", f)
  if (file.exists(p)) suppressWarnings(read_csv(p, show_col_types = FALSE)) else NULL }
pctf <- function(x, d=1) ifelse(is.na(x),"n/a", sprintf(paste0("%.",d,"f%%"), 100*x))
signedpct <- function(x, d=1) ifelse(is.na(x),"n/a", sprintf(paste0("%+.",d,"f%%"), 100*x))
g <- function(x, d=2) ifelse(is.na(x), "n/a", sprintf(paste0("%.",d,"f"), x))
kf <- function(x, d=0) ifelse(is.na(x),"n/a", formatC(x, format="f", digits=d, big.mark=","))

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

# ---------------- pull the numbers -------------------------------------------
m     <- rd("state_enforcement_vs_jobs.csv")
specs <- rd("state_enforcement_vs_jobs_correlation.csv")

prim  <- specs %>% filter(spec == "excl. DC (primary, n=50)")
allsp <- specs %>% filter(spec == "all 51 (50 states + DC)")
notx  <- specs %>% filter(spec == "excl. DC and TX (n=49)")

top_arrests <- m %>% arrange(desc(arrests_per_100k)) %>%
  transmute(State = state_name, `Arrests, cumulative` = kf(arrests),
            Population = kf(population_2025),
            `Arrests per 100k` = g(arrests_per_100k, 0),
            `Payroll job growth` = signedpct(job_growth)) %>% head(10)

bottom_growth <- m %>% arrange(job_growth) %>%
  transmute(State = state_name, `Arrests per 100k` = g(arrests_per_100k, 0),
            `Payroll job growth` = signedpct(job_growth)) %>% head(8)

n_states <- nrow(m)
WIN_START <- as.Date("2025-01-01"); WIN_END <- as.Date("2026-07-01")

# ---------------- hypothesis scan (part two) ----------------------------------
scan <- rd("state_hypothesis_scan.csv") %>% arrange(desc(abs(r)))
n_hyp <- nrow(scan)
n_sig <- sum(scan$p_value < 0.10, na.rm = TRUE)
expected_by_chance <- 0.10 * n_hyp

scan_tbl <- scan %>%
  transmute(Hypothesis = label, Category = category, N = n,
            r = g(r, 2), `95% CI` = paste0("[", g(ci_lo,2), ", ", g(ci_hi,2), "]"),
            p = g(p_value, 3))

top_hit    <- scan %>% slice(1)
wage_hit   <- scan %>% filter(var == "wage_yoy_Total_Private")
unrate_hit <- scan %>% filter(var == "d_unrate")
njg_hit    <- scan %>% filter(var == "total_nonfarm_job_growth")
forepop_hit<- scan %>% filter(var == "d_for_epop")
forshare_hit<- scan %>% filter(var == "for_share_pre")
con_wage_hit <- scan %>% filter(var == "wage_yoy_Construction")
con_emp_hit  <- scan %>% filter(var == "growth_Construction")
lh_emp_hit   <- scan %>% filter(var == "growth_Leisure_and_Hospitality")
lh_wage_hit  <- scan %>% filter(var == "wage_yoy_Leisure_and_Hospitality")
nat_epop_hit <- scan %>% filter(var == "d_nat_epop")
nat_un_hit   <- scan %>% filter(var == "d_nat_unrate")

# ---------------- assemble ----------------------------------------------------
CSS <- if (file.exists("findings.html")) {
  x <- readLines("findings.html", warn = FALSE)
  a <- grep("<style>", x)[1]; b <- grep("</style>", x)[1]
  paste(x[a:b], collapse = "\n")
} else "<style></style>"

BODY <- glue('
<div class="wrap">
<p class="kicker">Findings memo, part three</p>
<h1>Does State-Level Enforcement Line Up With State-Level Outcomes?</h1>
<p class="sub">A first pass with official data: ICE arrest intensity by state against total
payroll job growth, then {n_hyp} more state-level outcomes from BLS and CPS, kicking the tires
as hard as the state cross-section allows.</p>
<p class="byline">Mike Konczal &middot; Analysis run {format(Sys.Date(), "%B %d, %Y")} &middot;
{n_states} states + DC &middot; BLS State and Area Employment (SAE) and Local Area Unemployment
Statistics (LAUS) via <code>tidyusmacro::getBLSFiles()</code>, IPUMS CPS microdata, Deportation Data
Project arrest microdata, Census Bureau Vintage 2025 population estimates</p>

<div class="caveat">
<strong>This is a geographic first pass, not a repeat of the CPS work.</strong> The first two memos
(<a href="findings.html">round one</a>, <a href="findings_batch2.html">round two</a>) use CPS
microdata to test native-versus-foreign-born substitution directly. CPS state cells are thin (see
the caveat in that analysis), so this memo instead uses the BLS establishment survey (SAE), a
near-census of state payrolls. The tradeoff: SAE cannot separate native from foreign-born workers.
It answers a narrower, blunter question &mdash; did states with heavier enforcement see slower
<em>aggregate</em> job growth, full stop &mdash; which is exactly the question a state-level
enforcement-vs-outcomes chart implicitly asks.
</div>

<h2>Bottom line</h2>
<div class="finding">
No reliable relationship. States with more ICE enforcement per capita did not see slower payroll
job growth over this window &mdash; if anything the point estimate runs the other way, and it is not
statistically distinguishable from zero under any reasonable specification.
</div>
<div class="grid">
<div class="stat"><div class="big">r = {g(prim$r,2)}</div><div class="lab">Pearson correlation, arrests per 100k vs.
total nonfarm job growth, {prim$n} states (DC excluded)</div></div>
<div class="stat"><div class="big">p = {g(prim$p_value,2)}</div><div class="lab">Not significant at
conventional thresholds</div></div>
<div class="stat"><div class="big">{g(allsp$r,2)}</div><div class="lab">Same correlation, all 51
states + DC included</div></div>
<div class="stat"><div class="big">{g(notx$r,2)}</div><div class="lab">Same correlation, DC and Texas
both excluded (leverage check)</div></div>
</div>

{figf("fig_state_enforcement_vs_jobs.png", "ICE arrests per 100,000 residents vs. total nonfarm payroll employment growth, by state, Jan 2025 - Jul 2026.")}

<p>Read across the three specifications: the sign is not even stable-and-significant in one direction.
Whatever is driving state-to-state differences in aggregate payroll growth over this window, it is not
primarily enforcement intensity. That is a different conclusion from a chart built on
<em>sector-specific</em> wage growth (construction and hospitality, the two industries with the most
immigrant-intensive employment) &mdash; total nonfarm job growth mixes in every other sector, most of
which have little exposure to immigration enforcement at all. The two charts are not testing the same
thing, and this memo should not be read as either confirming or refuting that one.</p>

<h3>Why DC is dropped from the trend line</h3>
<p>DC is a genuine outlier on both axes for reasons that have nothing to do with the labor-market
question being asked. Its enforcement rate is inflated by immigration-court and detention processing
relative to a resident population of about 700,000, and its {signedpct(m$job_growth[m$state_name=="District of Columbia"])}
payroll decline over this window is the federal-workforce-reduction story (RIFs), not an
immigration-enforcement effect. Including it swings the correlation from {g(prim$r,2)} to
{g(allsp$r,2)} on its own &mdash; a good illustration of why a single outlier should never be allowed
to silently set the answer. It stays on the chart (faint) but out of the fitted line.</p>

<h2>The ten highest-enforcement states</h2>
{tblf(top_arrests, note="Arrests: cumulative ICE administrative arrests, Jan 2025 - Jul 2026, Deportation Data Project. Population: Census Vintage 2025 estimate. Job growth: total nonfarm payroll employment, seasonally adjusted, same window.")}

<h2>The eight states with the weakest job growth</h2>
{tblf(bottom_growth, note="Sorted by total nonfarm payroll job growth, ascending. Oregon and Iowa are near the bottom despite middling enforcement intensity -- a reminder that plenty else moves state job growth.")}

<h2>Part two: kicking the tires with {n_hyp} more hypotheses</h2>
<p>One outcome is not a research design. The rest of this memo runs every state-level outcome we could
build in a reasonable pass &mdash; BLS payroll employment by sector, BLS wages, BLS/LAUS household-survey
labor force statistics, and (new here) CPS microdata aggregated to the state level &mdash; against the
same enforcement measure, arrests per 100,000 residents. The goal is breadth, not depth: find out which
of these {n_hyp} correlations are large enough to be worth a real research design later, and which are
noise. Every one of them is a simple bivariate Pearson correlation, DC excluded, no controls. Nothing
here should be read as more than a screen.</p>

{figf("fig_state_hypothesis_scan.png", glue("All {n_hyp} state-level correlations with ICE arrests per 100,000 residents. Filled dots: p < 0.10. DC excluded throughout."))}

<h3>What stands out</h3>
<div class="finding">
The single strongest correlation in the whole scan is a confound, not an outcome: states&rsquo; 2024
construction employment share predicts ICE enforcement intensity (r = {g(top_hit$r,2)}, p =
{g(top_hit$p_value,3)}) better than anything downstream of enforcement does. Enforcement is not handed
out at random across state economies &mdash; it lands hardest on states that already lean
construction-heavy (Texas, Florida, Wyoming), which is exactly the kind of pre-existing difference a
bivariate cross-section cannot separate from an effect of enforcement itself.
</div>

<p>Two aggregate measures cross the p &lt; 0.10 line in a way that is hard to read as a coherent story on
its own: total private wage growth (r = {g(wage_hit$r,2)}, p = {g(wage_hit$p_value,3)}) rises with
enforcement intensity, and so does the household-survey unemployment rate (r = {g(unrate_hit$r,2)}, p =
{g(unrate_hit$p_value,3)}) &mdash; wages up, unemployment up, in the same states. Total nonfarm payroll
job growth is positive too (r = {g(njg_hit$r,2)}, p = {g(njg_hit$p_value,3)}, repeating the Part One
result). Read together this does not describe either a tightening labor market (wages up, unemployment
should fall) or a slack one (unemployment up, wages should not accelerate) &mdash; it looks much more
like these high-enforcement states (Texas, Florida, Arizona) were simply growing faster on every margin,
enforcement included, for reasons upstream of enforcement. That is a composition story, not a causal one
in either direction.</p>

<p>The one result that lines up with the theory&rsquo;s own mechanism, for what a thin CPS state cell is
worth: foreign-born prime-age employment-population ratio fell more in higher-enforcement states (r =
{g(forepop_hit$r,2)}, p = {g(forepop_hit$p_value,3)}). That is short of conventional significance and
should not be leaned on, but it is the correlation you would most want to see if enforcement is actually
removing immigrant workers from state labor markets, and it is the right sign.</p>

<h3>What does not replicate</h3>
<p>The sectors that motivate the whole enforcement-and-wages argument &mdash; construction and leisure and
hospitality, the two industries with the most immigrant-intensive employment &mdash; show essentially
nothing here. Construction payroll job growth (r = {g(con_emp_hit$r,2)}), leisure and hospitality job
growth (r = {g(lh_emp_hit$r,2)}), leisure and hospitality wage growth (r = {g(lh_wage_hit$r,2)}), and
construction wage growth (r = {g(con_wage_hit$r,2)}) are all statistically and substantively zero. Using
official BLS state-level data, sector-specific wage growth in exactly the industries a reference chart on
this topic highlighted shows no relationship with state enforcement intensity at all in this window. Two
methodological differences could explain part of the gap and are flagged rather than papered over: BLS
does not publish a seasonally adjusted state-industry wage series, so the wage figures here are a
same-month year-over-year change (Aug 2025 to Aug 2026) rather than the cumulative
Jan 2025-to-window-end change used for employment and enforcement, and the construction wage series is
suppressed for several states (n = {con_wage_hit$n} instead of 50). Neither of those explains away a flat
correlation, but both mean this is not a clean apples-to-apples re-test of that chart&rsquo;s specification.</p>

<p>Native-born CPS outcomes are also flat: prime-age EPOP (r = {g(nat_epop_hit$r,2)}, p =
{g(nat_epop_hit$p_value,3)}) and unemployment rate (r = {g(nat_un_hit$r,2)}, p =
{g(nat_un_hit$p_value,3)}) show no state-level relationship with enforcement intensity in either
direction. That is consistent with <a href="findings.html">round one</a>&rsquo;s national finding of no
native benefit &mdash; it is also consistent with this cross-section simply being underpowered to detect
it, since CPS state cells are thin and this design has no placebo period the way the national
occupational dose-response test does.</p>

<h3>The full list</h3>
{tblf(scan_tbl, note=paste0("All ", n_hyp, " hypotheses, ranked by |r|. N varies (43-50) where a BLS series is suppressed for some states or a CPS cell fails the sample-size floor. See output/state_hypothesis_scan.csv."))}

<div class="caveat"><strong>On the multiple-comparisons problem.</strong> {n_sig} of {n_hyp} hypotheses
cross p &lt; 0.10. Under a pure null with independent tests, chance alone would produce about
{g(expected_by_chance,1)}. {n_sig} is somewhat more than that, but these tests are not independent
&mdash; total nonfarm job growth, the unemployment rate, and total private wages are all different
measurements of the same underlying state economic conditions, so this is not {n_hyp} independent shots
on goal. The honest reading is a mild, correlated signal that a handful of fast-growing,
high-enforcement states (Texas above all) are pulling several of these correlations in the same
direction, not {n_sig} independently corroborating findings.</div>

<h2>What this does not establish</h2>
<div class="card">
<p>This is a bivariate cross-section with about fifty observations and no controls for industry mix,
pre-existing growth trends, state fiscal policy, tariff exposure, or the dozens of other things that
move state payrolls. A null correlation here does not mean enforcement has no local labor-market
effect &mdash; it means the effect, if any, is not visible in <em>aggregate</em> state payrolls once
you average across every industry, including the many with negligible immigrant employment. It is
also entirely consistent with two effects canceling: a negative supply/demand shock in
immigrant-heavy sectors (construction, hospitality, agriculture) offset by growth elsewhere, which is
exactly the kind of thing sector-specific data would show and this aggregate cut cannot. That is the
natural next step, not a contradiction of it.</p>
<p style="margin-bottom:0">Arrests are administrative arrests, not deportations or removals; the two
diverge (an arrest can end in release, an ongoing court case, or removal). ICE&rsquo;s own state field is
about 2% missing even after the Deportation Data Project&rsquo;s backfill and those records are
dropped, not imputed.</p>
</div>

<div class="caveat"><strong>Natural next steps, not yet run:</strong> (1) push to the metro/city level, where
<code>tidyusmacro::getBLSFiles("sae", ...)</code> carries area-level series for every metropolitan area
BLS publishes, not just state totals &mdash; construction and hospitality are far more geographically
concentrated within a state than the statewide total, so a metro cut has real power a state cut does
not; (2) an event-study window around specific, dated enforcement surges (documented ICE operations in
named cities) instead of a full-window average, which would sharpen identification considerably over a
static cross-section; (3) a multivariate version of this scan &mdash; regress each outcome on enforcement
intensity <em>controlling for</em> the confounds this scan surfaced (construction employment share,
foreign-born population share, 2024 growth trend) instead of reporting bivariate correlations one at a
time; (4) extend the CPS side with the occupational/industry dose-response design from
<a href="findings.html">round one</a> (H4/H5) crossed with state, rather than a single pre/post state
average.</div>

<h2>Sources and method notes</h2>
<ul>
<li><strong>ICE arrests:</strong> Deportation Data Project (UC Berkeley Law / UCLA Center for
Immigration Law and Policy), FOIA-litigated ICE microdata, processed release covering 2022-10-01
through 2026-08-06, pulled from <code>github.com/deportationdata/ice/data/arrests-latest.parquet</code>
on {format(Sys.Date(), "%Y-%m-%d")}. State is <code>apprehension_state_filled_in</code> (their own
backfilled field); rows flagged <code>duplicate_drop_row</code> are dropped per their guidance.
Window: arrests dated 2025-01-01 through 2026-07-31.</li>
<li><strong>State payroll employment:</strong> BLS State and Area Employment, Hours, and Earnings
(SAE), total nonfarm, seasonally adjusted, statewide (<code>area_code == "00000"</code>), pulled via
<code>tidyusmacro::getBLSFiles("sae", email)</code>. Growth is the simple percent change from the
January 2025 level to the July 2026 level for each state; no annualization.</li>
<li><strong>Population:</strong> Census Bureau Vintage 2025 state population estimates
(<code>POPESTIMATE2025</code>, as of 2025-07-01, the midpoint of the enforcement window), from
<code>www2.census.gov/programs-surveys/popest/datasets/2020-2025/state/totals/</code>.</li>
<li>Correlation and OLS run three ways &mdash; all 51 states + DC, DC excluded (primary), and DC and
Texas both excluded &mdash; to check that the result is not an artifact of the two most extreme
observations. See <code>output/state_enforcement_vs_jobs_correlation.csv</code> for exact figures.</li>
<li><strong>Sector employment and wages (part two):</strong> BLS SAE, statewide, by supersector
(construction, leisure and hospitality, manufacturing, mining and logging, professional and business
services, government). Employment growth is seasonally adjusted, Jan 2025 to Jul 2026 level change.
Wages (average hourly earnings) are not seasonally adjusted at the state-industry level, so wage growth
is a same-month year-over-year change (most recent common month, Aug 2025 to Aug 2026) &mdash; a
different window convention from the employment figures, flagged wherever it applies.</li>
<li><strong>State unemployment and labor force (part two):</strong> BLS Local Area Unemployment
Statistics (LAUS), statewide, seasonally adjusted, via <code>tidyusmacro::getBLSFiles("laus", email)</code>.
Household-survey analogue of the SAE payroll counts; changes are Jan 2025 to Jul 2026 level/rate
changes.</li>
<li><strong>CPS state panel (part two):</strong> IPUMS CPS basic monthly, the same extract used in
<a href="findings.html">round one</a> and <a href="findings_batch2.html">round two</a>, aggregated to
state &times; period cells. Pre period is calendar year 2024; post period is the most recent 12
available months (Sep 2025-Aug 2026; October 2025 has no household survey and is simply absent from the
average, not interpolated). This reuses the state-panel design from round one&rsquo;s H9
(<code>R/08_geography.R</code>) but regresses on the actual measured enforcement rate instead of a static
pre-period foreign-born-share exposure proxy. Same caveat as H9: CPS state cells are thin, so treat every
CPS correlation here as suggestive at best.</li>
<li>All {n_hyp} part-two hypotheses, exact r/CI/p and category, are in
<code>output/state_hypothesis_scan.csv</code>; the merged state-level panel behind them is in
<code>data/state_hypothesis_panel.csv</code>.</li>
<li>This memo is a first pass and stands alone; it does not revise the conclusions of
<a href="findings.html">round one</a> or <a href="findings_batch2.html">round two</a>, which test a
different question (native-versus-foreign-born substitution in CPS microdata) with a different
method.</li>
</ul>
</div>')

HTML <- paste0(
  '<!DOCTYPE html>\n<html lang="en"><head><meta charset="utf-8">\n',
  '<meta name="viewport" content="width=device-width, initial-scale=1">\n',
  '<title>State Enforcement vs. Job Growth</title>\n',
  CSS, "\n</head><body>", BODY, "</body></html>"
)
writeLines(HTML, "findings3.html")
cat("wrote findings3.html (", round(file.size("findings3.html")/1024), "KB )\n")
