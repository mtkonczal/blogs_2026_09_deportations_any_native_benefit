# Did Native-Born Workers Benefit from the 2025 Deportation Campaign?

**Planning document. Hypotheses and research design only. No analysis run yet.**
Mike Konczal, Economic Security Project. Drafted 2026-09-18.

---

## 0. The argument being tested

The restrictionist labor-market claim is a simple partial-equilibrium one: immigrant
and native workers are substitutes in production, so a large negative shock to
immigrant labor supply shifts the supply curve left, raising the wage and the
employment probability of the remaining (native) workers. Borjas (2003) is the
canonical statement of the elasticity that makes this work.

There are three competing channels that push the other way, and the post should
name all three up front because the hypotheses are designed to separate them:

1. **Complementarity / task specialization.** If immigrant and native labor are
   imperfect substitutes (Ottaviano and Peri 2012; Peri and Sparber 2009), losing
   immigrant workers destroys the native jobs built on top of them: the
   English-speaking foreman, the dispatcher, the front-of-house manager, the
   crew lead. Native employment in exposed sectors falls.
2. **Local aggregate demand.** Immigrants are consumers, renters, and household
   formers. Removing them removes demand for haircuts, groceries, rent, and
   child care in the places they lived. This is a pure negative demand shock to
   native workers in non-tradable local services.
3. **Enforcement spillovers and chilling.** Raids disrupt production for firms
   that lose no workers at all; legal immigrants and U.S.-citizen members of
   mixed-status households withdraw from work and from the survey. This
   contaminates both the outcome and the measurement.

The closest empirical precedents both cut against the substitution story:
Clemens, Lewis and Postel (2018, AER) on the Bracero exclusion find essentially
no native wage or employment gain from removing roughly half a million Mexican
farm workers, and East, Luck, Mansour and Velásquez (2023, JOLE) on Secure
Communities find that enforcement *lowered* employment and wages for U.S.-born
workers, concentrated among the non-college group the substitution story says
should gain most. **Verify both cites and their point estimates before quoting
them.** Watson (2013) on 287(g) and Amuedo-Dorantes and co-authors on E-Verify
are the secondary literature.

The post's structure: state seven claims the policy's defenders make, each one
falsifiable in the CPS, then show that on all or nearly all of them the data run
the wrong way.

---

## 1. The seven "natives should have benefited" claims

These are the blog's spine. Each is written as the restrictionist would write it,
and each maps to hypotheses in Section 2.

| # | Claim | Testable prediction | Hypotheses |
|---|---|---|---|
| **C1** | Fewer immigrant workers means fewer people competing for jobs, so native unemployment falls. | Native-born unemployment rate declines after Jan 2025, in level and relative to trend. | H1, H2 |
| **C2** | Jobs immigrants held get handed to natives, so more natives are working. | Prime-age native EPOP and LFP rise. | H1, H3 |
| **C3** | A tighter labor market bids up pay, especially at the bottom. | Native wage growth accelerates, most at the 10th–25th percentile. | H6 |
| **C4** | Natives move into the industries and occupations immigrants vacated. | Native employment share rises fastest in high-immigrant-share occupations and industries. | H4, H5 |
| **C5** | With fewer competitors, unemployed natives find work faster. | Native U-to-E job-finding rate rises; median unemployment duration falls. | H7 |
| **C6** | Employers short of labor give existing native workers more hours. | Native usual hours rise; involuntary part-time falls. | H8 |
| **C7** | The gains go to the natives most exposed to immigrant competition: less than a BA, and living in high-immigrant places. | Dose-response: bigger native gains for non-college natives and in high-exposure states/metros. | H5, H9, H10 |

The seven claims are ordered from the crudest aggregate to the sharpest
cross-sectional test. The post should escalate in the same order, because the
aggregate numbers invite the "but it would have been worse" rebuttal and the
cross-sectional ones do not (see Section 3.1).

---

## 2. Hypotheses

Notation throughout: "pre" is a 2018–2019 and 2022–2024 baseline (skip 2020–21),
"post" is 2025m1 onward. All CPS nativity work uses IPUMS microdata; all
aggregate cross-checks use the published BLS series so the two have to agree.

Each hypothesis is stated as the *restrictionist* prediction so that a rejection
is a finding, not a null.

### Tier 1: Did native outcomes improve at all?

**H1. Native-born prime-age employment improved in absolute terms.**
Prediction: prime-age (25–54) native-born EPOP and LFP are higher, and the
native unemployment rate lower, in 2025–26 than in 2024.
Data: IPUMS CPS, `CITIZEN` in {1,2,3} defines native-born (this matches the BLS
definition; do not use `BPL` alone). Weighted by `WTFINL`.
Method: own seasonal adjustment via `seasonal::seas()` on the NSA rate series
(BLS publishes nativity only NSA), plus a 12-month-moving-average version as a
robustness check. Cross-check against LNU04073413 / LNU02073413 etc.
Expected finding: worse. Native unemployment up, prime-age EPOP down.

**H2. The deterioration in native outcomes is smaller than the overall labor
market deterioration.**
This is the honest steelman: maybe natives did badly, but less badly than they
would have. Test: regress log(native unrate) on log(overall unrate) over
1994–2019 (log-log, the same specification as the `01_education_young_unrate`
post), predict 2025–26, and test whether the native rate is below prediction.
**Known problem, must be handled explicitly:** as the foreign-born share of the
labor force falls, the native rate mechanically converges on the overall rate,
which biases this test *toward* the restrictionist result. Two fixes: (a) build
a counterfactual overall rate holding the nativity composition of the labor
force at its Dec-2024 value, and regress native outcomes on that; (b) use a
demand-side right-hand-side variable that is not itself a composite of native
and foreign outcomes (job openings rate, quits rate, or CES private payroll
growth). Do both.

**H3. The native labor force expanded to fill the gap.**
Prediction: native LFP rose, native population-adjusted employment rose, and the
native share of total employment rose *because more natives are working*, not
just because the denominator shrank.
Method: decompose the change in native employment share into (i) change in
native EPOP, (ii) change in native share of population, (iii) change in foreign
EPOP. If the entire move is (ii)/(iii), the "natives filled the jobs" story is
arithmetic, not behavior.
Expected finding: the share shift is essentially all denominator.

### Tier 2: Dose-response on exposure (the core of the post)

These hold the aggregate labor market constant and are the answer to "it would
have been worse otherwise."

**H4. Natives moved into the jobs immigrants left.**
Construct pre-period foreign-born employment share by `OCC2010` (and separately
by `IND1990`), averaged 2022m1–2024m12, on cells with adequate sample.
Prediction: native employment growth 2025–26 is increasing in pre-period
foreign-born share.
Specification: cell-level regression of native employment growth (or change in
native unemployment rate, or change in native hours) on pre-period foreign share,
weighted by pre-period native employment, with 2018–2019 as a placebo period run
identically. Cluster or bootstrap by cell. Report the placebo coefficient next to
the treatment coefficient; if the placebo is nonzero the exposure measure is
picking up a secular trend and the design is dead.
Expected finding: flat or negative slope. Natives did *worse* where immigrant
labor was thickest, consistent with complementarity plus demand.

**H5. The gains concentrate among less-educated natives, who are the closest
substitutes.**
Prediction: native workers with less than a BA, and especially less than HS, see
larger employment and wage gains than BA+ natives.
Method: education × nativity × age cells; also interact education with the H4
occupational exposure measure, which is the sharpest version (low-education
natives in high-immigrant-share occupations should be the single biggest winner
under the substitution model).
Expected finding: the reverse ranking. This is the cleanest refutation available
and should probably be the post's headline chart.

**H6. Native wage growth accelerated, especially at the bottom.**
Data: CPS outgoing rotation groups (`MISH` in {4,8}), `EARNWEEK`, `HOURWAGE`,
`UHRSWORKT`, weighted by `EARNWT`. Deflate by CPI-U (state the base year in every
label). Drop imputed earnings using the IPUMS allocation flags (`QEARNWEEK`,
`QHOURWAGE`); CPS earnings imputation rates are high and rising, and imputation
is non-random with respect to nativity, so report results both ways.
Prediction: real wage growth for natives rises after 2025m1, with the largest
gains at p10–p25 and in high-exposure occupations.
Cross-check: CES average hourly earnings for construction, leisure and
hospitality, and agriculture-adjacent industries; the Atlanta Fed wage tracker
for the aggregate. If CPS ORG and CES disagree, say so rather than picking one.
Expected finding: no acceleration; possible deceleration in exposed sectors.

### Tier 3: Mechanism and margin

**H7. Unemployed natives found work faster.**
Data: month-to-month links on `CPSIDP` with `MISH` and `LNKFW1MWT`, restricted to
valid 1-month links.
Outcomes: native U-to-E, E-U, E-N, N-E transition rates; median and mean
`DURUNEMP`; share unemployed 27+ weeks.
Prediction: native job-finding rate up, duration down.
**Data break: there is no October 2025 household survey**, so the 2025m9→m10 and
m10→m11 links do not exist. Every flows chart needs that gap shown, not
interpolated. The existing repo caption language in `scripts/flows_data.R`
handles this correctly; reuse it.
Expected finding: job-finding rate down, long-term unemployment share up.

**H8. Native hours rose and involuntary part-time fell.**
Data: `UHRSWORKT`, `AHRSWORKT`, `WKSTAT`, `WHYPTLWK` (economic vs. non-economic
part-time), `WHYABSNT`.
Prediction: scarce labor means employers lengthen the hours of incumbent native
workers before raising wages, so hours should move first.
Expected finding: hours flat to down and economic part-time up, which is a
demand-shock signature and not a supply-shock one.

**H9. Native outcomes improved more in high-exposure states and metros.**
Exposure: pre-period foreign-born share of the state (or `METFIPS`) labor force,
2022–2024 average. Secondary: publicly documented enforcement-surge cities
(Los Angeles, Chicago, Charlotte, and others; **verify the operation list and
dates from primary sources before using them**).
Method: 12-month-averaged state panel, change in native prime-age EPOP and
unemployment rate on exposure, with the 2018–2019 placebo.
Caveat to state in the text: CPS state cells are thin, so use annual averages
and show the standard errors. This is suggestive, not decisive.
Note: `STATEFIP` and `METFIPS` are **not** in any existing extract. New pull
required.

**H10. The demand channel, not the supply channel, dominates.**
Test: split exposed industries into (a) those that *employed* immigrants
(construction, agriculture, meatpacking, landscaping, home health) and (b) those
that *sold to* immigrants in high-immigrant areas (grocery, restaurants, retail,
personal services, residential rental). If the substitution story is right,
native employment rises in (a). If the demand story is right, native employment
falls in both, and falls in (b) in proportion to local immigrant population loss.
This is the affirmative explanation the post owes the reader for why native
outcomes got worse rather than merely failing to improve.

**H11. Enforcement spilled onto U.S. citizens.**
`NATIVITY` distinguishes native-born with both parents foreign (code 4) from
native-born with both parents native (code 1). These are all U.S. citizens; one
group lives in mixed-status households and the other does not.
Prediction under the restrictionist model: no differential, or second-generation
natives gain like everyone else.
Test: prime-age EPOP, LFP, and hours for `NATIVITY` 4 vs. 1, and separately
Hispanic native-born vs. non-Hispanic native-born.
Expected finding: second-generation and Hispanic natives deteriorated the most,
which is direct evidence of a chilling effect on citizens and is probably the
most newsworthy single result in the post.

**H12. The labor market actually tightened.**
The whole substitution story requires tightness. Test it directly: vacancies per
unemployed worker (JOLTS), the quits rate, the hires rate, the share of
unemployment from job losers vs. entrants, and the Beveridge curve position.
Prediction: all should show tightening if labor supply contracted against stable
demand.
Expected finding: V/U falling, quits at post-2015 lows, hires rate falling. A
supply-driven tightening and a demand-driven slowdown look different in exactly
this way, and this is the cleanest aggregate discriminator available.

### Optional thirteenth

**H13. Is any of this measurement?** The foreign-born decline in the CPS is
partly real departure and partly survey non-response by frightened respondents,
and the January population-control revisions (see 3.2) move the levels
mechanically. A short section that bounds how much of the measured foreign-born
decline is real would strengthen every other result, and would be honest about
the one place where the skeptic has a real point. Compare CPS foreign-born
levels against DHS removal/return statistics, Census population estimates, and
the CBO demographic outlook.

---

## 3. Design problems to solve before running anything

### 3.1 The "it would have been worse" rebuttal
Any aggregate result invites the response that the counterfactual was worse. The
post should concede this openly in one paragraph and then pivot to H4, H5, H9 and
H10, where the aggregate is differenced out. Do not try to win the argument on
H1 alone.

### 3.2 January population controls
The CPS introduces new population controls each January and does **not** revise
prior months' levels. The January 2025 and January 2026 controls incorporated
large revisions to assumed net international migration. A substantial part of any
Dec-to-Jan drop in foreign-born employment levels is this revision, not behavior.
Consequences: (a) prefer rates to levels everywhere; (b) if levels are shown,
mark the January discontinuities on the chart; (c) never compute a 12-month
level change that spans a January without flagging it. This is the single most
likely way to publish a number that does not survive scrutiny.

### 3.3 Missing October 2025
No household survey was collected for October 2025. Flows are broken for two
months, seasonal adjustment routines need the gap handled explicitly (do not let
`seas()` silently interpolate), and 12-month moving averages are contaminated
from 2025m10 through 2026m9. Decide one convention and apply it in every script.

### 3.4 Seasonal adjustment
BLS publishes nativity series NSA only. Options: X-13 via `seasonal::seas()`
(what `old scripts/07_native.R` does), or same-month year-over-year comparisons,
or 12-month moving averages. Run at least two and show that the conclusion does
not depend on the choice. Seasonally adjusting a series with a structural break
in the middle is exactly where X-13 misbehaves.

### 3.5 Sample size
National nativity cells are fine. Nativity × occupation × education × month is
not. Rules: pool to quarters or 12-month windows for anything below the national
level, report cell counts in the appendix, and drop cells below a stated
unweighted-observation floor (suggest 100 per cell per period).

### 3.6 Weights
`WTFINL` for basic monthly stocks, `EARNWT` for ORG earnings (and ORG-only
sample restriction), `LNKFW1MWT` for month-to-month links, `HWTFINL` for
household-level work. Using the wrong one is a silent error, so assert the
expected weighted totals against published BLS aggregates in every script.

### 3.7 Causal language
None of this is a clean natural experiment. Enforcement intensity is correlated
with local immigrant share, industry mix, and politics. The exposure designs with
placebo periods are the strongest thing available and they are still descriptive
dose-response. Write "consistent with" and "the pattern runs the wrong way for,"
not "caused." One explicit paragraph on identification, near the end.

---

## 4. Data acquisition

### 4.1 New IPUMS CPS extract required

No existing extract works. `cps_00031` has the right nativity variables
(`NATIVITY`, `BPL`, `CITIZEN`, `YRIMMIG`) but ends **2025m8** and has no state
and no earnings. `cps_00036` runs to 2025m11 but has `CITIZEN` only. `cps_00032`
and `cps_00033` have no nativity at all.

New extract, basic monthly, **2015m1 through the latest available month** (needs
2018m1 minimum for a pre-period, 2015 gives a longer placebo window):

```
YEAR MONTH SERIAL PERNUM CPSID CPSIDP CPSIDV MISH ASECFLAG
WTFINL HWTFINL EARNWT LNKFW1MWT PANLWT
AGE SEX RACE HISPAN MARST NCHILD NCHLT5 FAMSIZE
EDUC EMPSTAT LABFORCE CLASSWKR
OCC OCC2010 IND IND1990
UHRSWORKT AHRSWORKT WKSTAT WHYPTLWK WHYABSNT WHYUNEMP DURUNEMP WNLOOK
EARNWEEK HOURWAGE PAIDHOUR UNION
NATIVITY BPL CITIZEN YRIMMIG
STATEFIP METFIPS COUNTY
```

Size estimate: roughly 6–8 GB fixed-width. Read with `read_ipums_micro()` and
convert immediately to a `data.table` or Parquet cache, the way
`01_big_graphic_YoY.R` does, so the downstream scripts are fast.

### 4.2 Published series for cross-checks
- Nativity CPS series via `blsR::get_n_series_table()` or the flat files via
  `tidyusmacro::getBLSFiles("cps", ...)`, which carries a `born_code` column so
  the full nativity family can be filtered without hardcoding IDs. Note the
  cached `data/cps_jobs_data.RData` in the BLS-CPS repo is stale (ends 2022m7);
  pull fresh.
- Native LF `LNU01073413`, native unemployed `LNU03073413`, native unrate
  `LNU04073413`; foreign-born `...73395`. Prime-age native pop/emp:
  `LNU00073417/18/19` and `LNU02073417/18/19` (the pattern in
  `old scripts/07_native.R` and `old scripts/4_1_immigration.R`).
- JOLTS for H12; CES for industry payrolls and AHE; CPI-U for deflation.

### 4.3 Definitions to lock down once, in a shared file
- **Native-born** = `CITIZEN` in {1,2,3}. State this in the post; a reader will
  ask whether Puerto Rico counts.
- **Prime age** = 25–54.
- **Education** = the `EDUC` cuts already used in `01_big_graphic_YoY.R`
  (`>=111` BA+, `73–110` HS through some college, `<73` less than HS) so the two
  posts are comparable.
- **Exposure** = foreign-born share of employment in the cell, 2022m1–2024m12
  average, computed once and written to `data/exposure_occ.csv` and
  `data/exposure_ind.csv` so every hypothesis uses the identical measure.

---

## 5. Proposed file layout

```
R/00_run_all.R              orchestrator
R/01_load_cps_micro.R       read IPUMS, build analysis frame, cache Parquet
R/02_build_exposure.R       pre-period foreign-born shares by occ/ind/state
R/03_aggregates.R           H1, H2, H3
R/04_dose_response.R        H4, H5
R/05_wages.R                H6
R/06_flows.R                H7
R/07_hours.R                H8
R/08_geography.R            H9, H10
R/09_citizens.R             H11
R/10_tightness.R            H12 (JOLTS, published series)
R/99_measurement.R          H13
deportations_native_benefit.qmd
```

Plots: `theme_esp()` from `BLS-CPS-Jobs-Numbers/scripts/graphic_scripts.R`,
ESP navy `#2c3254` and warm red `#ff8361`, direct labels over legends, explicit
units in every axis label.

Every script writes a datestamped CSV to `output/` and prints its own sanity
checks (weighted totals vs. published BLS, cell counts, date coverage).

---

## 6. Open questions for Mike before execution

1. **End date.** Pull through the latest available month, or freeze at a clean
   annual boundary (2026m6) to avoid a partial-year comparison?
2. **Is the geography section worth it?** H9 needs a new extract with
   `STATEFIP`/`METFIPS` and the CPS cells are thin. It is the most intuitive
   chart for a general reader and the weakest statistically. Include, or cut and
   point at ACS/QCEW instead?
3. **Framing.** Lead with H5 (less-educated natives did worst, exactly backwards
   from the theory) or with H11 (second-generation U.S. citizens took the hit)?
   H5 is the better economics, H11 is the better story.
4. **Scope of H13.** A serious measurement section is most of a post on its own.
   Two paragraphs and a chart, or spin it out separately?
