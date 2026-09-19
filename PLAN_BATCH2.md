# Batch 2: The Second Round of the Argument

**Planning document for the second iteration.** The first batch (C1–C7, H1–H13) is
in `PLAN.md` and `findings.html` and stands on its own. This is the follow-on:
the claims that got made *after* the first round, the ones the first round did
not test, and the hardest versions of the restrictionist case we can construct.
Mike Konczal. Drafted 2026-09-19.

---

## 0. Why there is a second batch

Three reasons, and the memo should say all three out loud so a reader knows this
is a sequel and not a replacement.

1. **The first batch won on rates, so the other side moved to levels done
   carefully.** Camarota and Zeigler (CIS, July 9 2026) concede the January
   population-control problem and route around it with *within-year consistent
   weights*: compare January to December of the same year, where the controls
   never change. On that basis they report native employment gains of **2.0
   million within 2025** and **1.9 million within 2026 through June**, against
   758,000 within 2024 and 367,000 within 2023, and conclude that "the general
   stability in the unemployment and labor force participation rates of natives
   indicate that the administration's stepped-up immigration enforcement efforts
   have not harmed their labor market prospects." This is a better argument than
   the one the first batch refuted and it deserves a direct answer.

2. **The administration moved the goalposts to prices and to distribution.**
   Bessent (September 2026): "workers without a college degree, their wages,
   their employment situation is the best that it's been in over a decade," and
   "the bottom 25 percent of wage earners, the wages increase almost 3.5 …
   times more than the top 25 percent." The White House (January and June 2026)
   claims deportations cut rents and home prices in high-immigrant metros, which
   is a *real* wage claim routed through the deflator rather than through the
   nominal wage. Neither is tested anywhere in batch 1.

3. **The male labor market is the biggest unexplained fact of 2026 and both
   sides want it.** Women took 158,000 of August's 162,000 payroll jobs; women
   crossed 50 percent of payroll employment for the first time since the series
   began; employed men are down roughly 1.5 million year over year. The
   restrictionist reading (Camarota's op-ed) is that immigration is what kept
   young U.S.-born men out of the workforce and that removing it should bring
   them back. The complementarity reading is that the removed workforce was
   two-thirds male and concentrated in male-intensive sectors, so native men are
   exactly who a complementarity shock should hit. These are opposite
   predictions about the same fact, which makes it the sharpest available test.
   Batch 1 never cut anything by sex.

**Framing rule for the memo:** batch 1 is sufficient to make the case. Batch 2
is not a rescue. It is a stress test of our own conclusion against the strongest
claims now in circulation, and it should be labeled that way on the page.

---

## 1. The twelve additional claims

Numbering continues from the first seven. Each is written the way its proponents
write it.

| # | Claim | Testable prediction | Hyp |
|---|---|---|---|
| **C8** | Immigration is a labor-supply subsidy to capital; removing it shifts income from capital back to labor. The labor share rises. | Labor share of nonfarm business income rises after 2025m1. | H14 |
| **C9** | Done properly, with within-year consistent weights, the levels show natives gained about two million jobs a year. | Within-year (Jan→Dec) native employment gains in 2025 and 2026 exceed 2023–24, and exceed what native population growth alone implies. | H15 |
| **C10** | Immigration is what kept young U.S.-born men out of the workforce. Remove it and they come back. | Native male LFP and EPOP rise at 16–24 and 25–34, most for those without a BA. | H16 |
| **C11** | The removed workforce was two-thirds male, so native men are the closest substitutes and should gain most. | Native male employment and wages improve relative to native female, and improve most where occupational overlap with immigrant labor is highest. | H17 |
| **C12** | Construction is the test case: heavily immigrant, heavily male, heavily non-college. | Native construction employment, hours, and wages rise; native construction unemployment falls. | H18 |
| **C13** | Wage growth accelerated at the bottom: the bottom quartile is growing three times faster than the top. | A composition-immune, matched-worker wage tracker shows bottom-quartile native wage growth accelerating after 2025m1. | H19 |
| **C14** | Even if nominal wages are flat, deportations cut rents and home prices, so native real wages rose. | Rent inflation fell faster in high-immigrant metros than elsewhere, and native real wages deflated by a shelter-heavy index rose. | H20 |
| **C15** | Go occupation by occupation: roofers, drywall, meatpacking, landscaping, housekeeping. That is where natives got their jobs back. | In the highest foreign-born-share occupations, native employment share, hours, and real wages all rise. | H21 |
| **C16** | Natives step into the businesses immigrants ran: native self-employment and trade contracting rise. | Native self-employment rate rises, especially in construction trades, trucking, landscaping, personal services. | H22 |
| **C17** | Illegal immigration undercut union standards; removing it raises union density and the union wage premium. | Native union membership/coverage rate and the native union wage premium rise. | H23 |
| **C18** | The zero *is* the proof. Male payroll growth is flat because a two-thirds-male immigrant workforce left and native men stepped into the jobs one for one. The two cancel. | Native male employment rises by roughly the foreign-born male decline, which requires native male EPOP to rise and native male unemployment to fall. | H24 |
| **C19** | The workers who actually compete with new arrivals are *prior* immigrants. Naturalized citizens should be the biggest winners, and they vote. | Naturalized-citizen employment, wages and hours improve, and improve most in high-exposure occupations. | H25 |

C8, C9, C13, C14 are claims people actually made in 2026. The rest are the
strongest versions we could construct ourselves. **C18 and C19 are the two that
worry us most and they belong at the top of the memo, not the bottom.** C18 is
the only argument on the list under which our own headline fact, men gaining no
jobs, becomes evidence *for* the other side. C19 is the substitution model's own
best case, drawn from the literature that is usually cited against it: the
studies finding little or no native wage effect from immigration generally do
find one on previously arrived immigrants, so the symmetric prediction of
removal is a gain for naturalized citizens. Neither has been tested.

---

## 2. Hypotheses

### H14. The labor share rose.

Data: BLS Productivity and Costs, nonfarm business labor share `PRS85006173`
(index, 2017=100), quarterly; business sector `PRS84006173` as a check.
Test: level and change, 2024Q4 → latest, against the 2015–2019 and 2022–2024
ranges; and the percentile of the current reading in the post-1947 distribution.

Also do the theory explicitly, because the claim is wrong on its own terms
before any data. Under CES production in K and L with elasticity of substitution
σ, the labor share s satisfies

```
d ln s / d ln(L/K) = (σ − 1) / σ
```

so a fall in L/K raises the labor share only if σ > 1. The empirical consensus
for aggregate capital–labor substitution is σ well below one (Chirinko's survey
puts the central range at roughly 0.4–0.6; Oberfield–Raval's production-side
estimates for U.S. manufacturing are similar). **Verify both citations before
quoting.** At σ < 1, cutting labor supply *lowers* the labor share. The
restrictionist prediction here requires a parameter value the profession does
not believe.

Expected finding: refuted, and refuted twice.

### H15. The within-year-weights result is real.

This is the batch-2 steelman and gets the most careful treatment.

Step 1, replicate. For every year 2015–2026, compute weighted employed
native-born and foreign-born in January and in December (August for 2026),
using only that year's weights, so population controls are constant within the
comparison. Report the within-year change. Confirm we reproduce roughly 2.0M /
1.9M / 758K / 367K.

Step 2, four tests that the replication has to survive.

(a) **Base rate.** Show the full 2015–2026 distribution of within-year native
    employment changes, not just 2023 and 2024. If +2.0M is inside the normal
    range this is not a 2025 phenomenon.

(b) **Adding up against payrolls.** Within-year native gain plus within-year
    foreign-born change equals the CPS within-year total. Compare that total to
    CES private payroll growth over the identical window. Two surveys, one
    labor market. If CPS says +2.0M natives while total employment barely moved,
    the native number is reallocation of survey weight, not job creation.

(c) **The demographic bound.** The native-born population 16+ grows by births
    aging in minus deaths, roughly 1.2–1.5 million a year, and it is not a
    policy variable. At a constant December-2024 native EPOP, that population
    growth alone mechanically delivers about 0.7–0.9 million more employed
    natives a year. Compute the implied native EPOP required to generate the
    claimed gains and compare to the measured native EPOP, which fell. Then
    state the fork: either the CPS native population level is trustworthy, in
    which case the employment gain is *below* what population growth alone
    implies, or it is not trustworthy, in which case the employment level
    cannot be used either. There is no third option.

(d) **Nonresponse tracking.** Within-year change in *unweighted* noncitizen
    respondent counts is a pure survey-participation measure carrying no
    weights. Plot the within-year native weighted employment gain against the
    within-year noncitizen unweighted respondent decline, by year. If the
    native "gains" appear exactly in the years and months when noncitizens stop
    answering, the mechanism is the reweighting, not hiring.

Step 3, the rate version. Within-year change in native EPOP and native
unemployment rate, computed the same way. Rates are invariant to proportional
reweighting inside an age-sex-race cell.

Expected finding: replication succeeds, all four tests fail for the claim.

### H16. Young native-born men returned.

Cells: native-born × sex × age group (16–19, 20–24, 25–34, 35–54) × education.
Outcomes: EPOP, LFP, unemployment rate, and the NILF share.
Comparison: 2023, 2024, 2025, 2026 YTD annual averages of monthly rates.
Also: teen (16–19) native EPOP overall and in leisure/hospitality and retail,
because "immigrants took the entry-level jobs" is the same claim one rung down.

Expected finding: native male LFP and EPOP fell at every young age band, and
fell more than native female.

### H17. Native men, the closest substitutes, gained most.

Three pieces.

1. **Document the male-intensity of the shock.** Pre-period (2022–2024) male
   share of employment for noncitizens, all foreign-born, and native-born,
   overall and in the top exposure-quartile occupations. If the removed
   workforce is far more male than the native workforce, the substitution model
   makes a sharp sex prediction and cannot disown it.

2. **Overlap index.** For each sex g, define

   ```
   Overlap_g = Σ_o  (share of native-g employment in occupation o) × (foreign-born share of o)
   ```

   using pre-period shares throughout. This is the standard exposure-weighted
   competition measure. Report it for native men, native women, and by
   education. Prediction under substitution: the group with the higher overlap
   gains more.

3. **Sex-specific dose-response.** Rerun the H4 occupation-cell regression
   separately for native men and native women, with the 2018–19 placebo, on
   change in native unemployment rate and change in native hours.

Expected finding: native men have substantially higher overlap and did
substantially worse; the male dose-response coefficient is flat or wrong-signed.

### H18. Construction.

The single best-identified sector for the substitution story: roughly a quarter
foreign-born, over 90 percent male, mostly non-college, and non-tradable so the
output cannot be offshored.

Outcomes: native construction unemployment rate, native usual hours, native real
hourly wages, native share of construction employment, involuntary part-time.
Demand check: CES construction payrolls, and residential building permits and
housing starts, so we can say whether the sector shrank.

Expected finding: native construction unemployment up and hours down while the
sector contracts. That is the complementarity-plus-output channel, not
substitution.

### H19. Bottom-quartile wage growth accelerated.

The honest problem with the batch-1 wage result: cross-sectional percentiles are
composition-contaminated in both directions. When low-wage employment falls, the
measured p10 rises for a bad reason; when high-wage workers are added, the
measured median rises for an unrelated reason. Bessent's number is plausibly the
Atlanta Fed Wage Growth Tracker by quartile, which is matched at the person
level and therefore immune to that.

So build our own. From the CPS outgoing rotation groups, match each person's
MIS-4 observation to their MIS-8 observation twelve months later on `CPSIDP`,
compute the log change in the real hourly wage, and take the median. This is the
Atlanta Fed construction, but cut by nativity, education, initial wage quartile,
and occupational exposure quartile, which the published tracker cannot do.

Report it honestly. If matched bottom-quartile native wage growth did
accelerate, say so, and then ask the question that separates the theories:
is the acceleration concentrated in high-exposure occupations, as substitution
requires, or is it flat across exposure, in which case it is a macro wage-floor
story with nothing to do with enforcement?

**Caveat to state:** a matched tracker conditions on being employed in both
periods, so it drops exactly the workers who lost jobs. It removes composition
bias and adds survivorship bias. Report it next to the cross-sectional numbers
rather than instead of them.

### H20. Real wages rose through cheaper housing.

Three steps.

1. Is shelter disinflation real and is it new? CPI rent of primary residence and
   owners' equivalent rent, 12-month rates, 2019 to present. Establish when the
   deceleration began. If it starts in 2023 it is the post-pandemic multifamily
   supply wave, not 2025 enforcement.
2. Is it differential? CPI shelter by region (Northeast, Midwest, South, West)
   and for the metro CPI areas that are publishable, against foreign-born
   population share. If enforcement is the mechanism, the deceleration is
   concentrated where immigrants lived.
3. Does it rescue the real wage? Recompute native real wage growth with a
   shelter-heavy deflator and with headline CPI. Even a generous shelter
   adjustment has to overcome the measured nominal shortfall.

Expected finding: the deceleration predates 2025 and is national, and native
real wages fall under either deflator.

### H21. Occupation by occupation.

Take the twenty highest foreign-born-share occupations that clear the sample
floor, name them in the table, and report for each: pre-period foreign-born
share, native employment share, native unemployment rate, native usual hours,
and native real median wage, 2024 versus 2026. Then count: in how many of the
twenty did native workers improve on each margin? Under substitution the answer
should be most of them. Report the count and the exposure-weighted slope.

This is the version a skeptical reader can check line by line, which is the
point.

### H22. Native self-employment.

`CLASSWKR` in {10, 13, 14}. Native self-employment rate overall, and within
construction, trucking, landscaping and building services, and personal
services. Also incorporated versus unincorporated, because unincorporated
self-employment in the trades is the margin the claim is about.

Expected finding: flat to down. Worth running because a rise here is genuinely
possible and would be the first real point on the board for the other side.

### H23. Union density and the union wage premium.

`UNION` from the supplementary ORG extract (2018m1 onward). Native union
membership rate and coverage rate, overall and in immigrant-intensive
industries; native union wage premium from a simple log-wage regression with
education, age, sex, industry and occupation controls.

Expected finding: no change. Included because it is the one restrictionist
argument with a genuine labor-movement pedigree and it has never been tested on
this episode.

### H24. The zero is the proof: did native men fill the vacated male jobs? (C18)

The one argument on this list under which our own most striking fact becomes
evidence for the other side. Payroll employment for men has gone essentially
nowhere since December 2024 while women took effectively all of the net gain.
Read one way that is a male labor market in trouble. Read the other way it is
arithmetic: a workforce that was roughly two-thirds male was removed, native men
filled the openings one for one, and the two cancel to zero. If that second
reading is right, it is the best evidence anyone has produced for substitution
and the rest of this memo has to reckon with it.

Four tests, in increasing order of how hard they are to argue with.

1. **The arithmetic the claim requires.** Male employment by nativity from
   December 2024 to the latest month. These are levels and they are
   contaminated, but note the direction: the reweighting artifact pushes weight
   from departing foreign-born respondents onto native respondents in the same
   age-sex-race cell, so measured native male employment is biased *up*. The
   claim gets the benefit of the doubt on its own most favourable measure. Then
   compute the requirement: if native men had absorbed the entire foreign-born
   male employment decline, native male EPOP would have had to rise by a
   specific number of points. Compare to what it did.

2. **Rates, no levels anywhere.** Prime-age male EPOP, LFP and unemployment by
   citizenship. Filling vacated jobs draws people out of unemployment and out of
   the sidelines. Both have to move.

3. **The occupational-mix test, which is the clean one.** If native men moved
   into the work immigrants left, the share of native male employment sitting in
   high-foreign-born-share occupations has to rise. Compute the overlap index
   year by year, with pre-period exposure held fixed:

   ```
   Overlap_{g,t} = Σ_o  s_{o,g,t} × foreign_share_o^{pre}
   ```

   where `s_{o,g,t}` is occupation o's share of group g's employment in year t.
   This is a distribution over occupations dotted with a fixed vector, so
   multiplying every native weight by the same factor leaves it unchanged. It is
   immune to the reweighting problem in a way no level or level-ratio is. Run it
   for native men, native women, naturalized citizens and noncitizens.

4. **Inside the exposed occupations.** Native men's unemployment rate, usual
   hours and involuntary part-time within each exposure quartile, year by year.

Expected finding: the replacement arithmetic requires a native male EPOP
increase that did not happen; the overlap index for native men is flat or
falling; and native male unemployment inside the top exposure quartile rose.
The honest alternative explanation for the male payroll zero is sectoral and has
little to do with immigration: health care, which is heavily female, accounted
for more than the entire net payroll gain, while manufacturing and
transportation and warehousing shrank.

### H25. Naturalized citizens: the substitution model's own best case (C19)

The literature usually cited to show immigration does not hurt native workers
does not say immigration hurts nobody. It says the incidence falls on
*previously arrived immigrants*, who are close substitutes for new arrivals in
production, while natives specialise into communication- and language-intensive
tasks and are partly complements. Ottaviano and Peri (2012) is the canonical
statement, Peri and Sparber (2009) is the task-specialisation mechanism, Card
(2009) is the sceptical companion, and Borjas (2003) gets the same incidence
result inside education-experience cells. **Verify all four cites and their
point estimates before publication; none has been re-read for this memo.**

Run the implication forward. If new immigration depresses prior immigrants'
wages, then removing new immigrants should *raise* them. Naturalized citizens
are the cleanest group to test this on: they are the closest substitutes, they
cannot be removed, and there are roughly 25 million of them. They are also
voters, which is why this matters beyond the economics: the political argument
that Democrats lose naturalized citizens on immigration has an economic premise
underneath it, and that premise is testable.

Three reasons this is the sharpest test in either batch:

- It is the substitution model's strongest prediction, not its weakest.
- It has a clean prediction *within* the model about relative magnitudes:
  the gain for naturalized citizens should exceed the gain for natives.
- Naturalized citizens are the best-measured group in the data. Batch 1 found
  unweighted CPS respondent counts fell 17.4 percent for noncitizens and 5.6
  percent for the native-born, but only 2.5 percent for naturalized citizens.
  The group the theory says should gain most is the group whose survey
  participation held up best, so the usual objection that we are looking at a
  selection artifact is weakest exactly here.

Tests: prime-age EPOP, LFP and unemployment for naturalized citizens against
natives and noncitizens; the same split by sex; by Hispanic ethnicity, which
separates the substitution channel from the enforcement-chilling channel; by
years since arrival, where long-settled naturalized citizens are the purest case
because they are maximally substitutable and minimally at personal risk; the
matched wage tracker; and the occupational dose-response, run for naturalized
citizens and natives side by side, where the model predicts a *larger* negative
unemployment coefficient for naturalized citizens.

Expected finding: no gain, and if anything the opposite, which would mean the
enforcement-chilling channel dominates the substitution channel even in the
population where substitution should be strongest.

---

## 3. Design problems specific to batch 2

**3.1 Everything in H15 is a level, on purpose.** The rest of the project
refuses to use nativity levels. H15 has to use them because the claim is about
them. Handle this by never asserting a level as truth: replicate the opponent's
number exactly, then show it fails internal consistency checks that do not
require us to believe any level.

**3.2 Sex cells are fine, sex × occupation × education cells are not.** The
overlap index pools across occupations so it is well powered. The sex-specific
dose-response splits the sample in half and roughly doubles the standard errors.
Report the standard errors and do not claim a significant difference between the
male and female coefficients unless the interaction is actually significant.

**3.3 The matched wage tracker loses two links.** Matching MIS-4 to MIS-8 twelve
months later means every match spanning October 2025 is missing. The affected
outgoing-rotation cohorts must be dropped, not interpolated, and the chart needs
the gap shown.

**3.4 Union and earnings coverage starts in 2018.** The supplementary extract
runs 2018m1 forward, so H23 and H19 have no 2015–2017 baseline. State it.

**3.5 CPI metro series are thin and noisy.** Only a handful of metro areas have
monthly CPI publication, and their sample sizes are small. H20's differential
test is suggestive at best. Prefer the four census regions and say why.

**3.6 H24 is a levels argument and has to be handled like H15.** The
replacement claim is stated in levels, so test it in levels first, note that the
measurement error runs in the claim's favour, and then move to the overlap index
and the rates, which do not depend on believing any level.

**3.7 The naturalized-citizen sample is smaller than it looks.** Roughly 8
percent of monthly CPS respondents are naturalized citizens, so annual
prime-age cells are fine but the Hispanic-by-arrival-cohort cuts are thin.
Report the cell counts and do not split three ways at once.

**3.8 Do not let batch 2 relitigate batch 1.** Where a batch-2 test points the
same way as a batch-1 test, say so in one line and move on. The value of batch 2
is entirely in the claims batch 1 did not test.

---

## 4. File layout

```
R/40_batch2_micro.R        H15, H16, H17, H18, H21, H22  (one load of the 5GB frame)
R/41_batch2_wages.R        H19, H23, and the naturalized-citizen wage cuts
R/42_batch2_published.R    H14, H20, and the CES sex split
R/43_batch2_men_citizens.R H24, H25
R/47_charts_batch2.R       figures
R/48_build_report_batch2.R findings_batch2.html
```

Same conventions as batch 1: every script writes datestamped CSVs to `output/`
with a `b2_` prefix, prints its own sanity checks, and the report is generated
from the CSVs so it cannot drift from the code.

---

## 5. What would change our mind

Stated in advance, before running anything, so it is not chosen after the fact.

We would treat the restrictionist case as having a real point if **any two** of
the following held:

1. Native male prime-age EPOP rose from 2024 to 2026, or rose relative to native
   female.
2. The occupational dose-response coefficient for native men turned negative
   (natives doing better where immigrant labor was thicker) and was
   statistically distinguishable from zero with a clean placebo.
3. The matched-worker wage tracker showed bottom-quartile native wage
   acceleration concentrated in high-exposure occupations.
4. Native construction unemployment fell while construction output held up.
5. A majority of the twenty most immigrant-intensive occupations showed native
   improvement on employment share, hours, and wages together.
6. The labor share rose.
7. Native men's occupational overlap index rose, meaning native men actually
   moved into the work immigrants left.
8. Naturalized citizens gained on employment or wages, and gained more than
   natives did.

If none of the eight hold, the second batch adds nothing to the first except
confirmation, and the memo should say that plainly rather than dressing it up.
