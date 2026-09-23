In response to my [recent post on women gaining 101% of jobs](https://newsletter.mikekonczal.com/p/why-are-women-over-100-of-jobs-gained) under the Trump administration, many commentators pointed to [John Carney at Breitbart](https://www.breitbart.com/economy/2026/09/10/breitbart-business-digest-liberals-are-worried-too-many-women-got-jobs-last-month/), who argued that this was largely about immigration. It made me realize that it's been a little bit since I took a dive into the immigration debates, and I'm braced to do this because I know the media and the Trump administration are not exactly about the preponderance of the evidence. People, if they find one thing, they'll run with it. Indeed, they spent a lot of 2025 arguing off the CPS raw levels, like a bunch of jokesters, even though [Jed Kolko](https://jedkolko.substack.com/p/no-native-born-employment-has-not) spent a lot of time explaining why that was not a good idea.

Going back into it, I was actually kind of surprised that I couldn't find evidence that native-born workers were better off for a year and a half of deportations. I kept digging, and I couldn't find any evidence. I dug some more, and every hypothesis I could come up with failed. I cannot find a reasonable metric by which native-born workers are better off out here in 2026 than they were in 2024.

We're going to move pretty fast through this. There's a GitHub where you can go ahead and ask your AI to download it and check it yourself, but I hope you trust me that I kicked the tires quite hard on this.

**Hypothesis 1: Native-born unemployment would fall.**

![Native-born unemployment rate by month, not seasonally adjusted: higher in 2026 than 2024 in every month](graphics/fig1_unrate_native_monthly.png)

It didn't happen. Instead, we can see it's basically the same. 

**Hypothesis 2: Prime-age employment would rise for native-born workers.**

![Native-born prime-age employment-population ratio by month, not seasonally adjusted: lower in 2026 than 2024 in every month](graphics/fig2_prime_epop_native_monthly.png)

It didn't happen. Instead, it looks like it fell. 

**Hypothesis 3: Prime-age employment would rise for native-born male workers.** This is the subtext of the 101% female job gain response I got, and people like Vice President JD Vance have alluded to this: the idea that there are a lot of workers on the sidelines because immigration is keeping them out of the labor force.

![Native-born men's prime-age employment-population ratio by month, not seasonally adjusted: lower in 2026 than 2024 in every month](graphics/fig3_native_men_epop_monthly.png)

It didn't happen. Instead, we can see that fell too.

**Hypothesis 4: Least-educated native workers would do better.** There's some debate about this, but in theory, native-born workers without a high school diploma should have been competing the most with immigrant labor and thus should have the most to gain.

![Native-born unemployment rate by education: the least-educated group did worst, not best](graphics/fig7_education.png)

It didn't happen. The least-educated native workers did worse on unemployment.

**Hypothesis 5: Wage growth would pick up for lower-income workers as deportations accelerated.**

![Real hourly wage at the 10th percentile for native-born workers fell in 2026](graphics/fig14_wage_p10.png)

It didn't happen. Real wages at the bottom of the income distribution fell. Here's the 10th percentile.

**Hypothesis 6: The highest-immigration-share job categories from my CES blog post should see the biggest wage increases.** This one uses no CPS microdata at all: I take the non-citizen worker share by industry from the 2024 ACS, apply the same crosswalk onto BLS's 249 CES industries as that post, split them into quintiles, and look at BLS's own published wage data by industry.

![Real wage growth by quintile of a CES industry's non-citizen worker share, from BLS establishment data: no acceleration in the highest-share quintile](graphics/fig11_wages_exposure.png)

It didn't happen. The most non-citizen-intensive industries never show the fastest wage growth in any year, including 2026. There isn't a clean monotonic story here the way there was by occupation above — the middle of the distribution grew fastest in 2026 — but the top quintile, the one that theory says should have gained the most, at no point comes out on top.

**Hypothesis 7: Unemployed native workers could at least find work faster because immigrants were out of the way.**

![Monthly job-finding rate for unemployed native-born workers fell](graphics/fig6_job_finding.png)

It didn't happen. The monthly job-finding rate fell for native-born workers out here in 2026.

**Hypothesis 8: Immigration is behind the fall in the labor share, so mass deportations should push it back up.** This was actually a specific argument Baron Sargon brought up during the 2024 election.

![Labor share of nonfarm business income fell to a postwar low](graphics/b2_fig1_labor_share.png)

It didn't happen. Labor share actually falls dramatically lower in 2025 and 2026, as we discussed in a recent post. It's falling so fast that people are genuinely unclear what is even going on.

**Hypothesis 9: Deportations would tighten the labor market. Job openings per unemployed worker and the quits rate should both rise.**

![Vacancies per unemployed worker and the quits rate, 2018-2026: both fell from their 2022-23 highs and stayed there](graphics/fig4_tightness.png)

It didn't happen. Both measures fell from their 2022–23 highs and have stayed near those lows straight through 2025 and 2026. That's the opposite of what a labor-supply squeeze should produce.

**Hypothesis 10: Construction, in particular, should benefit — high foreign-born share, mostly male and non-college, and hard to offshore or automate.**

![Construction industry: the foreign-born employment share fell while the native-born unemployment rate rose](graphics/b2_fig12_construction.png)

It didn't happen. The foreign-born share of construction employment did fall, from 30% in 2024 to 27% in 2026, exactly as the theory requires. But native-born unemployment in the industry rose over the same period, from 4.5% to 5.1% overall and from 5.9% to 6.7% among native-born men in the construction trades specifically — and the number of construction workers CPS is sampling fell too. That's a shrinking industry, not natives moving into vacated jobs. An industry-wide unemployment rate can't fully distinguish those two stories on its own, so the next test looks across states instead.

**Hypothesis 11: If the mechanism is real, it should show up in the cross-state pattern too — states with heavier ICE enforcement should see an acceleration in job growth, especially in construction.**

A raw level comparison here is contaminated: Texas and Florida have both a lot of enforcement and a lot of pre-existing growth for reasons that have nothing to do with 2025, so any positive correlation could just be "already-booming states get more enforcement," not an enforcement effect. The fix is to difference each state against its own trend — compare the post-enforcement growth rate (January 2025–July 2026) to the growth rate over the same-length window right before it (July 2023–January 2025), and correlate *that change* with enforcement intensity.

![Change in state payroll growth rate, post-period minus pre-period, versus ICE arrests per 100,000 residents: no relationship in either total nonfarm or construction](graphics/fig_state_trend_break.png)

It didn't happen. Once you net out each state's own trend, the correlation isn't just insignificant — it flips slightly negative, for both total nonfarm employment (r = -0.06, p = 0.66) and construction specifically (r = -0.09, p = 0.53), and neither moves when Texas is dropped. States with heavier enforcement weren't accelerating relative to where they were already headed. *[Mike: you mentioned wanting to reference a specific chart/claim here — Steve Rattner? I couldn't find that citation anywhere in the repo, so I left it generic. Send me the link and I'll wire it in properly.]*

**Hypothesis 12: Run the same enforcement-intensity comparison against every state-level outcome available — native and foreign-born employment, wages, unemployment, across 23 different measures — and something should line up.**

![23 state-level correlations with ICE enforcement intensity, filled dot means p<0.05: only one clears the bar, and it's a confound, not a result](graphics/fig_state_hypothesis_scan_p05.png)

It didn't happen. Wage growth here is already the trend-differenced version — each state's wage growth measured against its own prior-year trend, the same fix as Hypothesis 11 — rather than the raw level, because the level has the identical problem: high-enforcement states already had faster wage growth before the window started. Fixed that way, wage growth is unremarkable (r = 0.05, p = 0.74), and at the stricter p < 0.05 threshold across all 23 outcomes, exactly one is statistically significant: states' 2024 construction employment share predicts ICE enforcement intensity better than anything downstream of enforcement does. That's not a finding about what enforcement did — it's evidence that enforcement wasn't handed out at random across state economies in the first place. Nothing else in the scan comes close, which is what you'd expect if there's simply nothing here to find.

Nothing here is as cleanly instrumented as the occupation-level and industry-level tests above — this is closer to real-time, kick-the-tires analysis than a single well-designed test. But that's what makes it useful: run enough independent looks and a few should land in the restrictionists' favor by chance alone. They don't. The case was never especially well thought out to begin with.
