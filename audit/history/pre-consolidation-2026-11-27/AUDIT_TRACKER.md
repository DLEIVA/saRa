# jamovi library audit tracker — saRa 1.0.0 candidate

## Superseding update: Wilcoxon HL confidence follow-up

Baseline suite passed 850 expectations with zero failures, warnings or skips
(12.2 seconds, developer supplied). Uniform support, recovery, HTML/PDF and
operative .omv are accepted. The later HL metadata correction awaits a fresh
R run and GUI/export acceptance. See WILCOXON_HL_CONFIDENCE_2026-11-17.md.
Earlier pending statements below are historical where superseded.


## Empty-collection and note-lifetime correction

The guard follow-up reported two failures and 902 passes. Empty confidence
collections now become numeric(0); error notes are attached after setRow.
New execution is pending.

## Latest all-zero paired HL guard

The trimming revision passed 895 expectations without failures or warnings.
Developer confirmed dynamic headings/notes, recovery, HTML/PDF and operative
.omv. All-zero paired differences expose an unavailable HL interval with a
misleading 0% heading. The guard and neutral unavailable heading await R and
GUI acceptance. See WILCOXON_HL_CONFIDENCE_2026-11-17.md.

## Explicit trimming follow-up

The width=0 attempt still produced five padding failures (890 passes).
Formatted percentages now use trimws explicitly; fresh R execution pending.

## Latest percentage-format follow-up

Developer run: FAIL 5 | WARN 0 | SKIP 0 | PASS 890, 11.9 seconds.
Five exact-string failures exposed formatC field-width padding. Explicit
width=0 fixes percentage formatting; tests are unchanged. Fresh R run pending.

## Latest HL heading revision

The preceding note-only revision passed 882 expectations with no failures,
warnings or skips (12.4 seconds, developer supplied). The current heading and
concise-note revision awaits fresh R and GUI/export acceptance. In the recorded
R 4.5.0 fixture, requested 99% now displays returned 96% in the HL heading.

## Current status — 17 November 2026

The review builds on audited commit
`2cfe7ee61c580950140c832a4b22163a90bba2f8`. Target: jamovi library, not CRAN.
Overall manual acceptance remains in progress. The exact tested checkout
and full platform/software-version inventory are not yet recorded.

### Latest full-suite evidence — density-marker warning follow-up

Developer run: FAIL 0 | WARN 2 | SKIP 0 | PASS 824; 11.6 seconds.
The 798 preceding expectations pass plus 26 uniform-support expectations.
Two warnings arise from scalar density-marker aesthetics inheriting 1002
curve rows. This is not a warning-free acceptance run. The marker now uses
annotate("segment"); tests require warning-free grob construction and one
segment with correct coordinates. Fresh R execution remains pending.

### Accepted manual evidence

- New univariate nominal/ordinal/numerical frequency-table behaviour and
  the final aligned categorical controls were developer-confirmed. HTML/PDF
  files were generated correctly; .omv was created and operational with
  all new tables.
- Numerical bivariate constant-variable covariance/correlation notes,
  restoration of variability, HTML/PDF and .omv persistence were confirmed.
  The developer applied the requested marginal-notch removal.
- The nominal 2x2 example passed GUI and HTML/PDF/.omv checks, including
  Yule Q=.8, Y=.5, V=.5, Chuprov=.5 and Sakoda=.6325, the selected other
  indices, frequencies/percentages, bars and mosaic.
- After the ordinal scale correction, Heatmap and Alluvial worked alone,
  bars-first and combined, with off/on toggles and X-axis changes. The
  developer confirmed all four checks, HTML/PDF and operational .omv.
- Earlier recorded checks include SP50/SP80 transformations, pooled
  symmetric lambda and empty categories, ordered text and numeric scores,
  ordinal Range/numeric-only controls, ordinal plots and references.

These are scoped developer confirmations and screenshot evidence. The
assistant has not independently inspected the exported file contents.
They do not establish complete cross-platform acceptance.

### Latest accepted checks and numerical Q-Q follow-up

The developer confirmed corrected mixed-data single a markers and H subscripts,
correct numeric results, graphs, HTML/PDF and operational .omv persistence.
The new mixed-effect-footnotes tests passed 36 expectations in the 639 run.

Geometric summaries passed GUI checks for positive values, scale changes,
missing data, constant samples, zero/negative guards and restoration of values
without stale notes. HTML/PDF and operational .omv were developer-confirmed.

Numerical Q-Q constant-sample explanations, constant/variable transitions,
HTML/PDF and operational .omv are now developer-confirmed; its tests passed
35 expectations in the 674 run.

Hypergeometric m=4, n=6, k=3 passed mass/CDF tables and plot checks, rejection
of k=11, recovery with k=3, HTML/PDF and operational .omv. Negative binomial
r=2 with p=.5 and .1 passed the supplied mass/CDF tables and adaptive plot
range checks; exports and operational .omv were developer-confirmed.

### Accepted distribution follow-ups and current uniform issue

The developer confirmed shared highlight colours operational, normal sigma=0
validation and recovery at 1, HTML/PDF and operational .omv. Exponential rates
.1, 1 and 10, zero-rate validation and recovery, HTML/PDF and operational .omv
were also confirmed. These are scoped checks, not acceptance of every mode.

Uniform [2,6] incorrectly rejects table queries at 1 and 7. Uniform query
values and interval bounds may lie outside the support. Remove four legacy
uniform-specific query restrictions; retain distribution-parameter checks.
Existing dunif/punif calculations return zero density and appropriate tail
probabilities. New tests exercise real tables, point validators and plots.
R, GUI and export acceptance of this correction remain pending.

### Scope decisions and Data Library

The unpublished parent-analysis regression add-on stays excluded; separate
saRa regression development is deferred. Translations remain deferred to
1.1.0, and menu reorganisation is parked. This candidate remains 1.0.0.
The eight datasets remain registered; individual opening of every dataset
still needs recording.

### Remaining checks before commit

- Run full suite after uniform support fix.
- Verify uniform density/CDF/survival tables at values below/inside/above support and interval shading.
- Verify invalid uniform minimum/maximum and restoration.
- Verify uniform HTML/PDF and .omv save/reopen.
- Record individual dataset opening, tested checkout and uncommitted changes, platform and software versions.
- Perform final evidence and changed-file review before commit.

## Original audit finding register

This register preserves the original findings and recorded source actions.
It is not a claim of complete manual acceptance. Current evidence and pending
checks appear above; source-only checks do not replace runtime validation.

| ID | Severity | Finding | Recorded source action |
|---|---|---|---|
| A01 | HIGH | Geometric mean and geometric SD are reported on the log scale | Shared numeric-statistics.R computes exp(mean(log(x))) and exp(sd(log(x))) without internal rounding. |
| A02 | HIGH | Yule's V and Wilson's e compute the wrong values | Yule’s V uses the marginal square root; Wilson’s e uses unordered pair counts without doubling. |
| A03 | HIGH | The ordinal "inner range" is always the full range | Ordinal coverage uses NPerc/100 rather than length(NPerc). |
| A04 | HIGH | Ordinal Data analysis summarises factor codes, not the data's values | Ordinal numerical operations and plots use jmvcore::toNumeric instead of factor level codes. The nominal/ordinal frequency-table separation passed the preceding automated suite; numerical frequency tables have also passed automated tests and developer GUI/export/persistence checks. |
| A05 | HIGH | The Wilcoxon signed-rank analysis computes several statistics as if the samples were independent | Paired HL; corrected signed-rank denominator and one-sided tails; selected-pair CIs, requested confidence width and variable labels. Paired z was already in 0.1.2. |
| A06 | HIGH | Mann-Whitney: group 2's average rank and the tie-corrected z are wrong | Group 2 divides by n2; shared z helper uses N+1 and the tie correction. Rank sums safely handle empty groups. |
| A07 | HIGH | Discrete Distributions: the hypergeometric is mislabelled and its plots can crash | Hypergeometric label; shared validated finite plot support; corrected validation variable. Quantile-based infinite-support bounds avoid the old negative-binomial SD error. |
| A08 | HIGH | Normality Tests always runs all seven tests, so an unrequested test can fail the whole analysis | Only selected tests execute; errors are per-cell footnotes; missing/constant values are checked; K–S inference caveat. |
| A09 | HIGH | Grouped ECDF plot pairs values with the wrong groups | Shared ECDF frame retains group pairing, excludes incomplete rows, standardises per group and facets by group. |
| A10 | HIGH | Two plots call ggplot2 functions without the namespace and fail | Both namespace fixes already existed in the uploaded 0.1.2 source; retained. |
| A11 | HIGH | boot is used but not declared | boot declared; reproducible bootstrap with RNG restoration; computation gated by displayed CIs/plots. Q–Q plotting is internal; qqplotr/qqconf are not imported. |
| A12 | HIGH | The version is below the library's minimum | DESCRIPTION, manifest and analysis definitions use 1.0.0; generated R headers require jmvtools regeneration. |
| A13 | MEDIUM | Table visibility rules use names that aren't options | Bivariate visibility expressions name every contributing real option. |
| A14 | MEDIUM | Variable names with spaces break the bivariate plots | aes_string replaced with data-mask mappings; pasted table formulas replaced with table() and explicit dimension names. |
| A15 | MEDIUM | Inputs that crash an analysis with an R error instead of a message | Empty summaries, constant histogram widths, mixed one-level effects, empty contingency tables, percentile bounds and distribution parameter guards. |
| A16 | MEDIUM | A "plots only" type check stops the whole analysis | Plot preparation skips unsupported types rather than stopping tables; ordinal selectors permit factors and bivariate ordinal validation rejects unordered inputs clearly. |
| A17 | MEDIUM | Table rows and plot images are created in .run() when the structure is known earlier | Normality plots initialize once; group-dependent rows are populated from full data; unique variable/group keys; distribution rows moved to initialization; duplicate mixed plot initialization removed. |
| A18 | LOW | clearWith entries that name no option | Result clearWith keys corrected; CI/plot dependencies and group changes recorded. |
| A19 | LOW | Spelling, symbols and statistic names | Corrected names, symbols, percentiles and the names of squared Kendall tau, quartile dispersion and Kruskal–Wallis effect size. |
| A20 | LOW | Casing is inconsistent across panels and results | Sentence casing normalized for cited controls and columns; X/Y boxes use variable labels; optional grouping label corrected. |
| A21 | LOW | Box-plot fence controls are greyed out when they still have an effect | Fence controls and CI width/method controls include plot activation. |
| A22 | LOW | Wilcoxon Descriptives spreads categories across identically titled columns | Three rank-descriptive rows per pair with numeric missing tied-rank cells. |
| A23 | LOW | Raw data is stored in plot state, sometimes twice | Histogram/box plots store descriptors and declare requiresData; ordinal frequency bars keep aggregate state. |
| A24 | LOW | NAMESPACE imports the whole of jmvcore, and two Imports are unused | Removed whole-namespace and unused imports; qualified calls and imported the data-mask pronoun. Translation fallback supports older jmvcore versions. |
| A25 | LOW | No citations for the methods and packages doing the work | Package and method references added and connected in result YAML. Rendering/compilation remains to verify. |
| A26 | LOW | Very long functions | Shared numeric, Q-Q, distribution, rank, confidence-interval and error helpers reduce duplication. Further decomposition remains outside the completed refactor scope. |
| A27 | LOW | Deprecated ggplot2 usage | Active aes_string, fun.y and guides(fill=FALSE) removed; line geom widths use linewidth; density mappings use after_stat. |
| A28 | INFO | Option descriptions contradict their defaults | Bool descriptions updated to their actual defaults; stray embedded mode declaration removed; grouping descriptions corrected. |
| A29 | INFO | No unit tests | The latest developer-run suite passed 603 expectations with zero failures, warnings or skips. The subsequent mixed effect-size formatting change awaits a fresh run. |



## Single complete pair (2026-11-17)

Developer confirmed 911 passing expectations before this correction and manual
multiple-pair exclusion, bootstrap/exact transitions, exports and .omv operation.
An empty pair and data restoration were also checked successfully.

With one complete pair (22,20), GUI showed HL=2 with [2,2] at confidence 0%.
The median plot had no bounds but its legend announced 95% CI.
The paired HL helper now retains estimate 2, returns unavailable bounds, no
confidence metadata and a specific explanatory note. The unavailable interval
does not enter the shared confidence heading or row-level confidence notes.
The paired median plot legend identifies unavailable intervals.
Tests cover a lone pair and coexistence with a valid pair, plus plot legends.
R is unavailable here; full suite and GUI checks remain pending.


## Mann-Whitney superiority (2026-11-17)

Wilcoxon: developer confirmed full suite 926 pass, 0 fail/warn/skip,
12.8 s; manual single-pair legend, restoration, HTML/PDF and .omv accepted.

Mann-Whitney manual fixture: A=(3,5,5,8,9), B=(2,4,5,6,7,10,12).
U=16; Z=-0.2453209547; bilateral p with continuity correction=.870087532,
without correction=.806207928. Rank-biserial=.085714286, f=.542857143.
Rank sums A=31, B=47. GUI agrees except superiority=.4290.
There are 15 strict A>B comparisons and 2 ties among 35 comparisons;
P(A>B)+.5*P(A=B)=16/35=.457142857.
Correction uses existing tie-adjusted U divided by group-size product.
Independent pairwise-definition tests cover ties, group reversal, alternatives,
missing data and complete separation. R unavailable here: tests pending.


## Mann-Whitney median intervals (2026-11-17)

Developer suite 938 pass, no fail/warn/skip,14.3s. Superiority=.4571 GUI
accepted. Both unilateral alternatives and hypothesis notes agree.

Rj DescTools0.99.60 reference for A=(3,5,5,8,9),B=(2,4,5,6,7,10,12):
90%: A [3,9], metadata .9375; B [2,12], displayed metadata .9844.
95%: A [-Inf,Inf], metadata1; B [2,12], displayed metadata .9844.
99%: both [-Inf,Inf], metadata1.
These are unbounded intervals, not missing values. Rj and bundled dependency
versions need not be identical; native comparison is required after rebuild.

Added optional detailed median-interval assembly: one record per group,
values preserved including infinities, native metadata retained, errors isolated.
MW attaches notes after row assignment so errors cannot be cleared by setRow.
Headings use common achieved confidence or refer to per-group notes; missing
metadata is explicit. Plot legends explain unbounded limits, which are not
drawn, and direct users to table when effective confidence levels differ.
Default shared-helper matrix contract for other callers is retained.
Tests compare native function for each group/level, actual table cells,
error isolation, achieved headings and plot labels.
R unavailable here: suite, rebuilt GUI and exports remain pending.


## Superseding correction: exact-binomial small-sample endpoints

Developer confirmed 979 passing expectations, no fail/warn/skip,24.1s.
Rebuilt GUI preserved B at95%, but A at95% and both groups at99% exposed
DescTools error: names attribute [3] must be the same length as vector [2].
The problem is not footnote formatting: the dependency errors when endpoints
lie beyond the observed order statistics. Rj0.99.60 instead returns infinities.

Detailed Mann-Whitney median assembly now uses a central exact sign interval:
k=qbinom((1-confidence)/2,n,.5), bounds x[k],x[n-k+1] when k>0,
otherwise [-Inf,Inf]. Reported binomial coverage is
1-2*pbinom(k-1,n,.5), or1 for the whole real line. With ties this is the
nominal sign-distribution coverage, not an empirical coverage estimate.
The default shared matrix path used by other analyses remains unchanged.
HL calculations are unchanged. Existing per-group confidence notes and
unbounded-plot labels now receive successful interval results.

Independent combinatorial enumeration in Python confirmed indices and
coverage for n5/n7 at90/95/99%. Tests include fixed expected endpoints,
coverage, singleton and invalid-input handling plus native finite comparisons.
R unavailable here; new suite, rebuilt GUI and export validation pending.


## Median footnote marker revision

Developer suite 993 pass,0fail/warn/skip,12.6s. GUI confirms exact
intervals and native names-attribute error removed at90/95/99%.
At95%, two notes on A produced a,b followed by d on B; c was not displayed.
Presentation change: gather per-group explanations, register one combined
footnote per cell. Expected95% a/b;99% shared a;90% a/b.
Statistical values and full explanatory text retained. Regression checks
read table footnotes, inspect each cell's footnotes/sups and assert consecutive
zero-based indices plus proper shared-note counts. R execution pending.


### Median plot layout correction

Developer reports 1028 passing expectations, no failures, warnings or skips. GUI screenshots show long right-side legends collapsing the plot panel at 90% and 95%. Mann-Whitney now requests a bottom legend, wrapped at 42 characters. Other callers retain their existing theme placement. Added plot build assertions for two medians and 2/1/0 finite intervals at 90/95/99%. R is unavailable here; new runtime tests, GUI and exports remain pending.


### Median plot group order

Developer reports 1049 passing expectations with no failures, warnings or skips. New screenshots confirm readable bottom legends and 2/1/0 finite intervals at 90/95/99%. At 95%, the first errorbar layer contains only B and trains the discrete axis as B-A. Added explicit axis limits in the original state order and tests for A/B point positions and B-only interval position. New R and GUI validation pending; R unavailable here.


### Explicit footnote alphabet

Latest developer test output: 1056 passes, no failures/warnings/skips (13.9 s). GUI confirms axis order, bootstrap 4999 at 90/99%, exact transition, exports and operative omv. Empty B alongside a valid variable leaves A available; restoration recovers both tables and plots.

Root cause of the apparent missing c: jmvcore R/utils.R .SUPCHARS begins Unicode a,b,d,e, omitting c. Previous per-group note combination reduced note counts but did not fix the alphabet. Mann-Whitney now renders each distinct explanation as an explicitly marked table note, with supported <sup>a</sup> cell symbols, a complete a/b/c alphabet and deduplication per table. This avoids changing the jmvcore namespace. HL failure notes are attached after setRow so they remain visible.

Added tests cover three distinct notes, repeated explanations, empty/restored B and all selected effects. R is unavailable here: execution, rebuilt GUI and exports with this correction remain pending.


### All-tied groups: readable HL explanation

Developer reports 1264 passing expectations, no failures/warnings/skips; explicit a/b/c markers, empty-group recovery and HTML/PDF/omv confirmed. With both groups entirely equal to 5, GUI preserves medians and [5,5] intervals, plots two points at 5, and rank outputs show the existing all-tied explanation. HL exposes an internal missing TRUE/FALSE error. Added a guard for two observed groups with one common value before native CI inversion; HL remains unavailable with a readable all-tied explanation. Added tests for an unaffected valid variable and restored-data clearance. New R/GUI checks pending; R unavailable here.


### One observation per independent group

Developer reports 1277 passing expectations (15.1s), no failures/warnings/skips. All-tied HL explanation, restoration and exports/omv confirmed. Minimum-sample screenshots show correct isolated plots, median unbounded intervals and recovery. Native HL reports estimate1 and interval[1,1] at requested90% for A=3/B=2.

Added a two-singleton-group guard that retains group1-group2 estimate but returns unavailable limits and explicit explanatory note. Mann-Whitney consumes the detailed helper result so the note is preserved after row writing. Tests cover 90/95/99%, reversed factor order, unaffected valid variable and recovery. R unavailable locally: new tests and rebuilt GUI remain pending.
