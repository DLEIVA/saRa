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
