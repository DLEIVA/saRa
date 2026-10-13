# jamovi library audit tracker — saRa 1.0.0 candidate

Target: jamovi library (hub). This is not a CRAN submission. The original audit reviewed 0.1.1; the edited source starts from 0.1.2. Source corrections are distinguished from runtime acceptance. Earlier candidate tests do not establish that the latest candidate passes.

| ID | Severity | Finding | Source status / action |
|---|---|---|---|
| A01 | HIGH | Geometric mean and geometric SD are reported on the log scale | SOURCE APPLIED — R/jamovi verification pending. Shared numeric-statistics.R computes exp(mean(log(x))) and exp(sd(log(x))) without internal rounding. |
| A02 | HIGH | Yule's V and Wilson's e compute the wrong values | SOURCE APPLIED — R/jamovi verification pending. Yule’s V uses the marginal square root; Wilson’s e uses unordered pair counts without doubling. |
| A03 | HIGH | The ordinal "inner range" is always the full range | SOURCE APPLIED — R/jamovi verification pending. Ordinal coverage uses NPerc/100 rather than length(NPerc). |
| A04 | HIGH | Ordinal Data analysis summarises factor codes, not the data's values | SOURCE APPLIED — R/jamovi verification pending. Ordinal numerical operations and plots use jmvcore::toNumeric instead of factor level codes. |
| A05 | HIGH | The Wilcoxon signed-rank analysis computes several statistics as if the samples were independent | SOURCE APPLIED — R/jamovi verification pending. Paired HL; corrected signed-rank denominator and one-sided tails; selected-pair CIs, requested confidence width and variable labels. Paired z was already in 0.1.2. |
| A06 | HIGH | Mann-Whitney: group 2's average rank and the tie-corrected z are wrong | SOURCE APPLIED — R/jamovi verification pending. Group 2 divides by n2; shared z helper uses N+1 and the tie correction. Rank sums safely handle empty groups. |
| A07 | HIGH | Discrete Distributions: the hypergeometric is mislabelled and its plots can crash | SOURCE APPLIED — R/jamovi verification pending. Hypergeometric label; shared validated finite plot support; corrected validation variable. Quantile-based infinite-support bounds avoid the old negative-binomial SD error. |
| A08 | HIGH | Normality Tests always runs all seven tests, so an unrequested test can fail the whole analysis | SOURCE APPLIED — R/jamovi verification pending. Only selected tests execute; errors are per-cell footnotes; missing/constant values are checked; K–S inference caveat. |
| A09 | HIGH | Grouped ECDF plot pairs values with the wrong groups | SOURCE APPLIED — R/jamovi verification pending. Shared ECDF frame retains group pairing, excludes incomplete rows, standardises per group and facets by group. |
| A10 | HIGH | Two plots call ggplot2 functions without the namespace and fail | ALREADY PRESENT IN 0.1.2 — runtime verification pending. Both namespace fixes already existed in the uploaded 0.1.2 source; retained. |
| A11 | HIGH | boot is used but not declared | SOURCE APPLIED — R/jamovi verification pending. boot declared; reproducible bootstrap with RNG restoration; computation gated by displayed CIs/plots. Q–Q plotting is internal; qqplotr/qqconf are not imported. |
| A12 | HIGH | The version is below the library's minimum | SOURCE APPLIED — R/jamovi verification pending. DESCRIPTION, manifest and analysis definitions use 1.0.0; generated R headers require jmvtools regeneration. |
| A13 | MEDIUM | Table visibility rules use names that aren't options | SOURCE APPLIED — R/jamovi verification pending. Bivariate visibility expressions name every contributing real option. |
| A14 | MEDIUM | Variable names with spaces break the bivariate plots | SOURCE APPLIED — R/jamovi verification pending. aes_string replaced with data-mask mappings; pasted table formulas replaced with table() and explicit dimension names. |
| A15 | MEDIUM | Inputs that crash an analysis with an R error instead of a message | SOURCE APPLIED — R/jamovi verification pending. Empty summaries, constant histogram widths, mixed one-level effects, empty contingency tables, percentile bounds and distribution parameter guards. |
| A16 | MEDIUM | A "plots only" type check stops the whole analysis | SOURCE APPLIED — R/jamovi verification pending. Plot preparation skips unsupported types rather than stopping tables; ordinal selectors permit factors and bivariate ordinal validation rejects unordered inputs clearly. |
| A17 | MEDIUM | Table rows and plot images are created in .run() when the structure is known earlier | SOURCE APPLIED — R/jamovi verification pending. Normality plots initialize once; group-dependent rows are populated from full data; unique variable/group keys; distribution rows moved to initialization; duplicate mixed plot initialization removed. |
| A18 | LOW | clearWith entries that name no option | SOURCE APPLIED — R/jamovi verification pending. Result clearWith keys corrected; CI/plot dependencies and group changes recorded. |
| A19 | LOW | Spelling, symbols and statistic names | SOURCE APPLIED — R/jamovi verification pending. Corrected names, symbols, percentiles and the names of squared Kendall tau, quartile dispersion and Kruskal–Wallis effect size. |
| A20 | LOW | Casing is inconsistent across panels and results | SOURCE APPLIED — R/jamovi verification pending. Sentence casing normalized for cited controls and columns; X/Y boxes use variable labels; optional grouping label corrected. |
| A21 | LOW | Box-plot fence controls are greyed out when they still have an effect | SOURCE APPLIED — R/jamovi verification pending. Fence controls and CI width/method controls include plot activation. |
| A22 | LOW | Wilcoxon Descriptives spreads categories across identically titled columns | SOURCE APPLIED — R/jamovi verification pending. Three rank-descriptive rows per pair with numeric missing tied-rank cells. |
| A23 | LOW | Raw data is stored in plot state, sometimes twice | SOURCE APPLIED — R/jamovi verification pending. Histogram/box plots store descriptors and declare requiresData; ordinal frequency bars keep aggregate state. |
| A24 | LOW | NAMESPACE imports the whole of jmvcore, and two Imports are unused | SOURCE APPLIED — R/jamovi verification pending. Selective jmvcore translation import; qualified remaining calls; unused hrbrthemes/magrittr removed; data-mask pronoun imported from rlang. |
| A25 | LOW | No citations for the methods and packages doing the work | SOURCE APPLIED — rendering/build verification pending. Package and method references added and connected in result YAML. Rendering/compilation remains to verify. |
| A26 | LOW | Very long functions | PARTIAL REFACTOR — further decomposition pending. Shared numeric/Q–Q/distribution/rank/CI/error helpers reduce duplication. Further per-table decomposition of remaining long routines is pending. |
| A27 | LOW | Deprecated ggplot2 usage | SOURCE APPLIED — R/jamovi verification pending. Active aes_string, fun.y and guides(fill=FALSE) removed; line geom widths use linewidth; density mappings use after_stat. |
| A28 | INFO | Option descriptions contradict their defaults | SOURCE APPLIED — R/jamovi verification pending. Bool descriptions updated to their actual defaults; stray embedded mode declaration removed; grouping descriptions corrected. |
| A29 | INFO | No unit tests | TESTS ADDED — execution pending. 26 testthat blocks added. Latest candidate execution and logs pending; earlier candidate results do not validate this candidate. |

Additional correction: Chuprov’s T for larger contingency tables. Regression extensions now include RESET; Shapiro–Wilk, Lilliefors and Anderson–Darling; four heteroskedasticity tests; multi-lag DW (5,000 residual resamples, seed 123456); automatic-lag ACF; four residual/influence plots and an influence-index table. Latest build/install and 26-block R suite passed in the supplied user log. GUI functionality is reported working; HTML/PDF export and .omv save/reopen checks are developer-confirmed passed.

Compatibility review: the Regression image-owner/resource bridge uses private jamovi/jmvcore APIs. It needs explicit maintainer review; successful local rendering alone does not establish supported compatibility.

## Latest verification evidence

See USER_VALIDATION_LOG.md and USER_VALIDATION_EVIDENCE.json. On 11 October 2026 the user supplied a successful jamovi 2.7.30 Linux Flatpak compilation/install log with no testthat failures (26 blocks), and reported modules working. Individual tracker rows retain pending verification where complete GUI/export coverage is not established. This evidence supersedes historical execution-pending statements for the current suite.

Developer confirmation on 11 October 2026: HTML/PDF exports and .omv save/reopen checks passed. This supersedes earlier export/persistence pending notes; it does not establish cross-platform coverage or approval of private APIs.

## Follow-up source changes — 13 October 2026

See FOLLOWUP_2026-10-13.md for consolidated association helpers, the ordinal column-layout correction and additional shared routines. The current suite contains 34 blocks; a fresh build/test and GUI run is required. The previously recorded successful 26-block run applies to the preceding candidate only.
