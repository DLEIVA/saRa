# Response to the jamovi follow-up audit — saRa 1.0.0

27 November 2026. Prepared response for the source follow-up on branch
`fix/audit-submission-followup`, based on main snapshot `a4f8508`.

Thank you for the detailed follow-up audit. We have addressed all 22 findings
through corrections, removal of the regression add-on, and one explicitly declared
palette design exception. We distinguish those outcomes below rather than claim
literal implementation of every suggested approach.

## Point-by-point response

### 1. Symmetric Goodman-Kruskal lambda

**Status: Corrected.** Symmetric lambda now pools the prediction errors from both directions. The supplied table [[10, 0], [5, 5]] is tested against 1/3 and DescTools::Lambda. The non-standard symmetric tau option has been removed.

Source/test evidence: `R/utils.R`; `tests/testthat/test-audit-review.R`; `tests/testthat/test-nominal-refactor.R`.

### 2. H1/H3 percentile skewness

**Status: Corrected.** The statistics, now labelled SP50/SP80, use interquantile spreads rather than twice the median. Tests cover known answers, translation, positive scaling, reflection, zero median and zero spread.

Source/test evidence: `R/numeric-statistics.R`; `tests/testthat/test-audit-review.R`.

### 3. Private image bridge in the regression add-on

**Status: Removed.** We removed the entire regression add-on from this release, including its registration, entrypoints and private Image bridge. Mathematical diagnostic helpers remain unregistered. We did not retain tables in the parent analysis and have not filed the suggested image feature request. Any future standalone diagnostics analysis is outside this release.

Source/test evidence: `R/regression-diagnostics.R`; `jamovi/0000.yaml`; `tests/testthat/test-audit-review.R`.

### 4. Changes to native regression checkboxes and overlap with moretests

**Status: Removed.** Removal of the add-on prevents saRa from changing native jmv::linReg controls or injecting duplicate tests. Its unregistered bootstrap helpers are not triggered by those controls. We do not claim to have reduced the simulation counts in the retained mathematical helpers.

Source/test evidence: `jamovi/0000.yaml`; `tests/testthat/test-audit-review.R`.

### 5. Text-coded ordinal variables

**Status: Corrected.** Ordinal summaries use the defined level order and report order-based quantiles as labels. Statistics requiring numeric scores are omitted or explained individually. Stored numeric values continue to be respected.

Source/test evidence: `R/numeric-statistics.R`; `R/ordsummary.b.R`.

### 6. Numeric values stored as nominal/ordinal factors

**Status: Corrected.** Numeric and mixed computations convert stored numeric values before calculating statistics and preparing numeric plots. They no longer operate on factor codes or reject numeric factors merely because they are factors.

Source/test evidence: `R/numeric-statistics.R`; `R/bivnum.b.R`; `R/bivmix.b.R`.

### 7. Empty categories blank all association indices

**Status: Corrected.** Zero margins are removed for association calculations while frequency output retains defined categories. Notes explain empty margins and degenerate results. Known-answer and retained-empty-level cases are tested.

Source/test evidence: `R/bivcat.b.R`; `R/utils.R`; `tests/testthat/test-nominal-refactor.R`.

### 8. Percentage options and fence bounds

**Status: Corrected.** Trim and Winsorization percentages are per tail, from zero to less than 50. Inner-range coverage is restricted to 50–99. GUI bounds and R guards are consistent; fence multipliers must be non-negative with the outer fence larger than the inner one.

Source/test evidence: `jamovi/numsummary.a.yaml`; `jamovi/ordsummary.a.yaml`; `R/utils.R`.

### 9. Default exponential and F plotting windows

**Status: Corrected.** Shared quantile-based windows replace inappropriate fixed ranges. Finite-window handling accommodates long tails and selected values. Dedicated tests cover the display windows.

Source/test evidence: `R/distribution-plots.R`; `tests/testthat/test-distribution-display-window.R`.

### 10. Seven small code slips

**Status: Corrected.** The invalid clearEffSTable call was removed; eta-squared fallback is isolated; mixed-effect handling uses observed groups and explanatory notes; covariance is included in result visibility; uniform support bounds are corrected; ordinal paired comparisons have five rows; Wilcoxon pair labels use rankCorrps.

Source/test evidence: `R/bivmix.b.R`; `R/bivord.b.R`; `jamovi/bivnum.r.yaml`; `jamovi/wilcoxt.r.yaml`; `tests/testthat/test-audit-review.R`.

### 11. Numeric mode stored as text

**Status: Corrected.** Numeric mode is returned as a numeric value so it follows number formatting. Text-coded ordinal modes intentionally remain text.

Source/test evidence: `R/numeric-statistics.R`; `R/numsummary.b.R`.

### 12. Distribution titles and endpoint quantiles

**Status: Corrected.** Title probabilities are calculated directly with distribution functions, rather than looked up in the displayed grid. Small values retain three significant digits and spacing is consistent. Infinite endpoint quantiles are explained and displayed using finite windows rather than drawn at infinite coordinates.

Source/test evidence: `R/distribution-plots.R`; `R/contvars.b.R`; `R/discvars.b.R`.

### 13. Rank-test edge cases and direction

**Status: Corrected.** All-tied data and absent complete observations have explanatory messages. Notes explain the differing directions of probability of superiority and rank-biserial correlation. Paired Wilcoxon error matching handles the non-missing-observation wording.

Source/test evidence: `R/utils.R`; `R/umw.b.R`; `R/wilcoxt.b.R`.

### 14. Plot themes and palettes

**Status: Addressed with a declared design exception.** Distribution and ordinal plots use the supplied jamovi theme and palette. Categorical plots preserve theme typography and backgrounds; Pareto overrides were removed. Pie and doughnut support more than nine categories, hide meaningless axes, and retain readable labels. At the module author's explicit request, these two plots now use jmvcore::colorPalette(n, pal="jmv", type="fill") to reproduce the multicolour jmv default palette shown in vijPlots. Their fills therefore remain jmv when a different global palette is selected. This is a deliberate exception to the recommendation to follow every global palette. Their typography and background still follow ggtheme. Doughnut percentages are inside the ring, with categories in the legend.

Source/test evidence: `R/catsummary.b.R`; `R/bivord.b.R`; `R/distribution-plots.R`; `tests/testthat/test-categorical-plot-theme.R`; `tests/testthat/test-ordinal-plot-theme.R`.

### 15. Geometric and negative-binomial count convention

**Status: Corrected.** Output explicitly states that X counts failures before the first or requested number of successes, with support starting at zero.

Source/test evidence: `R/discvars.b.R`; `R/messages.R`.

### 16. Redundant frequency columns and missing denominators

**Status: Corrected.** Following the suggestion, proportion and cumulative proportion columns have been removed from every univariate frequency table: categorical, ordinal and numerical. Percentages exclude missing observations. Ordinal and numerical tables retain cumulative counts and percentages; nominal tables retain odds and omit cumulative columns. The missing-data row reports its count without percentages or cumulatives. This supersedes the earlier decision to retain both scales.

Source/test evidence: `R/frequency-tables.R`; `R/messages.R`; `tests/testthat/test-frequency-tables.R`; `tests/testthat/test-numeric-frequency-tables.R`.

### 17. Labels, casing and spelling

**Status: Corrected.** The reported notation, direction, axis and column labels have been harmonized, including omega-squared, the eta subscript, Blau index, Teachman index, confidence-interval labels and standardized spelling. Individual controls/columns use sentence case; grouping headings retain their established heading style.

Source/test evidence: `jamovi/catsummary.a.yaml`; `jamovi/bivmix.a.yaml`; `jamovi/umw.r.yaml`; `jamovi/wilcoxt.r.yaml`; `R/utils.R`; `R/messages.R`.

### 18. Enabled bar-only controls

**Status: Corrected.** Bar type and Y-axis options are enabled only with the bar plot selected. Percentage choices additionally require the percentage axis.

Source/test evidence: `jamovi/bivcat.u.yaml`; `jamovi/bivord.u.yaml`.

### 19. Translation marking

**Status: Corrected at source-marking scope.** Visible strings use translation markers or a literal shared-message dictionary routed through the owning analysis. Complete templates use placeholders for variable content. Tests cover translated headings, notes, axes and preservation of data labels. The work prepares strings for extraction; language catalogues are not included, and we do not claim end-to-end validation of translations in every language.

Source/test evidence: `R/messages.R`; `R/frequency-tables.R`; `tests/testthat/test-translation-output.R`.

### 20. Long functions

**Status: Corrected in the named routines.** Numeric statistic helpers are at top level and .saraComputeNumeric dispatches to them. Ordinal summary computation delegates to shared helpers. The nominal .run method has been reduced from 156 to 16 lines and delegates frequency rows, totals, associations, Yule notes and prediction indices to separate methods. We do not claim that every presentation routine in the package is short.

Source/test evidence: `R/numeric-statistics.R`; `R/ordsummary.b.R`; `R/bivcat.b.R`; `tests/testthat/test-nominal-refactor.R`.

### 21. Dead code and deprecations

**Status: Corrected in the cited locations.** The unused preparations and members listed in the audit were removed. cisMed no longer receives the nonexistent num[2] field; the same key remains valid in the separate descriptives table. The cited geom_path uses linewidth and normality code no longer uses dplyr::do. This is not a claim that all legacy aliases elsewhere have been eliminated.

Source/test evidence: `R/numsummary.b.R`; `R/ordsummary.b.R`; `R/bivmix.b.R`; `R/testnorm.b.R`; `R/umw.b.R`; `R/catsummary.b.R`; `jamovi/bivnum.a.yaml`.

### 22. Working notes bundled into the R package

**Status: Corrected.** .Rbuildignore excludes audit/, img/ and saRa_logo.svg. The audit records remain in the repository for traceability.

Source/test evidence: `.Rbuildignore`.

## Validation and release identity

The author-reported full testthat run passed **2,107 expectations**, with zero
failures, warnings or skips, in 22.5 seconds. These results are user-supplied console
evidence, not an independent R execution by the reviewer preparing this response.

Compilation and installation were confirmed on Ubuntu. The recorded manual checks
cover the updated frequency tables and categorical plots, including 12 categories,
and OMV save/reopen plus HTML/PDF export. The final screenshot confirms the jmv
multicolour palette, hidden axes and internal doughnut percentages. The author
confirmed the requested generated files are operational. Export files were not
independently inspected here. Earlier accepted analysis checks are retained as
historical evidence; we do not claim a fresh exhaustive manual rerun on every
platform after these follow-up changes.

The final source comparison has been reconciled across 113 source/configuration/
data/test paths using the author's report (109 exact matches) and the four
supplied regenerated headers. Those headers differ only in documented parameter
wording and table titles, consistent with the YAML source. The accepted source
manifest has been updated accordingly.

The author-reported SHA-256 of the newly compiled saRa_1.0.0.jmo is:

```text
20bdc32ce37742b1b2e77e2735573381e5e23bc8819bbe804a5ded09b69fb865
```

The binary was not independently received or hashed here. The follow-up commit
identifier must be recorded after committing; no release publication or submission
has been performed by this review. No cross-platform coverage or translated
language catalogues are claimed.
