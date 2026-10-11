# saRa 1.0.0 — audit candidate, 11 October 2026

This is edited source, not a validated release or installable corrected module. It is based on the uploaded 0.1.2 source; Claudia’s report describes 0.1.1. Regenerate generated R headers/results with jmvtools before testing or building.

## Changes

- Corrected original-scale geometric mean/SD, Yule’s V, Wilson’s e, ordinal value conversion and inner range, paired Hodges–Lehmann intervals, signed-rank correlation and directional normal-approximation p-values, Mann–Whitney average ranks and tie correction.
- Found and corrected an additional Chuprov’s T denominator error for tables larger than 2×2.
- Consolidated numerical summaries, signed-rank/Mann–Whitney z calculations, discrete plot bounds, bootstrap median CIs and Q–Q plotting.
- Normality tests execute only when selected; individual failures produce footnotes. Rows have variable/group keys. Missing and constant inputs are guarded, with an uncorrected K–S inference note.
- Grouped ECDF and Q–Q plots retain membership and standardise within groups.
- Added discrete/continuous parameter guards, constant histogram widths, empty-data results, and selected-pair extraction for CIs. Bootstrap CIs are reproducible and preserve caller RNG state.
- Fixed table visibility, clearWith dependencies, factor permissions, plot mappings for non-syntactic names, labels and fence enablement. Wilcoxon rank descriptives use three rows per pair.
- Raw-row plots use requiresData and compact state descriptors. Ordinal bars retain aggregate count state.
- Declared boot; Q–Q plotting is internal, removed unused hrbrthemes/magrittr, qualified remaining jmvcore calls and imported only translation and data-mask pronouns.
- Added package/method references and 26 testthat test blocks, including independent R-reference comparisons.
- Updated source version metadata to 1.0.0. Removed historical .jmo/build products from the delivered source archive; they would not contain these corrections.

## Verification performed here

R syntax was parsed with tree-sitter; YAML was parsed with PyYAML. Source-level dependency/option checks and patch comparison are recorded in VALIDATION_STATUS.json. This environment has no R or jamovi executable. Numerical R tests, jmvtools compilation, R CMD check, GUI behaviour and .omv/HTML/PDF exports have not run.

## Remaining

Execute the supplied validation script and GUI checklist; fix any failures before submission. Remaining editorial harmonisation and further decomposition of long functions are tracked separately. Regression diagnostics now use the existing parent checkboxes as described below. Independent new controls in the built-in panel are outside this add-on API.

## Ordinal validation follow-up

The ordinal summary now converts plain numeric-labelled ordered factors to numeric values before computing quantiles. jamovi stored-value conversion is applied first. Non-numeric factor labels without usable stored values produce an explicit error. This addresses the reported ordinal test failure; R tests still require rerunning.

## Regression diagnostics follow-up

- Keep the existing built-in Regression analysis and add-on registration.
- Selecting Normality test adds Ramsey RESET (fitted powers 2 and 3), classical Breusch–Pagan and Koenker studentized Breusch–Pagan tables for each model. These diagnostics test functional form/variance, not normality; table notes make the distinction explicit.
- Selecting Autocorrelation test adds a residual ACF for each model. stats::acf chooses its default maximum lag automatically. Residuals follow fitted complete-case row order; this is meaningful only when row order represents the observation sequence.
- Avoid requesting the parent's lazy models until the analysis has a dependent variable and a non-empty first model. Also skip all add-on work when both controlling options are disabled. This targets the empty-formula error seen before variable selection.
- Retain compact plot state for export; show an explanatory message for short/constant residual series.
- Add reference tests for diagnostics, automatic ACF lags with excluded rows, and incomplete/disabled analysis guards. Increase the Chuprov fixture counts to remove the expected small-count warning.
- This follow-up has passed static checks only; build, R tests and GUI/export checks must be rerun for this source version. Earlier user-reported successful validation applies to the preceding candidate.

## Regression GUI fixes

- Restore the 0.1.2 RESET column names and explicit addRow initialization. Also explicitly add both BP rows before setRow. A Table constructor's rows argument does not populate rows immediately.
- Use a small Image subclass that retains its saRa renderer owner after insertion into jmv's result tree. Share the parent resource-path callback with the add-on so its renderer can create the output PNG. Image names include the model number.
- Keep automatic ACF lags and compact state; GUI display, HTML/PDF export and .omv reload require checking in the installed jamovi version. The renderer bridge uses the parent resource callback exposed through its R6 private environment; it must be checked after jamovi/jmvcore updates.
- Add tests for row creation and rendering-owner dispatch. These 18 test blocks have passed syntax checks only in this environment.

## Validation fixture correction

The row-initialization test now represents the generated Regression model group with its assump binding, rather than a generic Group without that binding. This corrects the reported test-fixture failure and its four cascading assertions. Regression implementation is unchanged in this follow-up.

## ACF font rendering

Replace the base-graphics ACF renderer with ggplot2, an explicit generic sans font, and ASCII plot labels. Preserve automatic lags, confidence bounds, model state and the add-on image-owner bridge. User GUI verification remains required.

## Expanded Regression diagnostics

- Normality checkbox: saRa's Shapiro-Wilk, Kolmogorov-Smirnov with Lilliefors correction, Anderson-Darling and RESET. Hide the parent's single Shapiro-Wilk table while saRa supplies its three-test table. Other installed add-ons may still contribute their own tables.
- Rename the variance table Heteroskedasticity Tests; include classical BP, studentized BP/Koenker, Goldfeld-Quandt and Harrison-McCabe. GQ/HMC are ordered by fitted values and split at the median index. HMC uses 5,000 simulations and seed 123456.
- Autocorrelation checkbox: add generalized DW at lags 1-10 (capped at N-1), with car residual resampling, 5,000 replicates, seed 123456 and two-sided unadjusted p-values. Preserve caller RNG state. Retain the parent's lag-1 DW output.
- ACF uses automatic lags, ggplot2, a short three-line caption and explicit saRa attribution.
- Residual plots checkbox: add residuals-vs-fitted, scale-location, standardized-residual normal Q-Q with histogram inset, and a leverage/studentized-residual influence bubble plot. All include saRa attribution.
- Influence table contains the union of top-two absolute studentized residuals, leverages and Cook's distances, matching car::influencePlot's default noteworthy rule. Preserve original fitted-row labels after exclusions.
- Add the car companion-book reference and lmtest reference to module definitions; attach car to the DW and influence output.
- GQ/HMC and bootstrap DW show an unavailable explanation for weighted models, since these routines use unweighted OLS calibration.
- 22 test blocks now include bootstrap/reference, normality/heteroskedasticity, influence row alignment and ggplot construction checks. Runtime tests, module compilation and exports remain unrun here.

## Residual plot rendering follow-up

Bind dynamic image/table visibility to their parent checkbox. Draw ggplot objects inside the render callback, catching drawing-stage errors and rendering their message instead of leaving an image without a resource path. Limit label-placement time. Add a PDF-device drawing test that draws directly before testing the fallback, so drawing failures remain test failures. GUI verification remains required.

## Influence palette correction

Apply the continuous Cook-distance fill gradient after jamovi's global theme, whose discrete palette previously overwrote it. Add build and PDF-device tests with a supplied discrete theme. Static checks pass; runtime rerun required.

## Jamovi library submission scope

This candidate targets the jamovi library, not CRAN. Required acceptance evidence is compilation/install through jmvtools, numerical tests, jamovi GUI/export/persistence checks and an exact repository commit/tag with license, contact and example material. R CMD check is supplementary. Regression private renderer/resource APIs require maintainer compatibility review. Counts and prior incremental verification notes refer to successive candidates; the latest suite contains 26 blocks and remains unverified here.

## Q–Q dependency correction

Removed qqplotr from Imports and result references, eliminating its qqconf dependency path. All shared Q–Q plots use internal normal maximum-likelihood quantiles, quartile reference lines and 95% pointwise normal bands based on the functions embedded in 0.1.2. A new test checks these formulas, grouping and ggplot construction. The latest suite contains 26 blocks; execution remains pending.

## Mixed-data initialization correction

Qualified the grouped-table formatting constant as jmvcore::Cell.BEGIN_GROUP after selective namespace imports exposed an unqualified reference. This fixes the reported missing-object error in Mixed Data; GUI verification remains pending.

## Mann–Whitney z interface correction

Updated the analysis call to use alternative=Ha, matching the shared helper signature; the stale Ha argument caused the displayed NaN and unused-argument footnote. Returned tie counts restore the small-sample/ties warning. Existing R-reference tests now use the same named helper arguments and check tie counts. Runtime verification remains pending.

## Grouped normality lifecycle correction

Group-dependent rows are now created in .run() from full observations, since .init() may receive header-only data. Rows are rebuilt to remove stale groups. Added a lifecycle regression test covering empty headers, two variables/two groups, removal of a group and switching to ungrouped results. The suite contains 26 blocks; runtime verification remains pending.

## Composite row-key correction

The grouped-normality lifecycle test exposed jmvcore lookup treating unnamed list keys as equivalent. Variable/group keys now use named components, preventing getCell/footnote lookup from selecting another group. Wilcoxon pair/category keys use the same correction. The existing lifecycle test verifies each group against its own Shapiro–Wilk result. Runtime rerun is pending.

## Successful user validation — 11 October 2026

Latest candidate compiled and installed as saRa_1.0.0.jmo on jamovi 2.7.30 Linux Flatpak. The 26-block testthat suite finished without failures. The user confirms modules working. See USER_VALIDATION_LOG.md (session access key redacted). Historical runtime-pending notes above describe preceding candidates or this editing environment. HTML/PDF export, .omv persistence, exact submitted commit and private API review remain open. Installation warnings/messages are retained in the log rather than represented as a warning-free build.

## Export/persistence checks passed — developer confirmation

On 11 October 2026 the developer confirmed successful HTML/PDF exports and .omv save/reopen checks for the candidate whose build/install and 26-block suite had passed. This supersedes prior export/persistence pending statements. No analysis code changed following that successful run.
