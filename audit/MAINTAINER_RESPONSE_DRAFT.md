Subject: saRa audit — correctness corrections and preparation of version 1.0.0

Dear jamovi maintainers,

Thank you for the detailed audit of saRa. I agree that correctness is the first priority, especially for a module used in statistics teaching. I have reviewed the findings against the source archive I am currently working from, which is version 0.1.2 rather than the audited 0.1.1, and prepared a 1.0.0 source candidate.

The source now corrects the geometric mean and geometric SD; Yule’s V and Wilson’s e; ordinal value conversion and inner ranges; paired Hodges–Lehmann estimates and intervals; signed-rank rank-biserial correlation, requested median-CI widths and selected-pair extraction; Mann–Whitney average ranks and tie-corrected z; and one-sided normal-approximation p-values. The Wilcoxon z calculation already used `paired=TRUE` in the uploaded 0.1.2 source. I also found and corrected a Chuprov’s T denominator issue for contingency tables larger than 2×2.

Normality tests now execute only when selected. Individual test failures produce cell footnotes, missing values are removed, and the K–S output explains that its p-value is uncorrected when normal parameters are estimated from the data. Grouped ECDF and Q–Q calculations retain group membership and standardise within groups. Normality table rows use distinct variable/group keys and are populated after full data become available.

The discrete-distribution calculations use one validated plotting-bound helper, including degenerate hypergeometric populations and valid probability endpoints. The hypergeometric label and discrete-input validation message have been corrected. Continuous distribution parameters and percentile ranges are checked. Constant histogram widths and empty numerical summaries are handled. Confidence-interval computations are guarded and run only when displayed. Bootstrap median intervals use a fixed seed while preserving the caller’s RNG state.

I have updated result visibility and invalidation dependencies; replaced string-based aesthetic mappings and pasted contingency-table formulas; corrected labels, statistic names and dependent controls; and converted the Wilcoxon rank descriptives to three rows per pair. Raw-data plots use compact state descriptors with `requiresData`. Shared numerical summaries, internal Q–Q plotting, discrete bounds and rank-test error handling have been consolidated. The two reported missing ggplot2 namespaces were already present in the uploaded 0.1.2 source.

The source explicitly declares `boot` and uses internal Q–Q plotting without importing `qqplotr` or `qqconf`, removes unused `hrbrthemes` and `magrittr` imports, and uses selective imports and qualified calls. References for the relevant packages and ordinal methods have been added to `00refs.yaml` and connected through the result definitions. Version metadata is aligned at 1.0.0. Twenty-six testthat blocks include known-answer cases and comparisons with R’s own tests, as well as missing-data and confidence-width regressions.

**Current verification status:** static syntax and YAML/source consistency checks pass. The supplied validation log confirms successful compilation and installation of saRa_1.0.0.jmo on Linux Flatpak with jamovi 2.7.30, followed by completion of the 26-block testthat suite without failures. The developer reports that the modules now work in jamovi. The developer also confirms successful HTML/PDF exports and .omv save/reopen checks. The log contains a jmvtools repository warning and installation-time engine connection messages; neither prevented this run from completing. R and jmvtools version details and the exact submitted commit remain to be recorded.

Some larger functions have been shortened substantially by removing duplicated computations. Further decomposition of the remaining table-population routines is a maintainability follow-up rather than a claim that every long function has been eliminated.

The Regression add-on now uses the existing Normality, Autocorrelation and Residual plots checkboxes. It adds RESET; Shapiro–Wilk, Lilliefors and Anderson–Darling tests; classical Breusch–Pagan, Koenker, Goldfeld–Quandt and Harrison–McCabe diagnostics; multi-lag Durbin–Watson with 5,000 residual resamples and seed 123456; automatic-lag ACF; four ggplot2 residual/influence plots and an influence-index table. Outputs identify saRa and cite car where relevant. The parent single normality table is hidden to avoid duplication.

These extensions include image-owner/resource routing and drawing-error handling. The routing relies on private jamovi/jmvcore APIs, so I request your review of that approach before acceptance. Latest build/install and R tests passed in the supplied log; the developer also confirms working modules, HTML/PDF exports and .omv save/reopen checks.

The submission target is the jamovi library. R CMD build/check are optional supplementary package diagnostics, rather than CRAN submission requirements. The acceptance evidence will include a successful jmvtools build/install, numerical test results, jamovi GUI/export/persistence checks, version information and reviewer example material for the exact submitted commit.

Before sending this response, replace the following placeholders with verified evidence:
- Repository: https://github.com/DLEIVA/saRa
- Exact commit/tag and branch: [pending]
- Target jamovi: 2.7.30, Linux Flatpak; embedded R / jmvtools versions: [pending]
- Latest build and 26-block test log: audit/USER_VALIDATION_LOG.md — completed without failures
- GUI: developer confirms modules working; HTML/PDF exports and .omv save/reopen: developer-confirmed passed on 11 October 2026.
- Example data/usage material: [pending]
- Private API compatibility review: [pending maintainer decision]
