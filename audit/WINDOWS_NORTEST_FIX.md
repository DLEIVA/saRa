# On-demand nortest loading in the regression add-on — Windows and Linux

## Latest runtime validation — 11 November 2026

This section supersedes earlier statements that a fresh 35-block run or build was pending. Historical sections below are retained as dated evidence, not the current validation status.

- Developer supplied `testthat::test_local(".")` output: **FAIL 0 | WARN 0 | SKIP 0 | PASS 274**, duration 13.1 seconds. The source contains 35 test blocks; PASS counts individual expectations, not blocks.
- Compilation and installation completed successfully, as confirmed by the developer.
- Developer confirms normality tests and residual plots render correctly and that the functionality works independently of moretests.
- Example datasets open correctly, as separately confirmed by the developer.
- Windows is inferred from the supplied paths. Exact jamovi/R/jmvtools versions and the hash of the locally tested checkout were not supplied for this run.
- The fill-scale replacement message is informational; testthat reported zero warnings.
- No new HTML/PDF or .omv persistence run, Linux/macOS acceptance run, or completion of every manual checklist item is asserted. Earlier evidence remains dated separately. Private Regression API review remains pending.

See `RUNTIME_VALIDATION_2026-11-11.json` for the structured record.


This replaces the earlier eager-import fix. Keep nortest in DESCRIPTION Imports, but do not add nortest importFrom entries to NAMESPACE.

Observed: the Windows jamovi 2.7.14 log confirms successful bundling of nortest; the developer also reported the same missing-package behaviour on Linux. The Regression parent library paths can exclude the saRa library at runtime. The engine source sets paths during add-on creation, then restores the parent analysis paths. Bare nortest:: first-use resolution may therefore fail even though the package is bundled.

Fix: R/utils.R defines .saraLoadNamespace(package), which reuses an already loaded namespace or calls loadNamespace with the library containing the installed saRa package plus current library paths. find.package("saRa") uses the loaded namespace location. No absolute platform-specific path is hard-coded, no global .libPaths mutation is performed, and no library()/attach() or startup import of nortest is added. In .saraNormalityTest only Pearson chi-square, AD, Lilliefors, SF and CvM request nortest. SW and uncorrected KS remain independent of it. nortest:: calls remain in place.

The dependency and any dependencies it requires load on first use; cached namespaces are reused afterward. This follows R's public namespace-loading interface. It does not change private regression image routing.

The suite still has 35 blocks: the eager-import test is replaced by a fresh-R-process test for lazy use. The test sources shared helpers in isolation, supplies a module-library fixture, checks SW/KS do not load nortest, then restricts library paths and checks AD/Lilliefors succeed without changing paths or search(). On 10 November 2026 the developer confirmed that the modifications work. The confirmation does not enumerate results separately for each platform or provide a new full-suite log.

Apply immediately:
1. Replace project NAMESPACE with supplied NAMESPACE (three original lines; remove the earlier nortest import if added).
2. Replace project R/utils.R with supplied R/utils.R. The supplied copy is based on main 4e639ad, with just this helper and dispatch change.
3. Close all jamovi windows; run jmvtools::install() from the project in RStudio; close/reopen jamovi.
4. Open Regression first in a fresh session and verify Lilliefors/AD. Also check standalone normality. Avoid opening another saRa analysis first, which could mask first-use lookup errors.
5. Replacement files and the test were supplied directly. The audit-only update does not require recopying code or applying patches.

If packages remain unavailable, inspect .jmo entries to verify R/nortest/DESCRIPTION and R/nortest/NAMESPACE are present. Installing nortest in system R is not a substitute for a correct jamovi bundle.

## Recorded result

See NORTEST_VALIDATION_2026-11-10.json. The fix works according to the developer confirmation. Earlier 34-block success remains historical evidence; the current source contains 35 blocks. No new complete-suite log was provided in this confirmation. The current source-changes.patch describes the nortest code/test change against main 4e639ad.
