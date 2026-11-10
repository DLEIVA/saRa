# Apply and merge this follow-up in RStudio

The patch is based on main commit 8f54e75 (your previous PR #2 is already merged). It includes the removal/rename of R/audit-utils.R to R/utils.R, so use the patch rather than copying files over an old directory and leaving both helpers present.

1. In your existing saRa RStudio Git project, preserve any uncommitted work. Switch to main and Pull. In Terminal verify `git status` reports a clean working tree.
2. In Terminal run `git switch -c refactor-statistics-followup`.
3. Extract the supplied archive. Its root contains saRa-followup.patch. Run `git apply --check "/absolute/path/to/saRa-followup.patch"`, then `git apply "/absolute/path/to/saRa-followup.patch"`. Replace that path with the actual extracted file. Stop if the check fails; do not force or bypass it.
4. In the R Console run `source("audit/VALIDATE_IN_R.R")`. This rebuilds the old .jmo and runs 34 test blocks. Complete the targeted checks in FOLLOWUP_2026-10-13.md and exports/save-reopen.
5. In the Git pane review/stage the source, NEWS, tests and audit changes (including deleted audit-utils.R and new utils.R). Include regenerated tracked definitions if changed. Exclude the build folder and .jmo. Commit with “Consolidate association helpers and fix ordinal inner-range layout”.
6. In Terminal run `git push -u origin refactor-statistics-followup`. Open https://github.com/DLEIVA/saRa/compare/main...refactor-statistics-followup and create the PR. State the new validation results; the previous 26-block run does not cover this follow-up.
7. Once the follow-up checks pass and the PR has no conflicts, click Merge pull request and Confirm merge. This merges into your source repository; it does not automatically publish to the jamovi library or resolve the existing private-API review question.
8. Return to RStudio, switch to main, click Pull and record `git rev-parse HEAD` for maintainers.

The source retains version 1.0.0, as this updates your current candidate. Use a patch-version increment if a previously distributed binary must be distinguished from this new build. Never attach the old uploaded .jmo as though it contains these changes.
