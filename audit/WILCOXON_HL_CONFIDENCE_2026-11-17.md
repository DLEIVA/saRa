# Paired Wilcoxon HL confidence metadata — 17 November 2026

Developer evidence: RStudio uses R/stats 4.6.2; jamovi Rj uses R/stats 4.5.0.
The native Rj reference agrees with saRa's HL estimate 1 and bounds:
90% requested: [-1, 3.5], returned confidence .90;
99% requested: [-2, 4], returned confidence .96.
The old helper discarded confidence metadata and the table labelled the latter
interval 99% without explanation. Different R versions are separate references.

Correction: optional detailed helper result preserves the native confidence
attribute. Existing numeric-vector callers remain unchanged. Paired Wilcoxon
adds a translated footnote on the lower-bound cell whenever the returned level
differs from the requested one. The heading explicitly remains the requested
level, as explained in the note. Numerical calculations are unchanged.
Existing clearWith includes pairs, missing-data mode and confidence width.

New tests compare with native R on the executing version, test lower/higher
returned confidence and build actual result tables across requested levels.
Fresh-instance tests do not replace GUI off/on and 99-to-90 transition checks.

Baseline full suite: developer supplied FAIL 0 | WARN 0 | SKIP 0 | PASS 850,
12.2 seconds. Uniform GUI, invalid-bound recovery, HTML/PDF and operative .omv
were developer-confirmed. New HL tests have NOT been run here: R is unavailable.
Pending: full R suite, rebuild/install, HL note at 99%, clearance on returning
to an attained level, exports and .omv persistence.

## Superseding heading revision

Developer supplied full-suite PASS 882, FAIL 0, WARN 0, SKIP 0 in 12.4 seconds
for the previous note-only correction. GUI screenshots confirm the note,
visibility toggling and note removal at 90%. The developer requests the exact
note "Requested confidence level 99% is not reachable, 96% reported instead"
and a 96% heading for that HL interval. This revision implements both.
A common returned level is shown in the heading. Different levels across pairs
use a neutral heading and row-specific notes, avoiding a misleading shared level.
Median-table and median-plot headings are not changed by HL metadata.
New heading tests compare against the installed native R version.
New revision R execution, GUI transitions, exports and persistence are pending.

## Percentage formatting correction

The developer run reported FAIL 5 | WARN 0 | SKIP 0 | PASS 890 (11.9 seconds).
All five failures are exact-text comparisons: formatC(format="fg", digits=8)
adds default field-width padding before percentages. Explicit width=0 removes
that padding in notes, shared headings and mixed-pair row labels. The test
expectations are unchanged. No interval calculation is changed. Fresh R
execution and GUI/export acceptance remain pending; R is unavailable here.

## Explicit trimming correction

The width=0 revision still failed the same five text expectations (FAIL 5,
WARN 0, SKIP 0, PASS 890; 11.9 seconds). formatC fg output retained padding.
Apply trimws to each formatted percentage in notes, headings and per-row
labels. Tests and interval calculations are unchanged. Fresh R run pending.

## All-zero paired differences guard

Developer run: PASS 895, FAIL 0, WARN 0, SKIP 0 (11.8 seconds).
Dynamic headings and notes at 99%/96% and 98.7%/97.4%, clearance at 95%,
HTML/PDF and operative .omv are developer-confirmed.
Screenshots show all ten pairs tied with the test unavailable, then recovery
of W=25.5, Z=1.054 and rank-biserial correlation .4167 on restoration.
Activating HL for all-zero differences exposed an invalid 0% confidence heading
with NaN bounds. The paired helper now rejects this unavailable interval with
a clear explanation. When no valid HL confidence exists, the heading is neutral.
Nonpositive or unit confidence metadata is not displayed as a returned level.
Tests cover all-zero differences including missing rows, actual result tables
and valid/unavailable pairs together. Native references accommodate R versions.
Fresh R suite and GUI/export acceptance for this guard are pending.

## Empty confidence collection and error-footnote lifetime

Developer run: FAIL 2 | WARN 0 | SKIP 0 | PASS 902 (12.2 seconds).
An empty unlist result is NULL; coerce the confidence collection to numeric
before signif, also protecting the heading helper's NULL input. Store the
HL error until after setRow so assigning its cells does not clear the footnote.
The unavailable/valid pair integration assertions remain unchanged.
Fresh R and GUI acceptance are pending.


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
