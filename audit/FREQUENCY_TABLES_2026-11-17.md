# Nominal and ordinal frequency-table separation — 17 November 2026

## Approved behaviour

| Analysis | Offered measurement level | Frequency table | Cumulative values | Odds |
|---|---|---|---|---|
| Categorical Data | Nominal | Counts, proportions, percentages | No | Yes |
| Ordinal Data | Ordinal | Counts, proportions, percentages | Yes | No |

Ordinal frequency tables are controlled by a new Frequency tables checkbox
before Statistics and Plots, enabled by default. Text and numeric ordinal
categories follow defined dataset order. Retained unused categories have
zero counts. All proportions and percentages use valid observations; the
Missing row reports its count without relative or cumulative values.
Empty valid samples leave relative values undefined rather than dividing
by zero. A category named Missing remains distinct from the missing row.

No old-analysis migration or special compatibility message is implemented.
The nominal supplier filters ordinal variables out of the available list.
Both suppliers continue to use factor storage, as required by the jamovi
schema. The ordinal Range behaviour is retained: text-only selections
omit Range; numeric/mixed selections can use it. An initial saved numeric
selection with Range disabled is not forced back on.

## Implementation and packaging

Shared helpers in R/frequency-tables.R build and populate both analyses'
tables. Row data are refreshed on each run. The two generated R headers
are regenerated from their YAML definitions. Supplier handlers filter
measurement levels without converting variables. Existing tests are not
replaced; new tests are in tests/testthat/test-frequency-tables.R.

The update archive contains only changed/new files. It deliberately leaves
bivariate plots, correlation footnotes, dataset registration, references,
numeric-statistics helpers and earlier test files in the developer's current
checkout intact. It is a copy-over update, not a standalone source release.

## Validation

After correction 1, the developer-run suite passed 488 expectations with
zero failures, warnings or skips in 3.8 seconds. The nominal/ordinal frequency
tests and Range correction passed. New frequency-table HTML/PDF exports and
.omv save/reopen remain pending. Source/compiler verification is recorded separately in
VALIDATION_STATUS.json. No new runtime pass count is asserted.

Completed source-only checks: both analysis/results schema pairs validated;
both R headers regenerated; both GUI layouts compiled; JavaScript syntax
checks and supplier/Range behaviour checks passed. These checks did not
execute R or jamovi.

## Correction 1 following developer runtime feedback

The initial update produced four errors and 460 passing expectations, with
zero warnings or skips. Range columns were omitted correctly for text-only
selections, but the summary error-footnote loop still referenced them.
Both value population and footnote paths now check the actual initialized
column names. New regression tests preserve mixed-selection Range notes
and verify text-only operation with frequency tables enabled and disabled.
The subsequent complete run passed 488 expectations with no failures,
warnings or skips, validating this correction. Numerical frequency-table
additions are a later follow-up and require a new run.
