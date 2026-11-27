# Numerical frequency tables and categorical layout — 17 November 2026

## Requested changes

Categorical Data Statistics uses a single column. The category-count field
is nested below IQV, with the shorter suffix (0 = automatic). No statistical
calculation or categorical selection rule changes in this layout update.

Numerical Data has a Frequency tables checkbox before Statistics and Plots,
enabled by default as in Ordinal Data. Tables show each distinct observed
numeric value in ascending numeric order. Counts, proportions, percentages
and their cumulative versions are included, without odds or interval bins.
Each selected variable receives its own table. Missing observations have a
separate count row, without relative or cumulative values. Valid observations
form the denominator. Numeric values are counted before label formatting;
nearby distinct numbers remain separate rows. Existing summaries and plots
remain independent of the frequency-table checkbox.

## Tests and evidence

The preceding nominal/ordinal correction passed the developer's full suite:
FAIL 0 | WARN 0 | SKIP 0 | PASS 488, duration 3.8 seconds. The latest supplied
GUI screenshot confirms ordinal cumulative tables and mixed-selection notes.

New R tests cover numeric sorting, decimals, negative and zero values,
missing denominators, close values, empty/constant inputs, multiple variables,
checkbox visibility, unchanged summaries and stale-row removal. They have
not been executed in the preparation environment because R is unavailable.

All three univariate schema pairs and GUI layouts compile locally with
jamovi-compiler 0.3.5. Structural layout and supplier/Range JavaScript checks
pass. A running jamovi visual inspection is still needed to verify the
categorical clipping correction. Module build/install, new R suite output,
HTML/PDF exports and .omv save/reopen remain pending for this follow-up.
