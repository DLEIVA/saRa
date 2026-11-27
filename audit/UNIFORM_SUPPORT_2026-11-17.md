# Uniform queries outside support — 17 November 2026

Uniform [2,6] rejected table queries at 1 and 7. Remove legacy uniform-only
range guards from table values, plot query point and two interval limits.
The distribution parameters still require finite minimum < maximum.
Queries now reach the existing dunif/punif routines: zero density outside,
CDF 0 below and 1 above. Reversed interval validation in the plot helper stays.

Tests cover actual result tables, median, validators, full plot construction
and invalid parameter bounds. No R runtime is available in preparation.
Previous developer suite PASS 798 predates this fix; GUI/export/.omv pending.
