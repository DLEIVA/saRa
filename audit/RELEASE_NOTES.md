# saRa 1.0.0 — follow-up audit changes

- Correct statistical calculations and edge-case handling covered by the audit.
- Remove the regression add-on and its private image bridge.
- Complete translation marking, label harmonization and decomposition of the
  numeric, ordinal and nominal routines named in the report.
- Remove redundant proportions and cumulative proportions from every univariate
  frequency table; percentages use non-missing observations.
- Preserve categorical plot theme typography and backgrounds. Pie and doughnut
  explicitly use the multicolour jmv palette requested by the module author;
  their fills do not follow other global palette choices. Doughnut percentages
  are inside the ring, with categories in the legend.
- Include empirical/normalized Gini and the independent Lorenz curve from the
  preceding validated candidate.

Validation: author-reported full suite, 2,107 expectations passed, zero failures,
warnings or skips (22.5 s). Compilation/installation and recorded GUI, OMV reopening,
HTML and PDF checks confirmed by the author on Ubuntu. These are not independent
cross-platform checks. Language catalogues are not included.

The final 113-path source comparison is reconciled. The author-reported new JMO
SHA-256 is:

```text
20bdc32ce37742b1b2e77e2735573381e5e23bc8819bbe804a5ded09b69fb865
```

The follow-up commit identifier remains to be recorded after committing. The
previous JMO hash identifies a different binary. Full point-by-point response:
JAMOVI_AUDIT_RESPONSE_2026-11-27.md.
