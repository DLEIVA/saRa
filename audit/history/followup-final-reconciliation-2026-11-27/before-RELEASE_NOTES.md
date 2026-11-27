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

The new JMO hash and final source/commit identity are pending local reconciliation.
The earlier candidate hash must not be used for this build. Full point-by-point
response: JAMOVI_AUDIT_RESPONSE_2026-11-27.md.
