# Mixed effect-size footnote and notation correction — 17 November 2026

The three-group screenshot shows the expected numerical results. The aa
marker is caused by adding the same note twice to an unavailable two-level
effect cell. Remove the legacy group-count footnote loop and keep the
computation's error record as the sole source. jmvcore generates the marker;
no letter is hard-coded. Existing cell values are set before notes are added.

The result label and control title use η²<sub>H</sub> rather than η²[H], so
H is uppercase and formatted as a subscript. No effect-size formulas change.

New tests inspect actual cells and their marker indices, unique table notes,
the three-group reference effects, two observed groups with an unused third
level and disabled options. R execution is pending because R is unavailable
in preparation. Schema/header/UI compilation and source checks pass; GUI,
HTML/PDF and .omv appearance still require developer confirmation.

The preceding 603-pass run and accepted ordinal graphics exports/persistence
are recorded separately. Mixed plots/export/persistence are still pending.
