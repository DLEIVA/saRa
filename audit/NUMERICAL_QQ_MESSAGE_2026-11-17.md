# Numerical Q-Q explanation — 17 November 2026

Numerical Data silently omitted the Q-Q graphic for constant data because the
shared preparation helper returned FALSE. The Numerical Data render callback
now returns an explanatory text graphic for constant or insufficient finite
observations. This keeps the explanation in the image resource for exports.
Variable-data Q-Q computation is unchanged; no global analysis error is raised.

New tests call the production callback, build full plot grobs and exercise
constant -> variable -> constant -> variable transitions, plus insufficient
and missing data. R execution is pending (R unavailable in preparation).
The developer must verify GUI, HTML/PDF and .omv persistence after installation.
Other analyses using the shared Q-Q helper are outside this scoped change.
