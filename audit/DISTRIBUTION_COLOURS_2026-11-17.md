# Shared distribution highlights — 17 November 2026

User preference: retain yellow selection in both discrete and continuous
probability plots. Available audit revision requests theme colours and
extensible palettes; no specific objection to yellow was found.

Use jmvcore::colorPalette(2, palette, "fill"): second colour for selection,
first for unselected discrete bars. Under the default palette these are
yellow and blue. Respect theme$palette; default to jmv if absent. Apply the
manual scale after the global theme so its palette cannot override selection.
Numeric computations and selected regions are unchanged. Continuous areas
retain alpha=.5. Tests expanded for default and alternative palettes.
Previous run: PASS 760, including 86 continuous-shading expectations; it
predates this change. New R, GUI, HTML/PDF and .omv validation pending.
