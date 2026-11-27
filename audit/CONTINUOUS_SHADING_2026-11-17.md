# Continuous probability shading — 17 November 2026

The continuous area layer used theme$fill[1], jamovi's background fill, so
areas were white on the default white background. Use theme$fill[2], the
accent fill, retaining alpha=.5. This applies to cumulative, survival,
interval and quantile probability regions. Numeric calculations, bounds and
discrete plots are unchanged.

New tests use jmvcore getTheme colours and its scale-bearing ggplot theme;
check the built fill/alpha, positive area height, full grob construction and
selected-region bounds for all shaded modes. R tests remain pending because
R is unavailable in preparation. The previous developer run passed 674
expectations; GUI and export/persistence of this fix are not yet confirmed.
