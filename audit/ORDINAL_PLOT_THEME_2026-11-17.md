# Ordinal heatmap/alluvial theme correction — 17 November 2026

## Report and diagnosis

The developer observed Continuous values supplied to discrete scale when
selecting Heatmap/Alluvial, and graph headings without rendered content
when plot selections were combined. The valid ordered variables did not
cause this error. The renderer receives a global jamovi theme containing
a discrete fill palette. Applying it after a continuous frequency gradient
replaces that gradient and fails when frequencies train the discrete scale.

## Correction

Apply ggtheme before scale_fill_gradient in both plot builders. The explicit
continuous gradient becomes the final fill scale. Preserve ordinal mappings,
level order, contingency counts, statistics and the existing drawing wrapper.
No blanket try/catch is added. Image clearWith dependencies explicitly cover
rows, cols, xaxis and all three graph toggles. Bar images also depend on
yaxis, yaxisPc and bartype.

## Test scope

Earlier name-with-spaces tests used theme_minimal and missed the global
palette conflict. New tests use the real public jmvcore theme_default with
scale="discrete", construct full ggplot grobs, and invoke the actual render
callbacks on a PDF device for heatmap-only, alluvial-only, bar-first,
combined and disabled selections. Both axis choices, reversed levels,
missing pairs and unaffected ordinal tables/statistics are covered.

These tests are prepared but have not been executed in R. Schema validation,
header generation, GUI compilation and source dependency checks pass.
R is unavailable in the preparation environment. The previous 527-pass run
predates this change. Fresh installation/GUI, HTML/PDF and .omv checks are
required before this ordinal graphics block is accepted.

A console message about replacing the default fill scale can occur while
the plot is constructed; the replacement is intentional. It is distinct
from the reported red continuous/discrete scale error.
