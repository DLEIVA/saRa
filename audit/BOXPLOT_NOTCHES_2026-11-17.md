# Numerical bivariate marginal box plots without notches

The developer requested rectangular, non-notched marginal box plots for
visual preference. Set notch=FALSE for both xbox and ybox in R/bivnum.b.R.
This cosmetic change preserves variable mappings, orientations, quartiles,
whiskers, outliers and the correlation calculations and footnotes.

The full current bivnum source is not present in the preparation checkout:
its latest orientation and footnote changes were applied in the developer's
local project. Therefore APPLY_NO_NOTCHES.R edits only the two explicit
notch declarations in that current file. It requires exactly two declarations,
checks R syntax before writing, and is idempotent. It does not replace the
file with an older copy. No test file is replaced.

The preceding full test run passed 527 expectations with zero failures,
warnings or skips. The developer also confirmed correlation-footnote HTML/PDF
exports and .omv persistence. Those results predate this cosmetic change.
Post-change compilation/install and visual acceptance remain pending.
