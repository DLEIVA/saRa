# Run from the saRa package directory in an R session configured for jamovi.
# This script deliberately stops on failures; never treat syntax checks as runtime validation.
if (!file.exists("DESCRIPTION") || !dir.exists("jamovi"))
  stop("Set the working directory to the extracted saRa source directory first")
for (path in list.files("R", "\\.R$", full.names=TRUE)) parse(path)
if (!requireNamespace("jmvtools", quietly=TRUE))
  stop("Install/configure jmvtools and set options(jamovi_home=...) before continuing")
if (!requireNamespace("testthat", quietly=TRUE)) stop("Install testthat before continuing")
# Regenerates headers and result schemas from YAML and builds the .jmo.
jmvtools::install()
# Uses the compiled/generated source definitions; fails on statistical regressions.
testthat::test_local(reporter="summary", stop_on_failure=TRUE)
cat("Build and testthat checks completed. Run the jamovi GUI/export checklist next; R CMD build/check are optional supplementary diagnostics.\n")
