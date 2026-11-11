# test_local() loads R code from the source tree, but jmvcore's base
# system.file() can still resolve an older installed package. Also, jmvcore
# caches missing catalogs. For these source tests, load the candidate JSON
# into a temporary catalog cache; keep the real Options/Translator code.
# Restore the original cache after each test, including on failure.
.saraCaLoadSourceCatalog <- function() {
  path <- testthat::test_path("..", "..", "inst", "i18n", "ca.json")
  if (!file.exists(path)) stop("Copy inst/i18n/ca.json before running the tests")
  definition <- jsonlite::read_json(path)
  messages <- definition$locale_data$messages
  metadata <- messages[[match("", names(messages))]]
  if (!identical(metadata$lang, "ca"))
    stop("The candidate catalog is not Catalan")
  cache <- getFromNamespace(".i18n", "jmvcore")
  hadPackage <- exists("saRa", envir=cache, inherits=FALSE)
  previous <- if (hadPackage) get("saRa", envir=cache, inherits=FALSE) else NULL
  temporary <- new.env(parent=emptyenv())
  if (hadPackage) list2env(as.list(previous, all.names=TRUE), envir=temporary)
  temporary$ca <- definition
  assign("saRa", temporary, envir=cache)
  withr::defer({
    if (hadPackage) assign("saRa", previous, envir=cache)
    else if (exists("saRa", envir=cache, inherits=FALSE)) rm("saRa", envir=cache)
  }, envir=parent.frame())
  invisible(NULL)
}

# The approved fixture is independent of the compiled catalog.
.saraCaApproved <- function() {
  read.delim(testthat::test_path("fixtures", "catalan-approved.tsv"),
    sep="\t", quote="", comment.char="", check.names=FALSE,
    stringsAsFactors=FALSE, fileEncoding="UTF-8", na.strings=NULL)
}
.saraCaOwner <- function() {
  options <- jmvcore::Options$new(package="saRa")
  language <- list(s="ca", has=function(name) identical(name,"s"))
  options$fromProtoBuf(list(names=".lang", options=list(language)))
  list(options=options)
}

test_that("Catalan catalog matches all approved translations and contexts", {
  .saraCaLoadSourceCatalog()
  approved <- .saraCaApproved()
  expect_equal(nrow(approved), 574L)
  expect_false(anyDuplicated(approved$key) > 0L)
  owner <- .saraCaOwner()
  actual <- vapply(approved$key, owner$options$translate, character(1))
  expect_identical(unname(actual), approved$translation)
  expect_identical(owner$options$translate("Counts"), "Freqüències absolutes")
  expect_identical(owner$options$translate("Counts [plotfrequency]"), "Fr. absolutes")
  expect_identical(owner$options$translate("df (ν)"), "gll (ν)")
  expect_identical(owner$options$translate("df1"), "gll1")
  expect_identical(owner$options$translate("df2"), "gll2")
  expect_identical(owner$options$translate("ω² (k levels)"), "ω² (k nivells)")
})

test_that("Catalan messages preserve formatting placeholders", {
  .saraCaLoadSourceCatalog()
  approved <- .saraCaApproved()
  tokens <- function(x) lapply(regmatches(x, gregexpr(
    "\\{[^{}]+\\}|\\$[A-Za-z_][A-Za-z_0-9]*|%s|%%", x, perl=TRUE)), sort)
  expect_identical(tokens(approved$msgid), tokens(approved$translation))
  expect_false(any(!nzchar(approved$translation)))
  expect_identical(sprintf(.saraCaOwner()$options$translate("%s%% Inner range"),90),
    "Rang intern del 90%")
})

test_that("Catalan regression add-on uses the parent language and own catalog", {
  .saraCaLoadSourceCatalog()
  language <- list(s="ca",has=function(name) identical(name,"s"))
  pb <- list(names=".lang",options=list(language))
  owner <- list(parent=list(options=list(asProtoBuf=function() pb)))
  expect_identical(saRa:::.saraAddonMessage("Normality Tests",owner),
    "Contrasts de normalitat")
  expect_identical(saRa:::.saraAddonMessage("Durbin-Watson Tests by Lag",owner),
    "Contrasts de Durbin-Watson per retard")
  expect_identical(saRa:::.saraAddonMessage("Table obtained with the saRa module.",owner),
    "Taula obtinguda amb el mòdul saRa.")
  expect_identical(owner$parent$options$asProtoBuf(),pb)
})

test_that("Catalan regression plot labels preserve numerical data", {
  .saraCaLoadSourceCatalog()
  owner <- .saraCaOwner()
  model <- stats::lm(mpg~wt+hp,data=mtcars)
  state <- saRa:::.saraRegressionFrame(model)
  draw <- saRa:::.saraRegressionPlot
  for (kind in c("residual","scale","influence")) {
    original <- draw(state,kind)
    translated <- draw(state,kind,self=owner)
    expect_identical(original$data,translated$data)
    expect_error(ggplot2::ggplot_build(translated),NA)
  }
  scale <- draw(state,"scale",self=owner)
  expect_identical(scale$labels$y,"√(|residu tipificat|)")
  acfState <- saRa:::.saraResidualACF(model)
  original <- draw(acfState,"acf")
  translated <- draw(acfState,"acf",self=owner)
  expect_identical(original$data,translated$data)
  expect_identical(translated$labels$x,"Retard")
  expect_identical(translated$labels$y,"Autocorrelació")
  expect_match(translated$labels$caption,"retards seleccionats automàticament",fixed=TRUE)
})
