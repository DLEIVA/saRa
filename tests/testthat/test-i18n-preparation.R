test_that("shared messages retain English without an analysis context", {
  expect_identical(saRa:::.saraMessage("Residuals"), "Residuals")
  expect_identical(saRa:::.saraMessage("Requires a non-constant variable"),
                   "Requires a non-constant variable")
  expect_null(saRa:::.saraMessage(NULL))
  expect_identical(saRa:::.saraMessage(NA_character_), NA_character_)
})

test_that("shared translation uses the explicit owner and preserves unknown diagnostics", {
  # Exercise the real jmvcore::.(), not a replacement of the dot function.
  owner <- function(locale) list(options=list(translate=function(text,n=1) {
    paste(locale,text,sep=": ")
  }))
  expect_identical(saRa:::.saraMessage("Residuals",owner("es")),"es: Residuals")
  expect_identical(saRa:::.saraMessage("Residuals",owner("en")),"en: Residuals")
  expect_identical(saRa:::.saraMessage("External diagnostic: x",owner("es")),
                   "External diagnostic: x")
  expect_identical(saRa:::.saraMessage("",owner("es")), "")
})

test_that("translated plot labels preserve residual and influence data", {
  # The owner provides Options' translation contract to the real dot function.
  owner <- list(options=list(translate=function(text,n=1) paste0("translated: ",text)))
  draw <- saRa:::.saraRegressionPlot
  model <- stats::lm(mpg~wt+hp,data=mtcars)
  state <- saRa:::.saraRegressionFrame(model)
  for (kind in c("residual","scale","influence")) {
    original <- draw(state,kind)
    translated <- draw(state,kind,self=owner)
    expect_identical(original$data,translated$data)
    expect_identical(translated$labels$x,paste0("translated: ",original$labels$x))
    expect_identical(translated$labels$y,paste0("translated: ",original$labels$y))
  }
  acfState <- saRa:::.saraResidualACF(model)
  original <- draw(acfState,"acf")
  translated <- draw(acfState,"acf",self=owner)
  expect_identical(original$data,translated$data)
  expect_match(translated$labels$caption,"translated: Row order; [0-9]+ automatic lags")
  expect_identical(saRa:::.saraRankTestError("data are essentially constant"),
                   "All observations are tied")
})

test_that("translated inner-range titles preserve coverage and column metadata", {
  cls <- getFromNamespace("ordSummaryClass","saRa")
  for (coverage in c(50,90,99)) {
    context <- new.env(parent=asNamespace("saRa"))
    context$self <- list(options=list(pc=FALSE,irange=TRUE,NPerc=coverage,
      translate=function(text,n=1) {
        if (text=="%s%% Inner range") "Rango interno del %s%%" else text
      }))
    context$private <- new.env(parent=emptyenv())
    context$private$colArgs <- list(name=paste0("s",1:17),title=rep("Statistic",17),
      type=rep("number",17),visible=rep("(n)",17))
    method <- cls$private_methods$.addQuantiles
    environment(method) <- context
    method()
    args <- context$private$colArgs
    expect_identical(args$title[13],paste0("Rango interno del ",coverage,"%"))
    expect_identical(args$name[13],"ir")
    expect_equal(lengths(args),c(name=18L,title=18L,type=18L,visible=18L))
  }
})

test_that("distribution validation translates errors while preserving valid input values", {
  method <- getFromNamespace("contvarsClass","saRa")$private_methods$.getcontProbValues
  context <- new.env(parent=asNamespace("saRa"))
  opts <- list(cdistros="exp",contvaluesfunc="1,2,3",translate=function(text,n=1) {
    if (text=="Random variable values must be positive (%s).")
      "Los valores deben ser positivos (%s)." else text
  })
  context$self <- list(options=opts)
  environment(method) <- context
  expect_identical(method(),c(1,2,3))
  context$self$options$contvaluesfunc <- "-1,2"
  expect_error(method(),"Los valores deben ser positivos",fixed=TRUE)
})
