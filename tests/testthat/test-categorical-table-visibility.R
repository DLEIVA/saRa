.nominalTableOptions <- function() {
  groups <- list(freqs=c("obs","exp","pcRow","pcCol","pcTot"),
    asocind=c("chiSq","phiind","phiSq","contCoef","craV","chuprov",
      "sakoda","Qyule","Yyule","Vyule"),
    errorind=c("lambdaGKab","lambdaGKba","lambdaGKsym","tauGKab","tauGKba",
      "theilab","theilba","theilsym"))
  list(groups=groups, options=setNames(as.list(rep(FALSE,23)),
    unlist(groups, use.names=FALSE)))
}

test_that("nominal tables appear only for their selected output options", {
  setup <- .nominalTableOptions()
  data <- data.frame(X=factor(c("A","A","B","B")),
    Y=factor(c("C","C","D","D")))
  run <- function(options) do.call(saRa::bivcat,
    c(list(data=data, rows="X", cols="Y"), options))
  result <- run(setup$options)
  for (table in names(setup$groups)) expect_false(result[[table]]$visible)
  for (table in names(setup$groups)) for (option in setup$groups[[table]]) {
    options <- setup$options
    options[[option]] <- TRUE
    result <- run(options)
    for (name in names(setup$groups))
      expect_identical(result[[name]]$visible, identical(name,table))
    if (option %in% c("craV","tauGKab","theilsym"))
      expect_equal(unname(result[[table]]$getCell(col=paste0("v[",option,"]"),
        rowNo=1)$value), 1)
  }
  options <- setup$options
  options$obs <- TRUE
  result <- run(options)
  expect_true(result$freqs$visible)
  expect_false(result$asocind$visible)
  expect_false(result$errorind$visible)
})

test_that("nominal visibility resets on the same tables after option changes", {
  setup <- .nominalTableOptions()
  tables <- setNames(lapply(names(setup$groups), function(name)
    jmvcore::Table$new(options=jmvcore::Options$new(), name=name,
      columns=list(list(name="value",type="number")))), names(setup$groups))
  context <- new.env(parent=asNamespace("saRa"))
  context$self <- list(options=c(list(rows="X",cols="Y"),setup$options),
    results=tables)
  update <- getFromNamespace("bivcatClass","saRa")$private_methods$.setTableVisibility
  environment(update) <- context
  for (option in c("craV","tauGKab","theilsym","obs")) {
    context$self$options[[option]] <- TRUE
    update()
    target <- names(setup$groups)[vapply(setup$groups,
      function(names) option %in% names, logical(1))]
    for (name in names(tables))
      expect_identical(tables[[name]]$visible, identical(name,target))
    context$self$options[[option]] <- FALSE
    update()
    for (table in tables) expect_false(table$visible)
  }
  context$self$options$obs <- TRUE
  context$self$options$craV <- TRUE
  context$self$options$theilsym <- TRUE
  update()
  for (table in tables) expect_true(table$visible)
  context$self$options$rows <- NULL
  update()
  for (table in tables) expect_false(table$visible)
})
