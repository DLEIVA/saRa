test_that("undefined ordinal indices have notes and recover after data edits", {
  indices <- c("gammaGK", "tauKa", "tauKb", "tauKc", "dSommerR",
    "dSommerC", "dSommerS", "eWilson")
  columns <- lapply(indices, function(x)
    list(name=paste0("v[", x, "]"), type="number"))
  table <- jmvcore::Table$new(options=jmvcore::Options$new(),
    name="asocind", columns=columns)
  table$addRow(rowKey=1, values=list())
  opts <- c(list(rows="Y", cols="X"), setNames(as.list(rep(TRUE, 8)), indices))
  context <- new.env(parent=asNamespace("saRa"))
  unused <- list(setRow=function(...) invisible(NULL))
  context$self <- list(options=opts,
    results=list(asocind=table, freqs=unused, desc=unused),
    data=data.frame(X=ordered(c(1,1,1,2,2,3)), Y=ordered(rep(0,6))))
  run <- getFromNamespace("bivordClass", "saRa")$private_methods$.run
  environment(run) <- context
  cell <- function(index) table$getCell(col=paste0("v[",index,"]"), rowNo=1)
  notes <- function() {
    keys <- names(table$notes)
    if (is.null(keys)) return(character())
    vapply(table$notes[startsWith(keys,"sara_letter_")], function(x) x$note,
      character(1), USE.NAMES=FALSE)
  }
  expect_error(run(), NA)
  undefined <- c("gammaGK", "tauKb", "tauKc", "dSommerC")
  for (index in indices) {
    if (index %in% undefined) {
      expect_true(is.nan(cell(index)$value))
      expect_equal(cell(index)$symbols, "<sup>a</sup>")
    } else {
      expect_equal(cell(index)$value, 0)
      expect_length(cell(index)$symbols, 0L)
    }
  }
  expect_length(notes(), 1L)
  expect_match(notes(), "denominator is zero")

  # Swapping the axes swaps the undefined directional Somers index.
  context$self$options$rows <- "X"
  context$self$options$cols <- "Y"
  expect_error(run(), NA)
  expect_true(is.nan(cell("dSommerR")$value))
  expect_equal(cell("dSommerR")$symbols, "<sup>a</sup>")
  expect_equal(cell("dSommerC")$value, 0)
  expect_length(cell("dSommerC")$symbols, 0L)

  context$self$data$Y <- ordered(c(1,1,2,2,3,3))
  expect_error(run(), NA)
  expect_length(notes(), 0L)
  for (index in indices) {
    expect_true(is.finite(cell(index)$value))
    expect_length(cell(index)$symbols, 0L)
  }

  # One complete pair leaves every denominator zero.
  context$self$data <- data.frame(X=ordered(1), Y=ordered(1))
  expect_error(run(), NA)
  expect_length(notes(), 1L)
  expect_match(notes(), "two complete observations")
  for (index in indices) {
    expect_true(is.nan(cell(index)$value))
    expect_equal(cell(index)$symbols, "<sup>a</sup>")
  }

  for (index in indices) context$self$options[[index]] <- FALSE
  expect_error(run(), NA)
  expect_length(notes(), 0L)
})
