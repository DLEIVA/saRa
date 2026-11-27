test_that("HL details preserve the native R confidence metadata and values", {
  x <- c(11,12,8,10,13,9,14,7,NA,15)
  y <- rep(10,10)
  for (level in c(.90,.99)) {
    reference <- suppressWarnings(stats::wilcox.test(x,y,paired=TRUE,
      conf.int=TRUE,conf.level=level))
    actual <- suppressWarnings(saRa:::.saraHodgesLehmann(x,y,paired=TRUE,
      conf.level=level,details=TRUE))
    expect_equal(actual$values,unname(c(reference$estimate,reference$conf.int)))
    expect_equal(actual$confidence,attr(reference$conf.int,"conf.level"))
    expect_equal(suppressWarnings(saRa:::.saraHodgesLehmann(x,y,
      paired=TRUE,conf.level=level)),actual$values)
  }
})

test_that("confidence notes distinguish requested and returned levels", {
  note <- saRa:::.saraHLConfidenceNote
  expect_null(note(.90,.90))
  expect_null(note(.90,.90+1e-10))
  expect_null(note(.90,NULL))
  expect_identical(note(.99,.96),
    "Requested confidence level 99% is not reachable, 96% reported instead")
  expect_match(note(.90,.9140625),"90%.*91.40625%")
  expect_null(note(.90,.90))
})

test_that("paired HL tables display confidence notes only when required", {
  d <- data.frame(Before=c(11,12,8,10,13,9,14,7,NA,15),After=rep(10,10))
  for (level in c(.99,.90,.99,.90)) {
    reference <- suppressWarnings(stats::wilcox.test(d$Before,d$After,
      paired=TRUE,conf.int=TRUE,conf.level=level))
    result <- suppressWarnings(saRa::wilcoxT(d,
      pairs=list(list(i1="Before",i2="After")),ciHLps=TRUE,
      ciWidthps=100*level))
    table <- result$cisHLps
    heading <- saRa:::.saraHLConfidenceHeading(
      attr(reference$conf.int,"conf.level"),level)
    expect_identical(table$getColumn("cilHL")$superTitle,heading)
    expect_identical(table$getColumn("ciuHL")$superTitle,heading)
    expect_equal(as.numeric(table$asDF[[2]]),unname(reference$estimate))
    expect_equal(as.numeric(table$asDF[[3]]),unname(reference$conf.int[1]))
    expect_equal(as.numeric(table$asDF[[4]]),unname(reference$conf.int[2]))
    expected <- saRa:::.saraHLConfidenceNote(level,
      attr(reference$conf.int,"conf.level"))
    if (is.null(expected)) expect_length(table$footnotes,0L)
    else {
      expect_true(expected %in% table$footnotes)
      expect_length(table$getCell(col="cilHL",rowNo=1)$footnotes,1L)
    }
  }
})

test_that("HL headings use returned levels and distinguish mixed levels", {
  heading <- saRa:::.saraHLConfidenceHeading
  expect_identical(heading(.96,.99),"96% Confidence Interval")
  expect_identical(heading(.90,.90),"90% Confidence Interval")
  expect_identical(heading(c(.96,.96),.99),"96% Confidence Interval")
  expect_identical(heading(c(.96,.99),.99),
    "Confidence Interval (level reported per row)")
  expect_identical(heading(numeric(0),.90),"Confidence Interval")
})


test_that("all-zero paired differences do not report a zero-confidence interval", {
  message <- "Hodges-Lehmann confidence interval is unavailable because all paired differences are zero."
  expect_error(saRa:::.saraHodgesLehmann(rep(10,10),rep(10,10),
    paired=TRUE,details=TRUE),message,fixed=TRUE)
  expect_error(saRa:::.saraHodgesLehmann(c(10,NA,10),c(10,10,10),
    paired=TRUE),message,fixed=TRUE)
  expect_null(saRa:::.saraHLConfidenceNote(.95,0))
  expect_identical(saRa:::.saraHLConfidenceHeading(0,.95),"Confidence Interval")
  d <- data.frame(Before=rep(10,10),After=rep(10,10))
  result <- suppressWarnings(saRa::wilcoxT(d,
    pairs=list(list(i1="Before",i2="After")),ciHLps=TRUE,ciWidthps=95))
  table <- result$cisHLps
  expect_true(message %in% table$footnotes)
  expect_false(any(grepl("reported instead",table$footnotes,fixed=TRUE)))
  expect_identical(table$getColumn("cilHL")$superTitle,"Confidence Interval")
  for (col in 2:4) expect_true(all(is.nan(as.numeric(table$asDF[[col]]))))
})

test_that("an unavailable pair does not affect a valid HL pair", {
  d <- data.frame(Before=c(11,12,8,10,13,9,14,7,NA,15),
    After=rep(10,10),Equal=rep(10,10))
  result <- suppressWarnings(saRa::wilcoxT(d,pairs=list(
    list(i1="Equal",i2="After"),list(i1="Before",i2="After")),
    ciHLps=TRUE,ciWidthps=90))
  table <- result$cisHLps
  reference <- suppressWarnings(stats::wilcox.test(d$Before,d$After,
    paired=TRUE,conf.int=TRUE,conf.level=.90))
  expect_identical(table$getColumn("cilHL")$superTitle,
    saRa:::.saraHLConfidenceHeading(attr(reference$conf.int,"conf.level"),.90))
  expect_true(is.nan(as.numeric(table$asDF[[2]][1])))
  expect_equal(as.numeric(table$asDF[[2]][2]),1)
  expect_true(any(grepl("all paired differences are zero",table$footnotes,fixed=TRUE)))
})


test_that("no successful HL pair leaves a safe empty confidence collection", {
  expect_identical(saRa:::.saraHLConfidenceHeading(NULL,.95),"Confidence Interval")
  expect_identical(saRa:::.saraHLConfidenceHeading(unlist(list()),.95),
    "Confidence Interval")
})

test_that("one complete pair retains the HL estimate without a zero-confidence interval", {
  actual <- saRa:::.saraHodgesLehmann(c(NA,22,NA),rep(20,3),
    paired=TRUE,details=TRUE)
  expect_equal(actual$values[1],2)
  expect_true(all(is.nan(actual$values[2:3])))
  expect_null(actual$confidence)
  expect_match(actual$note,"only one complete pair",fixed=TRUE)
  d <- data.frame(Before=c(11,12,8,10,13,9,14,7,NA,15),After=rep(10,10),
    Single=c(NA,22,rep(NA,8)),After2=rep(20,10))
  result <- suppressWarnings(saRa::wilcoxT(d,pairs=list(
    list(i1="Before",i2="After"),list(i1="Single",i2="After2")),
    ciHLps=TRUE,ciWidthps=95))
  table <- result$cisHLps
  reference <- suppressWarnings(stats::wilcox.test(d$Before,d$After,
    paired=TRUE,conf.int=TRUE,conf.level=.95))
  expect_equal(as.numeric(table$asDF[[2]]),c(unname(reference$estimate),2))
  for (col in 3:4) expect_true(is.nan(as.numeric(table$asDF[[col]][2])))
  expect_equal(as.numeric(table$asDF[[3]][1]),unname(reference$conf.int[1]))
  expect_equal(as.numeric(table$asDF[[4]][1]),unname(reference$conf.int[2]))
  expect_true(actual$note %in% table$footnotes)
  expect_false(any(grepl("Confidence level: 0%",table$footnotes,fixed=TRUE)))
  expect_identical(table$getColumn("cilHL")$superTitle,
    saRa:::.saraHLConfidenceHeading(attr(reference$conf.int,"conf.level"),.95))
  single <- suppressWarnings(saRa::wilcoxT(d,pairs=list(list(i1="Single",i2="After2")),
    ciHLps=TRUE,ciWidthps=95))
  expect_identical(single$cisHLps$getColumn("cilHL")$superTitle,"Confidence Interval")
})

test_that("median plot legends identify unavailable intervals", {
  state <- saRa:::.saraMedianPlotData(c("Before","After"),c(22,20),
    matrix(NaN,2,2))
  expect_identical(saRa:::.saraMedianPlotLabel(state,95),
    "Median (confidence interval unavailable)")
  state$lower <- c(21,20); state$upper <- c(23,20)
  expect_identical(saRa:::.saraMedianPlotLabel(state,95),"Median (95% CI)")
})
