test_that("Gini and normalized Gini match exact descriptive references", {
  fixtures <- list(list(x=1:5, g=4/15, normalized=1/3),
    list(x=c(0,0,0,10), g=.75, normalized=1),
    list(x=rep(5,4), g=0, normalized=0),
    list(x=c(1,1,2,4), g=.3125, normalized=5/12))
  for (fixture in fixtures) {
    result <- saRa:::.saraInequality(fixture$x)
    expect_equal(result$gini, fixture$g)
    expect_equal(result$normalized, fixture$normalized)
    expect_equal(result$gini,
      DescTools::Gini(fixture$x, unbiased=FALSE, conf.level=NA))
    expect_equal(result$normalized,
      DescTools::Gini(fixture$x, unbiased=TRUE, conf.level=NA))
    reference <- DescTools::Lc(fixture$x)
    expect_equal(result$curve$p, reference$p)
    expect_equal(result$curve$L, reference$L)
    area <- sum(diff(result$curve$p) *
      (head(result$curve$L,-1) + tail(result$curve$L,-1))/2)
    expect_equal(result$gini, 1-2*area)
    expect_true(all(diff(result$curve$L) >= 0))
    expect_true(all(result$curve$L <= result$curve$p + 1e-14))
  }
})

test_that("Lorenz includes endpoints, retains zeros and ignores missing values", {
  result <- saRa:::.saraInequality(c(4,0,NA,2,0))
  expect_equal(result$n, 4)
  expect_equal(result$curve$p, c(0,.25,.5,.75,1))
  expect_equal(result$curve$L, c(0,0,0,1/3,1))
  expect_equal(result, saRa:::.saraInequality(c(0,0,2,4)))
  expect_equal(result, saRa:::.saraInequality(c(0,0,2,4)*100))
  expect_equal(saRa:::.saraInequality(c(1e308,1e308))$gini, 0)
  expect_equal(saRa:::.saraInequality(c(0,1e-300))$normalized, 1)
  single <- saRa:::.saraInequality(c(7,NA))
  expect_equal(single$gini, 0)
  expect_true(is.nan(single$normalized))
  expect_equal(single$curve, data.frame(p=c(0,1), L=c(0,1)))
  for (x in list(numeric(), c(NA_real_,NA_real_)))
    expect_error(saRa:::.saraInequality(x), "at least one")
  expect_error(saRa:::.saraInequality(c(-1,2)), "non-negative")
  expect_error(saRa:::.saraInequality(c(0,0)), "positive sum")
  expect_error(saRa:::.saraInequality(c(1,Inf)), "Infinite")
})

.lorenzCallback <- function(x) {
  context <- new.env(parent=asNamespace("saRa"))
  context$self <- list(data=data.frame("Score value"=x, check.names=FALSE))
  fun <- getFromNamespace("numSummaryClass", "saRa")$private_methods$.lorenz
  environment(fun) <- context
  fun(list(state="Score value"), ggplot2::theme_minimal(),
    list(fill=c("#112233","#445566"), color=c("#778899","#AABBCC")))
}

test_that("Lorenz uses ggplot and the supplied jamovi palette", {
  plot <- .lorenzCallback(c(0,1,2,3))
  expect_s3_class(plot,"ggplot")
  built <- ggplot2::ggplot_build(plot)
  expect_equal(unique(built$data[[1]]$fill), "#445566")
  expect_equal(unique(built$data[[2]]$colour), "#778899")
  expect_equal(unique(built$data[[3]]$colour), "#AABBCC")
  expect_equal(built$data[[3]]$x, (0:4)/4)
  expect_equal(built$data[[3]]$y, c(0,0,1/6,.5,1))
  expect_equal(plot$labels$title, "Score value")
  expect_error(ggplot2::ggplotGrob(plot), NA)
  for (x in list(rep(0,3), c(-1,2,3), c(NA_real_,NA_real_), c(1,Inf))) {
    plot <- .lorenzCallback(x)
    built <- ggplot2::ggplot_build(plot)
    expect_match(built$data[[1]]$label[1], "Lorenz curve unavailable")
    expect_error(ggplot2::ggplotGrob(plot), NA)
  }
  expect_error(ggplot2::ggplotGrob(.lorenzCallback(7)), NA)
})

test_that("one Gini option supplies both indices independently of the curve", {
  data <- data.frame("Score value"=1:5, check.names=FALSE)
  result <- saRa::numSummary(data, vars="Score value", freq=FALSE,
    n=FALSE, mean=FALSE, stddev=FALSE, min=FALSE, max=FALSE,
    gini=TRUE, lorenz=FALSE)
  table <- result$indicesnum
  expect_true(table$visible)
  expect_equal(table$getCell(col="Score value[gini]", rowNo=1)$value, 4/15)
  expect_equal(table$getCell(col="Score value[giniNormalized]", rowNo=1)$value, 1/3)
  curve <- saRa::numSummary(data, vars="Score value", freq=FALSE,
    n=FALSE, mean=FALSE, stddev=FALSE, min=FALSE, max=FALSE,
    gini=FALSE, lorenz=TRUE)
  expect_false(curve$indicesnum$visible)
  expect_equal(curve$plots$get("Score value")$get("lorenz")$state, "Score value")
})

test_that("undefined Gini cells explain the reason and recover without stale notes", {
  columns <- lapply(c("gini","giniNormalized"), function(x)
    list(name=paste0("x[",x,"]"), type="number"))
  table <- jmvcore::Table$new(options=jmvcore::Options$new(),
    name="indicesnum", columns=columns)
  table$addRow(rowKey=1, values=list())
  context <- new.env(parent=asNamespace("saRa"))
  context$self <- list(options=list(vars="x",gini=TRUE,mode=FALSE,
    geommean=FALSE,geomsd=FALSE), results=list(indicesnum=table))
  context$private <- list(colArgs=list(name=c("gini","giniNormalized"),
    visible=c("(gini)","(gini)")), .skipOption=function(x) FALSE,
    .getTrim=function() .05, .getWin=function() .05,
    .getNPerc=function() .9, .getPcValues=function() c(.25,.5,.75))
  cls <- getFromNamespace("numSummaryClass","saRa")
  compute <- cls$private_methods$.computeIndicesNum
  populate <- cls$private_methods$.populateIndicesTableNum
  environment(compute) <- environment(populate) <- context
  cell <- function(x) table$getCell(col=paste0("x[",x,"]"),rowNo=1)
  fixtures <- list(c(-1,2), c(0,0), NA_real_, 7, 1:5)
  for (x in fixtures) {
    result <- compute(x)
    expect_error(populate(list(indic=list(x=result))), NA)
    if (length(x)==1 && !is.na(x)) {
      expect_equal(cell("gini")$value,0)
      expect_length(cell("gini")$footnotes,0L)
      expect_match(cell("giniNormalized")$footnotes, "at least two")
    } else if (identical(x,1:5)) {
      expect_equal(cell("gini")$value,4/15)
      expect_length(cell("gini")$footnotes,0L)
      expect_length(cell("giniNormalized")$footnotes,0L)
    } else {
      expect_true(is.nan(cell("gini")$value))
      expect_length(cell("gini")$footnotes,1L)
      expect_length(cell("giniNormalized")$footnotes,1L)
    }
  }
  # Negative data are accepted by the numeric selector but invalid for Gini.
  # Their cell notes must not prevent another variable's valid statistics.
  result <- saRa::numSummary(data.frame(bad=c(1,-1,2), good=c(1,2,3)),
    vars=c("bad","good"), gini=TRUE, freq=FALSE)
  expect_true(is.nan(result$indicesnum$getCell(col="bad[gini]", rowNo=1)$value))
  expect_equal(result$indicesnum$getCell(col="good[gini]", rowNo=1)$value, 2/9)
  expect_match(result$indicesnum$getCell(col="bad[gini]", rowNo=1)$footnotes,
    "non-negative")
  # jmvcore rejects infinite input before the analysis callbacks run.
  expect_error(saRa::numSummary(data.frame(x=c(1,Inf,2)), vars="x",
    gini=TRUE, freq=FALSE), "infinite values")
})
