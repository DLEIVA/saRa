test_that("pooled symmetric lambda agrees with its reference definition", {
  tab <- matrix(c(10,0,5,5),2,byrow=TRUE)
  result <- saRa:::.saraNominalPrediction(tab)
  expect_equal(result$lambdaGKab,.5)
  expect_equal(result$lambdaGKba,0)
  expect_equal(result$lambdaGKsym,1/3)
  expect_equal(result$lambdaGKsym,DescTools::Lambda(tab,direction="symmetric"))
  withEmpty <- rbind(cbind(tab,0),0)
  expect_equal(saRa:::.saraNominalPrediction(withEmpty),result)
  expect_true(all(is.nan(unlist(saRa:::.saraNominalPrediction(matrix(0,2,2))))))
})

test_that("SP50 and SP80 preserve shifts, scale and reflection", {
  x <- c(1,2,3,5,10)
  for(coverage in c(.5,.8)) {
    f <- function(x) saRa:::.saraPercentileSkewness(x,coverage)
    expect_equal(f(x+100),f(x))
    expect_equal(f(x*3),f(x))
    expect_equal(f(-x),-f(x))
    expect_equal(f(c(-2,-1,0,1,2)),0)
    expect_true(is.nan(f(rep(0,5))))
  }
  expect_equal(saRa:::.saraPercentileSkewness(x,.5),1/3)
})

test_that("text ordinals report ordered categories without numeric codes", {
  x <- ordered(c("low","high","middle","high",NA),levels=c("low","middle","high"))
  opts <- list(median=TRUE,mode=TRUE,min=TRUE,max=TRUE,q1=TRUE,q3=TRUE,
    pc=TRUE,range=TRUE,h1=TRUE)
  result <- saRa:::.saraOrdinalSummary(x,opts,list(percentiles=function() c(0,.5,1)))
  expect_identical(result$median,"middle")
  expect_identical(result$mode,"high")
  expect_identical(result$min,"low")
  expect_identical(result$max,"high")
  expect_identical(result$q1,"low")
  expect_identical(result$q3,"high")
  expect_identical(result$perc2,"middle")
  expect_equal(result$n,4)
  expect_equal(result$missing,1)
  expect_true(is.nan(result$range))
  expect_match(attr(result,"errors")$range,"text categories")
  nominal <- factor(x,ordered=FALSE)
  expect_false(is.ordered(nominal))
  expect_error(saRa:::.saraOrdinalSummary(nominal,opts),"ordinal")
})

test_that("stored numeric scores take precedence over ordinal labels", {
  x <- ordered(c("none","some","many"),levels=c("none","some","many"))
  attr(x,"values") <- c(0,2,9)
  result <- saRa:::.saraOrdinalSummary(x,list(median=TRUE,range=TRUE,mode=TRUE))
  expect_equal(result$median,2)
  expect_equal(result$range,9)
  expect_type(result$mode,"double")
  expect_equal(result$mode,c(0,2,9))
})

test_that("invalid summary options leave unrelated statistics available", {
  result <- saRa:::.saraComputeNumeric(1:10,list(mean=TRUE,trimmean=TRUE,irange=TRUE),
    list(trim=function() .5,inner=function() 1.5))
  expect_equal(result$mean,5.5)
  expect_true(is.nan(result$trimmean))
  expect_true(is.nan(result$ir))
  expect_match(attr(result,"errors")$trimmean,"less than 50")
  expect_match(attr(result,"errors")$ir,"50 and 99")
  expect_error(saRa:::.saraValidateFences(-1,3),"Fence")
  expect_error(saRa:::.saraValidateFences(3,3),"Fence")
  expect_error(saRa:::.saraValidateFences(1.5,3),NA)
})

test_that("continuous windows adapt to distribution parameters and selected values", {
  for(rate in c(.1,10,100)) {
    o <- list(cdistros="exp",rate=rate)
    expect_equal(saRa:::.saraContinuousRange(o),c(0,qexp(.999,rate)))
    expect_equal(saRa:::.saraContinuousRange(o,100/rate),c(0,100/rate))
  }
  o <- list(cdistros="fdist",f1nu=2,f2nu=3)
  expect_equal(saRa:::.saraContinuousRange(o),c(0,qf(.99,2,3)))
  expect_true(all(is.finite(saRa:::.saraContinuousRange(o,c(-Inf,Inf)))))
  expect_error(saRa:::.saraContinuousRange(list(cdistros="exp",rate=0)))
})

test_that("distribution plots handle exact probabilities and infinite quantiles", {
  theme <- list(fill=c("#4477aa","#aaccdd"),color=c("black","#cc3311"))
  o <- list(cdistros="norm",mu=0,sigma=1,contppvalue=6,contx1value=0,
    contx2value=1,contpqvalue=1,conttail="left")
  plot <- saRa:::.saraDistributionPlot(o,"survival",FALSE,ggplot2::theme_minimal(),theme)
  expect_match(plot$labels$title,saRa:::.saraPlotNumber(pnorm(6,lower.tail=FALSE)),fixed=TRUE)
  expect_error(ggplot2::ggplot_build(plot),NA)
  plot <- saRa:::.saraDistributionPlot(o,"quantile",FALSE,ggplot2::theme_minimal(),theme)
  expect_match(plot$labels$title,"Inf")
  expect_match(plot$labels$caption,"Infinite quantiles")
  expect_error(ggplot2::ggplot_build(plot),NA)
  o <- list(distros="binom",binomn=2,binomp=.5,ppvalue=5,x1value=0,x2value=2,pqvalue=1,tail="left")
  plot <- saRa:::.saraDistributionPlot(o,"density",TRUE,ggplot2::theme_minimal(),theme)
  expect_match(plot$labels$title,"Pr(X = 5) = 0",fixed=TRUE)
  expect_error(ggplot2::ggplot_build(plot),NA)
})

test_that("bivariate plot builders preserve names containing spaces", {
  theme <- list(fill=c("#4477aa","#aaccdd"),color=c("black","#cc3311"))
  for(class in c("bivcatClass","bivordClass")) {
    cls <- getFromNamespace(class,"saRa")
    context <- new.env(parent=asNamespace("saRa"))
    context$self <- list(options=list(rows="A score",cols="B score",barplot=TRUE,
      heatmap=TRUE,alluvial=TRUE,xaxis="xrows",yaxis="ycounts",bartype="dodge"),
      data=data.frame("A score"=ordered(c("low","high","low","high")),
        "B score"=ordered(c("yes","no","no","yes")),check.names=FALSE))
    methods <- if(class=="bivcatClass") ".barPlotObject" else c(".barPlotObject",".heatmapObject",".alluvialObject")
    for(name in methods) {
      build <- cls$private_methods[[name]];environment(build) <- context
      plot <- build(NULL,ggplot2::theme_minimal(),theme)
      expect_identical(names(plot$data)[1:2],c("A score","B score"))
      expect_equal(nrow(plot$data),4L)
      count <- if(name==".barPlotObject") "Counts" else "Freq"
      expect_equal(sum(plot$data[[count]]),4)
      expect_error(ggplot2::ggplot_build(plot),NA)
    }
  }
})

test_that("rank tests explain all-tied samples", {
  expect_error(saRa:::.saraMWZ(rep(2,6),factor(rep(c("A","B"),each=3))),"All observations are tied")
  result <- saRa:::.saraMWTest(data.frame(dep=rep(2,6),group=factor(rep(c("A","B"),each=3))),"mwu")
  expect_true(jmvcore::isError(result))
})

test_that("the removed regression add-on is no longer registered", {
  expect_false(exists("linRegClass",envir=asNamespace("saRa"),inherits=FALSE))
  expect_false(exists(".saraAddonImage",envir=asNamespace("saRa"),inherits=FALSE))
})


test_that("translation markers use the public analysis translator without imports", {
  translate <- getFromNamespace(".","saRa")
  run <- function() {
    self <- list(options=list(translate=function(text,n=1) paste0("translated: ",text," / ",n)))
    translate("A complete sentence",2)
  }
  expect_identical(run(),"translated: A complete sentence / 2")
  expect_identical(translate("English fallback"),"English fallback")
  owner <- list(options=list(translate=function(text,n=1) paste0("translated: ",text)))
  expect_identical(saRa:::.saraMessage("Normal",owner),"translated: Normal")
})


test_that("translation markers support jmvcore 2.2 Options without a translator", {
  translate <- getFromNamespace(".","saRa")
  # An active binding reproduces the old member-lookup error without S3 registration.
  options <- new.env(parent=emptyenv())
  makeActiveBinding("translate",function(value) stop("options$translate does not exist"),options)
  run <- function() {
    self <- list(options=options)
    translate("Original English text")
  }
  expect_identical(run(),"Original English text")
  expect_identical(saRa:::.saraMessage("Normal",list(options=options)),"Normal")
  # Also exercise the actually installed jmvcore implementation.
  runInstalled <- function() {
    self <- list(options=jmvcore::Options$new())
    translate("Original English text")
  }
  expect_identical(runInstalled(),"Original English text")
})


test_that("ordinal range is omitted for text-only selections and kept for numeric selections", {
  text <- ordered(c("low","middle","high"),levels=c("low","middle","high"))
  numeric <- ordered(c("zero","two","five"),levels=c("zero","two","five"))
  attr(numeric,"values") <- c(0,2,5)
  data <- data.frame(text=text,numeric=numeric)
  expect_false(saRa:::.saraOrdinalHasNumeric(data,"text"))
  expect_true(saRa:::.saraOrdinalHasNumeric(data,c("text","numeric")))
  # Base factor subsetting may drop the custom stored-score attribute.
  # A jamovi-style header fixture must explicitly retain that metadata.
  header <- data[FALSE,,drop=FALSE]
  attr(header$numeric,"values") <- attr(data$numeric,"values",exact=TRUE)
  expect_true(saRa:::.saraOrdinalHasNumeric(header,"numeric"))
  expect_false(saRa:::.saraOrdinalHasNumeric(header,"text"))
  # Without scores, labels such as zero/two/five cannot be assumed numeric.
  attr(header$numeric,"values") <- NULL
  expect_false(saRa:::.saraOrdinalHasNumeric(header,"numeric"))
  result <- saRa::ordSummary(data,vars="text",range=TRUE)
  expect_false("Range" %in% unlist(result$indicesord$asDF,use.names=FALSE))
  result <- saRa::ordSummary(data,vars=c("text","numeric"),range=TRUE)
  expect_true("Range" %in% unlist(result$indicesord$asDF,use.names=FALSE))
  result <- saRa::ordSummary(data,vars="numeric",range=FALSE)
  expect_false("Range" %in% unlist(result$indicesord$asDF,use.names=FALSE))
})

test_that("bivariate correlations explain constant variables and clear old footnotes", {
  columns <- c(
    "v[covar]", "v[pearsonR]", "v[spearmanR]", "v[tauK]",
    "v[pearsonR2]", "v[spearmanR2]", "v[kendallR2]"
  )
  correlationColumns <- columns[-1L]
  note <- paste(
    "Correlation is undefined because",
    "at least one variable is constant."
  )
  
  makeCell <- function() {
    cell <- new.env(parent = emptyenv())
    cell$value <- NaN
    cell$footnotes <- character(0)
    
    cell$setValue <- function(value) {
      cell$value <- value
      cell$footnotes <- character(0)
      invisible(NULL)
    }
    
    cell$addFootnote <- function(note) {
      cell$footnotes <- c(cell$footnotes, note)
      invisible(NULL)
    }
    
    cell
  }
  
  cells <- setNames(
    lapply(columns, function(column) makeCell()),
    columns
  )
  
  table <- list(
    setRow = function(rowNo, values) {
      for (column in names(values))
        cells[[column]]$setValue(values[[column]])
    },
    getCell = function(rowNo, col) cells[[col]],
    addFootnote = function(rowNo, col, note) {
      cells[[col]]$addFootnote(note)
    }
  )
  
  context <- new.env(parent = asNamespace("saRa"))
  context$self <- list(
    options = list(xvar = "X", yvar = "Y"),
    results = list(asocind = table)
  )
  run <- getFromNamespace("bivnumClass", "saRa")$private_methods$.run
  environment(run) <- context
  
  # Test constant X, constant Y, and both variables constant.
  samples <- list(
    data.frame(X = rep(3, 5), Y = 1:5),
    data.frame(X = 1:5, Y = rep(3, 5)),
    data.frame(X = rep(3, 5), Y = rep(3, 5))
  )
  
  for (data in samples) {
    context$self$data <- data
    expect_warning(run(), NA)
    
    expect_equal(cells[["v[covar]"]]$value, 0)
    expect_length(cells[["v[covar]"]]$footnotes, 0L)
    
    for (column in correlationColumns) {
      expect_true(is.nan(cells[[column]]$value))
      expect_identical(cells[[column]]$footnotes, note)
    }
  }
  
  # Reuse the same cells: no stale notes after variability returns.
  context$self$data <- data.frame(X = 1:5, Y = c(10, 8, 6, 4, 2))
  expect_warning(run(), NA)
  
  expected <- c(-5, -1, -1, -1, 1, 1, 1)
  for (i in seq_along(columns)) {
    expect_equal(cells[[columns[i]]]$value, expected[i])
    expect_length(cells[[columns[i]]]$footnotes, 0L)
  }
  
  # A non-perfect association also retains the correct calculations.
  data <- data.frame(X = c(1, 2, 2, 4, 5), Y = c(5, 1, 3, 2, 4))
  context$self$data <- data
  expect_warning(run(), NA)
  
  reference <- c(
    stats::cov(data$X, data$Y),
    stats::cor(data$X, data$Y, method = "pearson"),
    stats::cor(data$X, data$Y, method = "spearman"),
    stats::cor(data$X, data$Y, method = "kendall")
  )
  expected <- c(reference, reference[2:4]^2)
  
  for (i in seq_along(columns)) {
    expect_equal(cells[[columns[i]]]$value, expected[i])
    expect_length(cells[[columns[i]]]$footnotes, 0L)
  }
})

test_that("numerical scatterplot marginals follow their corresponding axes", {
  # Different scales reveal accidental swaps between X and Y.
  data <- data.frame(
    "X score" = c(0:39, 100),
    "Y score" = seq(200, 280, by = 2),
    check.names = FALSE
  )
  
  context <- new.env(parent = asNamespace("saRa"))
  context$self <- list(
    options = list(
      xvar = "X score",
      yvar = "Y score",
      scat = TRUE,
      line = "none",
      se = FALSE,
      marg = "none"
    ),
    data = data
  )
  context$captured <- new.env(parent = emptyenv())
  
  build <- getFromNamespace("bivnumClass", "saRa")$private_methods$.scat
  environment(build) <- context
  
  # Capture the actual marginal plots without changing production code
  # or replacing cowplot functions.
  body(build) <- substitute({
    on.exit({
      for (name in c("xbox", "ybox", "xhist", "yhist")) {
        if (exists(name, inherits = FALSE)) {
          assign(
            name,
            get(name, inherits = FALSE),
            envir = captured
          )
        }
      }
    }, add = TRUE)
    
    ORIGINAL
  }, list(ORIGINAL = body(build)))
  
  context$self$options$marg <- "box"
  boxPlot <- build(NULL, ggplot2::theme_minimal())
  
  top <- context$captured$xbox
  right <- context$captured$ybox
  
  # Explicit orientations: horizontal top box, vertical right box.
  expect_identical(top$layers[[1]]$geom_params$orientation, "y")
  expect_identical(right$layers[[1]]$geom_params$orientation, "x")
  expect_false(inherits(top$coordinates, "CoordFlip"))
  expect_false(inherits(right$coordinates, "CoordFlip"))
  
  topBox <- ggplot2::ggplot_build(top)$data[[1]]
  rightBox <- ggplot2::ggplot_build(right)$data[[1]]
  
  expect_equal(nrow(topBox), 1L)
  expect_equal(nrow(rightBox), 1L)
  
  expect_equal(
    unname(c(topBox$xlower, topBox$xmiddle, topBox$xupper)),
    unname(stats::quantile(data[["X score"]], c(.25, .5, .75)))
  )
  expect_equal(
    unname(c(rightBox$lower, rightBox$middle, rightBox$upper)),
    unname(stats::quantile(data[["Y score"]], c(.25, .5, .75)))
  )
  
  expect_equal(topBox$y, 1)
  expect_equal(rightBox$x, 1)
  expect_true(100 %in% unlist(topBox$outliers))
  
  expect_error(
    ggplot2::ggplotGrob(boxPlot),
    NA
  )
  
  context$self$options$marg <- "hist"
  histogramPlot <- build(NULL, ggplot2::theme_minimal())
  
  top <- context$captured$xhist
  right <- context$captured$yhist
  
  # Top histogram retains X; right histogram rotates Y onto the vertical axis.
  expect_false(inherits(top$coordinates, "CoordFlip"))
  expect_true(inherits(right$coordinates, "CoordFlip"))
  
  expect_equal(
    rlang::eval_tidy(top$layers[[1]]$mapping$x, data = data),
    data[["X score"]]
  )
  expect_equal(
    rlang::eval_tidy(right$layers[[1]]$mapping$x, data = data),
    data[["Y score"]]
  )
  
  topHist <- ggplot2::ggplot_build(top)$data[[1]]
  rightHist <- ggplot2::ggplot_build(right)$data[[1]]
  
  expect_equal(sum(topHist$count), nrow(data))
  expect_equal(sum(rightHist$count), nrow(data))
  
  expect_error(
    ggplot2::ggplotGrob(histogramPlot),
    NA
  )
})

test_that("all eight example datasets are registered and available", {
  root <- testthat::test_path("..", "..")
  manifest <- yaml::read_yaml(file.path(root, "jamovi", "0000.yaml"))
  
  expected <- c(
    "phobia.RData",
    "beliefs.RData",
    "NEOFFI.RData",
    "attitudes.immigration.RData",
    "Ginzberg.RData",
    "polynomial_regression.RData",
    "mediation.RData",
    "Anscombe.RData"
  )
  
  paths <- vapply(manifest$datasets, function(x) x$path, character(1))
  expect_setequal(paths, expected)
  expect_length(paths, 8L)
  expect_true(all(file.exists(file.path(root, "data", paths))))
  
  anscombe <- manifest$datasets[[match("Anscombe.RData", paths)]]
  expect_setequal(anscombe$tags, c("Descriptives", "Correlation"))
})
