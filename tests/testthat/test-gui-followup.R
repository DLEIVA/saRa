test_that("add-on translation uses a fresh saRa context with the parent language", {
  language <- list(s="es",has=function(name) identical(name,"s"))
  pb <- list(names=".lang",options=list(language))
  parentOptions <- list(asProtoBuf=function() pb)
  owner <- list(options=list(translate=function(text,n=1) paste0("child:",text)),
                parent=list(options=parentOptions))
  context <- saRa:::.saraAddonTranslationOwner(owner)
  expect_s3_class(context$options,"Options")
  expect_identical(context$options$asProtoBuf()$names,".lang")
  expect_identical(context$options$asProtoBuf()$options[[1]]$s,"es")
  expect_identical(owner$parent$options$asProtoBuf(),pb)
  expect_false(identical(context$options,parentOptions))
  expect_identical(saRa:::.saraAddonTranslationOwner(list(options=owner$options)),
                   list(options=owner$options))
})

test_that("bivariate bars build for counts and percentages in both languages", {
  for (class in c("bivcatClass","bivordClass")) {
    cls <- getFromNamespace(class,"saRa")
    method <- cls$private_methods$.barPlotObject
    for (language in c("en","es")) for (axis in c("ycounts","ypc")) {
      context <- new.env(parent=asNamespace("saRa"))
      context$self <- list(options=list(barplot=TRUE,rows="Row score",cols="Column score",
        yaxis=axis,yaxisPc="total_pc",xaxis="xrows",bartype="dodge",
        translate=function(text,n=1) if(language=="es") paste0("es:",text) else text),
        data=data.frame("Row score"=ordered(c("a","b","a","b")),
          "Column score"=ordered(c("c","c","d","d")),check.names=FALSE))
      environment(method) <- context
      plot <- method(NULL,ggtheme=ggplot2::theme_minimal(),theme=NULL)
      expect_s3_class(plot,"ggplot")
      expect_error(ggplot2::ggplot_build(plot),NA)
      expect_equal(sum(plot$data$Counts),4)
      if (axis=="ycounts") expect_identical(plot$labels$y,
        if(language=="es") "es:Counts" else "Counts")
    }
  }
})

test_that("ordinal heatmap and alluvial callbacks build and draw", {
  cls <- getFromNamespace("bivordClass","saRa")
  for (kind in c("heatmap","alluvial")) {
    context <- new.env(parent=asNamespace("saRa"))
    context$self <- list(options=list(heatmap=TRUE,alluvial=TRUE,rows="A score",cols="B score",
      xaxis="xrows",translate=function(text,n=1) text),
      data=data.frame("A score"=ordered(c("a","b","a","b")),
        "B score"=ordered(c("c","c","d","d")),check.names=FALSE))
    context$private <- new.env(parent=emptyenv())
    build <- cls$private_methods[[paste0(".",kind,"Object")]]
    environment(build) <- context
    context$private[[paste0(".",kind,"Object")]] <- build
    callback <- cls$private_methods[[paste0(".",kind)]]
    environment(callback) <- context
    expect_error(ggplot2::ggplot_build(build(NULL)),NA)
    path <- tempfile(fileext=".pdf")
    grDevices::pdf(path)
    tryCatch(expect_true(callback(NULL)),finally={grDevices::dev.off();unlink(path)})
  }
})

test_that("bivariate drawing produces a diagnostic rather than an unfinished image", {
  path <- tempfile(fileext=".pdf")
  grDevices::pdf(path)
  tryCatch({
    expect_false(saRa:::.saraDrawBivariatePlot(function() NULL))
    expect_true(saRa:::.saraDrawBivariatePlot(function() stop("controlled failure")))
  },finally={grDevices::dev.off();unlink(path)})
})
