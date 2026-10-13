test_that("geometric summaries remain on the original scale without rounding", {
  opts <- setNames(as.list(rep(FALSE, 35)), c(
    "mean","median","mode","geommean","trimean","avquartile","midrange",
    "trimmean","winsmean","variance","stddev","geomsd","meandev","cv",
    "range","iqr","qd","mad","rcv","irange","min","max","q1","q2","q3",
    "pc","beta1","gamma1","h1","h3","beta2","gamma2","k2","k3","n"))
  opts$geommean <- TRUE; opts$geomsd <- TRUE
  r <- saRa:::.saraComputeNumeric(c(1,10,100), opts, list())
  expect_equal(r$geommean, 10)
  expect_equal(r$geomsd, 10)
  expect_equal(saRa:::.saraComputeNumeric(c(1,2), opts, list())$geommean, sqrt(2))
  expect_true(is.na(saRa:::.saraComputeNumeric(c(0,1), opts, list())$geommean))
  r <- saRa:::.saraComputeNumeric(c(NA_real_,NA_real_), opts, list())
  expect_equal(r$n, 0); expect_equal(r$missing, 2)
  expect_true(is.nan(r$geommean)); expect_true(is.nan(r$geomsd))
})

test_that("all Yule indices have the right values and reject invalid tables", {
  yule <- saRa:::.saraYulePhi
  expect_equal(unlist(yule(matrix(c(10,0,0,10),2))),c(Y=1,Q=1,V=1))
  expect_equal(unlist(yule(matrix(c(0,10,10,0),2))),c(Y=-1,Q=-1,V=-1))
  expect_equal(unlist(yule(matrix(10,2,2))),c(Y=0,Q=0,V=0))
  tab <- matrix(c(12,3,5,10),2)
  a <- tab[1,1];b <- tab[1,2];c <- tab[2,1];d <- tab[2,2]
  expect_equal(yule(tab)$Y,(sqrt(a*d)-sqrt(b*c))/(sqrt(a*d)+sqrt(b*c)))
  expect_equal(yule(tab)$Q,(a*d-b*c)/(a*d+b*c))
  expect_equal(yule(tab)$V,(a*d-b*c)/sqrt(prod(rowSums(tab))*prod(colSums(tab))))
  expect_equal(yule(tab*100),yule(tab))
  for (bad in list(matrix(0,2,2),matrix(1,3,2),matrix(c(-1,1,2,3),2),
                   matrix(c(NA,1,2,3),2),matrix(c(Inf,1,2,3),2)))
    expect_true(all(is.nan(unlist(yule(bad)))))
  expect_true(is.nan(yule(matrix(c(0,0,1,1),2))$V))
})

test_that("Wilson's e counts unordered pairs once", {
  # Twenty subjects on a perfect diagonal: 100 concordant pairs, 90 tied on both.
  expect_equal(saRa:::.saraWilson(100,0,20,90), 1)
  expect_equal(saRa:::.saraWilson(0,100,20,90), -1)
  expect_true(is.nan(saRa:::.saraWilson(0,0,10,45)))
})

test_that("signed ranks agree with R's uncorrected normal approximation", {
  x <- c(9,5,7,11,2,8,8,3); y <- c(1,6,5,10,2,4,10,3)
  for (alternative in c("two.sided","less","greater")) {
    result <- saRa:::.saraSignedRanks(x,y,alternative)
    reference <- suppressWarnings(wilcox.test(x,y,paired=TRUE,
      exact=FALSE,correct=FALSE,alternative=alternative))
    expect_equal(result$W, unname(reference$statistic))
    expect_equal(result$p.value, reference$p.value)
    expect_lte(abs(result$correlation), 1)
  }
  expect_equal(saRa:::.saraSignedRanks(c(1,3,4),c(1,1,1))$correlation, 1)
  expect_true(is.nan(saRa:::.saraSignedRanks(c(1,1),c(1,1))$correlation))
})

test_that("Mann-Whitney ties and unequal group sizes agree with R", {
  x <- c(1,1,2,4,5,5,7,9)
  g <- factor(c(rep("A",3),rep("B",5)))
  for (alternative in c("two.sided","less","greater")) {
    result <- saRa:::.saraMWZ(dep=x,group=g,alternative=alternative)
    reference <- suppressWarnings(wilcox.test(x~g,exact=FALSE,correct=FALSE,
                                              alternative=alternative))
    expect_equal(result$t, as.numeric(table(x)))
    expect_equal(result$U, unname(reference$statistic))
    expect_equal(result$p.value, reference$p.value)
  }
})

test_that("discrete ranges are finite at degenerate and small-population limits", {
  upper <- saRa:::.saraDiscreteUpper
  expect_equal(upper(list(distros="hypergeom",hyperm=1,hypern=10,hyperk=1)),1L)
  expect_equal(upper(list(distros="hypergeom",hyperm=1,hypern=0,hyperk=1)),1L)
  expect_equal(upper(list(distros="geom",geomp=1)),0L)
  expect_equal(upper(list(distros="poiss",lambda=0)),0L)
  expect_error(upper(list(distros="geom",geomp=0)))
  expect_error(upper(list(distros="negbinom",negbinomr=2,negbinomp=0)))
  expect_error(upper(list(distros="hypergeom",hyperm=1,hypern=1,hyperk=3)))
})

test_that("grouped ECDFs preserve membership and handle missing observations", {
  value <- c(100,2,200,1,300,NA,3)
  group <- c("large","small","large","small","large","small","small")
  df <- saRa:::.saraECDFData(value,group)
  expect_equal(sort(df$value[df$group=="large"]), c(100,200,300))
  expect_equal(sort(df$value[df$group=="small"]), c(1,2,3))
  for (g in unique(df$group)) {
    d <- df[df$group==g,]
    expect_equal(mean(d$z),0,tolerance=1e-12)
    expect_equal(sd(d$z),1)
    expect_equal(d$empirical,c(1/3,2/3,1))
  }
  expect_null(saRa:::.saraECDFData(c(1,1,NA)))
})

test_that("bootstrap intervals are reproducible and do not alter caller RNG state", {
  set.seed(321); oldSeed <- .Random.seed
  x <- c(1,2,3,4,6,8,10,13,17)
  a <- saRa:::.saraBootstrapMedian(x,R=199)
  expect_identical(.Random.seed, oldSeed)
  expect_equal(a,saRa:::.saraBootstrapMedian(x,R=199))
  expect_identical(.Random.seed, oldSeed)
})

test_that("ordinal values and requested inner ranges use stored values", {
  method <- getFromNamespace("ordSummaryClass","saRa")$private_methods$.computeIndicesOrd
  opts <- setNames(as.list(rep(FALSE,23)), c("median","mode","trimean","avquartile",
     "midrange","range","iqr","qd","mad","rcv","min","max","q1","q2","q3",
     "pc","irange","h1","h3","k2","k3","n","missing"))
  opts$median <- TRUE; opts$min <- TRUE; opts$max <- TRUE; opts$range <- TRUE
  opts$iqr <- TRUE; opts$irange <- TRUE; opts$NPerc <- 90
  environment(method) <- list2env(list(self=list(options=opts)),parent=asNamespace("saRa"))
  x <- ordered(c(0,1,2,3,4),levels=0:4)
  r <- method(x)
  expect_equal(r$min,0); expect_equal(r$max,4); expect_equal(r$median,2)
  expect_equal(unname(r$ir),3.6)
  x <- ordered(c(1,2,5),levels=c(1,2,5))
  r <- method(x)
  expect_equal(r$max,5); expect_equal(unname(r$range),4)
})

test_that("normality analyses run only selected tests and tolerate small groups", {
  d <- data.frame(score=c(1,2,4,6,9),group=factor(c("A","A","B","B","B")))
  expect_error(saRa::testnorm(d, vars="score", groupBy="group",
    chisqtest=FALSE,kstest=TRUE,swtest=FALSE,adtest=FALSE,
    lillietest=FALSE,sftest=FALSE,cvmtest=FALSE), NA)
  expect_error(saRa::testnorm(d, vars="score", groupBy="group",
    chisqtest=FALSE,kstest=FALSE,swtest=FALSE,adtest=TRUE,
    lillietest=FALSE,sftest=FALSE,cvmtest=FALSE), NA)
})

test_that("paired Hodges-Lehmann table agrees with the paired R reference", {
  d <- data.frame(pre=c(2,4,7,11,15,18,22,25), post=c(1,2,4,5,9,13,16,17))
  reference <- suppressWarnings(wilcox.test(d$pre,d$post,paired=TRUE,
                                            conf.int=TRUE,conf.level=.90))
  result <- suppressWarnings(saRa::wilcoxT(d,pairs=list(list(i1="pre",i2="post")),
                                          ciHLps=TRUE,ciWidthps=90))
  table <- result$cisHLps$asDF
  expect_equal(as.numeric(table[[2]]),unname(reference$estimate))
  expect_equal(as.numeric(table[[3]]),reference$conf.int[1])
  expect_equal(as.numeric(table[[4]]),reference$conf.int[2])
})

test_that("Chuprov's T reaches one for a perfect 3 by 3 association", {
  tab <- diag(100,3)
  chi <- unname(chisq.test(tab,correct=FALSE)$statistic)
  expect_equal(saRa:::.saraChuprov(chi,sum(tab),3,3),1)
})

test_that("listwise paired median CIs use each pair and the requested confidence width", {
  d <- data.frame(unused=rep(0,12),a=1:12,b=(1:12)+2,
                  c=(1:12)*10,d=(1:12)*10+5)
  pairs <- list(list(i1="a",i2="b"),list(i1="c",i2="d"))
  result <- suppressWarnings(saRa::wilcoxT(d,pairs=pairs,missps="listwise",
                    ciMediansps=TRUE,ciWidthps=90,ciMethodps="exact"))
  table <- result$cisMedps$asDF
  expect_equal(as.character(table[[2]]),c("a","c"))
  expect_equal(as.character(table[[6]]),c("b","d"))
  expected <- DescTools::MedianCI(d$c,conf.level=.90,method="exact")[2:3]
  expect_equal(c(as.numeric(table[[4]][2]),as.numeric(table[[5]][2])),unname(expected))
})

test_that("regression diagnostics agree with lmtest for each fitted model", {
  x <- seq(-2,2,length.out=120)
  d <- data.frame(x=x,y=1+2*x+0.5*x^2+sin(seq_along(x)))
  for (model in list(lm(y~x,d),lm(y~x+I(x^2),d))) {
    result <- saRa:::.saraRegressionTests(model)
    reference <- list(reset=lmtest::resettest(model,power=2:3,type="fitted"),
                      bp=lmtest::bptest(model,studentize=FALSE),
                      koenker=lmtest::bptest(model,studentize=TRUE))
    for (name in names(reference)) {
      expect_null(result[[name]]$error)
      expect_equal(result[[name]]$stat,unname(reference[[name]]$statistic))
      expect_equal(result[[name]]$p,reference[[name]]$p.value)
      expect_equal(result[[name]]$df1,unname(reference[[name]]$parameter)[1])
    }
  }
})

test_that("residual ACF uses automatic lags and fitted rows after exclusions", {
  x <- seq_len(100)
  d <- data.frame(x=x,y=x+sin(x))
  d$y[10] <- NA
  model <- lm(y~x,d,na.action=na.exclude)
  result <- saRa:::.saraResidualACF(model,2L)
  reference <- stats::acf(as.numeric(model$residuals),plot=FALSE)
  expect_equal(result$lag,as.numeric(reference$lag))
  expect_equal(result$acf,as.numeric(reference$acf))
  expect_equal(result$n,99L)
  expect_equal(result$model,2L)
  expect_type(saRa:::.saraResidualACF(list(residuals=rep(0,10)))$error,"character")
  expect_type(saRa:::.saraResidualACF(list(residuals=1))$error,"character")
})

test_that("regression add-on leaves incomplete and disabled analyses alone", {
  cls <- getFromNamespace("linRegClass","saRa")
  for (opts in list(list(dep=NULL,norm=TRUE,durbin=TRUE),
                   list(dep="y",norm=FALSE,durbin=FALSE),
                   list(dep="y",covs=character(),factors=character(),
                        blocks=list(character()),norm=TRUE,durbin=TRUE))) {
    parent <- new.env(parent=emptyenv())
    parent$options <- opts
    makeActiveBinding("models",function(value) stop("Incomplete models were requested"),parent)
    methods <- cls$private_methods
    context <- new.env(parent=asNamespace("saRa"))
    context$self <- list(parent=parent)
    context$private <- new.env(parent=emptyenv())
    for (name in c(".getModelTerms",".isReady",".enabled",".table")) {
      method <- methods[[name]]
      environment(method) <- context
      context$private[[name]] <- method
    }
    for (name in c(".init",".run")) {
      method <- methods[[name]]
      environment(method) <- context
      expect_error(method(),NA)
    }
  }
})

test_that("regression tables explicitly create their rows before filling", {
  parentOptions <- jmvcore::Options$new()
  assumptions <- jmvcore::Group$new(options=parentOptions,name="assump")
  # Regression's generated model group has an active assump binding;
  # a generic Group does not. Preserve that interface in the fixture.
  modelGroup <- list(assump=assumptions)
  groups <- list(get=function(key) modelGroup)
  parent <- list(options=list(dep="y",norm=TRUE,durbin=FALSE,
      blocks=list("x")),results=list(models=groups))
  # Table expressions use parent options, whereas the fake option values drive init.
  # Use a small R6 option fixture with real Options evaluation.
  parent$options <- R6::R6Class("RegressionOptionFixture",inherit=jmvcore::Options,
    public=list(dep="y",norm=TRUE,durbin=FALSE,resPlots=FALSE,blocks=list("x")))$new()
  cls <- getFromNamespace("linRegClass","saRa")
  context <- new.env(parent=asNamespace("saRa"))
  context$self <- list(parent=parent)
  context$private <- new.env(parent=emptyenv())
  for (name in c(".getModelTerms",".isReady",".enabled",".table")) {
    method <- cls$private_methods[[name]]
    environment(method) <- context
    context$private[[name]] <- method
  }
  method <- cls$private_methods$.init
  environment(method) <- context
  expect_error(method(),NA)
  reset <- assumptions$get("reset")
  bp <- assumptions$get("saraBP")
  expect_equal(reset$rowCount,1L)
  expect_equal(bp$rowCount,4L)
  expect_error(reset$setRow(rowNo=1,list(`s[reset]`=2,`p[reset]`=.1)),NA)
  expect_error(bp$setRow(rowNo=2,list(stat=3,df1=1,p=.08)),NA)
})

test_that("inserted ACF images retain the add-on renderer owner", {
  calls <- character()
  owner <- list(.render=function(funName,image,...) {
    calls <<- c(calls,funName)
    TRUE
  },.createImage=function(funName,image,...) {
    calls <<- c(calls,funName)
    TRUE
  })
  options <- jmvcore::Options$new()
  image <- saRa:::.saraAddonImage$new(owner=owner,options=options,
    name="saraACF1",renderFun=".diagnosticPlot")
  group <- jmvcore::Group$new(options=options,name="assump")
  group$add(image)
  expect_identical(image$analysis,owner)
  expect_true(image$.render())
  expect_true(image$.createImages())
  expect_identical(calls,c(".diagnosticPlot",".diagnosticPlot"))
})

test_that("multi-lag DW reproduces car bootstrap and restores random state", {
  d <- data.frame(x=seq_len(40),y=seq_len(40)+sin(seq_len(40)))
  model <- lm(y~x,d)
  set.seed(811)
  before <- .Random.seed
  result <- saRa:::.saraRegressionDW(model)
  expect_null(result$error)
  expect_identical(.Random.seed,before)
  reference <- saRa:::.saraRegressionSeed(function()
    car::durbinWatsonTest(model,max.lag=10,simulate=TRUE,reps=5000,
                         method="resample",alternative="two.sided"))
  expect_equal(result$data$dw,as.numeric(reference$dw))
  expect_equal(result$data$acf,as.numeric(reference$r))
  expect_equal(result$data$p,as.numeric(reference$p))
  expect_equal(result$data$lag,seq_len(10))
  expect_identical(.Random.seed,before)
})

test_that("regression normality and heteroskedasticity tests match their references", {
  x <- seq(-2,2,length.out=100)
  model <- lm(y~x,data=data.frame(x=x,y=1+x+sin(seq_along(x))))
  normal <- saRa:::.saraRegressionNormality(model)
  refs <- list(sw=shapiro.test(model$residuals),ks=nortest::lillie.test(model$residuals),
               ad=nortest::ad.test(model$residuals))
  for (name in names(refs)) {
    expect_null(normal[[name]]$error)
    expect_equal(normal[[name]]$stat,unname(refs[[name]]$statistic))
    expect_equal(normal[[name]]$p,refs[[name]]$p.value)
  }
  results <- saRa:::.saraRegressionTests(model)
  gq <- lmtest::gqtest(model,order.by=model$fitted.values,alternative="two.sided")
  hmc <- saRa:::.saraRegressionSeed(function()
    lmtest::hmctest(model,order.by=model$fitted.values,simulate.p=TRUE,nsim=5000))
  expect_equal(results$gq$stat,unname(gq$statistic))
  expect_equal(results$gq$p,gq$p.value)
  expect_equal(results$hmc$stat,unname(hmc$statistic))
  expect_equal(results$hmc$p,hmc$p.value)
})

test_that("influence indices retain fitted rows and match base R with exclusions", {
  d <- data.frame(x=seq_len(60),y=seq_len(60)+sin(seq_len(60)))
  d$y[12] <- NA
  model <- lm(y~x,d,na.action=na.exclude)
  result <- saRa:::.saraRegressionFrame(model)
  reference <- model
  reference$na.action <- NULL
  expect_null(result$error)
  expect_equal(result$data$case,rownames(model.frame(reference)))
  expect_equal(result$data$studentized,as.numeric(rstudent(reference)))
  expect_equal(result$data$hat,as.numeric(hatvalues(reference)))
  expect_equal(result$data$cook,as.numeric(cooks.distance(reference)))
  expect_false("12" %in% result$data$case)
  expected <- sort(unique(c(head(order(abs(rstudent(reference)),decreasing=TRUE),2),
    head(order(hatvalues(reference),decreasing=TRUE),2),
    head(order(cooks.distance(reference),decreasing=TRUE),2))))
  expect_equal(result$selected,expected)
})

test_that("all regression plot types build from compact state", {
  d <- data.frame(x=seq_len(40),y=seq_len(40)+sin(seq_len(40)))
  model <- lm(y~x,d)
  frame <- saRa:::.saraRegressionFrame(model)
  for (kind in c("residual","scale","qq","influence")) {
    plot <- saRa:::.saraRegressionPlot(frame,kind)
    expect_error(ggplot2::ggplot_build(plot),NA)
  }
  plot <- saRa:::.saraRegressionPlot(saRa:::.saraResidualACF(model),"acf")
  expect_error(ggplot2::ggplot_build(plot),NA)
})

test_that("regression plots draw on a device and failed draws yield a visible result", {
  d <- data.frame(x=seq_len(40),y=seq_len(40)+sin(seq_len(40)))
  model <- lm(y~x,d)
  state <- saRa:::.saraRegressionFrame(model)
  path <- tempfile(fileext=".pdf")
  grDevices::pdf(path,width=9,height=6)
  on.exit({grDevices::dev.off();unlink(path)},add=TRUE)
  for (kind in c("residual","scale","qq","influence")) {
    # Draw directly so underlying errors cannot be hidden by the fallback.
    expect_error(print(saRa:::.saraRegressionPlot(state,kind)),NA)
    expect_true(saRa:::.saraDrawRegressionPlot(list(values=state,kind=kind)))
  }
  expect_true(saRa:::.saraDrawRegressionPlot(NULL))
  expect_true(saRa:::.saraDrawRegressionPlot(list(values=state,kind="invalid")))
})

test_that("influence plot retains numeric Cook scale after a discrete jamovi palette", {
  d <- data.frame(x=seq_len(40),y=seq_len(40)+sin(seq_len(40)))
  state <- saRa:::.saraRegressionFrame(lm(y~x,d))
  theme <- list(ggplot2::theme_minimal(),
    ggplot2::scale_fill_manual(values=c("#3366AA","#CC6677")),
    ggplot2::scale_colour_manual(values=c("#3366AA","#CC6677")))
  plot <- saRa:::.saraRegressionPlot(state,"influence",theme)
  expect_error(ggplot2::ggplot_build(plot),NA)
  path <- tempfile(fileext=".pdf")
  grDevices::pdf(path)
  on.exit({grDevices::dev.off();unlink(path)},add=TRUE)
  expect_error(print(plot),NA)
})


test_that("internal Q-Q calculations preserve 0.1.2 pointwise bands", {
  z <- as.vector(scale(c(-2,-1,0,1,4)))
  d <- saRa:::.saraQQData(data.frame(z=z, group="A"))
  sigma <- sqrt(mean((z-mean(z))^2))
  x <- qnorm(ppoints(length(z)), mean(z), sigma)
  slope <- diff(quantile(z,c(.25,.75),names=FALSE))/
    diff(qnorm(c(.25,.75),mean(z),sigma))
  line <- slope*x+quantile(z,.25,names=FALSE)-slope*qnorm(.25,mean(z),sigma)
  width <- qnorm(.975)*slope/dnorm(x,mean(z),sigma)*
    sqrt(ppoints(length(z))*(1-ppoints(length(z)))/length(z))
  expect_equal(d$theoretical,x)
  expect_equal(d$sample,sort(z))
  expect_equal(d$lower,as.numeric(line-width))
  expect_equal(d$upper,as.numeric(line+width))
  grouped <- saRa:::.saraQQData(rbind(data.frame(z=z,group="A"),
                                    data.frame(z=-z,group="B")))
  expect_equal(table(grouped$group),table(rep(c("A","B"),each=5)))
  plot <- saRa:::.saraQQPlot(data.frame(x=c(-2,-1,0,1,4)), "x")
  expect_silent(ggplot2::ggplot_build(plot))
  expect_null(saRa:::.saraQQData(data.frame(z=rep(1,3),group="A")))
})


test_that("grouped normality populates rows after header-only initialization", {
  opts <- list(vars=c("x","y"),groupBy="g",chisqtest=FALSE,kstest=FALSE,
    swtest=TRUE,adtest=FALSE,lillietest=FALSE,sftest=FALSE,cvmtest=FALSE)
  table <- jmvcore::Table$new(options=jmvcore::Options$new(),name="normality",
    columns=list(list(name="depvar",type="text"),list(name="group",type="text"),
      list(name="stat[swtest]",type="number"),list(name="p[swtest]",type="number")))
  context <- new.env(parent=asNamespace("saRa"))
  context$self <- list(options=opts,results=list(normtests=table),
    data=data.frame(x=numeric(),y=numeric(),g=factor(levels=c("A","B"))))
  context$private <- list(.initPlots=function() NULL,.preparePlots=function() NULL)
  cls <- getFromNamespace("testnormClass","saRa")
  init <- cls$private_methods$.init; environment(init) <- context
  run <- cls$private_methods$.run; environment(run) <- context
  expect_error(init(),NA)
  d <- data.frame(x=c(1,3,2,5,8,4,6,10),y=c(2,4,8,3,6,9,5,12),
                  g=factor(rep(c("A","B"),4)))
  context$self$data <- d
  expect_error(run(),NA)
  expect_equal(table$rowCount,4L)
  for (v in opts$vars) for (g in c("A","B")) {
    cell <- table$getCell(col="stat[swtest]",rowKey=list(variable=v,group=g))
    expect_equal(cell$value,unname(shapiro.test(d[[v]][d$g==g])$statistic))
  }
  context$self$data <- d[d$g=="B",]
  expect_error(run(),NA)
  expect_equal(table$rowCount,2L)
  context$self$options$groupBy <- NULL
  expect_error(run(),NA)
  expect_equal(table$rowCount,2L)
  expect_equal(table$getCell(col="stat[swtest]",rowKey=list(variable="x",group=""))$value,
    unname(shapiro.test(context$self$data$x)$statistic))
})


test_that("chi-square association helpers agree with reference formulas", {
  tab <- matrix(c(10,20,30,20,15,25),nrow=2)
  chi <- unname(chisq.test(tab,correct=FALSE)$statistic); n <- sum(tab)
  expect_equal(saRa:::.saraCramerV(chi,n,2,3),sqrt(chi/n))
  expect_equal(saRa:::.saraContingency(chi,n),sqrt(chi/(n+chi)))
  expect_equal(saRa:::.saraSakoda(chi,n,2,3),sqrt(2*chi/(n+chi)))
  expect_equal(saRa:::.saraChuprov(chi,n,2,3),sqrt(chi/(n*sqrt(2))))
  for (args in list(list(NaN,n,2,3),list(-1,n,2,3),list(chi,0,2,3),
                   list(chi,n,1,3),list(chi,n,2.5,3),list(Inf,n,2,3))) {
    for (fun in list(saRa:::.saraCramerV,saRa:::.saraSakoda,saRa:::.saraChuprov))
      expect_true(is.nan(do.call(fun,args)))
  }
  expect_true(is.nan(saRa:::.saraContingency(chi,0)))
})

test_that("ordinal inner-range column metadata stays aligned at fractional coverage", {
  cls <- getFromNamespace("ordSummaryClass","saRa")
  for (coverage in c(50,90,99)) {
    context <- new.env(parent=asNamespace("saRa"))
    context$self <- list(options=list(pc=FALSE,irange=TRUE,NPerc=coverage))
    context$private <- new.env(parent=emptyenv())
    context$private$colArgs <- list(name=paste0("s",1:17),title=rep("Statistic",17),
      type=rep("number",17),visible=rep("(n)",17))
    method <- cls$private_methods$.addQuantiles;environment(method) <- context
    method()
    args <- context$private$colArgs
    expect_equal(lengths(args),c(name=18L,title=18L,type=18L,visible=18L))
    expect_equal(args$name[13],"ir")
    expect_equal(args$title[13],paste0(coverage,"% Inner range"))
    expect_equal(args$type[13],"number")
    expect_equal(args$visible[13],"(irange)")
  }
})

test_that("shared rank effects and intervals retain paired and independent results", {
  x <- c(1,3,5,8,10,12,16,20);y <- c(2,2,4,7,9,13,15,19)
  g <- factor(rep(c("A","B"),each=4))
  effects <- saRa:::.saraMWEffects(x,g)
  U <- unname(wilcox.test(x~g)$statistic)
  expect_equal(effects$correlation,1-2*U/16)
  expect_equal(effects$f,max(U,16-U)/16)
  paired <- suppressWarnings(wilcox.test(x,y,paired=TRUE,conf.int=TRUE,conf.level=.9))
  independent <- suppressWarnings(wilcox.test(x~g,conf.int=TRUE,conf.level=.9))
  expect_equal(suppressWarnings(saRa:::.saraHodgesLehmann(x,y,paired=TRUE,conf.level=.9)),
               unname(c(paired$estimate,paired$conf.int)))
  expect_equal(suppressWarnings(saRa:::.saraHodgesLehmann(x,group=g,conf.level=.9)),
               unname(c(independent$estimate,independent$conf.int)))
  samples <- list(x=x,y=y)
  expect_equal(saRa:::.saraMedianIntervals(samples,"exact",.9),
    t(vapply(samples,function(z) DescTools::MedianCI(z,conf.level=.9,method="exact")[2:3],numeric(2))))
  summary <- saRa:::.saraPairedRankSummary(c(3,2,1,NA),c(1,2,3,4))
  expect_equal(unlist(summary[c("positive","negative","tied")]),c(positive=1,negative=1,tied=1))
  expect_equal(summary$sumPositive,1.5);expect_equal(summary$sumNegative,1.5)
})

test_that("shared option parsing and normality dispatch preserve selection semantics", {
  expect_equal(saRa:::.saraPercentages("10,50,110",split=TRUE,drop=TRUE),c(.1,.5))
  expect_equal(saRa:::.saraPercentages(c(10,60),0,50),c(.1,NA_real_))
  expect_equal(saRa:::.saraPercentages(90,50,99),.9)
  expect_equal(saRa:::.saraAlternative("oneGreater"),"greater")
  expect_equal(saRa:::.saraAlternative("twoGreater"),"less")
  expect_equal(saRa:::.saraAlternative("twoSided"),"two.sided")
  x <- c(1,2,4,5,8,9,10,14,17)
  expect_equal(saRa:::.saraNormalityTest(x,"swtest"),shapiro.test(x))
  expect_equal(saRa:::.saraNormalityTest(x,"lillietest"),nortest::lillie.test(x))
  expect_error(saRa:::.saraNormalityTest(rep(1,10),"swtest"),"non-constant")
})


test_that("rank table helper preserves successful cells and isolates test failures", {
  options <- jmvcore::Options$new()
  table <- jmvcore::Table$new(options=options,name="rank",columns=list(
    list(name="stat[mwu]",type="number"),list(name="p[mwu]",type="number")))
  table$addRow("score")
  d <- data.frame(dep=c(1,2,3,5,7,8),group=factor(rep(c("A","B"),each=3)))
  result <- saRa:::.saraMWTest(d,"mwu","less")
  reference <- wilcox.test(dep~group,d,alternative="less")
  expect_true(saRa:::.saraWriteRankResult(table,"score","mwu",result,TRUE))
  expect_equal(unname(table$getCell(col="stat[mwu]",rowKey="score")$value),unname(reference$statistic))
  expect_equal(table$getCell(col="p[mwu]",rowKey="score")$value,reference$p.value)
  d$dep[1] <- Inf
  failed <- saRa:::.saraMWTest(d,"mwu")
  expect_false(saRa:::.saraWriteRankResult(table,"score","mwu",failed,TRUE))
  expect_true(is.nan(table$getCell(col="stat[mwu]",rowKey="score")$value))
  expect_true(length(table$getCell(col="stat[mwu]",rowKey="score")$footnotes)>0)
})


test_that("shared rank errors do not require an analysis translation context", {
  numeric <- data.frame(dep=c(1,2,3,4),group=factor(c("A","A","B","B")))
  factorData <- numeric;factorData$dep <- factor(factorData$dep)
  bad <- saRa:::.saraMWTest(factorData,"mwu")
  expect_true(jmvcore::isError(bad))
  expect_match(jmvcore::extractErrorMessage(bad),"Variable is not numeric")
  numeric$group <- factor(rep("A",4))
  bad <- saRa:::.saraMWTest(numeric,"mwu")
  expect_true(jmvcore::isError(bad))
  expect_equal(saRa:::.saraRankTestError(jmvcore::extractErrorMessage(bad)),
    "One or both groups do not contain enough observations")
  expect_equal(saRa:::.saraRankTestError("data are essentially constant"),
    "All observations are tied")
})


test_that("median plot schema works for exact and named bootstrap intervals", {
  samples <- list(A=c(1,2,4,5,7,8,10,12),B=c(2,3,4,6,7,9,11,13))
  medians <- vapply(samples,median,numeric(1))
  for (method in c("exact","boot")) {
    ci <- suppressWarnings(saRa:::.saraMedianIntervals(samples,method,.95,R=199))
    state <- saRa:::.saraMedianPlotData(names(samples),medians,ci)
    expect_named(state,c("group","median","lower","upper","type"))
    expect_equal(state$lower,as.numeric(ci[,1]))
    expect_equal(state$upper,as.numeric(ci[,2]))
    plot <- saRa:::.saraMedianPlot(state,"Median (95% CI)")
    built <- ggplot2::ggplot_build(plot)
    expect_equal(nrow(built$data[[1]]),2L)
    expect_equal(nrow(built$data[[2]]),2L)
    path <- tempfile(fileext=".pdf")
    grDevices::pdf(path)
    tryCatch({
      expect_error(print(plot),NA)
      expect_true(saRa:::.saraDrawMedianPlot(state,"Median (95% CI)"))
    },finally=grDevices::dev.off())
    expect_gt(file.info(path)$size,0)
    unlink(path)
  }
})

test_that("rank-analysis plot callbacks use the explicit median schema", {
  state <- saRa:::.saraMedianPlotData(c("A","B"),c(2,3),
    matrix(c(1,2,3,4),nrow=2,dimnames=list(NULL,c("lwr.ci","upr.ci"))))
  for (cls in c("umwClass","wilcoxTClass")) {
    # Test the actual callback with options and its namespace translation context.
    context <- new.env(parent=asNamespace("saRa"))
    context$self <- list(options=list(ciWidth=95,ciWidthps=95,group="Group"),
      .translate=function(message) message)
    method <- getFromNamespace(cls,"saRa")$private_methods$.desc
    environment(method) <- context
    # Translation is analysis-specific; give the fixture a literal label formatter.
    context[["."]] <- function(message) message
    path <- tempfile(fileext=".pdf")
    grDevices::pdf(path)
    tryCatch(expect_true(method(list(state=state,key="Score"),NULL,
      list(color=c("black","blue"),fill="white"))),finally=grDevices::dev.off())
    expect_gt(file.info(path)$size,0)
    unlink(path)
  }
})
