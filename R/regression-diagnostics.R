# Reproducible simulation without changing the caller's random stream.
.saraRegressionSeed <- function(fun,seed=123456L) {
  existed <- exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE)
  if (existed) previous <- get(".Random.seed",envir=.GlobalEnv)
  on.exit({
    if (existed) assign(".Random.seed",previous,envir=.GlobalEnv)
    else if (exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE))
      rm(".Random.seed",envir=.GlobalEnv)
  },add=TRUE)
  set.seed(seed)
  fun()
}
.saraRegressionTest <- function(fun) {
  tryCatch({
    r <- fun(); df <- unname(r$parameter)
    list(stat=unname(r$statistic),p=r$p.value,
         df1=if(length(df)) df[1] else NA_real_,
         df2=if(length(df)>1L) df[2] else NA_real_,error=NULL)
  },error=function(e) list(stat=NaN,p=NaN,df1=NA_real_,df2=NA_real_,error=conditionMessage(e)))
}
.saraRegressionTests <- function(model) {
  model$na.action <- NULL
  ordinary <- function(fun) {
    if (!is.null(model$weights) && any(model$weights!=1))
      stop("This diagnostic is available for unweighted OLS models only.")
    fun()
  }
  list(reset=.saraRegressionTest(function() lmtest::resettest(model,power=2:3,type="fitted")),
       bp=.saraRegressionTest(function() lmtest::bptest(model,studentize=FALSE)),
       koenker=.saraRegressionTest(function() lmtest::bptest(model,studentize=TRUE)),
       gq=.saraRegressionTest(function() ordinary(function()
         lmtest::gqtest(model,order.by=as.numeric(model$fitted.values),alternative="two.sided"))),
       hmc=.saraRegressionTest(function() ordinary(function()
         .saraRegressionSeed(function() lmtest::hmctest(model,
           order.by=as.numeric(model$fitted.values),simulate.p=TRUE,nsim=5000)))))
}
.saraRegressionNormality <- function(model) {
  x <- as.numeric(model$residuals)
  tests <- c(sw="swtest",ks="lillietest",ad="adtest")
  lapply(tests,function(name) .saraRegressionTest(function() .saraNormalityTest(x,name)))
}
.saraRegressionDW <- function(model,reps=5000L,seed=123456L,maxLag=10L) {
  tryCatch({
    model$na.action <- NULL
    e <- as.numeric(model$residuals)
    if (length(e)<3L || any(!is.finite(e)) || sum(e^2)==0 || stats::df.residual(model)<1)
      stop("Durbin-Watson tests require a fitted model with finite, non-zero residuals.")
    if (!is.null(model$weights) && any(model$weights!=1))
      stop("Bootstrap Durbin-Watson tests are available for unweighted OLS models only.")
    lag <- min(as.integer(maxLag),length(e)-1L)
    r <- .saraRegressionSeed(function() car::durbinWatsonTest(model,max.lag=lag,
      simulate=TRUE,reps=reps,method="resample",alternative="two.sided"),seed)
    list(data=data.frame(lag=seq_len(lag),acf=as.numeric(r$r),dw=as.numeric(r$dw),
                         p=as.numeric(r$p)),error=NULL)
  },error=function(e) list(data=NULL,error=conditionMessage(e)))
}
.saraResidualACF <- function(model,modelNumber=1L) {
  tryCatch({
    e <- as.numeric(model$residuals)
    if (length(e)<2L || any(!is.finite(e)) || !is.finite(stats::sd(e)) || stats::sd(e)==0)
      stop("Residual ACF requires at least two finite, non-constant residuals.")
    r <- stats::acf(e,plot=FALSE)
    list(lag=as.numeric(r$lag),acf=as.numeric(r$acf),n=r$n.used,model=modelNumber)
  },error=function(e) list(error=conditionMessage(e)))
}
.saraRegressionFrame <- function(model,modelNumber=1L) {
  tryCatch({
    model$na.action <- NULL
    n <- length(model$residuals)
    labels <- rownames(stats::model.frame(model))
    if (is.null(labels)) labels <- as.character(seq_len(n))
    frame <- data.frame(case=labels,fitted=as.numeric(model$fitted.values),
      residual=as.numeric(model$residuals),standardized=as.numeric(stats::rstandard(model)),
      studentized=as.numeric(stats::rstudent(model)),hat=as.numeric(stats::hatvalues(model)),
      cook=as.numeric(stats::cooks.distance(model)))
    # Same noteworthy rule as car::influencePlot(id=TRUE): top two for each index.
    top <- function(x) head(order(x,decreasing=TRUE,na.last=NA),2L)
    selected <- sort(unique(c(top(abs(frame$studentized)),top(frame$hat),top(frame$cook))))
    list(data=frame,selected=selected,rank=model$rank,n=n,model=modelNumber,error=NULL)
  },error=function(e) list(error=conditionMessage(e)))
}
# Preserve the renderer owner after insertion into the parent's result tree.
