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
  test <- function(fun) .saraRegressionTest(function() {
    if (length(x)<3L || any(!is.finite(x)) || stats::sd(x)==0)
      stop("Normality tests require finite, non-constant residuals.")
    fun(x)
  })
  list(sw=test(stats::shapiro.test),ks=test(nortest::lillie.test),ad=test(nortest::ad.test))
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
.saraAddonImage <- R6::R6Class("saraAddonImage",inherit=jmvcore::Image,
  private=list(.owner=NULL),active=list(analysis=function() private$.owner),
  public=list(initialize=function(owner,...) {
    private$.owner <- owner
    super$initialize(...)
  }))
.saraRegressionPlot <- function(state,kind,ggtheme=NULL) {
  note <- "Note. Plot obtained with the saRa module."
  finish <- function(p) {
    p <- p+ggplot2::theme_minimal(base_size=12)
    if (!is.null(ggtheme)) p <- p+ggtheme
    p+ggplot2::theme(plot.caption=ggplot2::element_text(hjust=0,size=10),
                    plot.margin=ggplot2::margin(12,24,12,12))
  }
  if (!is.null(state$error)) return(finish(ggplot2::ggplot()+
    ggplot2::annotate("text",x=0,y=0,label=state$error)+ggplot2::labs(caption=note)))
  if (kind=="acf") {
    d <- data.frame(lag=state$lag,acf=state$acf)
    bound <- stats::qnorm(.975)/sqrt(state$n)
    caption <- paste0(note,"\nRow order; ",max(state$lag)," automatic lags.\nApproximate 95% white-noise bounds.")
    return(finish(ggplot2::ggplot(d,ggplot2::aes(x=lag,y=acf))+
      ggplot2::geom_segment(ggplot2::aes(xend=lag,yend=0),linewidth=.4)+
      ggplot2::geom_hline(yintercept=0,colour="grey40")+
      ggplot2::geom_hline(yintercept=c(-bound,bound),linetype="dashed",colour="#3366AA")+
      ggplot2::labs(x="Lag",y="Autocorrelation",caption=caption)+
      ggplot2::scale_x_continuous(breaks=pretty(state$lag))))
  }
  d <- state$data
  labels <- d[state$selected,,drop=FALSE]
  if (kind=="residual") {
    labels <- d[head(order(abs(d$residual),decreasing=TRUE,na.last=NA),3),,drop=FALSE]
    p <- ggplot2::ggplot(d,ggplot2::aes(x=fitted,y=residual))+
      ggplot2::geom_hline(yintercept=0,linetype="dotted")+
      ggplot2::geom_point(shape=21,fill="white")+
      ggplot2::geom_smooth(method="loess",formula=y~x,se=FALSE,colour="#C44E52")+
      ggrepel::geom_text_repel(data=labels,ggplot2::aes(label=case),seed=123456,max.time=.5,max.iter=5000)+
      ggplot2::labs(x="Fitted values",y="Residuals",caption=note)
  } else if (kind=="scale") {
    d$spread <- sqrt(abs(d$standardized))
    labels <- d[head(order(d$spread,decreasing=TRUE,na.last=NA),3),,drop=FALSE]
    p <- ggplot2::ggplot(d,ggplot2::aes(x=fitted,y=spread))+
      ggplot2::geom_point(shape=21,fill="white")+
      ggplot2::geom_smooth(method="loess",formula=y~x,se=FALSE,colour="#C44E52")+
      ggrepel::geom_text_repel(data=labels,ggplot2::aes(label=case),seed=123456,max.time=.5,max.iter=5000)+
      ggplot2::labs(x="Fitted values",y="Sqrt(|standardized residual|)",caption=note)
  } else if (kind=="qq") {
    d <- d[is.finite(d$standardized),,drop=FALSE]
    p <- finish(ggplot2::ggplot(d,ggplot2::aes(sample=standardized))+
      ggplot2::stat_qq(shape=21,fill="white")+
      ggplot2::stat_qq_line(colour="#C44E52",linetype="dashed")+
      ggplot2::labs(x="Theoretical normal quantiles",y="Standardized residual quantiles",caption=note))
    histogram <- ggplot2::ggplot(d,ggplot2::aes(x=standardized))+
      ggplot2::geom_histogram(bins=15,fill="lightblue",colour="grey30")+
      ggplot2::theme_minimal(base_size=8)+ggplot2::labs(x=NULL,y=NULL)
    return(cowplot::ggdraw()+cowplot::draw_plot(p)+
      cowplot::draw_plot(histogram,x=.12,y=.66,width=.25,height=.23))
  } else if (kind=="influence") {
    p <- ggplot2::ggplot(d,ggplot2::aes(x=hat,y=studentized))+
      ggplot2::geom_hline(yintercept=c(-2,0,2),linetype="dashed",colour="grey50")+
      ggplot2::geom_vline(xintercept=c(2,3)*state$rank/state$n,linetype="dashed",colour="grey50")+
      ggplot2::geom_point(ggplot2::aes(size=cook,fill=cook),shape=21,alpha=.7)+
      ggplot2::scale_size_area(max_size=15,name="Cook's D")+
      ggrepel::geom_text_repel(data=labels,ggplot2::aes(label=case),seed=123456,max.time=.5,max.iter=5000)+
      ggplot2::labs(x="Leverage (hat value)",y="Studentized residuals",caption=note)
  } else stop("Unknown regression plot type")
  p <- finish(p)
  # jamovi's global theme includes discrete palette scales. Cook's D is numeric,
  # so its continuous fill scale must be applied after the supplied theme.
  if (kind=="influence") p <- p+ggplot2::scale_fill_gradient(
    low="white",high="#3366AA",name="Cook's D")
  p
}

# Draw inside the callback: jamovi otherwise prints the returned ggplot outside
# its render try block, which can leave the image without a resource path.
.saraDrawRegressionPlot <- function(state,ggtheme=NULL) {
  tryCatch({
    if (is.null(state)) stop("Plot data unavailable. Reselect Residual plots to refresh.")
    plot <- .saraRegressionPlot(state$values,state$kind,ggtheme)
    print(plot)
    TRUE
  },error=function(e) {
    message <- paste(strwrap(paste("Plot could not be drawn:",conditionMessage(e)),width=65),collapse="\n")
    graphics::plot.new()
    graphics::text(.5,.55,message,cex=.85)
    graphics::text(.5,.15,"Note. Plot obtained with the saRa module.",cex=.8)
    TRUE
  })
}
