# Distribution calculations and finite plotting windows share one implementation.
.saraDistribution <- function(o, discrete=FALSE, translate=identity) {
  if(discrete) {
    .saraDiscreteUpper(o,translate=translate) # validate parameters independently of GUI restrictions
    spec <- switch(o$distros,
      binom=list("binom",list(size=o$binomn,prob=o$binomp),"Binomial",c(0,o$binomn)),
      poiss=list("pois",list(lambda=o$lambda),"Poisson",c(0,Inf)),
      negbinom=list("nbinom",list(size=o$negbinomr,prob=o$negbinomp),"Negative binomial",c(0,Inf)),
      geom=list("geom",list(prob=o$geomp),"Geometric",c(0,Inf)),
      hypergeom=list("hyper",list(m=o$hyperm,n=o$hypern,k=o$hyperk),"Hypergeometric",c(max(0,o$hyperk-o$hypern),min(o$hyperm,o$hyperk))))
  } else {
    spec <- switch(o$cdistros,
      norm=list("norm",list(mean=o$mu,sd=o$sigma),"Normal",c(-Inf,Inf)),
      tdist=list("t",list(df=o$tnu),"t",c(-Inf,Inf)),
      chisqdist=list("chisq",list(df=o$chinu),"χ²",c(0,Inf)),
      fdist=list("f",list(df1=o$f1nu,df2=o$f2nu),"F",c(0,Inf)),
      exp=list("exp",list(rate=o$rate),"Exponential",c(0,Inf)),
      unif=list("unif",list(min=o$unifmin,max=o$unifmax),"Uniform",c(o$unifmin,o$unifmax)))
    if(is.null(spec)) jmvcore::reject(translate("Unknown continuous distribution"),code="invalid_distribution_parameters")
    positive <- setdiff(names(spec[[2]]),c("mean","min","max"))
    valid <- all(vapply(spec[[2]],function(x) length(x)==1L && is.finite(x),logical(1)))
    if(!valid || any(unlist(spec[[2]][positive])<=0) ||
       (o$cdistros=="unif" && o$unifmin>=o$unifmax))
      jmvcore::reject(translate("Distribution parameters must be finite, scales and degrees of freedom positive, and minimum smaller than maximum"),code="invalid_distribution_parameters")
  }
  evaluate <- function(prefix,x,...) do.call(get(paste0(prefix,spec[[1]]),envir=asNamespace("stats")),c(list(x),spec[[2]],list(...)))
  list(d=function(x) evaluate("d",x),p=function(x,...) evaluate("p",x,...),
       q=function(x,...) evaluate("q",x,...),label=spec[[3]],support=spec[[4]])
}
.saraContinuousRange <- function(o, points=numeric(), translate=identity) {
  d <- .saraDistribution(o,translate=translate)
  bounds <- switch(o$cdistros,
    norm=d$q(c(.0001,.9999)),tdist=d$q(c(.001,.999)),
    chisqdist=c(0,d$q(.999)),fdist=c(0,d$q(.99)),
    exp=c(0,d$q(.999)),unif=d$support)
  bounds <- range(c(bounds,points[is.finite(points)]))
  bounds <- c(max(bounds[1],d$support[1]),min(bounds[2],d$support[2]))
  if(any(!is.finite(bounds)) || diff(bounds)<=0)
    jmvcore::reject(translate("These parameters do not give a usable finite plotting window"),code="invalid_distribution_parameters")
  bounds
}
# Limit exceptionally long default tails, while retaining requested finite values.
.saraContinuousDisplayRange <- function(o, points=numeric(), translate=identity) {
  bounds <- .saraContinuousRange(o, points, translate=translate)
  if (o$cdistros %in% c("tdist", "fdist")) {
    d <- .saraDistribution(o,translate=translate)
    quartiles <- d$q(c(.25,.75))
    spread <- diff(quartiles)
    core <- c(max(d$support[1], quartiles[1]-3*spread),
              min(d$support[2], quartiles[2]+3*spread))
    if (all(is.finite(core)) && spread>0 && diff(bounds)>2*diff(core)) {
      requested <- points[is.finite(points)]
      requested <- requested[requested>=d$support[1] & requested<=d$support[2]]
      bounds <- range(c(core,requested))
    }
  }
  bounds
}
.saraWindowCaption <- function(d, bounds, discrete, translate=identity) {
  omitted <- if (discrete) d$p(bounds[1]-1) + d$p(bounds[2],lower.tail=FALSE) else
    d$p(bounds[1]) + d$p(bounds[2],lower.tail=FALSE)
  jmvcore::format(translate("The graph shows a finite window; probability outside the displayed range = {}%. Calculations use the full distribution."),
    format(signif(100*omitted,3),trim=TRUE))
}
.saraPlotNumber <- function(x) format(signif(x,3),trim=TRUE)
.saraDiscreteBreaks <- function(x) {
  bounds <- range(x[is.finite(x)])
  lower <- ceiling(bounds[1]); upper <- floor(bounds[2])
  if (!all(is.finite(bounds)) || lower > upper) return(numeric())
  if (upper-lower <= 10) return(seq.int(lower,upper))
  breaks <- unique(round(pretty(c(lower,upper),n=6)))
  breaks[breaks >= lower & breaks <= upper]
}
.saraDistributionPlot <- function(o,kind,discrete,ggtheme,theme,translate=identity) {
  d <- .saraDistribution(o,discrete,translate=translate)
  palette <- if (is.null(theme$palette)) "jmv" else theme$palette
  fills <- jmvcore::colorPalette(2, palette, "fill")
  prefix <- if(discrete) "" else "cont"
  k <- o[[paste0(prefix,"ppvalue")]]
  a <- o[[paste0(prefix,"x1value")]]; b <- o[[paste0(prefix,"x2value")]]
  cuts <- numeric(); caption <- NULL
  if(kind=="quantile") {
    probability <- o[[paste0(prefix,"pqvalue")]]
    if(length(probability)!=1L || !is.finite(probability) || probability<0 || probability>1)
      jmvcore::reject(translate("Quantile probabilities must be between 0 and 1"),code="invalid_distribution_parameters")
    tail <- o[[paste0(prefix,"tail")]]
    if(tail=="left") { cuts <- d$q(probability); a <- -Inf; b <- cuts }
    else if(tail=="right") { cuts <- d$q(probability,lower.tail=FALSE); a <- cuts; b <- Inf }
    else { cuts <- d$q(c((1-probability)/2,(1+probability)/2)); a <- cuts[1]; b <- cuts[2] }
    title <- paste0(translate(d$label),": ",translate("Quantile")," = ",paste(.saraPlotNumber(cuts),collapse=", "))
  } else if(kind=="density") {
    if(length(k)!=1L || !is.finite(k)) jmvcore::reject(translate("Select a finite random-variable value"),code="invalid_distribution_parameters")
    a <- k; b <- k
    title <- paste0(translate(d$label),": ",if(discrete) "Pr(X = " else "f(",.saraPlotNumber(k),") = ",.saraPlotNumber(d$d(k)))
  } else if(kind=="cdf") {
    a <- -Inf; b <- k
    title <- paste0(translate(d$label),": Pr(X ≤ ",.saraPlotNumber(k),") = ",.saraPlotNumber(d$p(k)))
  } else if(kind=="survival") {
    a <- k; b <- Inf
    title <- paste0(translate(d$label),": Pr(X > ",.saraPlotNumber(k),") = ",.saraPlotNumber(d$p(k,lower.tail=FALSE)))
  } else {
    if(length(a)!=1L || length(b)!=1L || !is.finite(a) || !is.finite(b) || a>b)
      jmvcore::reject(translate("Interval bounds must be finite and lower bound must not exceed upper bound"),code="invalid_distribution_parameters")
    probability <- if(discrete) d$p(b)-d$p(a-1) else d$p(b)-d$p(a)
    title <- paste0(translate(d$label),": Pr(",.saraPlotNumber(a)," ≤ X ≤ ",.saraPlotNumber(b),") = ",.saraPlotNumber(probability))
  }
  if(kind=="quantile" && any(!is.finite(cuts)))
    caption <- translate("Infinite quantiles are reported exactly; the graph shows a finite portion of the distribution.")
  if(discrete) {
    upper <- max(.saraDiscreteUpper(o,translate=translate),c(a,b,cuts)[is.finite(c(a,b,cuts))])
    upper <- min(upper,d$support[2])
    lower <- d$support[1]
    if (upper-lower>100) {
      central <- d$q(c(.001,.999))
      requested <- c(a,b,cuts)
      requested <- requested[is.finite(requested)]
      requested <- requested[requested>=d$support[1] & requested<=d$support[2]]
      window <- range(c(central,requested))
      if (all(is.finite(window))) {
        lower <- max(lower,floor(window[1]))
        upper <- min(upper,ceiling(window[2]))
      }
    }
    if (upper-lower>100000) jmvcore::reject(translate("These parameters require too many plotting points"),code="invalid_distribution_parameters")
    x <- seq.int(lower,max(lower,ceiling(upper)))
    if (lower>d$support[1] || upper<d$support[2])
      caption <- paste(c(caption,.saraWindowCaption(d,range(x),TRUE,translate)),collapse="\n")
    selected <- if(kind=="survival") x>a else x>=a & x<=b
    data <- data.frame(x=x,y=d$d(x),selected=selected)
    plot <- ggplot2::ggplot(data,ggplot2::aes(x=.data$x,y=.data$y,fill=.data$selected)) + ggplot2::geom_col()
    if (kind=="density" && length(x)>50 && k %in% x && d$d(k)>0) {
      # A point and stem remain visible when an individual bar is subpixel wide.
      plot <- plot + ggplot2::annotate("segment",x=k,xend=k,y=0,yend=d$d(k),
                    color=theme$color[2],linewidth=1) +
        ggplot2::annotate("point",x=k,y=d$d(k),shape=21,size=3,
                         fill=fills[2],color=theme$color[1])
    }
  } else {
    if (o$cdistros=="unif") {
      # Explicit vertical jumps prevent interpolation outside the support.
      limits <- range(c(d$support,c(a,b,cuts)[is.finite(c(a,b,cuts))]))
      height <- d$d(mean(d$support))
      data <- data.frame(x=c(limits[1],rep(d$support,each=2),limits[2]),
                         y=c(0,0,height,height,0,0))
      lower <- max(a,d$support[1]); upper <- min(b,d$support[2])
      region <- data.frame(xmin=lower,xmax=upper,ymin=0,ymax=height)
      region <- region[lower < upper,,drop=FALSE]
      plot <- ggplot2::ggplot(data,ggplot2::aes(x=.data$x,y=.data$y)) +
        ggplot2::geom_rect(data=region,inherit.aes=FALSE,
          mapping=ggplot2::aes(xmin=.data$xmin,xmax=.data$xmax,
                              ymin=.data$ymin,ymax=.data$ymax),
          fill=fills[2],alpha=.5) +
        ggplot2::geom_path(color=theme$color[1],linewidth=.8)
    } else {
      bounds <- .saraContinuousDisplayRange(o,c(a,b,cuts),translate=translate)
      original <- .saraContinuousRange(o,c(a,b,cuts),translate=translate)
      if (!isTRUE(all.equal(bounds,original)))
        caption <- paste(c(caption,.saraWindowCaption(d,bounds,FALSE,translate)),collapse="\n")
      x <- sort(unique(c(seq(bounds[1],bounds[2],length.out=1001),c(a,b,cuts)[is.finite(c(a,b,cuts))])))
      y <- d$d(x)
      y[!is.finite(y)] <- NA_real_
      data <- data.frame(x=x,y=y)
      selected <- is.finite(y) & x>=a & x<=b
      plot <- ggplot2::ggplot(data,ggplot2::aes(x=.data$x,y=.data$y)) +
        ggplot2::geom_area(data=data[selected,,drop=FALSE],fill=fills[2],alpha=.5) +
        ggplot2::geom_line(color=theme$color[1],linewidth=.8,na.rm=TRUE)
    }
    if(kind=="density" && is.finite(d$d(k))) plot <- plot +
      ggplot2::annotate("segment", x=k, xend=k, y=0, yend=d$d(k),
                        color=theme$color[2])
    finiteCuts <- cuts[is.finite(cuts)]
    if(length(finiteCuts)) plot <- plot + ggplot2::geom_vline(xintercept=finiteCuts,linetype=2,color=theme$color[2])
  }
  plot <- plot + ggtheme + ggplot2::scale_fill_manual(values=c("FALSE"=fills[1],"TRUE"=fills[2])) +
    ggplot2::guides(fill="none") + ggplot2::labs(title=title,x="x",y=if(discrete) "Pr(X = x)" else "f(x)",caption=caption)
  if (discrete) plot <- plot + ggplot2::scale_x_continuous(
    breaks=.saraDiscreteBreaks(x), minor_breaks=NULL,
    labels=function(values) format(values,trim=TRUE,scientific=FALSE,digits=15))
  plot
}
