# Shared finite plotting bound; keep distribution parameters unchanged for evaluation.
.saraDiscreteUpper <- function(options, translate=identity) {
  reject <- function(message) jmvcore::reject(translate(message), code="invalid_distribution_parameters")
  integer <- function(x) length(x) == 1L && is.finite(x) && x >= 0 && x == floor(x)
  probability <- function(x, positive=FALSE) length(x) == 1L && is.finite(x) &&
    x <= 1 && (if (positive) x > 0 else x >= 0)
  upper <- switch(options$distros,
    binom = {
      if (!integer(options$binomn) || !probability(options$binomp))
        reject("Binomial size must be a non-negative integer and probability between 0 and 1")
      options$binomn
    },
    poiss = {
      if (!is.finite(options$lambda) || options$lambda < 0)
        reject("Poisson rate must be non-negative")
      qpois(0.9999, options$lambda)
    },
    negbinom = {
      if (!is.finite(options$negbinomr) || options$negbinomr <= 0 ||
          !probability(options$negbinomp, TRUE))
        reject("Negative binomial size must be positive and probability greater than 0 and at most 1")
      qnbinom(0.9999, options$negbinomr, options$negbinomp)
    },
    geom = {
      if (!probability(options$geomp, TRUE))
        reject("Geometric probability must be greater than 0 and at most 1")
      qgeom(0.9999, options$geomp)
    },
    hypergeom = {
      m <- options$hyperm; n <- options$hypern; k <- options$hyperk
      if (!integer(m) || !integer(n) || !integer(k) || m+n == 0 || k > m+n)
        reject("Hypergeometric counts must be non-negative integers, the population positive, and draws no larger than the population")
      min(m, k)
    },
    reject("Unknown discrete distribution")
  )
  if (!is.finite(upper) || upper > 100000)
    reject("These parameters require too many plotting points; use less extreme values")
  as.integer(ceiling(upper))
}

.saraTailP <- function(z, alternative) {
  switch(alternative, two.sided=2*pnorm(-abs(z)),
         greater=pnorm(z, lower.tail=FALSE), less=pnorm(z), NA_real_)
}

.saraSignedRanks <- function(x, y, alternative="two.sided") {
  d <- x-y
  d <- d[!is.na(d)]
  if (any(!is.finite(d))) stop("Infinite paired differences are not supported")
  d <- d[d != 0]
  n <- length(d)
  if (n == 0L) stop("All observations are tied")
  ranks <- rank(abs(d))
  W <- sum(ranks[d > 0])
  ties <- table(abs(d))
  variance <- n*(n+1)*(2*n+1)/24 - sum(ties^3-ties)/48
  z <- (W-n*(n+1)/4)/sqrt(variance)
  list(statistic=z, p.value=.saraTailP(z, alternative),
       correlation=2*W/(n*(n+1)/2)-1, W=W, n=n)
}

.saraMWZ <- function(dep, group, alternative="two.sided") {
  ok <- complete.cases(dep, group)
  dep <- dep[ok]; group <- droplevels(as.factor(group[ok]))
  if (nlevels(group) != 2L || any(!is.finite(dep)))
    stop("Requires two observed groups and finite values")
  n <- as.numeric(table(group)); N <- sum(n)
  ties <- table(dep)
  ranks <- rank(dep)
  U <- sum(ranks[group == levels(group)[1]]) - n[1]*(n[1]+1)/2
  variance <- prod(n)/12 * (N+1-sum(ties^3-ties)/(N*(N-1)))
  if (!is.finite(variance) || variance<=0) stop("All observations are tied")
  z <- (U-prod(n)/2)/sqrt(variance)
  list(statistic=z, p.value=.saraTailP(z, alternative), U=U, n=n, t=as.numeric(ties))
}

# Fixed bootstrap seed, with caller's RNG state restored even on failure.
.saraBootstrapMedian <- function(x, conf.level=0.95, na.rm=FALSE, R=999) {
  if (na.rm) x <- na.omit(x)
  if (length(x) == 0L || any(!is.finite(x))) stop("Requires finite observations")
  if (length(unique(x)) == 1L) return(setNames(rep(x[1], 3), c("median", "lwr.ci", "upr.ci")))
  hadSeed <- exists(".Random.seed", envir=.GlobalEnv, inherits=FALSE)
  if (hadSeed) oldSeed <- get(".Random.seed", envir=.GlobalEnv)
  on.exit({
    if (hadSeed) assign(".Random.seed", oldSeed, envir=.GlobalEnv)
    else if (exists(".Random.seed", envir=.GlobalEnv, inherits=FALSE)) rm(".Random.seed", envir=.GlobalEnv)
  }, add=TRUE)
  set.seed(104729)
  result <- boot::boot(x, function(x, i) median(x[i]), R=R)
  ci <- boot::boot.ci(result, conf=conf.level, type="basic")[[4]][4:5]
  setNames(c(median(x), ci), c("median", "lwr.ci", "upr.ci"))
}

.saraPlotFrame <- function(data, state) {
  frame <- data[unlist(state$labels, use.names=FALSE)]
  frame[[1]] <- jmvcore::toNumeric(frame[[1]])
  if (!is.numeric(frame[[1]])) {
    frame <- frame[FALSE,,drop=FALSE]
    names(frame) <- unlist(state$names,use.names=FALSE)
    return(frame)
  }
  frame <- frame[complete.cases(frame) & is.finite(frame[[1]]), , drop=FALSE]
  names(frame) <- unlist(state$names, use.names=FALSE)
  frame
}

# All Yule indices from a valid 2 x 2 count table. Undefined indices are NaN.
.saraYulePhi <- function(tab) {
  unavailable <- list(Y=NaN, Q=NaN, V=NaN)
  if (!is.matrix(tab) || !is.numeric(tab) || !identical(dim(tab), c(2L,2L)) ||
      any(!is.finite(tab)) || any(tab < 0) || sum(tab) <= 0)
    return(unavailable)
  # Normalise first: each index is invariant to a common positive scale.
  # This also avoids overflow in products of large counts.
  tab <- tab/max(tab)
  tab <- tab/sum(tab)
  a <- tab[1,1]; b <- tab[1,2]; c <- tab[2,1]; d <- tab[2,2]
  ad <- a*d; bc <- b*c
  divide <- function(num, den) if (den > 0) num/den else NaN
  list(Y=divide(sqrt(ad)-sqrt(bc), sqrt(ad)+sqrt(bc)),
       Q=divide(ad-bc, ad+bc),
       V=divide(ad-bc, sqrt((a+b)*(a+c)*(b+d)*(c+d))))
}

# Shared validation for chi-square-based association coefficients.
.saraValidAssociation <- function(chi, n, rows=NULL, columns=NULL) {
  scalar <- function(x) is.numeric(x) && length(x)==1L && is.finite(x)
  if (!scalar(chi) || chi < 0 || !scalar(n) || n <= 0) return(FALSE)
  if (is.null(rows) && is.null(columns)) return(TRUE)
  scalar(rows) && scalar(columns) && rows >= 2 && columns >= 2 &&
    rows == floor(rows) && columns == floor(columns)
}
.saraCramerV <- function(chi, n, rows, columns) {
  if (!.saraValidAssociation(chi,n,rows,columns)) return(NaN)
  sqrt((chi/n)/min(rows-1,columns-1))
}
.saraContingency <- function(chi, n) {
  if (!.saraValidAssociation(chi,n)) return(NaN)
  sqrt((chi/n)/(1+chi/n))
}
.saraSakoda <- function(chi, n, rows, columns) {
  if (!.saraValidAssociation(chi,n,rows,columns)) return(NaN)
  k <- min(rows,columns)
  sqrt(k/(k-1))*.saraContingency(chi,n)
}

.saraWilson <- function(concordant, discordant, n, tiedBoth) {
  denominator <- choose(n, 2)-tiedBoth
  if (denominator <= 0) NaN else (concordant-discordant)/denominator
}

.saraECDFData <- function(value, group=NULL) {
  df <- data.frame(value=value, group=if (is.null(group)) "All" else as.character(group))
  df <- df[complete.cases(df) & is.finite(df$value), , drop=FALSE]
  pieces <- lapply(split(df, df$group), function(d) {
    if (nrow(d) < 2L || sd(d$value) == 0) return(NULL)
    d$z <- as.vector(scale(d$value))
    d <- d[order(d$z), , drop=FALSE]
    d$empirical <- stats::ecdf(d$z)(d$z)
    d$normal <- stats::pnorm(d$z)
    d
  })
  do.call(rbind, pieces)
}

.saraChuprov <- function(chi, n, rows, columns) {
  if (!.saraValidAssociation(chi,n,rows,columns)) return(NaN)
  sqrt((chi/n)/sqrt((rows-1)*(columns-1)))
}

# Internal normal Q-Q calculations, following the pointwise normal-band method
# used by the embedded plotting functions in saRa 0.1.2. No qqplotr/qqconf load.
.saraQQData <- function(frame, conf=0.95) {
  if (!is.finite(conf) || conf <= 0 || conf >= 1)
    stop("Confidence level must be between zero and one")
  pieces <- lapply(split(frame, frame$group), function(d) {
    sample <- sort(d$z[is.finite(d$z)])
    n <- length(sample)
    if (n < 2L || stats::sd(sample) == 0) return(NULL)
    # Normal maximum-likelihood estimates, equivalent to the former fitdistr.
    mu <- mean(sample)
    sigma <- sqrt(mean((sample-mu)^2))
    probs <- stats::ppoints(n)
    theoretical <- stats::qnorm(probs, mean=mu, sd=sigma)
    xQuartiles <- stats::qnorm(c(.25,.75), mean=mu, sd=sigma)
    yQuartiles <- stats::quantile(sample, c(.25,.75), type=7, names=FALSE)
    slope <- as.numeric(diff(yQuartiles)/diff(xQuartiles))
    intercept <- yQuartiles[1]-slope*xQuartiles[1]
    fitted <- slope*theoretical+intercept
    se <- slope/stats::dnorm(theoretical, mean=mu, sd=sigma)*
      sqrt(probs*(1-probs)/n)
    width <- stats::qnorm((1+conf)/2)*se
    data.frame(group=d$group[1], theoretical=theoretical, sample=sample,
               fitted=fitted, lower=fitted-width, upper=fitted+width)
  })
  do.call(rbind, pieces)
}

.saraQQPlot <- function(data, variable, groupBy=NULL, ggtheme=NULL, translate=identity) {
  frame <- .saraECDFData(jmvcore::toNumeric(data[[variable]]),
                        if (is.null(groupBy)) NULL else data[[groupBy]])
  if (is.null(frame) || nrow(frame) == 0L) return(FALSE)
  frame <- .saraQQData(frame)
  if (is.null(frame) || nrow(frame) == 0L) return(FALSE)
  plot <- ggplot2::ggplot(frame, ggplot2::aes(x=.data$theoretical)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin=.data$lower, ymax=.data$upper),
                         fill="grey70", alpha=.5) +
    ggplot2::geom_line(ggplot2::aes(y=.data$fitted), linewidth=.8) +
    ggplot2::geom_point(ggplot2::aes(y=.data$sample)) +
    ggplot2::xlab(translate("Theoretical quantiles")) + ggplot2::ylab(translate("Standardized values")) + ggtheme
  if (!is.null(groupBy)) plot <- plot + ggplot2::facet_wrap(~ group)
  plot
}

.saraRankTestError <- function(message) {
  if (grepl("not enough",message,fixed=TRUE) && grepl("observations",message,fixed=TRUE))
    return("One or both variables do not contain enough observations")
  switch(message,
    "grouping factor must have exactly 2 levels" = "One or both groups do not contain enough observations",
    "not enough observations" = "One or both groups do not contain enough observations",
    "data are essentially constant" = "All observations are tied",
    "cannot compute confidence interval when all observations are tied" = "All observations are tied",
    message)
}

# Map both independent and paired GUI alternatives in one place.
.saraAlternative <- function(hypothesis) {
  switch(hypothesis, oneGreater="greater", twoGreater="less", "two.sided")
}

# Central order-statistic interval from the exact binomial sign distribution.
# With k=0 no finite endpoints can attain the requested coverage.
.saraExactMedianCI <- function(x,conf.level=.95) {
  if (length(conf.level)!=1L || !is.finite(conf.level) ||
      conf.level<=0 || conf.level>=1)
    stop("Confidence level must be between zero and one")
  if (!is.numeric(x) || !length(x) || any(!is.finite(x)))
    stop("Exact median confidence interval requires finite observations")
  x <- sort(x)
  n <- length(x)
  k <- stats::qbinom((1-conf.level)/2,n,.5)
  bounds <- if (k==0) c(-Inf,Inf) else c(x[k],x[n-k+1])
  result <- setNames(c(stats::median(x),bounds),c("median","lwr.ci","upr.ci"))
  attr(result,"conf.level") <- if (k==0) 1 else 1-2*stats::pbinom(k-1,n,.5)
  result
}

# Shared exact/bootstrap median-CI assembly. Samples retain their input order.
.saraMedianIntervals <- function(samples, method, conf.level, R=999, details=FALSE) {
  if (!method %in% c("exact","boot")) stop("Unknown median confidence-interval method")
  if (details) {
    records <- lapply(samples,function(x) tryCatch({
      result <- if (method=="exact")
        .saraExactMedianCI(x,conf.level=conf.level)
      else .saraBootstrapMedian(x,conf.level=conf.level,R=R)
      level <- if (method=="exact") attr(result,"conf.level") else conf.level
      # Older DescTools versions may omit confidence metadata.
      if (length(level)!=1L || !is.finite(level) || level<=0 || level>1)
        level <- NA_real_
      list(values=unname(result[2:3]),confidence=level,error=NULL)
    },error=function(e) list(values=c(NaN,NaN),confidence=NA_real_,
                            error=conditionMessage(e))))
    return(list(values=t(vapply(records,function(x) x$values,numeric(2))),
      confidence=vapply(records,function(x) x$confidence,numeric(1)),
      errors=lapply(records,function(x) x$error)))
  }
  t(vapply(samples, function(x) {
    if (method == "exact")
      DescTools::MedianCI(x, conf.level=conf.level, method="exact")[2:3]
    else .saraBootstrapMedian(x, conf.level=conf.level, R=R)[2:3]
  }, numeric(2)))
}

.saraMedianConfidenceHeading <- function(levels,requested,translate=identity) {
  valid <- levels[is.finite(levels) & levels>0 & levels<=1]
  if (length(valid)!=length(levels) || !length(valid))
    return(translate("Confidence Interval (requested level; see notes)"))
  valid <- unique(signif(valid,10))
  if (length(valid)>1L)
    return(translate("Confidence Interval (level reported per group)"))
  sprintf(translate("%s%% Confidence Interval"),
    trimws(formatC(100*valid,format="fg",digits=8,width=0)))
}

.saraMWMedianPlotLabel <- function(state,width,translate=identity) {
  if (!is.null(state) && any(is.infinite(state$lower) | is.infinite(state$upper)))
    return(translate("Median (unbounded intervals; see table)"))
  if (!is.null(state) && "confidence" %in% names(state) &&
      any(!is.finite(state$confidence) | abs(state$confidence-width/100)>1e-8))
    return(translate("Median (confidence intervals reported in table)"))
  .saraMedianPlotLabel(state,width,translate)
}

# Same output contract for paired differences and independent group shifts.
.saraHodgesLehmann <- function(x, y=NULL, group=NULL, paired=FALSE, conf.level=.95, details=FALSE) {
  if (paired && is.null(group)) {
    differences <- x-y
    differences <- differences[!is.na(differences)]
    if (length(differences)>0L && all(differences==0))
      stop("Hodges-Lehmann confidence interval is unavailable because all paired differences are zero.")
    if (length(differences)==1L) {
      values <- c(differences,NaN,NaN)
      if (!details) return(values)
      return(list(values=values,confidence=NULL,
        note="Hodges-Lehmann confidence interval is unavailable because only one complete pair is available."))
    }
  }
  if (is.null(group))
    result <- stats::wilcox.test(x,y,paired=paired,conf.int=TRUE,conf.level=conf.level)
  else {
    data <- data.frame(value=x,group=group)
    complete <- !is.na(data$value) & !is.na(data$group)
    observed <- data$value[complete]
    if (length(unique(data$group[complete]))==2L &&
        length(observed)>0L && length(unique(observed))==1L)
      stop("Hodges-Lehmann confidence interval is unavailable because all observations are tied.")
    samples <- split(data$value[complete],droplevels(factor(data$group[complete])))
    if (length(samples)==2L && all(lengths(samples)==1L)) {
      values <- c(unname(samples[[1]]-samples[[2]]),NaN,NaN)
      if (!details) return(values)
      return(list(values=values,confidence=NULL,
        note="Hodges-Lehmann confidence interval is unavailable because only one observation per group is available."))
    }
    result <- stats::wilcox.test(value~group,data=data,conf.int=TRUE,conf.level=conf.level)
  }
  values <- unname(c(result$estimate,result$conf.int))
  if (!details) return(values)
  list(values=values, confidence=attr(result$conf.int, "conf.level"))
}

.saraHLConfidenceNote <- function(requested, returned, translate=identity) {
  if (length(returned) != 1L || !is.finite(returned) || returned<=0 || returned>=1 ||
      abs(returned-requested) <= 1e-8) return(NULL)
  sprintf(translate("Requested confidence level %s%% is not reachable, %s%% reported instead"),
          trimws(formatC(100*requested, format="fg", digits=8, width=0)),
          trimws(formatC(100*returned, format="fg", digits=8, width=0)))
}

.saraMWEffects <- function(dep, group) {
  ranks <- .saraMWZ(dep,group)
  denominator <- prod(ranks$n)
  list(superiority=ranks$U/denominator,
       correlation=1-2*ranks$U/denominator,
       f=max(ranks$U,denominator-ranks$U)/denominator)
}

.saraPairedRankSummary <- function(x,y) {
  differences <- x-y
  differences <- differences[!is.na(differences)]
  positive <- sum(differences>0); negative <- sum(differences<0)
  tied <- sum(differences==0)
  differences <- differences[differences!=0]
  ranks <- rank(abs(differences))
  sumPositive <- if (positive) sum(ranks[differences>0]) else NaN
  sumNegative <- if (negative) sum(ranks[differences<0]) else NaN
  list(positive=positive,negative=negative,tied=tied,
       sumPositive=sumPositive,sumNegative=sumNegative,
       meanPositive=if(positive) sumPositive/positive else NaN,
       meanNegative=if(negative) sumNegative/negative else NaN)
}

# Common percentage parsing for both numerical analyses. Bounds use percent units.
.saraPercentages <- function(value, minimum=0, maximum=100, split=FALSE, drop=FALSE) {
  if (is.character(value)) {
    if (split) value <- unlist(strsplit(value,","))
    value <- as.numeric(value)
  }
  value[value < minimum | value > maximum] <- NA_real_
  value <- value/100
  if (drop) value <- value[!is.na(value)]
  value
}

# Add-ons run with the parent's library paths. Resolve a dependency in saRa's
# bundled library only when requested, without changing global library paths.
.saraLoadNamespace <- function(package) {
  if (isNamespaceLoaded(package)) return(getNamespace(package))
  moduleLibrary <- dirname(find.package("saRa"))
  loadNamespace(package,lib.loc=unique(c(moduleLibrary,.libPaths())))
}

# Common test dispatch for standalone variables and regression residuals.
# Callers decide selection and convert failures into their own cell footnotes.
.saraNormalityTest <- function(x, test) {
  x <- x[!is.na(x)]
  if (length(x)<2L) stop("Requires at least two non-missing observations")
  if (any(!is.finite(x))) stop("Infinite values are not supported")
  if (stats::sd(x)==0) stop("Requires a non-constant variable")
  if (test %in% c("chisqtest","adtest","lillietest","sftest","cvmtest"))
    .saraLoadNamespace("nortest")
  switch(test,
    chisqtest=nortest::pearson.test(x),
    kstest=stats::ks.test(x,"pnorm",mean=mean(x),sd=stats::sd(x)),
    swtest=stats::shapiro.test(x),
    adtest=nortest::ad.test(x),
    lillietest=nortest::lillie.test(x),
    sftest=nortest::sf.test(x),
    cvmtest=nortest::cvm.test(x),
    stop("Unknown normality test"))
}

# Independent-rank test dispatch with one input/error contract.
.saraMWTest <- function(data, test, alternative="two.sided", conf.level=.95) {
  # Shared helpers have no analysis `self`; do not call jamovi's .() here.
  try({
    if (is.factor(data$dep)) stop("Variable is not numeric")
    if (any(is.infinite(data$dep))) stop("Variable contains infinite values")
    values <- data$dep[!is.na(data$dep)]
    if(length(values)>0L && length(unique(values))==1L) stop("All observations are tied")
    suppressWarnings(switch(test,
      mwu=stats::wilcox.test(dep~group,data=data,alternative=alternative,conf.level=conf.level),
      zstat=.saraMWZ(data$dep,data$group,alternative),
      pSup=list(statistic=.saraMWEffects(data$dep,data$group)$superiority),
      rankCorr=list(statistic=.saraMWEffects(data$dep,data$group)$correlation),
      fstat=list(statistic=.saraMWEffects(data$dep,data$group)$f),
      stop("Unknown Mann-Whitney output")))
  },silent=TRUE)
}

# Shared error-to-cell rendering; return success for test-specific footnotes.
.saraWriteRankResult <- function(table, rowKey, name, result, withP=FALSE, translate=identity) {
  statistic <- paste0("stat[",name,"]")
  probability <- paste0("p[",name,"]")
  failed <- jmvcore::isError(result)
  values <- setNames(list(if(failed) NaN else result$statistic),statistic)
  if (withP) values[[probability]] <- if(failed) NaN else result$p.value
  table$setRow(rowKey=rowKey,values)
  if (failed) table$addFootnote(rowKey=rowKey,statistic,
    translate(.saraRankTestError(jmvcore::extractErrorMessage(result))))
  !failed
}

# jmvcore's Unicode superscript alphabet omits c (and other letters).
# Render a complete alphabet with supported HTML symbols and table notes instead.
.saraLetterMarker <- function(index) {
  marker <- ""
  while (index>0L) {
    index <- index-1L
    marker <- paste0(letters[index%%26L+1L],marker)
    index <- index%/%26L
  }
  marker
}
.saraTableLetterNotes <- function(table) {
  prefix <- "sara_letter_"
  for (key in names(table$notes))
    if (startsWith(key,prefix)) table$setNote(key,NULL)
  notes <- character()
  for (row in seq_len(table$rowCount)) {
    for (column in table$columns) {
      if (!column$visible) next
      cell <- column$getCell(row)
      texts <- unique(as.character(cell$footnotes))
      if (!length(texts)) next
      cell$footnotes <- character()
      cell$sups <- integer()
      for (note in texts) {
        index <- match(note,notes)
        if (is.na(index)) {
          notes <- c(notes,note)
          index <- length(notes)
          marker <- .saraLetterMarker(index)
          table$setNote(paste0(prefix,index),paste0("<sup>",marker,"</sup> ",note),
            init=FALSE)
        }
        table$addSymbol(col=column$name,rowNo=row,
          paste0("<sup>",.saraLetterMarker(index),"</sup>"))
      }
    }
  }
  invisible(notes)
}

# Explicit plot schema: never rely on names produced by cbind() or CI methods.
.saraMedianPlotData <- function(groups, medians, intervals) {
  if (!is.matrix(intervals) || nrow(intervals)!=length(groups) ||
      ncol(intervals)!=2L || length(medians)!=length(groups))
    stop("Median plot values and confidence intervals are not aligned")
  data.frame(group=factor(as.character(groups),levels=unique(as.character(groups))),
    median=as.numeric(medians),lower=as.numeric(intervals[,1]),
    upper=as.numeric(intervals[,2]),type="median")
}
.saraMedianPlotLabel <- function(state, width, translate=identity) {
  if (!is.null(state) && any(is.finite(state$median)) &&
      !any(is.finite(state$median) & is.finite(state$lower) & is.finite(state$upper)))
    return(translate("Median (confidence interval unavailable)"))
  sprintf(translate("Median (%s%% CI)"),width)
}
.saraMedianPlot <- function(state, label, xLabel=NULL, yLabel=NULL,
                            ggtheme=NULL, theme=NULL, legendPosition=NULL) {
  if (identical(legendPosition,"bottom"))
    label <- paste(strwrap(label,width=42),collapse="\n")
  if (is.null(state) || !any(is.finite(state$median)))
    stop("No finite medians are available for this plot")
  points <- state[is.finite(state$median),,drop=FALSE]
  intervals <- points[is.finite(points$lower) & is.finite(points$upper),,drop=FALSE]
  colour <- if(length(theme$color)) theme$color[1] else "black"
  intervalColour <- if(length(theme$color)>1) theme$color[2] else colour
  fill <- if(length(theme$fill)) theme$fill[1] else "white"
  p <- ggplot2::ggplot(points,ggplot2::aes(x=.data$group,y=.data$median,shape=.data$type))+
    ggplot2::geom_errorbar(data=intervals,
      ggplot2::aes(ymin=.data$lower,ymax=.data$upper),width=.1,
      linewidth=.8,colour=intervalColour)+
    ggplot2::geom_point(colour=colour,fill=fill,size=3)+
    ggplot2::scale_x_discrete(limits=unique(as.character(state$group)))+
    ggplot2::labs(x=xLabel,y=yLabel)
  if (!is.null(ggtheme)) p <- p+ggtheme
  p <- p+ggplot2::scale_shape_manual(name="",values=c(median=22),labels=c(median=label))+
    ggplot2::theme(plot.margin=ggplot2::margin(8,12,8,8))
  if (!is.null(legendPosition))
    p <- p+ggplot2::theme(legend.position=legendPosition)
  p
}
.saraDrawMedianPlot <- function(state,label,xLabel=NULL,yLabel=NULL,
                                ggtheme=NULL,theme=NULL,legendPosition=NULL, translate=identity) {
  tryCatch({
    print(.saraMedianPlot(state,label,xLabel,yLabel,ggtheme,theme,legendPosition))
    TRUE
  },error=function(e) {
    graphics::plot.new()
    graphics::text(.5,.5,paste(strwrap(jmvcore::format(translate("Plot could not be drawn: {}"),
      translate(conditionMessage(e))),width=55),collapse="\n"),cex=.85)
    TRUE
  })
}

# Empty defined levels are retained for display, excluded from association maths.
.saraObservedTable <- function(tab) tab[rowSums(tab)>0,colSums(tab)>0,drop=FALSE]
.saraNominalPrediction <- function(tab) {
  keys <- c("lambdaGKab","lambdaGKba","lambdaGKsym","tauGKab","tauGKba","theilab","theilba","theilsym")
  out <- setNames(as.list(rep(NaN,length(keys))),keys)
  tab <- .saraObservedTable(tab)
  n <- sum(tab)
  if (n<=0 || any(dim(tab)<2L)) return(out)
  rows <- rowSums(tab); columns <- colSums(tab)
  maxRows <- max(rows); maxCols <- max(columns)
  gainRows <- sum(apply(tab,2,max))-maxRows
  gainCols <- sum(apply(tab,1,max))-maxCols
  out$lambdaGKab <- .saraDivide(gainRows,n-maxRows)
  out$lambdaGKba <- .saraDivide(gainCols,n-maxCols)
  out$lambdaGKsym <- .saraDivide(gainRows+gainCols,2*n-maxRows-maxCols)
  p <- tab/n; pr <- rows/n; pc <- columns/n
  out$tauGKab <- .saraDivide(sum(sweep(p^2,2,pc,"/"))-sum(pr^2),1-sum(pr^2))
  out$tauGKba <- .saraDivide(sum(sweep(p^2,1,pr,"/"))-sum(pc^2),1-sum(pc^2))
  entropy <- function(p) -sum(p[p>0]*log(p[p>0]))
  ha <- entropy(pr); hb <- entropy(pc); information <- ha+hb-entropy(p)
  out$theilab <- .saraDivide(information,ha)
  out$theilba <- .saraDivide(information,hb)
  out$theilsym <- .saraDivide(2*information,ha+hb)
  out
}
.saraValidateFences <- function(inner,outer,translate=identity) {
  if(length(inner)!=1L || length(outer)!=1L || !is.finite(inner) ||
     !is.finite(outer) || inner<0 || outer<=inner)
    stop(translate("Fence multipliers must be non-negative, with outer greater than inner"))
  invisible(TRUE)
}

# The renderer must draw a resource rather than merely return a ggplot object.
.saraDrawBivariatePlot <- function(build) {
  plot <- build()
  if(is.null(plot) || identical(plot,FALSE)) return(FALSE)
  print(plot)
  TRUE
}

.saraHLConfidenceHeading <- function(levels, requested, translate=identity) {
  levels <- as.numeric(levels)
  levels <- unique(signif(levels[is.finite(levels) & levels>0 & levels<1], 10))
  if (length(levels) > 1L)
    return(translate("Confidence Interval (level reported per row)"))
  if (length(levels) == 0L) return(translate("Confidence Interval"))
  sprintf(translate("%s%% Confidence Interval"),
          trimws(formatC(100*levels, format="fg", digits=8, width=0)))
}

# Resolve data-label typography from the supplied public ggplot theme. The theme
# may be jamovi's list of theme and scale objects; no global defaults are changed.
.saraPlotTextStyle <- function(ggtheme) {
  plot <- ggplot2::ggplot() + ggtheme
  element <- ggplot2::calc_element("text", ggplot2::theme_get() + plot$theme)
  list(size=element$size / ggplot2::.pt,
       family=if (is.null(element$family)) "" else element$family,
       face=if (is.null(element$face)) "plain" else element$face)
}
