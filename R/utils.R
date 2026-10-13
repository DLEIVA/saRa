# Shared finite plotting bound; keep distribution parameters unchanged for evaluation.
.saraDiscreteUpper <- function(options) {
  reject <- function(message) jmvcore::reject(message, code="invalid_distribution_parameters")
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
  if (n == 0L) return(list(statistic=NaN, p.value=NaN, correlation=NaN, W=0, n=0))
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
  z <- if (variance > 0) (U-prod(n)/2)/sqrt(variance) else NaN
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

.saraQQPlot <- function(data, variable, groupBy=NULL, ggtheme=NULL) {
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
    ggplot2::xlab("Theoretical quantiles") + ggplot2::ylab("Standardised values") + ggtheme
  if (!is.null(groupBy)) plot <- plot + ggplot2::facet_wrap(~ group)
  plot
}

.saraRankTestError <- function(message) {
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

# Shared exact/bootstrap median-CI assembly. Samples retain their input order.
.saraMedianIntervals <- function(samples, method, conf.level, R=999) {
  if (!method %in% c("exact","boot")) stop("Unknown median confidence-interval method")
  t(vapply(samples, function(x) {
    if (method == "exact")
      DescTools::MedianCI(x, conf.level=conf.level, method="exact")[2:3]
    else .saraBootstrapMedian(x, conf.level=conf.level, R=R)[2:3]
  }, numeric(2)))
}

# Same output contract for paired differences and independent group shifts.
.saraHodgesLehmann <- function(x, y=NULL, group=NULL, paired=FALSE, conf.level=.95) {
  if (is.null(group))
    result <- stats::wilcox.test(x,y,paired=paired,conf.int=TRUE,conf.level=conf.level)
  else {
    data <- data.frame(value=x,group=group)
    result <- stats::wilcox.test(value~group,data=data,conf.int=TRUE,conf.level=conf.level)
  }
  unname(c(result$estimate,result$conf.int))
}

.saraMWEffects <- function(dep, group) {
  ranks <- .saraMWZ(dep,group)
  denominator <- prod(ranks$n)
  list(correlation=1-2*ranks$U/denominator,
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

# Common test dispatch for standalone variables and regression residuals.
# Callers decide selection and convert failures into their own cell footnotes.
.saraNormalityTest <- function(x, test) {
  x <- x[!is.na(x)]
  if (length(x)<2L) stop("Requires at least two non-missing observations")
  if (any(!is.finite(x))) stop("Infinite values are not supported")
  if (stats::sd(x)==0) stop("Requires a non-constant variable")
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
    suppressWarnings(switch(test,
      mwu=stats::wilcox.test(dep~group,data=data,alternative=alternative,conf.level=conf.level),
      zstat=.saraMWZ(data$dep,data$group,alternative),
      pSup=list(statistic=rcompanion::wilcoxonPS(dep~group,data=data)[[1]]),
      rankCorr=list(statistic=.saraMWEffects(data$dep,data$group)$correlation),
      fstat=list(statistic=.saraMWEffects(data$dep,data$group)$f),
      stop("Unknown Mann-Whitney output")))
  },silent=TRUE)
}

# Shared error-to-cell rendering; return success for test-specific footnotes.
.saraWriteRankResult <- function(table, rowKey, name, result, withP=FALSE) {
  statistic <- paste0("stat[",name,"]")
  probability <- paste0("p[",name,"]")
  failed <- jmvcore::isError(result)
  values <- setNames(list(if(failed) NaN else result$statistic),statistic)
  if (withP) values[[probability]] <- if(failed) NaN else result$p.value
  table$setRow(rowKey=rowKey,values)
  if (failed) table$addFootnote(rowKey=rowKey,statistic,
    .saraRankTestError(jmvcore::extractErrorMessage(result)))
  !failed
}
