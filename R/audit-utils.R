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

.saraYulePhi <- function(tab) {
  if (!all(dim(tab) == 2L)) return(NaN)
  a <- tab[1,1]; b <- tab[1,2]; c <- tab[2,1]; d <- tab[2,2]
  denominator <- sqrt((a+b)*(a+c)*(b+d)*(c+d))
  if (denominator == 0) NaN else (a*d-b*c)/denominator
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
  if (n <= 0 || rows < 2 || columns < 2) return(NaN)
  sqrt(chi/(n*sqrt((rows-1)*(columns-1))))
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
    "grouping factor must have exactly 2 levels" = .("One or both groups do not contain enough observations"),
    "not enough observations" = .("One or both groups do not contain enough observations"),
    "data are essentially constant" = .("All observations are tied"),
    "cannot compute confidence interval when all observations are tied" = .("All observations are tied"),
    message)
}
