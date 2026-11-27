# Empirical Lorenz curve and descriptive Gini; scale before summing to avoid
# overflow. The normalized Gini multiplies the empirical value by n/(n-1).
.saraInequality <- function(x) {
  x <- .saraNumeric(x)
  x <- x[!is.na(x)]
  if (!length(x)) stop("Gini and Lorenz require at least one non-missing observation")
  if (any(x < 0)) stop("Gini and Lorenz require non-negative values")
  if (max(x) == 0) stop("Gini and Lorenz require a positive sum of values")
  x <- sort(x / max(x))
  n <- length(x)
  p <- (0:n) / n
  L <- c(0, cumsum(x) / sum(x))
  L[length(L)] <- 1
  gini <- max(0, min(1, sum((2*seq_len(n)-n-1)*x)/(n*sum(x))))
  list(gini=gini, normalized=if (n > 1L) gini*n/(n-1) else NaN,
       curve=data.frame(p=p, L=L), n=n)
}

.saraLorenzPlot <- function(x, variable, ggtheme, palette, translate=identity) {
  result <- tryCatch(.saraInequality(x), error=function(e) e)
  if (inherits(result, "error")) {
    message <- paste(translate("Lorenz curve unavailable."),
      translate(conditionMessage(result)), sep="\n")
    return(ggplot2::ggplot() + ggtheme +
      ggplot2::annotate("text", x=0, y=0,
        label=paste(strwrap(message, width=52), collapse="\n"), size=4) +
      ggplot2::labs(title=variable) + ggplot2::theme_void())
  }
  ggplot2::ggplot(result$curve, ggplot2::aes(x=p, y=L)) + ggtheme +
    ggplot2::geom_ribbon(ggplot2::aes(ymin=L, ymax=p),
      fill=palette$fill[2], alpha=.25) +
    ggplot2::geom_abline(slope=1, intercept=0, linetype="dashed",
      colour=palette$color[1], linewidth=.6) +
    ggplot2::geom_line(colour=palette$color[2], linewidth=.9) +
    ggplot2::scale_x_continuous(limits=c(0,1), breaks=seq(0,1,.2),
      expand=ggplot2::expansion(mult=0)) +
    ggplot2::scale_y_continuous(limits=c(0,1), breaks=seq(0,1,.2),
      expand=ggplot2::expansion(mult=0)) +
    ggplot2::coord_fixed() +
    ggplot2::labs(title=variable,
      x=translate("Cumulative proportion of observations"),
      y=translate("Cumulative proportion of total value"))
}
