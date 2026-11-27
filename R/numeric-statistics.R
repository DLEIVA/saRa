# Each statistic is independently testable. Numeric summaries use stored values.
.saraNumeric <- function(x) {
  value <- jmvcore::toNumeric(x)
  if(is.ordered(value) && is.null(attr(value,"values",exact=TRUE))) {
    scores <- suppressWarnings(as.numeric(levels(value)))
    if(all(is.finite(scores))) value <- scores[as.integer(value)]
  }
  if (!is.numeric(value)) stop("This variable has no numeric stored values")
  if (any(is.infinite(value))) stop("Infinite values are not supported")
  value
}
.saraDivide <- function(numerator, denominator) {
  if (length(denominator)!=1L || !is.finite(denominator) || denominator==0)
    return(NaN)
  numerator/denominator
}
# Quantile type 7 throughout SP50/SP80; no hidden mixture with Tukey hinges.
.saraPercentileSkewness <- function(x, coverage=.5) {
  x <- .saraNumeric(x); x <- x[!is.na(x)]
  if (!length(x)) return(NaN)
  if (!coverage %in% c(.5,.8)) stop("Skewness coverage must be 0.5 or 0.8")
  q <- stats::quantile(x,c((1-coverage)/2,.5,(1+coverage)/2),type=7,names=FALSE)
  .saraDivide(q[3]+q[1]-2*q[2],q[3]-q[1])
}
.saraNummediageom <- function(x) {
  if(is.factor(x)){ res <- NA} else{
    res <- ifelse(sum(x<=0,na.rm = TRUE)==0,exp(mean(log(x), na.rm = TRUE)),NA)
  }
  res
}

.saraNumtrimedia <- function(x){
  if(is.factor(x)){ res <- NA} else{
    FN <- fivenum(x)
    Finf <- FN[2]
    Md <- FN[3]
    Fsup <- FN[4]
    res <- (Finf+2*Md+Fsup)/4
  }
  res
}

.saraNumpromcuar <- function(x){
  if(is.factor(x)){ res <- NA} else{
    FN <- fivenum(x,na.rm=TRUE)
    Q1 <- FN[2]
    Q3 <- FN[4]
    res <- (Q1+Q3)/2
  }
  res
}

.saraNummidR <- function(x){
  if(is.factor(x)){ res <- NA} else{
    res <- (min(x)+max(x))/2
  }
  res
}

.saraNumsdgeom <- function(x) {
  if(is.factor(x)){ res <- NA} else{
    res <- ifelse(sum(x<=0,na.rm = TRUE)==0,exp(sd(log(x), na.rm = TRUE)),NA)
  }
  res
}

.saraNumdesvmed <- function(x) {
  if(is.factor(x)){ res <- NA} else{
    .media <- mean(x, na.rm = TRUE)
    res <- sum(abs(x - .media), na.rm = TRUE)/length(na.omit(x))
  }
  res
}

.saraNumCV <- function(x) {
  if(is.factor(x)){ res <- NA} else{
    .media <- mean(x, na.rm = TRUE)
    .dt <- sd(x, na.rm = TRUE)
    res <- .dt/.media
  }
  res
}

.saraNumrango <- function(x){
  if(is.factor(x)){ res <- NA} else{
    res <- max(x)-min(x)
  }
  res
}

.saraNumIQR <- function(x){
  if(is.factor(x)){ res <- NA} else{
    qtls <- quantile(x,na.rm=TRUE) 
    Q1 <- qtls[2]
    Q3 <- qtls[4]
    res <- Q3-Q1
  }
  res
}

.saraNumQD <- function(x){
  if(is.factor(x)){ res <- NA} else{
    qtls <- quantile(x,na.rm=TRUE) 
    Q1 <- qtls[2]
    Q3 <- qtls[4]
    res <- (Q3-Q1)/2
  }
  res
}

.saraNumMAD <- function(x){
  if(is.factor(x)){ res <- NA} else{
    mediana <- median(x,na.rm=TRUE)
    res <- median(abs(x-mediana),na.rm=TRUE)
  }
  res
}

.saraNumRCV <- function(x){
  if(is.factor(x)){ res <- NA} else{
    FN <- fivenum(x)
    Finf <- FN[2]
    Fsup <- FN[4]
    res <- (Fsup-Finf)/(Finf+Fsup)
  }
  res
}

.saraNumbeta1 <- function(x) {
  if(is.factor(x)){ res <- NA} else{
    .media <- mean(x, na.rm = TRUE)
    .n <- length(na.omit(x))
    res <- (sum((x - .media)^3, na.rm = TRUE)/.n)^2/(sum((x -.media)^2,
                                                         na.rm = TRUE)/.n)^3
  }
  res
}

.saraNumgamma1 <- function(x) {
  if(is.factor(x)){ res <- NA} else{
    .media <- mean(x, na.rm = TRUE)
    .dt <- sd(x, na.rm = TRUE)
    .n <- length(na.omit(x))
    res <- .n * sum((x - .media)^3, na.rm = TRUE)/((.n - 1) * (.n - 2))/(.dt^3)
  }
  res
}

.saraNumbeta2 <- function(x) {
  if(is.factor(x)){ res <- NA} else{
    .media <- mean(x, na.rm = TRUE)
    .n <- length(na.omit(x))
    res <- sum((x - .media)^4, na.rm = TRUE)/.n/(sum((x - .media)^2,
                                                     na.rm = TRUE)/.n)^2
  }
  res
}

.saraNumgamma2 <- function(x) {
  if(is.factor(x)){ res <- NA} else{
    .media <- mean(x, na.rm = TRUE)
    .dt <- sd(x, na.rm = TRUE)
    .n <- length(na.omit(x))
    res <- (.n * (.n + 1) * sum((x - .media)^4, na.rm = TRUE)/
              ((.n - 1) * (.n - 2) * (.n - 3)) - 3 * sum((x - .media)^2,
                                                         na.rm = TRUE)^2/((.n - 2) * (.n - 3)))/(.dt^4)
  }
  res
}

.saraNumK2 <- function(x){
  if(is.factor(x)){ res <- NA} else{
    QU <- quantile(as.numeric(x),na.rm=TRUE) 
    AC90 <- quantile(as.numeric(x),probs=0.9,na.rm=TRUE)
    AC10 <- quantile(as.numeric(x),probs=0.1,na.rm=TRUE)
    Q1 <- QU[2]
    Q3 <- QU[4]
    res <- (AC90-AC10)/(1.9*(Q3-Q1))
  }
  res
}

.saraNumK3 <- function(x){
  if(is.factor(x)){ res <- NA} else{
    FN <- fivenum(as.numeric(x))
    Einf <- quantile(as.numeric(x),probs=0.125,na.rm=TRUE)
    Esup <- quantile(as.numeric(x),probs=0.875,na.rm=TRUE)
    Finf <- FN[2]
    Fsup <- FN[4]
    res <- (Esup-Einf)/(1.7*(Fsup-Finf))   
  }
  res
}

.saraNumericMode <- function(x) {
  if (!length(x)) return(NaN)
  f <- table(x)
  as.numeric(names(f)[f==max(f)])
}
.saraComputeNumeric <- function(column, options, accessors=list()) {
  total <- length(column)
  column <- .saraNumeric(column)
  column <- column[!is.na(column)]
  n <- length(column)
  result <- list(n=n,missing=total-n)
  errors <- list()
  selected <- function(name) isTRUE(options[[name]])
  put <- function(name, compute) {
    result[[name]] <<- tryCatch({
      if (!n) NaN else unname(compute())
    },error=function(e) {
      errors[[name]] <<- conditionMessage(e)
      NaN
    })
    if (n && is.numeric(result[[name]]) && any(!is.finite(result[[name]])) &&
        is.null(errors[[name]]))
      errors[[name]] <<- "Statistic is undefined for these data (zero spread, zero denominator or insufficient observations)"
  }
  functions <- list(mean=base::mean,median=stats::median,mode=.saraNumericMode,
    geommean=.saraNummediageom,trimean=.saraNumtrimedia,avquartile=.saraNumpromcuar,
    midrange=.saraNummidR,variance=stats::var,stddev=stats::sd,cv=.saraNumCV,
    geomsd=.saraNumsdgeom,meandev=.saraNumdesvmed,range=.saraNumrango,
    iqr=.saraNumIQR,qd=.saraNumQD,mad=.saraNumMAD,rcv=.saraNumRCV,
    min=base::min,max=base::max,beta1=.saraNumbeta1,gamma1=.saraNumgamma1,
    beta2=.saraNumbeta2,gamma2=.saraNumgamma2,k2=.saraNumK2,k3=.saraNumK3,
    h1=function(x) .saraPercentileSkewness(x,.5),
    h3=function(x) .saraPercentileSkewness(x,.8))
  for (name in names(functions)) if (selected(name))
    put(name,function() functions[[name]](column))
  for (i in 1:3) if (selected(paste0("q",i)))
    put(paste0("q",i),function() stats::quantile(column,i/4,type=7))
  for (name in c("trimmean","winsmean")) if (selected(name)) {
    put(name,function() {
      amount <- if(name=="trimmean") accessors$trim() else accessors$win()
      if(length(amount)!=1L || !is.finite(amount) || amount<0 || amount>=.5)
        stop("Percentage per tail must be at least 0 and less than 50")
      if(name=="trimmean") mean(column,trim=amount) else WRS2::winmean(column,tr=amount)
    })
  }
  if (selected("irange")) put("ir",function() {
    amount <- accessors$inner()
    if(length(amount)!=1L || !is.finite(amount) || amount<.5 || amount>.99)
      stop("Inner-range coverage must be between 50 and 99 percent")
    diff(stats::quantile(column,c((1-amount)/2,(1+amount)/2),type=7))
  })
  if (selected("pc")) {
    probs <- accessors$percentiles()
    for (i in seq_along(probs)) put(paste0("perc",i),function() {
      if (!is.finite(probs[i]) || probs[i]<0 || probs[i]>1)
        stop("Percentiles must be numbers between 0 and 100")
      stats::quantile(column,probs[i],type=7)
    })
  }
  attr(result,"errors") <- errors
  result
}

# Text ordinals use inverse empirical CDF categories, never interpolated codes.
.saraOrdinalSummary <- function(column, options, accessors=list()) {
  numeric <- tryCatch(.saraNumeric(column),error=function(e) NULL)
  if (is.numeric(numeric)) return(.saraComputeNumeric(numeric,options,accessors))
  if (!is.ordered(column)) stop("Select ordinal variables with ordered levels")
  x <- column[!is.na(column)]
  n <- length(x)
  out <- list(n=n,missing=length(column)-n)
  errors <- list()
  quantileLabel <- function(p) {
    if(!n) return(NA_character_)
    if(!is.finite(p) || p<0 || p>1) stop("Percentiles must be numbers between 0 and 100")
    as.character(sort(x)[max(1L,ceiling(n*p))])
  }
  for(name in c("median","q1","q2","q3","min","max")) if(isTRUE(options[[name]])) {
    probability <- switch(name,median=.5,q1=.25,q2=.5,q3=.75,min=0,max=1)
    out[[name]] <- quantileLabel(probability)
  }
  if(isTRUE(options$mode)) {
    f <- table(x)
    out$mode <- if(n) names(f)[f==max(f)] else NA_character_
  }
  if(isTRUE(options$pc)) {
    probs <- accessors$percentiles()
    for(i in seq_along(probs)) out[[paste0("perc",i)]] <- quantileLabel(probs[i])
  }
  for(name in c("trimean","avquartile","midrange","range","iqr","qd","mad","rcv","h1","h3","k2","k3","irange"))
    if(isTRUE(options[[name]])) {
      key <- if(name=="irange") "ir" else name
      out[[key]] <- NaN
      errors[[key]] <- "Not applicable: this statistic requires numeric values; the variable contains text categories"
    }
  attr(out,"errors") <- errors
  out
}

# Header-only data can be classified when stored-score metadata is retained.
# Text labels alone never imply numeric scores.
.saraOrdinalHasNumeric <- function(data,vars) {
  any(vapply(vars,function(variable) {
    is.numeric(tryCatch(.saraNumeric(data[[variable]]),error=function(e) NULL))
  },logical(1)))
}
