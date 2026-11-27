# Keep the standard translation marker on jmvcore versions that do not export it.
# Translation uses the analysis' public Options API; no private jmvcore access.
. <- function(text,n=1) {
  analysis <- get0("self",envir=parent.frame(),inherits=TRUE)
  if(is.null(analysis)) return(text)
  # Older Options objects throw on lookup of an unknown member.
  translator <- tryCatch(analysis$options$translate,error=function(e) NULL)
  if(!is.function(translator)) return(text)
  translator(text,n)
}

# Shared computations do not translate; the analysis translates at the output boundary.
.saraMessage <- function(message,self=NULL) {
  if(is.null(self)) return(message)
  switch(message,
    "Gini and Lorenz require at least one non-missing observation"=.("Gini and Lorenz require at least one non-missing observation"),
    "Gini and Lorenz require non-negative values"=.("Gini and Lorenz require non-negative values"),
    "Gini and Lorenz require a positive sum of values"=.("Gini and Lorenz require a positive sum of values"),
    "Normalized Gini requires at least two non-missing observations"=.("Normalized Gini requires at least two non-missing observations"),

    "All observations are tied"=.("All observations are tied"),
    "Binomial"=.("Binomial"),
    "Binomial size must be a non-negative integer and probability between 0 and 1"=.("Binomial size must be a non-negative integer and probability between 0 and 1"),
    "Bootstrap Durbin-Watson tests are available for unweighted OLS models only."=.("Bootstrap Durbin-Watson tests are available for unweighted OLS models only."),
    "Confidence level must be between zero and one"=.("Confidence level must be between zero and one"),
    "Distribution parameters must be finite, scales and degrees of freedom positive, and minimum smaller than maximum"=.("Distribution parameters must be finite, scales and degrees of freedom positive, and minimum smaller than maximum"),
    "Durbin-Watson tests require a fitted model with finite, non-zero residuals."=.("Durbin-Watson tests require a fitted model with finite, non-zero residuals."),
    "Exponential"=.("Exponential"),
    "Fence multipliers must be non-negative, with outer greater than inner"=.("Fence multipliers must be non-negative, with outer greater than inner"),
    "Geometric"=.("Geometric"),
    "Geometric probability must be greater than 0 and at most 1"=.("Geometric probability must be greater than 0 and at most 1"),
    "Grouping variable '{a}' must have exactly 2 levels"=.("Grouping variable '{a}' must have exactly 2 levels"),
    "Grouping variable '{a}' must not also be a dependent variable"=.("Grouping variable '{a}' must not also be a dependent variable"),
    "Grouping variable must not also be a dependent variable"=.("Grouping variable must not also be a dependent variable"),
    "Hypergeometric"=.("Hypergeometric"),
    "Hypergeometric counts must be non-negative integers, the population positive, and draws no larger than the population"=.("Hypergeometric counts must be non-negative integers, the population positive, and draws no larger than the population"),
    "Infinite paired differences are not supported"=.("Infinite paired differences are not supported"),
    "Infinite quantiles are reported exactly; the graph shows a finite portion of the distribution."=.("Infinite quantiles are reported exactly; the graph shows a finite portion of the distribution."),
    "Infinite values are not supported"=.("Infinite values are not supported"),
    "Inner-range coverage must be between 50 and 99 percent"=.("Inner-range coverage must be between 50 and 99 percent"),
    "Interval bounds must be finite and lower bound must not exceed upper bound"=.("Interval bounds must be finite and lower bound must not exceed upper bound"),
    "Median plot values and confidence intervals are not aligned"=.("Median plot values and confidence intervals are not aligned"),
    "Negative binomial"=.("Negative binomial"),
    "Negative binomial size must be positive and probability greater than 0 and at most 1"=.("Negative binomial size must be positive and probability greater than 0 and at most 1"),
    "No complete observations are available for the selected variables"=.("No complete observations are available for the selected variables"),
    "No finite medians are available for this plot"=.("No finite medians are available for this plot"),
    "Normal"=.("Normal"),
    "Not applicable: this statistic requires numeric values; the variable contains text categories"=.("Not applicable: this statistic requires numeric values; the variable contains text categories"),
    "Percentage per tail must be at least 0 and less than 50"=.("Percentage per tail must be at least 0 and less than 50"),
    "Percentiles must be numbers between 0 and 100"=.("Percentiles must be numbers between 0 and 100"),
    "Poisson"=.("Poisson"),
    "Poisson rate must be non-negative"=.("Poisson rate must be non-negative"),
    "Quantile"=.("Quantile"),
    "Quantile probabilities must be between 0 and 1"=.("Quantile probabilities must be between 0 and 1"),
    "Requires a non-constant variable"=.("Requires a non-constant variable"),
    "Requires at least two non-missing observations"=.("Requires at least two non-missing observations"),
    "Requires finite observations"=.("Requires finite observations"),
    "Requires two observed groups and finite values"=.("Requires two observed groups and finite values"),
    "Residual ACF requires at least two finite, non-constant residuals."=.("Residual ACF requires at least two finite, non-constant residuals."),
    "Scale, rate and degrees of freedom must be positive"=.("Scale, rate and degrees of freedom must be positive"),
    "Select a Group by variable"=.("Select a Group by variable"),
    "Select a finite random-variable value"=.("Select a finite random-variable value"),
    "Select ordinal variables with ordered levels"=.("Select ordinal variables with ordered levels"),
    "Skewness coverage must be 0.5 or 0.8"=.("Skewness coverage must be 0.5 or 0.8"),
    "Statistic is undefined for these data (zero spread, zero denominator or insufficient observations)"=.("Statistic is undefined for these data (zero spread, zero denominator or insufficient observations)"),
    "These parameters do not give a usable finite plotting window"=.("These parameters do not give a usable finite plotting window"),
    "These parameters require too many plotting points"=.("These parameters require too many plotting points"),
    "These parameters require too many plotting points; use less extreme values"=.("These parameters require too many plotting points; use less extreme values"),
    "This diagnostic is available for unweighted OLS models only."=.("This diagnostic is available for unweighted OLS models only."),
    "This variable has no numeric stored values"=.("This variable has no numeric stored values"),
    "Uniform"=.("Uniform"),
    "Uniform minimum must be smaller than maximum"=.("Uniform minimum must be smaller than maximum"),
    "Unknown Mann-Whitney output"=.("Unknown Mann-Whitney output"),
    "Unknown continuous distribution"=.("Unknown continuous distribution"),
    "Unknown discrete distribution"=.("Unknown discrete distribution"),
    "Unknown median confidence-interval method"=.("Unknown median confidence-interval method"),
    "Unknown normality test"=.("Unknown normality test"),
    "Variable contains infinite values"=.("Variable contains infinite values"),
    "Variable is not numeric"=.("Variable is not numeric"),
    message)
}
