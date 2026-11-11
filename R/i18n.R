# Translate saRa-owned messages at presentation boundaries.
# self must be the saRa analysis, including when rendering an add-on image.
# With no analysis context (e.g. standalone statistical tests), retain English.
# Unknown external-package diagnostics are preserved verbatim.
.saraMessage <- function(message,self=NULL) {
  if (is.null(self) || is.null(message) || length(message)!=1L || is.na(message))
    return(message)
  switch(message,
    "All observations are tied" = .("All observations are tied"),
    "Autocorrelation" = .("Autocorrelation"),
    "Binomial size must be a non-negative integer and probability between 0 and 1" = .("Binomial size must be a non-negative integer and probability between 0 and 1"),
    "Bootstrap Durbin-Watson tests are available for unweighted OLS models only." = .("Bootstrap Durbin-Watson tests are available for unweighted OLS models only."),
    "Confidence level must be between zero and one" = .("Confidence level must be between zero and one"),
    "Cook's D" = .("Cook's D"),
    "Durbin-Watson tests require a fitted model with finite, non-zero residuals." = .("Durbin-Watson tests require a fitted model with finite, non-zero residuals."),
    "Fitted values" = .("Fitted values"),
    "Geometric probability must be greater than 0 and at most 1" = .("Geometric probability must be greater than 0 and at most 1"),
    "Hypergeometric counts must be non-negative integers, the population positive, and draws no larger than the population" = .("Hypergeometric counts must be non-negative integers, the population positive, and draws no larger than the population"),
    "Infinite paired differences are not supported" = .("Infinite paired differences are not supported"),
    "Infinite values are not supported" = .("Infinite values are not supported"),
    "Lag" = .("Lag"),
    "Leverage (hat value)" = .("Leverage (hat value)"),
    "Median plot values and confidence intervals are not aligned" = .("Median plot values and confidence intervals are not aligned"),
    "Negative binomial size must be positive and probability greater than 0 and at most 1" = .("Negative binomial size must be positive and probability greater than 0 and at most 1"),
    "No finite medians are available for this plot" = .("No finite medians are available for this plot"),
    "Note. Plot obtained with the saRa module." = .("Note. Plot obtained with the saRa module."),
    "One or both groups do not contain enough observations" = .("One or both groups do not contain enough observations"),
    "Plot could not be drawn: {message}" = .("Plot could not be drawn: {message}"),
    "Plot data unavailable. Reselect Residual plots to refresh." = .("Plot data unavailable. Reselect Residual plots to refresh."),
    "Poisson rate must be non-negative" = .("Poisson rate must be non-negative"),
    "Requires a non-constant variable" = .("Requires a non-constant variable"),
    "Requires at least two non-missing observations" = .("Requires at least two non-missing observations"),
    "Requires finite observations" = .("Requires finite observations"),
    "Requires two observed groups and finite values" = .("Requires two observed groups and finite values"),
    "Residual ACF requires at least two finite, non-constant residuals." = .("Residual ACF requires at least two finite, non-constant residuals."),
    "Residuals" = .("Residuals"),
    "Row order; {lags} automatic lags." = .("Row order; {lags} automatic lags."),
    "Approximate 95% white-noise bounds." = .("Approximate 95% white-noise bounds."),
    "Sqrt(|standardized residual|)" = .("Sqrt(|standardized residual|)"),
    "Standardised values" = .("Standardised values"),
    "Standardized residual quantiles" = .("Standardized residual quantiles"),
    "Studentized residuals" = .("Studentized residuals"),
    "Theoretical normal quantiles" = .("Theoretical normal quantiles"),
    "Theoretical quantiles" = .("Theoretical quantiles"),
    "These parameters require too many plotting points; use less extreme values" = .("These parameters require too many plotting points; use less extreme values"),
    "This diagnostic is available for unweighted OLS models only." = .("This diagnostic is available for unweighted OLS models only."),
    "Unknown Mann-Whitney output" = .("Unknown Mann-Whitney output"),
    "Unknown discrete distribution" = .("Unknown discrete distribution"),
    "Unknown median confidence-interval method" = .("Unknown median confidence-interval method"),
    "Unknown normality test" = .("Unknown normality test"),
    "Unknown regression plot type" = .("Unknown regression plot type"),
    "Variable contains infinite values" = .("Variable contains infinite values"),
    "Variable is not numeric" = .("Variable is not numeric"),
    "Case" = .("Case"),
    "Durbin-Watson Tests by Lag" = .("Durbin-Watson Tests by Lag"),
    "Heteroskedasticity Tests" = .("Heteroskedasticity Tests"),
    "Influence Indices (Noteworthy Cases)" = .("Influence Indices (Noteworthy Cases)"),
    "Influence Plot" = .("Influence Plot"),
    "Koenker (studentized Breusch-Pagan)" = .("Koenker (studentized Breusch-Pagan)"),
    "Kolmogorov-Smirnov (Lilliefors)" = .("Kolmogorov-Smirnov (Lilliefors)"),
    "Kolmogorov-Smirnov uses the Lilliefors correction for estimated mean and variance. Residual normality diagnostics." = .("Kolmogorov-Smirnov uses the Lilliefors correction for estimated mean and variance. Residual normality diagnostics."),
    "Lags 1-10, capped at N-1. Two-sided p-values from residual bootstrapping with 5,000 resamples; seed = 123456. Complete-case row order; p-values are not adjusted across lags." = .("Lags 1-10, capped at N-1. Two-sided p-values from residual bootstrapping with 5,000 resamples; seed = 123456. Complete-case row order; p-values are not adjusted across lags."),
    "Normal Q-Q with Residual Histogram" = .("Normal Q-Q with Residual Histogram"),
    "Normality Tests" = .("Normality Tests"),
    "Null hypothesis: constant error variance. GQ: two-sided, ordered by fitted values, split at 50%, no central omission. HMC: ordered by fitted values, split at 50%; 5,000 simulations, seed 123456. BP statistic is chi-square, GQ is F; HMC is a residual sum-of-squares fraction." = .("Null hypothesis: constant error variance. GQ: two-sided, ordered by fitted values, split at 50%, no central omission. HMC: ordered by fitted values, split at 50%; 5,000 simulations, seed 123456. BP statistic is chi-square, GQ is F; HMC is a residual sum-of-squares fraction."),
    "Residual ACF" = .("Residual ACF"),
    "Residuals vs Fitted" = .("Residuals vs Fitted"),
    "Same noteworthy rule as car::influencePlot(): union of the two largest absolute studentized residuals, leverage values and Cook's distances. Case labels identify original fitted rows; selection is diagnostic, not an exclusion rule." = .("Same noteworthy rule as car::influencePlot(): union of the two largest absolute studentized residuals, leverage values and Cook's distances. Case labels identify original fitted rows; selection is diagnostic, not an exclusion rule."),
    "Scale-Location" = .("Scale-Location"),
    "Specification Test" = .("Specification Test"),
    "Statistic" = .("Statistic"),
    "Table obtained with the saRa module. Tests squared and cubed fitted values for functional-form misspecification." = .("Table obtained with the saRa module. Tests squared and cubed fitted values for functional-form misspecification."),
    "Table obtained with the saRa module." = .("Table obtained with the saRa module."),
    "Test" = .("Test"),
    "Unavailable" = .("Unavailable"),
    "{title} - Model {model}" = .("{title} - Model {model}"),
    message)
}

# Add-on options may not receive the parent's language. Reuse only the public
# language protobuf entry, with a fresh Options object bound to saRa's catalog.
# Never attach the parent's jmv translator or persist an R6 owner in plot state.
.saraAddonTranslationOwner <- function(owner) {
  if (is.null(owner) || is.null(owner$parent)) return(owner)
  parentOptions <- owner$parent$options
  if (is.null(parentOptions) || !is.function(parentOptions$asProtoBuf)) return(owner)
  pb <- parentOptions$asProtoBuf()
  if (is.null(pb) || is.null(pb$names)) return(owner)
  index <- match(".lang",pb$names)
  if (is.na(index)) return(owner)
  options <- jmvcore::Options$new(package="saRa")
  options$fromProtoBuf(list(names=".lang",options=list(pb$options[[index]])))
  list(options=options)
}
.saraAddonMessage <- function(message,owner) {
  .saraMessage(message,self=.saraAddonTranslationOwner(owner))
}
