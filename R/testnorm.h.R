
# This file is automatically generated, you probably don't want to edit this

testnormOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "testnormOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            vars = NULL,
            groupBy = NULL,
            chisqtest = FALSE,
            kstest = FALSE,
            swtest = FALSE,
            lillietest = FALSE,
            adtest = FALSE,
            sftest = FALSE,
            cvmtest = FALSE,
            hist = FALSE,
            dens = FALSE,
            norm = FALSE,
            qq = FALSE,
            ecdf = FALSE, ...) {

            super$initialize(
                package="saRa",
                name="testnorm",
                requiresData=TRUE,
                ...)

            private$..vars <- jmvcore::OptionVariables$new(
                "vars",
                vars,
                suggested=list(
                    "continuous"),
                permitted=list(
                    "numeric"))
            private$..groupBy <- jmvcore::OptionVariable$new(
                "groupBy",
                groupBy,
                suggested=list(
                    "nominal"),
                permitted=list(
                    "factor"),
                default=NULL)
            private$..chisqtest <- jmvcore::OptionBool$new(
                "chisqtest",
                chisqtest,
                default=FALSE)
            private$..kstest <- jmvcore::OptionBool$new(
                "kstest",
                kstest,
                default=FALSE)
            private$..swtest <- jmvcore::OptionBool$new(
                "swtest",
                swtest,
                default=FALSE)
            private$..lillietest <- jmvcore::OptionBool$new(
                "lillietest",
                lillietest,
                default=FALSE)
            private$..adtest <- jmvcore::OptionBool$new(
                "adtest",
                adtest,
                default=FALSE)
            private$..sftest <- jmvcore::OptionBool$new(
                "sftest",
                sftest,
                default=FALSE)
            private$..cvmtest <- jmvcore::OptionBool$new(
                "cvmtest",
                cvmtest,
                default=FALSE)
            private$..hist <- jmvcore::OptionBool$new(
                "hist",
                hist,
                default=FALSE)
            private$..dens <- jmvcore::OptionBool$new(
                "dens",
                dens,
                default=FALSE)
            private$..norm <- jmvcore::OptionBool$new(
                "norm",
                norm,
                default=FALSE)
            private$..qq <- jmvcore::OptionBool$new(
                "qq",
                qq,
                default=FALSE)
            private$..ecdf <- jmvcore::OptionBool$new(
                "ecdf",
                ecdf,
                default=FALSE)

            self$.addOption(private$..vars)
            self$.addOption(private$..groupBy)
            self$.addOption(private$..chisqtest)
            self$.addOption(private$..kstest)
            self$.addOption(private$..swtest)
            self$.addOption(private$..lillietest)
            self$.addOption(private$..adtest)
            self$.addOption(private$..sftest)
            self$.addOption(private$..cvmtest)
            self$.addOption(private$..hist)
            self$.addOption(private$..dens)
            self$.addOption(private$..norm)
            self$.addOption(private$..qq)
            self$.addOption(private$..ecdf)
        }),
    active = list(
        vars = function() private$..vars$value,
        groupBy = function() private$..groupBy$value,
        chisqtest = function() private$..chisqtest$value,
        kstest = function() private$..kstest$value,
        swtest = function() private$..swtest$value,
        lillietest = function() private$..lillietest$value,
        adtest = function() private$..adtest$value,
        sftest = function() private$..sftest$value,
        cvmtest = function() private$..cvmtest$value,
        hist = function() private$..hist$value,
        dens = function() private$..dens$value,
        norm = function() private$..norm$value,
        qq = function() private$..qq$value,
        ecdf = function() private$..ecdf$value),
    private = list(
        ..vars = NA,
        ..groupBy = NA,
        ..chisqtest = NA,
        ..kstest = NA,
        ..swtest = NA,
        ..lillietest = NA,
        ..adtest = NA,
        ..sftest = NA,
        ..cvmtest = NA,
        ..hist = NA,
        ..dens = NA,
        ..norm = NA,
        ..qq = NA,
        ..ecdf = NA)
)

testnormResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "testnormResults",
    inherit = jmvcore::Group,
    active = list(
        text = function() private$.items[["text"]],
        normtests = function() private$.items[["normtests"]],
        plots = function() private$.items[["plots"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Normality tests")
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="text",
                title="Normality tests"))
            self$add(jmvcore::Table$new(
                options=options,
                name="normtests",
                title="Tests",
                visible="(chisqtest || kstest || swtest || adtest || lillietest || sftest || cvmtest)",
                rows=0,
                columns=list(
                    list(
                        `name`="depvar", 
                        `title`="Variable", 
                        `type`="text"),
                    list(
                        `name`="group", 
                        `title`="Group", 
                        `type`="text"),
                    list(
                        `name`="var[chisqtest]", 
                        `title`="", 
                        `type`="text", 
                        `combineBelow`=TRUE, 
                        `visible`="(chisqtest)"),
                    list(
                        `name`="name[chisqtest]", 
                        `title`="", 
                        `type`="text", 
                        `content`="Pearson \u03C7\u00B2 test", 
                        `visible`="(chisqtest)"),
                    list(
                        `name`="stat[chisqtest]", 
                        `title`="Statistic", 
                        `type`="number", 
                        `visible`="(chisqtest)"),
                    list(
                        `name`="p[chisqtest]", 
                        `title`="p", 
                        `type`="number", 
                        `format`="zto,pvalue", 
                        `visible`="(chisqtest)"),
                    list(
                        `name`="var[kstest]", 
                        `title`="", 
                        `type`="text", 
                        `combineBelow`=TRUE, 
                        `visible`="(kstest)"),
                    list(
                        `name`="name[kstest]", 
                        `title`="", 
                        `type`="text", 
                        `content`="Kolmogorov-Smirnov test", 
                        `visible`="(kstest)"),
                    list(
                        `name`="stat[kstest]", 
                        `title`="Statistic", 
                        `type`="number", 
                        `visible`="(kstest)"),
                    list(
                        `name`="p[kstest]", 
                        `title`="p", 
                        `type`="number", 
                        `format`="zto,pvalue", 
                        `visible`="(kstest)"),
                    list(
                        `name`="var[swtest]", 
                        `title`="", 
                        `type`="text", 
                        `combineBelow`=TRUE, 
                        `visible`="(swtest)"),
                    list(
                        `name`="name[swtest]", 
                        `title`="", 
                        `type`="text", 
                        `content`="Shapiro-Wilk test", 
                        `visible`="(swtest)"),
                    list(
                        `name`="stat[swtest]", 
                        `title`="Statistic", 
                        `type`="number", 
                        `visible`="(swtest)"),
                    list(
                        `name`="p[swtest]", 
                        `title`="p", 
                        `type`="number", 
                        `format`="zto,pvalue", 
                        `visible`="(swtest)"),
                    list(
                        `name`="var[adtest]", 
                        `title`="", 
                        `type`="text", 
                        `combineBelow`=TRUE, 
                        `visible`="(adtest)"),
                    list(
                        `name`="name[adtest]", 
                        `title`="", 
                        `type`="text", 
                        `content`="Anderson-Darling test", 
                        `visible`="(adtest)"),
                    list(
                        `name`="stat[adtest]", 
                        `title`="Statistic", 
                        `type`="number", 
                        `visible`="(adtest)"),
                    list(
                        `name`="p[adtest]", 
                        `title`="p", 
                        `type`="number", 
                        `format`="zto,pvalue", 
                        `visible`="(adtest)"),
                    list(
                        `name`="var[lillietest]", 
                        `title`="", 
                        `type`="text", 
                        `combineBelow`=TRUE, 
                        `visible`="(lillietest)"),
                    list(
                        `name`="name[lillietest]", 
                        `title`="", 
                        `type`="text", 
                        `content`="Lilliefors test", 
                        `visible`="(lillietest)"),
                    list(
                        `name`="stat[lillietest]", 
                        `title`="Statistic", 
                        `type`="number", 
                        `visible`="(lillietest)"),
                    list(
                        `name`="p[lillietest]", 
                        `title`="p", 
                        `type`="number", 
                        `format`="zto,pvalue", 
                        `visible`="(lillietest)"),
                    list(
                        `name`="var[sftest]", 
                        `title`="", 
                        `type`="text", 
                        `combineBelow`=TRUE, 
                        `visible`="(sftest)"),
                    list(
                        `name`="name[sftest]", 
                        `title`="", 
                        `type`="text", 
                        `content`="Shapiro-Francia test", 
                        `visible`="(sftest)"),
                    list(
                        `name`="stat[sftest]", 
                        `title`="Statistic", 
                        `type`="number", 
                        `visible`="(sftest)"),
                    list(
                        `name`="p[sftest]", 
                        `title`="p", 
                        `type`="number", 
                        `format`="zto,pvalue", 
                        `visible`="(sftest)"),
                    list(
                        `name`="var[cvmtest]", 
                        `title`="", 
                        `type`="text", 
                        `combineBelow`=TRUE, 
                        `visible`="(cvmtest)"),
                    list(
                        `name`="name[cvmtest]", 
                        `title`="", 
                        `type`="text", 
                        `content`="Cramer-von Mises test", 
                        `visible`="(cvmtest)"),
                    list(
                        `name`="stat[cvmtest]", 
                        `title`="Statistic", 
                        `type`="number", 
                        `visible`="(cvmtest)"),
                    list(
                        `name`="p[cvmtest]", 
                        `title`="p", 
                        `type`="number", 
                        `format`="zto,pvalue", 
                        `visible`="(cvmtest)"))))
            self$add(jmvcore::Array$new(
                options=options,
                name="plots",
                title="Plots",
                items="(vars)",
                clearWith=list(
                    "depvar",
                    "group"),
                template=R6::R6Class(
                    inherit = jmvcore::Group,
                    active = list(),
                    private = list(),
                    public=list(
                        initialize=function(options) {
                            super$initialize(
                                options=options,
                                name="undefined",
                                title="($key)")}))$new(options=options)))}))

testnormBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "testnormBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "saRa",
                name = "testnorm",
                version = c(1,0,0),
                options = options,
                results = testnormResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'auto')
        }))

#' Normality tests
#'
#' 
#' @param data .
#' @param vars a vector of strings naming the variables of interest in
#'   \code{data}
#' @param groupBy a vector of strings naming the variables used to segment
#'   \code{vars}
#' @param chisqtest \code{TRUE} or \code{FALSE} (default), perform Pearson's
#'   X² tests
#' @param kstest \code{TRUE} or \code{FALSE} (default), perform
#'   Kolmogorov-Smirnov tests
#' @param swtest \code{TRUE} or \code{FALSE} (default), perform Shapiro-Wilk
#'   tests
#' @param lillietest \code{TRUE} or \code{FALSE} (default), perform Lilliefors
#'   tests
#' @param adtest \code{TRUE} or \code{FALSE} (default), perform
#'   Anderson-Darling tests
#' @param sftest \code{TRUE} or \code{FALSE} (default), perform
#'   Shapiro-Francia tests
#' @param cvmtest \code{TRUE} or \code{FALSE} (default), perform Cramer-von
#'   Mises tests
#' @param hist \code{TRUE} or \code{FALSE} (default), provides histograms
#'   (continuous variables only)
#' @param dens \code{TRUE} or \code{FALSE} (default), provides density plots
#'   (continuous variables only)
#' @param norm \code{TRUE} or \code{FALSE} (default), provides overlapped
#'   normal curve (continuous variables only)
#' @param qq \code{TRUE} or \code{FALSE} (default), provide Q-Q plots
#'   (continuous variables only)
#' @param ecdf \code{TRUE} or \code{FALSE} (default), provide ECDF plots
#'   (continuous variables only)
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$text} \tab \tab \tab \tab \tab a preformatted \cr
#'   \code{results$normtests} \tab \tab \tab \tab \tab A table for the normality tests \cr
#'   \code{results$plots} \tab \tab \tab \tab \tab An array of descriptive plots \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$normtests$asDF}
#'
#' \code{as.data.frame(results$normtests)}
#'
#' @export
testnorm <- function(
    data,
    vars,
    groupBy = NULL,
    chisqtest = FALSE,
    kstest = FALSE,
    swtest = FALSE,
    lillietest = FALSE,
    adtest = FALSE,
    sftest = FALSE,
    cvmtest = FALSE,
    hist = FALSE,
    dens = FALSE,
    norm = FALSE,
    qq = FALSE,
    ecdf = FALSE) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("testnorm requires jmvcore to be installed (restart may be required)")

    if ( ! missing(vars)) vars <- jmvcore::resolveQuo(jmvcore::enquo(vars))
    if ( ! missing(groupBy)) groupBy <- jmvcore::resolveQuo(jmvcore::enquo(groupBy))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(vars), vars, NULL),
            `if`( ! missing(groupBy), groupBy, NULL))

    for (v in groupBy) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])

    options <- testnormOptions$new(
        vars = vars,
        groupBy = groupBy,
        chisqtest = chisqtest,
        kstest = kstest,
        swtest = swtest,
        lillietest = lillietest,
        adtest = adtest,
        sftest = sftest,
        cvmtest = cvmtest,
        hist = hist,
        dens = dens,
        norm = norm,
        qq = qq,
        ecdf = ecdf)

    analysis <- testnormClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

