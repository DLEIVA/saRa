
# This file is automatically generated, you probably don't want to edit this

umwOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "umwOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            vars = NULL,
            group = NULL,
            mwu = TRUE,
            zstat = TRUE,
            pSup = FALSE,
            rankCorr = FALSE,
            fstat = FALSE,
            hypothesis = "different",
            ciMedians = FALSE,
            ciHL = FALSE,
            ciWidth = 95,
            ciMethod = "exact",
            numR = 999,
            desc = FALSE,
            plots = FALSE,
            miss = "perAnalysis", ...) {

            super$initialize(
                package="saRa",
                name="umw",
                requiresData=TRUE,
                ...)

            private$..vars <- jmvcore::OptionVariables$new(
                "vars",
                vars,
                suggested=list(
                    "continuous"),
                permitted=list(
                    "numeric"),
                rejectInf=FALSE)
            private$..group <- jmvcore::OptionVariable$new(
                "group",
                group,
                suggested=list(
                    "nominal",
                    "ordinal"),
                permitted=list(
                    "factor"))
            private$..mwu <- jmvcore::OptionBool$new(
                "mwu",
                mwu,
                default=TRUE)
            private$..zstat <- jmvcore::OptionBool$new(
                "zstat",
                zstat,
                default=TRUE)
            private$..pSup <- jmvcore::OptionBool$new(
                "pSup",
                pSup,
                default=FALSE)
            private$..rankCorr <- jmvcore::OptionBool$new(
                "rankCorr",
                rankCorr,
                default=FALSE)
            private$..fstat <- jmvcore::OptionBool$new(
                "fstat",
                fstat,
                default=FALSE)
            private$..hypothesis <- jmvcore::OptionList$new(
                "hypothesis",
                hypothesis,
                options=list(
                    "different",
                    "oneGreater",
                    "twoGreater"),
                default="different")
            private$..ciMedians <- jmvcore::OptionBool$new(
                "ciMedians",
                ciMedians,
                default=FALSE)
            private$..ciHL <- jmvcore::OptionBool$new(
                "ciHL",
                ciHL,
                default=FALSE)
            private$..ciWidth <- jmvcore::OptionNumber$new(
                "ciWidth",
                ciWidth,
                min=50,
                max=99.9,
                default=95)
            private$..ciMethod <- jmvcore::OptionList$new(
                "ciMethod",
                ciMethod,
                options=list(
                    "exact",
                    "boot"),
                default="exact")
            private$..numR <- jmvcore::OptionNumber$new(
                "numR",
                numR,
                min=499,
                max=9999,
                default=999)
            private$..desc <- jmvcore::OptionBool$new(
                "desc",
                desc,
                default=FALSE)
            private$..plots <- jmvcore::OptionBool$new(
                "plots",
                plots,
                default=FALSE)
            private$..miss <- jmvcore::OptionList$new(
                "miss",
                miss,
                options=list(
                    "perAnalysis",
                    "listwise"),
                default="perAnalysis")

            self$.addOption(private$..vars)
            self$.addOption(private$..group)
            self$.addOption(private$..mwu)
            self$.addOption(private$..zstat)
            self$.addOption(private$..pSup)
            self$.addOption(private$..rankCorr)
            self$.addOption(private$..fstat)
            self$.addOption(private$..hypothesis)
            self$.addOption(private$..ciMedians)
            self$.addOption(private$..ciHL)
            self$.addOption(private$..ciWidth)
            self$.addOption(private$..ciMethod)
            self$.addOption(private$..numR)
            self$.addOption(private$..desc)
            self$.addOption(private$..plots)
            self$.addOption(private$..miss)
        }),
    active = list(
        vars = function() private$..vars$value,
        group = function() private$..group$value,
        mwu = function() private$..mwu$value,
        zstat = function() private$..zstat$value,
        pSup = function() private$..pSup$value,
        rankCorr = function() private$..rankCorr$value,
        fstat = function() private$..fstat$value,
        hypothesis = function() private$..hypothesis$value,
        ciMedians = function() private$..ciMedians$value,
        ciHL = function() private$..ciHL$value,
        ciWidth = function() private$..ciWidth$value,
        ciMethod = function() private$..ciMethod$value,
        numR = function() private$..numR$value,
        desc = function() private$..desc$value,
        plots = function() private$..plots$value,
        miss = function() private$..miss$value),
    private = list(
        ..vars = NA,
        ..group = NA,
        ..mwu = NA,
        ..zstat = NA,
        ..pSup = NA,
        ..rankCorr = NA,
        ..fstat = NA,
        ..hypothesis = NA,
        ..ciMedians = NA,
        ..ciHL = NA,
        ..ciWidth = NA,
        ..ciMethod = NA,
        ..numR = NA,
        ..desc = NA,
        ..plots = NA,
        ..miss = NA)
)

umwResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "umwResults",
    inherit = jmvcore::Group,
    active = list(
        mwutest = function() private$.items[["mwutest"]],
        cisMed = function() private$.items[["cisMed"]],
        cisHL = function() private$.items[["cisHL"]],
        desc = function() private$.items[["desc"]],
        plots = function() private$.items[["plots"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Mann-Whitney test")
            self$add(jmvcore::Table$new(
                options=options,
                name="mwutest",
                title="Mann-Whitney U test and ES indices",
                rows="(vars)",
                visible="(mwu || zstat || pSup || rankCorr || fstat)",
                clearWith=list(
                    "group",
                    "hypothesis",
                    "ciWidth",
                    "miss"),
                columns=list(
                    list(
                        `name`="var[mwu]", 
                        `title`="", 
                        `content`="($key)", 
                        `type`="text", 
                        `combineBelow`=TRUE, 
                        `visible`="(mwu)"),
                    list(
                        `name`="name[mwu]", 
                        `title`="", 
                        `type`="text", 
                        `content`="Mann-Whitney U", 
                        `visible`="(mwu)"),
                    list(
                        `name`="stat[mwu]", 
                        `title`="Statistic", 
                        `type`="number", 
                        `visible`="(mwu)"),
                    list(
                        `name`="p[mwu]", 
                        `title`="p", 
                        `type`="number", 
                        `format`="zto,pvalue", 
                        `visible`="(mwu)"),
                    list(
                        `name`="var[zstat]", 
                        `title`="", 
                        `content`="($key)", 
                        `type`="text", 
                        `combineBelow`=TRUE, 
                        `visible`="(zstat)"),
                    list(
                        `name`="name[zstat]", 
                        `title`="", 
                        `type`="text", 
                        `content`="z Statistic", 
                        `visible`="(zstat)"),
                    list(
                        `name`="stat[zstat]", 
                        `title`="Statistic", 
                        `type`="number", 
                        `visible`="(zstat)"),
                    list(
                        `name`="p[zstat]", 
                        `title`="p", 
                        `type`="number", 
                        `format`="zto,pvalue", 
                        `visible`="(zstat)"),
                    list(
                        `name`="var[pSup]", 
                        `title`="", 
                        `content`="($key)", 
                        `type`="text", 
                        `combineBelow`=TRUE, 
                        `visible`="(pSup)"),
                    list(
                        `name`="name[pSup]", 
                        `title`="", 
                        `type`="text", 
                        `content`="Probability of Superiority", 
                        `visible`="(pSup)"),
                    list(
                        `name`="stat[pSup]", 
                        `title`="Statistic", 
                        `visible`="(pSup)"),
                    list(
                        `name`="var[rankCorr]", 
                        `title`="", 
                        `content`="($key)", 
                        `type`="text", 
                        `combineBelow`=TRUE, 
                        `visible`="(rankCorr)"),
                    list(
                        `name`="name[rankCorr]", 
                        `title`="", 
                        `type`="text", 
                        `content`="Rank Biserial Correlation", 
                        `visible`="(rankCorr)"),
                    list(
                        `name`="stat[rankCorr]", 
                        `title`="Statistic", 
                        `content`=".", 
                        `visible`="(rankCorr)"),
                    list(
                        `name`="var[fstat]", 
                        `title`="", 
                        `content`="($key)", 
                        `type`="text", 
                        `combineBelow`=TRUE, 
                        `visible`="(fstat)"),
                    list(
                        `name`="name[fstat]", 
                        `title`="", 
                        `type`="text", 
                        `content`="f Statistic", 
                        `visible`="(fstat)"),
                    list(
                        `name`="stat[fstat]", 
                        `title`="Statistic", 
                        `content`=".", 
                        `visible`="(fstat)"))))
            self$add(jmvcore::Table$new(
                options=options,
                name="cisMed",
                title="Medians CIs",
                visible="(ciMedians)",
                rows="(vars)",
                clearWith=list(
                    "group",
                    "miss"),
                columns=list(
                    list(
                        `name`="dep", 
                        `title`="", 
                        `content`="($key)", 
                        `type`="text"),
                    list(
                        `name`="group[1]", 
                        `title`="Group", 
                        `type`="text"),
                    list(
                        `name`="median[1]", 
                        `title`="Median", 
                        `type`="number"),
                    list(
                        `name`="cilMed[1]", 
                        `title`="Lower Bound", 
                        `type`="number"),
                    list(
                        `name`="ciuMed[1]", 
                        `title`="Upper Bound", 
                        `type`="number"),
                    list(
                        `name`="group[2]", 
                        `title`="Group", 
                        `type`="text"),
                    list(
                        `name`="median[2]", 
                        `title`="Median", 
                        `type`="number"),
                    list(
                        `name`="cilMed[2]", 
                        `title`="Lower Bound", 
                        `type`="number"),
                    list(
                        `name`="ciuMed[2]", 
                        `title`="Upper Bound", 
                        `type`="number"))))
            self$add(jmvcore::Table$new(
                options=options,
                name="cisHL",
                title="Hodges-Lehmann Estimator CIs",
                visible="(ciHL)",
                rows="(vars)",
                clearWith=list(
                    "group",
                    "miss"),
                columns=list(
                    list(
                        `name`="dep", 
                        `title`="", 
                        `content`="($key)", 
                        `type`="text"),
                    list(
                        `name`="hlestimate", 
                        `title`="Hodges-Lehmann Estimator", 
                        `type`="number"),
                    list(
                        `name`="cilHL", 
                        `title`="Lower Bound", 
                        `type`="number"),
                    list(
                        `name`="ciuHL", 
                        `title`="Upper Bound", 
                        `type`="number"))))
            self$add(jmvcore::Table$new(
                options=options,
                name="desc",
                title="Groups Descriptives",
                visible="(desc)",
                rows="(vars)",
                clearWith=list(
                    "group",
                    "miss"),
                columns=list(
                    list(
                        `name`="dep", 
                        `title`="", 
                        `content`="($key)", 
                        `type`="text"),
                    list(
                        `name`="group[1]", 
                        `title`="Group", 
                        `type`="text"),
                    list(
                        `name`="num[1]", 
                        `title`="N", 
                        `type`="integer"),
                    list(
                        `name`="median[1]", 
                        `title`="Median", 
                        `type`="number"),
                    list(
                        `name`="rankA[1]", 
                        `title`="Average Rank", 
                        `type`="number"),
                    list(
                        `name`="rankS[1]", 
                        `title`="Ranks Sum", 
                        `type`="number"),
                    list(
                        `name`="group[2]", 
                        `title`="Group", 
                        `type`="text"),
                    list(
                        `name`="num[2]", 
                        `title`="N", 
                        `type`="integer"),
                    list(
                        `name`="median[2]", 
                        `title`="Median", 
                        `type`="number"),
                    list(
                        `name`="rankA[2]", 
                        `title`="Average Rank", 
                        `type`="number"),
                    list(
                        `name`="rankS[2]", 
                        `title`="Ranks Sum", 
                        `type`="number"))))
            self$add(jmvcore::Array$new(
                options=options,
                name="plots",
                title="Plots",
                items="(vars)",
                clearWith=list(
                    "group",
                    "miss",
                    "ciWidth"),
                template=R6::R6Class(
                    inherit = jmvcore::Group,
                    active = list(
                        desc = function() private$.items[["desc"]]),
                    private = list(),
                    public=list(
                        initialize=function(options) {
                            super$initialize(
                                options=options,
                                name="undefined",
                                title="$key")
                            self$add(jmvcore::Image$new(
                                options=options,
                                name="desc",
                                width=450,
                                height=400,
                                visible="(plots)",
                                renderFun=".desc",
                                clearWith=list()))}))$new(options=options)))}))

umwBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "umwBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "saRa",
                name = "umw",
                version = c(1,0,0),
                options = options,
                results = umwResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'auto')
        }))

#' Mann-Whitney test
#'
#' 
#' @param data the data as a data frame
#' @param vars the dependent variables
#' @param group the grouping variable with two levels
#' @param mwu \code{TRUE} (default) or \code{FALSE}, perform Mann Whitney's U
#'   tests
#' @param zstat \code{TRUE} (default) or \code{FALSE}, perform Z tests
#' @param pSup \code{TRUE} or \code{FALSE} (default), compute probability of
#'   superiority index
#' @param rankCorr \code{TRUE} or \code{FALSE} (default), compute rank
#'   biserial correlation.
#' @param fstat \code{TRUE} or \code{FALSE} (default), compute f statistic.
#' @param hypothesis \code{'different'} (default), \code{'oneGreater'} or
#'   \code{'twoGreater'}, the alternative hypothesis; group 1 different to group
#'   2, group 1 greater than group 2, and group 2 greater than group 1
#'   respectively
#' @param ciMedians \code{TRUE} or \code{FALSE} (default), provide confidence
#'   intervals for the medians
#' @param ciHL \code{TRUE} or \code{FALSE} (default), provide confidence
#'   intervals for the  Hodges-Lehmann estimator
#' @param ciWidth a number between 50 and 99.9 (default: 95), the width of
#'   confidence intervals
#' @param ciMethod \code{exact} (default), or \code{boot}, specifies the
#'   method for obtaining CIs
#' @param numR a number between 499 and 9999 (default: 999), the number of
#'   replications for Bootstrap CIs.
#' @param desc \code{TRUE} or \code{FALSE} (default), provide descriptive
#'   statistics
#' @param plots \code{TRUE} or \code{FALSE} (default), provide descriptive
#'   plots
#' @param miss \code{'perAnalysis'} or \code{'listwise'}, how to handle
#'   missing values; \code{'perAnalysis'} excludes missing values for individual
#'   dependent variables, \code{'listwise'} excludes a row from all analyses if
#'   one of its entries is missing....
#' @param formula (optional) the formula to use, see the examples
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$mwutest} \tab \tab \tab \tab \tab a table containing the Mann-Whitney U test results \cr
#'   \code{results$cisMed} \tab \tab \tab \tab \tab a table containing confidence interval estimates for the medians \cr
#'   \code{results$cisHL} \tab \tab \tab \tab \tab a table containing confidence interval estimates for HL estimator \cr
#'   \code{results$desc} \tab \tab \tab \tab \tab a table containing the group descriptives \cr
#'   \code{results$plots} \tab \tab \tab \tab \tab an array of groups of plots \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$mwutest$asDF}
#'
#' \code{as.data.frame(results$mwutest)}
#'
#' @export
umw <- function(
    data,
    vars,
    group,
    mwu = TRUE,
    zstat = TRUE,
    pSup = FALSE,
    rankCorr = FALSE,
    fstat = FALSE,
    hypothesis = "different",
    ciMedians = FALSE,
    ciHL = FALSE,
    ciWidth = 95,
    ciMethod = "exact",
    numR = 999,
    desc = FALSE,
    plots = FALSE,
    miss = "perAnalysis",
    formula) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("umw requires jmvcore to be installed (restart may be required)")

    if ( ! missing(formula)) {
        if (missing(vars))
            vars <- jmvcore::marshalFormula(
                formula=formula,
                data=`if`( ! missing(data), data, NULL),
                from="lhs",
                required=TRUE)
        if (missing(group))
            group <- jmvcore::marshalFormula(
                formula=formula,
                data=`if`( ! missing(data), data, NULL),
                from="rhs",
                subset="1")
    }

    if ( ! missing(vars)) vars <- jmvcore::resolveQuo(jmvcore::enquo(vars))
    if ( ! missing(group)) group <- jmvcore::resolveQuo(jmvcore::enquo(group))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(vars), vars, NULL),
            `if`( ! missing(group), group, NULL))

    for (v in group) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])

    options <- umwOptions$new(
        vars = vars,
        group = group,
        mwu = mwu,
        zstat = zstat,
        pSup = pSup,
        rankCorr = rankCorr,
        fstat = fstat,
        hypothesis = hypothesis,
        ciMedians = ciMedians,
        ciHL = ciHL,
        ciWidth = ciWidth,
        ciMethod = ciMethod,
        numR = numR,
        desc = desc,
        plots = plots,
        miss = miss)

    analysis <- umwClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

