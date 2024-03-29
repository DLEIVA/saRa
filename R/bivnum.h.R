
# This file is automatically generated, you probably don't want to edit this

bivnumOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "bivnumOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            xvar = NULL,
            yvar = NULL,
            covar = FALSE,
            pearsonR = FALSE,
            spearmanR = FALSE,
            tauK = FALSE,
            pearsonR2 = FALSE,
            spearmanR2 = FALSE,
            kendallR2 = FALSE,
            scat = FALSE,
            marg = "none",
            line = "none",
            se = FALSE, ...) {

            super$initialize(
                package="saRa",
                name="bivnum",
                requiresData=TRUE,
                ...)

            private$..xvar <- jmvcore::OptionVariable$new(
                "xvar",
                xvar,
                suggested=list(
                    "continuous"),
                permitted=list(
                    "numeric"))
            private$..yvar <- jmvcore::OptionVariable$new(
                "yvar",
                yvar,
                suggested=list(
                    "continuous"),
                permitted=list(
                    "numeric"))
            private$..covar <- jmvcore::OptionBool$new(
                "covar",
                covar,
                default=FALSE)
            private$..pearsonR <- jmvcore::OptionBool$new(
                "pearsonR",
                pearsonR,
                default=FALSE)
            private$..spearmanR <- jmvcore::OptionBool$new(
                "spearmanR",
                spearmanR,
                default=FALSE)
            private$..tauK <- jmvcore::OptionBool$new(
                "tauK",
                tauK,
                default=FALSE)
            private$..pearsonR2 <- jmvcore::OptionBool$new(
                "pearsonR2",
                pearsonR2,
                default=FALSE)
            private$..spearmanR2 <- jmvcore::OptionBool$new(
                "spearmanR2",
                spearmanR2,
                default=FALSE)
            private$..kendallR2 <- jmvcore::OptionBool$new(
                "kendallR2",
                kendallR2,
                default=FALSE)
            private$..scat <- jmvcore::OptionBool$new(
                "scat",
                scat,
                default=FALSE)
            private$..marg <- jmvcore::OptionList$new(
                "marg",
                marg,
                options=list(
                    "none",
                    "dens",
                    "hist",
                    "box"),
                default="none")
            private$..line <- jmvcore::OptionList$new(
                "line",
                line,
                options=list(
                    "none",
                    "linear",
                    "smooth"),
                default="none")
            private$..se <- jmvcore::OptionBool$new(
                "se",
                se,
                default=FALSE)

            self$.addOption(private$..xvar)
            self$.addOption(private$..yvar)
            self$.addOption(private$..covar)
            self$.addOption(private$..pearsonR)
            self$.addOption(private$..spearmanR)
            self$.addOption(private$..tauK)
            self$.addOption(private$..pearsonR2)
            self$.addOption(private$..spearmanR2)
            self$.addOption(private$..kendallR2)
            self$.addOption(private$..scat)
            self$.addOption(private$..marg)
            self$.addOption(private$..line)
            self$.addOption(private$..se)
        }),
    active = list(
        xvar = function() private$..xvar$value,
        yvar = function() private$..yvar$value,
        covar = function() private$..covar$value,
        pearsonR = function() private$..pearsonR$value,
        spearmanR = function() private$..spearmanR$value,
        tauK = function() private$..tauK$value,
        pearsonR2 = function() private$..pearsonR2$value,
        spearmanR2 = function() private$..spearmanR2$value,
        kendallR2 = function() private$..kendallR2$value,
        scat = function() private$..scat$value,
        marg = function() private$..marg$value,
        line = function() private$..line$value,
        se = function() private$..se$value),
    private = list(
        ..xvar = NA,
        ..yvar = NA,
        ..covar = NA,
        ..pearsonR = NA,
        ..spearmanR = NA,
        ..tauK = NA,
        ..pearsonR2 = NA,
        ..spearmanR2 = NA,
        ..kendallR2 = NA,
        ..scat = NA,
        ..marg = NA,
        ..line = NA,
        ..se = NA)
)

bivnumResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "bivnumResults",
    inherit = jmvcore::Group,
    active = list(
        text = function() private$.items[["text"]],
        asocind = function() private$.items[["asocind"]],
        scat = function() private$.items[["scat"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Bivariate Data Analysis")
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="text",
                title="Numerical Data"))
            self$add(jmvcore::Table$new(
                options=options,
                name="asocind",
                title="Association Indices",
                visible="( covar || pearsonR || spearmanR || tauK || pearsonR2 || spearmanR2 || kendallR2 )",
                columns=list(
                    list(
                        `name`="t[covar]", 
                        `title`="", 
                        `type`="text", 
                        `content`="Covariance", 
                        `visible`="(covar)"),
                    list(
                        `name`="v[covar]", 
                        `title`="Value", 
                        `visible`="(covar)"),
                    list(
                        `name`="t[pearsonR]", 
                        `title`="", 
                        `type`="text", 
                        `content`="Pearson's r", 
                        `visible`="(pearsonR)"),
                    list(
                        `name`="v[pearsonR]", 
                        `title`="Value", 
                        `visible`="(pearsonR)"),
                    list(
                        `name`="t[spearmanR]", 
                        `title`="", 
                        `type`="text", 
                        `content`="Spearman's r", 
                        `visible`="(spearmanR)"),
                    list(
                        `name`="v[spearmanR]", 
                        `title`="Value", 
                        `visible`="(spearmanR)"),
                    list(
                        `name`="t[tauK]", 
                        `title`="", 
                        `type`="text", 
                        `content`="Kendall's \u03C4-b", 
                        `visible`="(tauK)"),
                    list(
                        `name`="v[tauK]", 
                        `title`="Value", 
                        `visible`="(tauK)"),
                    list(
                        `name`="t[pearsonR2]", 
                        `title`="", 
                        `type`="text", 
                        `content`="Pearson's R\u00B2", 
                        `visible`="(pearsonR2)"),
                    list(
                        `name`="v[pearsonR2]", 
                        `title`="Value", 
                        `visible`="(pearsonR2)"),
                    list(
                        `name`="t[spearmanR2]", 
                        `title`="", 
                        `type`="text", 
                        `content`="Spearman's R\u00B2", 
                        `visible`="(spearmanR2)"),
                    list(
                        `name`="v[spearmanR2]", 
                        `title`="Value", 
                        `visible`="(spearmanR2)"),
                    list(
                        `name`="t[kendallR2]", 
                        `title`="", 
                        `type`="text", 
                        `content`="Kendall's R\u00B2", 
                        `visible`="(kendallR2)"),
                    list(
                        `name`="v[kendallR2]", 
                        `title`="Value", 
                        `visible`="(kendallR2)"))))
            self$add(jmvcore::Image$new(
                options=options,
                name="scat",
                title="Plots",
                width=450,
                height=400,
                renderFun=".scat",
                visible="(scat)",
                requiresData=TRUE,
                clearWith=list(
                    "xvar",
                    "yvar",
                    "marg",
                    "line",
                    "se")))}))

bivnumBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "bivnumBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "saRa",
                name = "bivnum",
                version = c(0,1,0),
                options = options,
                results = bivnumResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'auto')
        }))

#' Numerical Data
#'
#' 
#' @param data .
#' @param xvar x variable. It must be numeric
#' @param yvar y variable. It must be numeric
#' @param covar \code{TRUE} or \code{FALSE} (default), provide linear
#'   covariance for numeric variables.
#' @param pearsonR \code{TRUE} or \code{FALSE} (default), provide Pearson's
#'   correlation for numeric variables.
#' @param spearmanR \code{TRUE} or \code{FALSE} (default), provide Spearman's
#'   correlation for numeric variables.
#' @param tauK \code{TRUE} or \code{FALSE} (default), provide Kendall's tau-b
#'   association index for numeric variables.
#' @param pearsonR2 \code{TRUE} or \code{FALSE} (default), provide Pearson's
#'   coefficient of determination for numeric variables (rows dependent).
#' @param spearmanR2 \code{TRUE} or \code{FALSE} (default), provide Spearman's
#'   coefficient of determination for numeric variables (rows dependent).
#' @param kendallR2 \code{TRUE} or \code{FALSE} (default), provide Kendall's
#'   coefficient of determination for numeric variables (rows dependent).
#' @param scat \code{TRUE} or \code{FALSE} (default), show scatterplots
#' @param marg \code{none} (default), \code{dens}, 'hist', or \code{box},
#'   provide respectively no plots, density plots, histograms, or box plots on
#'   the axes
#' @param line \code{none} (default), \code{linear}, or \code{smooth}, provide
#'   respectively no fitted function,  a fitted regression line, or a smoothed
#'   curve
#' @param se \code{TRUE} or \code{FALSE} (default), show the standard error
#'   for the regression line
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$text} \tab \tab \tab \tab \tab a preformatted \cr
#'   \code{results$asocind} \tab \tab \tab \tab \tab A table of different bivariate association indicators \cr
#'   \code{results$scat} \tab \tab \tab \tab \tab an image \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$asocind$asDF}
#'
#' \code{as.data.frame(results$asocind)}
#'
#' @export
bivnum <- function(
    data,
    xvar,
    yvar,
    covar = FALSE,
    pearsonR = FALSE,
    spearmanR = FALSE,
    tauK = FALSE,
    pearsonR2 = FALSE,
    spearmanR2 = FALSE,
    kendallR2 = FALSE,
    scat = FALSE,
    marg = "none",
    line = "none",
    se = FALSE) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("bivnum requires jmvcore to be installed (restart may be required)")

    if ( ! missing(xvar)) xvar <- jmvcore::resolveQuo(jmvcore::enquo(xvar))
    if ( ! missing(yvar)) yvar <- jmvcore::resolveQuo(jmvcore::enquo(yvar))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(xvar), xvar, NULL),
            `if`( ! missing(yvar), yvar, NULL))


    options <- bivnumOptions$new(
        xvar = xvar,
        yvar = yvar,
        covar = covar,
        pearsonR = pearsonR,
        spearmanR = spearmanR,
        tauK = tauK,
        pearsonR2 = pearsonR2,
        spearmanR2 = spearmanR2,
        kendallR2 = kendallR2,
        scat = scat,
        marg = marg,
        line = line,
        se = se)

    analysis <- bivnumClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

