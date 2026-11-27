test_that("shared messages use the owning analysis and retain unknown details", {
    seen <- character()
    owner <- list(options=list(translate=function(text, n=1) {
        seen <<- c(seen, text)
        paste0("translated: ", text)
    }))
    keys <- c("Counts", "Missing", "Theoretical quantiles",
              "Q-Q plot unavailable.", "Lorenz curve unavailable.",
              "Cumulative proportion of observations",
              "Median (confidence interval unavailable)",
              "One or both groups do not contain enough observations")
    for (key in keys)
        expect_identical(saRa:::.saraMessage(key, owner), paste0("translated: ", key))
    expect_identical(seen, keys)
    expect_identical(saRa:::.saraMessage("Counts"), "Counts")
    expect_identical(saRa:::.saraMessage("External error detail", owner), "External error detail")
})

test_that("translated frequency output preserves data labels, keys and values", {
    owner <- list(options=list(translate=function(text, n=1) paste0("translated: ", text)))
    translate <- function(message) saRa:::.saraMessage(message, owner)
    columns <- list(); rows <- list(); notes <- list()
    table <- list(
        addColumn=function(name, title, type, format) {
            columns[[name]] <<- list(title=title, type=type, format=format)
        },
        deleteRows=function() { rows <<- list() },
        addRow=function(rowKey, values) NULL,
        setRow=function(rowNo, values) { rows[[rowNo]] <<- values },
        setNote=function(key, value) { notes[[key]] <<- value }
    )
    tables <- list(get=function(key) table)
    data <- data.frame(x=ordered(c("Missing", "other", "Missing", NA),
                                levels=c("Missing", "other")))
    saRa:::.saraInitFrequencyTables(tables, data[FALSE,,drop=FALSE], "x",
                                   cumulative=TRUE, translate=translate)
    saRa:::.saraPopulateFrequencyTables(tables, data, "x",
                                       cumulative=TRUE, translate=translate)
    expect_identical(names(columns), c("levels", "counts", "cumcounts", "pc", "cumpc"))
    expect_identical(columns$counts$title, "translated: Counts")
    expect_identical(columns$pc$format, "pc")
    expect_identical(columns$cumpc$format, "pc")
    expect_identical(vapply(rows, `[[`, character(1), "levels"),
                     c("Missing", "other", "translated: Missing"))
    expect_equal(vapply(rows, `[[`, numeric(1), "counts"), c(2, 1, 1))
    expect_equal(rows[[1]]$pc, 2/3)
    expect_match(notes$valid, "^translated: Percentages")
})

test_that("summary headings translate complete templates and keep column keys", {
    dictionary <- c("{}% trimmed mean"="trimmed mean: {}%",
                    "{}% winsorized mean"="winsorized mean: {}%",
                    "{}% inner range"="inner range: {}%")
    context <- new.env(parent=asNamespace("saRa"))
    context$self <- list(options=list(freq=FALSE, Trim=5, Win=10, NPerc=90,
        translate=function(text, n=1) {
            if (text %in% names(dictionary)) unname(dictionary[[text]]) else text
        }))
    context$private <- new.env(parent=emptyenv())
    for (name in c(".addQuantiles", ".initIndicesTableNum", ".initPlots"))
        context$private[[name]] <- function() NULL
    init <- saRa:::numSummaryClass$private_methods$.init
    environment(init) <- context
    init()
    columns <- context$private$colArgs
    title <- function(name) columns$title[match(name, columns$name)]
    expect_identical(title("trimmean"), "trimmed mean: 5%")
    expect_identical(title("winsmean"), "winsorized mean: 10%")
    expect_identical(title("ir"), "inner range: 90%")
    expect_identical(columns$name[1:5], c("n", "missing", "mean", "median", "mode"))
})

test_that("Q-Q axes and normality messages translate without changing plot data", {
    owner <- list(options=list(translate=function(text, n=1) paste0("translated: ", text)))
    translate <- function(message) saRa:::.saraMessage(message, owner)
    data <- data.frame(x=c(1,2,3,5,8))
    plain <- saRa:::.saraQQPlot(data, "x", ggtheme=ggplot2::theme_minimal())
    marked <- saRa:::.saraQQPlot(data, "x", ggtheme=ggplot2::theme_minimal(), translate=translate)
    expect_equal(marked$data, plain$data)
    expect_identical(marked$labels$x, "translated: Theoretical quantiles")
    expect_identical(marked$labels$y, "translated: Standardized values")
    expect_error(ggplot2::ggplot_build(marked), NA)
    invalid <- saRa:::.saraNormalityPlot(data.frame(x=rep(2,4)), "x",
        kind="qq", ggtheme=ggplot2::theme_minimal(), translate=translate)
    note <- invalid$layers[[1]]$data$label
    expect_match(note, "translated: Q-Q plot unavailable.", fixed=TRUE)
    expect_match(note, "translated: The variable is constant", fixed=TRUE)
})

test_that("complete finite-window captions allow reordered translated values", {
    seen <- character()
    translate <- function(message) {
        seen <<- c(seen, message)
        "Outside the window: {}%. Full distribution used."
    }
    distribution <- saRa:::.saraDistribution(list(cdistros="norm", mu=0, sigma=1))
    caption <- saRa:::.saraWindowCaption(distribution, c(-1,1), FALSE, translate)
    expect_length(seen, 1L)
    expect_identical(seen, paste0("The graph shows a finite window; probability outside ",
        "the displayed range = {}%. Calculations use the full distribution."))
    expect_match(caption, "Outside the window: 31.7%", fixed=TRUE)
})

test_that("distribution and fence guards use translated messages with stable defaults", {
    translate <- function(message) paste0("translated: ", message)
    expect_error(saRa:::.saraDiscreteUpper(
        list(distros="poiss", lambda=-1), translate=translate),
        "translated: Poisson rate must be non-negative", fixed=TRUE)
    expect_error(saRa:::.saraDistribution(
        list(cdistros="exp", rate=-1), translate=translate),
        "translated: Distribution parameters must be finite", fixed=TRUE)
    expect_error(saRa:::.saraValidateFences(3, 1, translate=translate),
        "translated: Fence multipliers", fixed=TRUE)
    expect_error(saRa:::.saraValidateFences(3, 1),
        "Fence multipliers must be non-negative", fixed=TRUE)
    expect_equal(saRa:::.saraDiscreteUpper(list(distros="poiss", lambda=1),
        translate=translate), saRa:::.saraDiscreteUpper(list(distros="poiss", lambda=1)))
})

test_that("rank-result errors translate after normalization without changing NaN cells", {
    values <- NULL; note <- NULL
    table <- list(
        setRow=function(rowKey, value) { values <<- value },
        addFootnote=function(rowKey, column, message) { note <<- message }
    )
    result <- try(stop("not enough (non-missing) 'x' observations"), silent=TRUE)
    ok <- saRa:::.saraWriteRankResult(table, "x", "zstat", result, withP=TRUE,
        translate=function(message) paste0("translated: ", message))
    expect_false(ok)
    expect_identical(names(values), c("stat[zstat]", "p[zstat]"))
    expect_true(all(is.nan(unlist(values))))
    expect_identical(note, "translated: One or both variables do not contain enough observations")
})
