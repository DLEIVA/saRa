.ordinalPlotFixture <- function() {
    data.frame("Row group"=ordered(c(rep("R1", 8), rep("R2", 8), NA),
                                   levels=c("R2", "R1")),
               "Column group"=ordered(c(rep("C1", 6), rep("C2", 2),
                                        rep("C1", 2), rep("C2", 6), "C1"),
                                      levels=c("C1", "C2")), check.names=FALSE)
}

.ordinalPlotMethods <- function() {
    context <- new.env(parent=asNamespace("saRa"))
    context$self <- list(options=list(rows="Row group", cols="Column group",
        barplot=TRUE, heatmap=TRUE, alluvial=TRUE, xaxis="xrows",
        yaxis="ypc", yaxisPc="total_pc", bartype="dodge"),
        data=.ordinalPlotFixture())
    context$private <- new.env(parent=emptyenv())
    cls <- getFromNamespace("bivordClass", "saRa")
    for (name in c(".barPlotObject", ".heatmapObject", ".alluvialObject",
                   ".barPlot", ".heatmap", ".alluvial")) {
        fun <- cls$private_methods[[name]]
        environment(fun) <- context
        context$private[[name]] <- fun
    }
    context
}

.ordinalJamoviTheme <- function() {
    # Older jmvcore can warn about its own legacy discrete_scale constructor.
    # Keep the actual scale-bearing theme; suppress only fixture-construction warnings.
    suppressWarnings(jmvcore::theme_default(scale="discrete"))
}

test_that("ordinal continuous-fill plots override the real jamovi discrete palette", {
    context <- .ordinalPlotMethods()
    ggtheme <- .ordinalJamoviTheme()
    theme <- list(fill=c("#FFFFFF", "#4477AA"), color=c("black", "#4477AA"))
    for (axis in c("xrows", "xcols")) {
        context$self$options$xaxis <- axis
        for (name in c(".heatmapObject", ".alluvialObject")) {
            plot <- suppressMessages(context$private[[name]](NULL, ggtheme, theme))
            expect_false(plot$scales$get_scales("fill")$is_discrete())
            expect_equal(sum(plot$data$Freq), 16)
            expect_equal(sort(plot$data$Freq), c(2L, 2L, 6L, 6L))
            expect_identical(levels(plot$data[["Row group"]]), c("R2", "R1"))
            expect_identical(levels(plot$data[["Column group"]]), c("C1", "C2"))
            expect_error(ggplot2::ggplot_build(plot), NA)
            # Building the full grob also trains scales and lays out legends/text.
            expect_error(ggplot2::ggplotGrob(plot), NA)
        }
    }
})

test_that("ordinal render callbacks draw independently of checkbox activation order", {
    context <- .ordinalPlotMethods()
    ggtheme <- .ordinalJamoviTheme()
    theme <- list(fill=c("#FFFFFF", "#4477AA"), color=c("black", "#4477AA"))
    sequences <- list("heatmap", "alluvial", c("heatmap", "alluvial"),
        "barplot", c("barplot", "heatmap"), c("barplot", "alluvial"),
        c("barplot", "heatmap", "alluvial"), character())
    callbacks <- c(barplot=".barPlot", heatmap=".heatmap", alluvial=".alluvial")
    path <- tempfile(fileext=".pdf")
    grDevices::pdf(path)
    on.exit({ grDevices::dev.off(); unlink(path) }, add=TRUE)
    for (selected in sequences) {
        for (name in names(callbacks))
            context$self$options[[name]] <- name %in% selected
        for (name in names(callbacks)) {
            drawn <- suppressMessages(context$private[[callbacks[[name]]]](NULL, ggtheme, theme))
            expect_identical(drawn, name %in% selected,
                             info=paste("Selected:", paste(selected, collapse=", "), "callback:", name))
        }
    }
})

test_that("ordinal plot selections preserve contingency tables and association indices", {
    data <- .ordinalPlotFixture()
    selections <- list(c(FALSE, TRUE, FALSE), c(FALSE, FALSE, TRUE),
                       c(TRUE, FALSE, FALSE), c(TRUE, TRUE, TRUE))
    reference <- saRa::bivord(data, rows="Row group", cols="Column group")
    for (selected in selections) {
        result <- saRa::bivord(data, rows="Row group", cols="Column group",
            barplot=selected[1], heatmap=selected[2], alluvial=selected[3])
        expect_equal(result$freqs$asDF, reference$freqs$asDF)
        expect_equal(result$asocind$asDF, reference$asocind$asDF)
        expect_equal(result$desc$asDF, reference$desc$asDF)
        expect_identical(result$barplot$visible, selected[1])
        expect_identical(result$heatmap$visible, selected[2])
        expect_identical(result$alluvial$visible, selected[3])
    }
})
