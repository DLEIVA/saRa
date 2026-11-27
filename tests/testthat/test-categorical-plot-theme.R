.categoricalThemeFixture <- function() {
    context <- new.env(parent=asNamespace("saRa"))
    states <- new.env(parent=emptyenv())
    group <- list(get=function(name) {
        force(name)
        list(setState=function(state) assign(name, state, envir=states))
    })
    context$self <- list(options=list(vars="Category", pie=TRUE, dough=TRUE,
        lolli=TRUE, bar=TRUE, pareto=TRUE, f2p.lolli="absolutefreq", f2p.bar="absolutefreq"),
        data=data.frame(Category=factor(rep(letters[1:12], 1:12), levels=rev(letters[1:12]))),
        results=list(plots=list(get=function(name) group)))
    prepare <- saRa:::catSummaryClass$private_methods$.preparePlots
    environment(prepare) <- context
    prepare()
    context$states <- states
    context
}

.categoricalThemePlot <- function(context, method, state, ggtheme, theme) {
    fun <- saRa:::catSummaryClass$private_methods[[method]]
    environment(fun) <- context
    suppressMessages(fun(list(state=get(state, envir=context$states)), ggtheme, theme))
}

test_that("categorical plots retain supplied theme typography and backgrounds", {
    context <- .categoricalThemeFixture()
    theme <- list(fill=c("#FFF6D5", "#007A5E"), color=c("#112233", "#990044"))
    ggtheme <- ggplot2::theme_minimal(base_size=19, base_family="serif") +
        ggplot2::theme(legend.text=ggplot2::element_text(size=17),
            plot.title=ggplot2::element_text(size=25),
            axis.text=ggplot2::element_text(size=13),
            panel.background=ggplot2::element_rect(fill="#F8E8DD", colour=NA))
    methods <- c(pie=".piePlot", dough=".doughnutPlot", pareto=".paretoPlot",
                 lolli=".lolliPlot", bar=".barPlot")
    for (name in names(methods)) {
        plot <- .categoricalThemePlot(context, methods[[name]], name, ggtheme, theme)
        expect_identical(plot$data, get(name, envir=context$states)$data)
        expect_equal(ggplot2::calc_element("text", plot$theme)$size, 19)
        expect_identical(ggplot2::calc_element("text", plot$theme)$family, "serif")
        expect_equal(ggplot2::calc_element("legend.text", plot$theme)$size, 17)
        expect_equal(ggplot2::calc_element("plot.title", plot$theme)$size, 25)
        expect_identical(ggplot2::calc_element("panel.background", plot$theme)$fill, "#F8E8DD")
        if (name %in% c("pareto", "bar", "lolli"))
            expect_equal(ggplot2::calc_element("axis.text.x", plot$theme)$size, 13)
        expect_error(ggplot2::ggplotGrob(plot), NA)
    }
})

test_that("pie and doughnut palettes work beyond nine levels with real jamovi scales", {
    context <- .categoricalThemeFixture()
    # Older jmvcore versions warn in their own discrete_scale constructor.
    ggtheme <- suppressWarnings(jmvcore::theme_default(base_size=18, scale="discrete"))
    theme <- list(fill=c("#FFF6D5", "#007A5E"), color=c("#112233", "#990044"))
    for (spec in list(c(".piePlot", "pie"), c(".doughnutPlot", "dough"))) {
        plot <- .categoricalThemePlot(context, spec[1], spec[2], ggtheme, theme)
        built <- ggplot2::ggplot_build(plot)
        expected <- jmvcore::colorPalette(12, pal="jmv", type="fill")
        expect_equal(length(unique(built$data[[1]]$fill)), 12L)
        expect_setequal(built$data[[1]]$fill, expected)
        expect_false(anyNA(built$data[[1]]$fill))
        if (spec[2] == "pie") {
            expect_true(all(built$data[[2]]$colour == theme$color[1]))
            expect_equal(unique(built$data[[2]]$size), 18/ggplot2::.pt)
        } else {
            scale <- built$plot$scales$get_scales("fill")
            expect_equal(length(scale$get_labels()), 12L)
            expect_identical(as.character(scale$get_labels()), as.character(plot$data$x))
            expect_identical(built$data[[2]]$label, paste0(plot$data$pct, "%"))
            expect_equal(built$data[[2]]$x, rep(3.25, 12))
            expect_true(all(built$data[[2]]$colour == theme$color[1]))
        }
        expect_error(ggplot2::ggplotGrob(plot), NA)
    }
})

test_that("Pareto cumulative layers use the selected palette without changing counts", {
    context <- .categoricalThemeFixture()
    theme <- list(fill=c("#FFF6D5", "#007A5E"), color=c("#112233", "#990044"))
    plot <- .categoricalThemePlot(context, ".paretoPlot", "pareto",
        ggplot2::theme_minimal(), theme)
    built <- ggplot2::ggplot_build(plot)
    expect_equal(sum(plot$data$y), sum(1:12))
    expect_equal(tail(plot$data$cumulative, 1), sum(1:12))
    expect_true(all(built$data[[1]]$fill == theme$fill[2]))
    expect_true(all(built$data[[2]]$colour == theme$color[2]))
    expect_true(all(built$data[[3]]$colour == theme$color[2]))
    expect_identical(plot$labels$y, "Counts")
})

test_that("namespaced calls in the corrected Pareto and Wilcoxon paths are public exports", {
    # Parsing alone accepts misspelled exported names such as ylab.(). Check the
    # callable API as well, including the branch not exercised by numeric pairs.
    check <- function(expression) {
        if (rlang::is_missing(expression)) return(invisible(NULL))
        if (is.call(expression) && identical(expression[[1]], as.name("::"))) {
            package <- as.character(expression[[2]])
            member <- as.character(expression[[3]])
            expect_true(member %in% getNamespaceExports(package),
                info=paste(package, member, sep="::"))
        }
        if (is.call(expression) || is.expression(expression) || is.pairlist(expression))
            for (part in as.list(expression)) check(part)
    }
    check(body(saRa:::catSummaryClass$private_methods$.paretoPlot))
    check(body(saRa:::wilcoxTClass$private_methods$.run))
})


test_that("polar charts hide explicit theme axes and keep doughnut labels in the legend", {
    context <- .categoricalThemeFixture()
    theme <- list(fill=c("#FFF6D5", "#007A5E"), color=c("#112233", "#990044"))
    ggtheme <- ggplot2::theme_classic() + ggplot2::theme(
        axis.title.x=ggplot2::element_text(), axis.title.y=ggplot2::element_text(),
        axis.text.x=ggplot2::element_text(), axis.text.y=ggplot2::element_text(),
        axis.ticks.x=ggplot2::element_line(), axis.ticks.y=ggplot2::element_line(),
        axis.line.x=ggplot2::element_line(), axis.line.y=ggplot2::element_line())
    for (spec in list(c(".piePlot", "pie"), c(".doughnutPlot", "dough"))) {
        plot <- .categoricalThemePlot(context, spec[1], spec[2], ggtheme, theme)
        for (element in c("axis.title.x", "axis.title.y", "axis.text.x", "axis.text.y",
                          "axis.ticks.x", "axis.ticks.y", "axis.line.x", "axis.line.y"))
            expect_s3_class(ggplot2::calc_element(element, plot$theme), "element_blank")
        expect_null(plot$labels$x)
        expect_null(plot$labels$y)
        expect_error(ggplot2::ggplotGrob(plot), NA)
        if (spec[2] == "dough") {
            expect_identical(plot$theme$legend.position, "right")
            expect_length(plot$layers, 2L)
        }
    }
})


test_that("polar charts use the jmv categorical palette regardless of other fill anchors", {
    context <- .categoricalThemeFixture()
    expected <- jmvcore::colorPalette(12, pal="jmv", type="fill")
    for (fill in list(c("white", "#A6C4F0"), c("#112233", "#CC0055"))) {
        theme <- list(fill=fill, color=c("#112233", "#990044"))
        for (spec in list(c(".piePlot", "pie"), c(".doughnutPlot", "dough"))) {
            plot <- .categoricalThemePlot(context, spec[1], spec[2],
                ggplot2::theme_minimal(), theme)
            scale <- ggplot2::ggplot_build(plot)$plot$scales$get_scales("fill")
            expect_identical(as.character(scale$map(levels(plot$data$x))), as.character(expected))
            expect_equal(length(unique(scale$map(levels(plot$data$x)))), 12L)
            expect_false(anyNA(scale$map(levels(plot$data$x))))
            expect_error(ggplot2::ggplotGrob(plot), NA)
        }
    }
})
