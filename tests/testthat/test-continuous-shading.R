test_that("continuous probability regions use the shared jamovi highlight fill", {
    getTheme <- getFromNamespace("getTheme", "jmvcore")
    for (name in c("default", "iheartspss")) {
        theme <- getTheme(name, "jmv")
        ggtheme <- suppressWarnings(jmvcore::theme_default(scale="discrete"))
        o <- list(cdistros="norm", mu=0, sigma=1, contppvalue=0,
                  contx1value=0, contx2value=1, contpqvalue=.5, conttail="left")
        cases <- list(c("cdf","left"), c("survival","left"),
                      c("interval","left"), c("quantile","left"),
                      c("quantile","right"), c("quantile","central"))
        for (case in cases) {
            o$conttail <- case[2]
            plot <- suppressMessages(saRa:::.saraDistributionPlot(
                o, case[1], FALSE, ggtheme, theme))
            built <- ggplot2::ggplot_build(plot)
            area <- built$data[[1]]
            expect_gt(nrow(area), 2)
            expect_true(all(area$fill == jmvcore::colorPalette(2, theme$palette, "fill")[2]))
            expect_false(any(area$fill == theme$fill[1]))
            expect_true(all(area$alpha == .5))
            expect_gt(max(area$ymax-area$ymin), 0)
            expect_error(ggplot2::ggplotGrob(plot), NA)
            region <- plot$layers[[1]]$data
            if (case[1]=="cdf" || (case[1]=="quantile" && case[2]=="left"))
                expect_lte(max(region$x), 0)
            if (case[1]=="survival" || (case[1]=="quantile" && case[2]=="right"))
                expect_gte(min(region$x), 0)
            if (case[1]=="interval") {
                expect_equal(range(region$x), c(0,1))
                expect_match(plot$labels$title, "0.341", fixed=TRUE)
            }
            if (case[1]=="quantile" && case[2]=="central")
                expect_equal(range(region$x), stats::qnorm(c(.25,.75)))
        }
    }
})


test_that("discrete and continuous regions share palette colours after the global theme", {
    for (palette in c("jmv", "spss")) {
        theme <- getFromNamespace("getTheme", "jmvcore")("default", palette)
        ggtheme <- suppressWarnings(jmvcore::theme_default(scale="discrete", palette=palette))
        colours <- jmvcore::colorPalette(2, palette, "fill")
        discrete <- list(distros="binom",binomn=3,binomp=.5,ppvalue=1,
                         x1value=0,x2value=1,pqvalue=.5,tail="left")
        continuous <- list(cdistros="norm",mu=0,sigma=1,contppvalue=0,
                           contx1value=0,contx2value=1,contpqvalue=.5,conttail="left")
        for (kind in c("density","cdf","survival","interval","quantile")) {
            bars <- suppressMessages(saRa:::.saraDistributionPlot(
                discrete,kind,TRUE,ggtheme,theme))
            built <- ggplot2::ggplot_build(bars)$data[[1]]
            expect_true(all(built$fill[bars$data$selected] == colours[2]))
            expect_true(all(built$fill[!bars$data$selected] == colours[1]))
            area <- suppressMessages(saRa:::.saraDistributionPlot(
                continuous,kind,FALSE,ggtheme,theme))
            if (kind != "density")
                expect_true(all(ggplot2::ggplot_build(area)$data[[1]]$fill == colours[2]))
            expect_error(ggplot2::ggplotGrob(bars), NA)
        }
    }
})
