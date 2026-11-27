test_that("uniform interval plots have vertical jumps and exact shaded areas", {
    theme <- getFromNamespace("getTheme", "jmvcore")("default", "jmv")
    o <- list(cdistros="unif", unifmin=2, unifmax=6, contppvalue=4,
              contx1value=3, contx2value=5, contpqvalue=.5, conttail="left")
    cases <- list(c(3,5,.5), c(1,7,1), c(7,8,0), c(0,1,0),
                  c(1,3,.25), c(5,7,.25), c(4,4,0))
    for (case in cases) {
        o$contx1value <- case[1]; o$contx2value <- case[2]
        plot <- suppressMessages(saRa:::.saraDistributionPlot(
            o, "interval", FALSE, ggplot2::theme_minimal(), theme))
        built <- ggplot2::ggplot_build(plot)
        region <- built$data[[1]]
        curve <- built$data[[2]]
        expect_true(all(diff(curve$x)==0 | diff(curve$y)==0))
        expect_true(all(curve$y[curve$x < 2 | curve$x > 6]==0))
        expect_equal(sum((region$xmax-region$xmin)*(region$ymax-region$ymin)),
                     case[3])
        if (case[3] > 0) {
            expect_gte(min(region$xmin), 2)
            expect_lte(max(region$xmax), 6)
        } else expect_equal(nrow(region), 0L)
        expect_warning(expect_error(ggplot2::ggplotGrob(plot), NA), NA)
    }
})

test_that("uniform tails and quantiles shade only inside the support", {
    theme <- getFromNamespace("getTheme", "jmvcore")("default", "jmv")
    o <- list(cdistros="unif", unifmin=2, unifmax=6, contppvalue=1,
              contx1value=1, contx2value=7, contpqvalue=.5, conttail="central")
    for (kind in c("cdf", "survival", "quantile")) {
        for (point in c(1,4,7)) {
            o$contppvalue <- point
            plot <- suppressMessages(saRa:::.saraDistributionPlot(
                o, kind, FALSE, ggplot2::theme_minimal(), theme))
            region <- ggplot2::ggplot_build(plot)$data[[1]]
            actual <- sum((region$xmax-region$xmin)*(region$ymax-region$ymin))
            expected <- switch(kind, cdf=stats::punif(point,2,6),
                survival=stats::punif(point,2,6,lower.tail=FALSE), quantile=.5)
            expect_equal(actual, expected)
        }
    }
})
