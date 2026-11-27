test_that("uniform tables evaluate points outside the support", {
    result <- saRa::contvars(cdistros="unif", unifmin=2, unifmax=6,
        contvaluesfunc="1,2,4,6,7", contpdf=TRUE, contcdf=TRUE, contsurv=TRUE,
        contqvalues="0.5", conticdf=TRUE)
    table <- result$Contprobstab$asDF
    expect_equal(table$contpdf, c(0,.25,.25,.25,0))
    expect_equal(table$contcdf, c(0,0,.5,1,1))
    expect_equal(table$contsurv, c(1,1,.5,0,0))
    expect_equal(result$Contquantstab$asDF$contlTail, 4)
    expect_equal(result$Contquantstab$asDF$contrTail, 4)
})

test_that("uniform point and interval validators accept outside-support values", {
    context <- new.env(parent=asNamespace("saRa"))
    context$self <- list(options=list(cdistros="unif",unifmin=2,unifmax=6,
        contppvalue=1,contx1value=1,contx2value=7))
    methods <- getFromNamespace("contvarsClass", "saRa")$private_methods
    for (item in list(c(".getcontppvalue",1), c(".getcontX1value",1),
                      c(".getcontX2value",7))) {
        fun <- methods[[item[1]]]
        environment(fun) <- context
        expect_equal(fun(), as.numeric(item[2]))
    }
})

test_that("uniform shaded regions clip to the support without changing probabilities", {
    theme <- getFromNamespace("getTheme", "jmvcore")("default","jmv")
    o <- list(cdistros="unif",unifmin=2,unifmax=6,contppvalue=1,
              contx1value=1,contx2value=7,contpqvalue=.5,conttail="left")
    for (point in c(1,7)) {
        o$contppvalue <- point
        for (kind in c("density","cdf","survival","interval")) {
            plot <- suppressMessages(saRa:::.saraDistributionPlot(
                o,kind,FALSE,ggplot2::theme_minimal(),theme))
            expect_warning(expect_error(ggplot2::ggplotGrob(plot), NA), NA)
            expect_true(all(plot$data$y[plot$data$x < 2 | plot$data$x > 6] == 0))
        }
    }
    expect_error(saRa::contvars(cdistros="unif",unifmin=6,unifmax=2),
                 "minimum must be smaller")
    expect_error(saRa::contvars(cdistros="unif",unifmin=2,unifmax=2),
                 "minimum must be smaller")
})


test_that("continuous density marker is a single segment", {
    theme <- getFromNamespace("getTheme", "jmvcore")("default","jmv")
    for (point in c(1,4,7)) {
        o <- list(cdistros="unif",unifmin=2,unifmax=6,contppvalue=point,
                  contx1value=3,contx2value=5,contpqvalue=.5,conttail="left")
        plot <- suppressMessages(saRa:::.saraDistributionPlot(
            o,"density",FALSE,ggplot2::theme_minimal(),theme))
        built <- NULL
        expect_warning(built <- ggplot2::ggplot_build(plot), NA)
        marker <- built$data[[3]]
        expect_equal(nrow(marker), 1L)
        expect_equal(marker$x, point)
        expect_equal(marker$xend, point)
        expect_equal(marker$y, 0)
        expect_equal(marker$yend, stats::dunif(point,2,6))
    }
})
