.numericQQCallback <- function(values) {
    context <- new.env(parent=asNamespace("saRa"))
    context$self <- list(data=data.frame("Test value"=values, check.names=FALSE))
    fun <- getFromNamespace("numSummaryClass", "saRa")$private_methods[[".qq"]]
    environment(fun) <- context
    fun(list(state="Test value"), ggplot2::theme_minimal(), NULL)
}

test_that("numeric Q-Q explains constant and insufficient samples", {
    for (values in list(rep(4,5), c(4,4,NA), c(4,NA), c(NA_real_,NA_real_))) {
        plot <- .numericQQCallback(values)
        expect_s3_class(plot, "ggplot")
        built <- ggplot2::ggplot_build(plot)
        expected <- if (sum(is.finite(values)) < 2L) "at least two finite" else "constant"
        expect_match(built$data[[1]]$label, expected)
        expect_identical(plot$labels$title, "Q-Q: Test value")
        expect_error(ggplot2::ggplotGrob(plot), NA)
    }
})

test_that("numeric Q-Q switches between explanation and plot as data change", {
    context <- new.env(parent=asNamespace("saRa"))
    context$self <- list(data=data.frame(x=rep(4,5)))
    fun <- getFromNamespace("numSummaryClass", "saRa")$private_methods[[".qq"]]
    environment(fun) <- context
    image <- list(state="x")
    for (values in list(rep(4,5), c(4,2,4,4,4), rep(4,5), 1:5)) {
        context$self$data$x <- values
        plot <- fun(image, ggplot2::theme_minimal(), NULL)
        expect_s3_class(plot, "ggplot")
        built <- ggplot2::ggplot_build(plot)
        if (stats::sd(values)==0) {
            expect_match(built$data[[1]]$label, "constant")
        } else {
            expect_length(plot$layers, 3)
            expect_equal(nrow(plot$data), 5)
            expect_equal(plot$data$sample, sort(as.vector(scale(values))))
            expect_null(built$data[[1]]$label)
        }
        expect_error(ggplot2::ggplotGrob(plot), NA)
    }
    expect_false(fun(list(state=NULL), ggplot2::theme_minimal(), NULL))
})
