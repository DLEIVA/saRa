test_that("ordinal frequency tables follow defined levels and valid denominators", {
    x <- ordered(c("high", "low", "high", NA),
                 levels=c("low", "middle", "high"))
    values <- saRa:::.saraFrequencyRows(x, cumulative=TRUE)
    expect_identical(values$levels, c("low", "middle", "high", "Missing"))
    expect_equal(values$counts, c(1, 0, 2, 1))
    expect_equal(values$cumcounts, c(1, 1, 3, NA))
    expect_equal(values$pc, c(1/3, 0, 2/3, NA))
    expect_equal(values$cumpc, c(1/3, 1/3, 1, NA))
    expect_false(any(c("prop", "cumprop") %in% names(values)))
    expect_false("odds" %in% names(values))
})

test_that("ordinal stored scores remain categories rather than intervals", {
    x <- ordered(c("none", "many", "some", "many", NA),
                 levels=c("none", "some", "many"))
    attr(x, "values") <- c(0, 2, 9)
    values <- saRa:::.saraFrequencyRows(x, cumulative=TRUE)
    expect_equal(values$counts, c(1, 1, 2, 1))
    expect_equal(values$cumcounts, c(1, 2, 4, NA))
    expect_equal(values$cumpc, c(.25, .5, 1, NA))
    expect_identical(values$levels[1:3], levels(x))
    labels <- ordered(c("9", "0", "9"), levels=c("0", "2", "9"))
    expect_equal(saRa:::.saraFrequencyRows(labels, TRUE)$counts, c(1, 0, 2, 0))
})

test_that("nominal frequencies retain odds and omit all cumulative columns", {
    x <- factor(c("b", "a", "b", NA), levels=c("b", "a", "unused"))
    values <- saRa:::.saraFrequencyRows(x, odds=TRUE)
    expect_identical(names(values), c("levels", "counts", "pc", "odds"))
    expect_identical(values$levels, c("b", "a", "unused", "Missing"))
    expect_equal(values$counts, c(2, 1, 0, 1))
    expect_equal(values$odds, c(2, .5, 0, NA))
    one <- saRa:::.saraFrequencyRows(factor(c("a", "a")), odds=TRUE)
    expect_identical(one$odds, c(Inf, NA_real_))
})

test_that("empty and missing-only tables keep counts without undefined cumulative ratios", {
    for (x in list(ordered(character(), levels=c("low", "high")),
                   ordered(c(NA, NA), levels=c("low", "high")))) {
        values <- saRa:::.saraFrequencyRows(x, cumulative=TRUE)
        expect_equal(values$counts[1:2], c(0, 0))
        expect_equal(values$cumcounts, c(0, 0, NA))
        expect_true(all(is.na(values$pc)))
        expect_true(all(is.na(values$cumpc)))
        expect_equal(tail(values$counts, 1), length(x))
    }
    empty <- saRa:::.saraFrequencyRows(ordered(character()), TRUE)
    expect_equal(nrow(empty), 1L)
    expect_identical(empty$levels, "Missing")
    expect_equal(empty$counts, 0)
})

test_that("both analyses populate real jamovi frequency tables", {
    data <- data.frame("Ordered score"=ordered(c("high", "low", "high", NA),
                       levels=c("low", "middle", "high")),
                       "Nominal score"=factor(c("b", "a", "b", NA)),
                       check.names=FALSE)
    result <- saRa::ordSummary(data, vars="Ordered score", freq=TRUE)
    table <- result$frequencies$get("Ordered score")
    values <- table$asDF
    expect_equal(values$counts, c(1, 0, 2, 1))
    expect_equal(values$cumcounts, c(1, 1, 3, NA))
    expect_equal(values$cumpc, c(1/3, 1/3, 1, NA))
    expect_false("odds" %in% names(values))
    expect_true(result$frequencies$visible)
    off <- saRa::ordSummary(data, vars="Ordered score", freq=FALSE)
    expect_false(off$frequencies$visible)
    nominal <- saRa::catSummary(data, vars="Nominal score", freq=TRUE)
    values <- nominal$frequencies$get("Nominal score")$asDF
    expect_true("odds" %in% names(values))
    expect_false(any(c("cumcounts", "cumprop", "cumpc") %in% names(values)))
    # The original statistics continue to be available with frequencies disabled.
    expect_equal(nrow(off$indicesord$asDF), 1L)
})

test_that("frequency tables distinguish a Missing label from missing observations", {
    data <- data.frame(x=ordered(c("Missing", "other", NA),
                                levels=c("Missing", "other")))
    result <- saRa::ordSummary(data, vars="x", freq=TRUE)
    values <- result$frequencies$get("x")$asDF
    expect_identical(values$levels, c("Missing", "other", "Missing"))
    expect_equal(values$counts, c(1, 1, 1))
    expect_equal(values$cumpc, c(.5, 1, NA))
})

test_that("one ordinal frequency table is created per selected variable", {
    data <- data.frame(first=ordered(c("a", "b", "b")),
                       second=ordered(c("low", "high", NA), levels=c("low", "high")))
    result <- saRa::ordSummary(data, vars=c("first", "second"), freq=TRUE)
    expect_equal(result$frequencies$get("first")$asDF$counts, c(1, 2, 0))
    expect_equal(result$frequencies$get("second")$asDF$counts, c(1, 1, 1))
})

test_that("all univariate frequency outputs omit redundant proportion columns", {
    cases <- list(
        list(run=saRa::catSummary, x=factor(c("a", "b", "a", NA)),
             columns=c("levels", "counts", "pc", "odds")),
        list(run=saRa::ordSummary, x=ordered(c("a", "b", "a", NA)),
             columns=c("levels", "counts", "cumcounts", "pc", "cumpc")),
        list(run=saRa::numSummary, x=c(1, 2, 1, NA),
             columns=c("levels", "counts", "cumcounts", "pc", "cumpc")))
    for (case in cases) {
        result <- do.call(case$run, list(data=data.frame(x=case$x), vars="x", freq=TRUE))
        values <- result$frequencies$get("x")$asDF
        expect_identical(names(values), case$columns)
        expect_equal(values$counts, c(2, 1, 1))
        # jmvcore stores percentage fractions and renders them with format="pc".
        expect_equal(values$pc, c(2/3, 1/3, NA))
        if ("cumpc" %in% names(values))
            expect_equal(values$cumpc, c(2/3, 1, NA))
    }
})
