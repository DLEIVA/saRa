test_that("numeric frequencies sort observed values numerically and exclude missing denominators", {
    values <- saRa:::.saraFrequencyRows(c(10, -2, 2, 10, 0, NA),
                                      cumulative=TRUE, numericValues=TRUE)
    expect_identical(values$levels, c("-2", "0", "2", "10", "Missing"))
    expect_equal(values$counts, c(1, 1, 1, 2, 1))
    expect_equal(values$cumcounts, c(1, 2, 3, 5, NA))
    expect_equal(values$prop, c(.2, .2, .2, .4, NA))
    expect_equal(values$cumprop, c(.2, .4, .6, 1, NA))
    expect_equal(values$pc, values$prop)
    expect_equal(values$cumpc, values$cumprop)
    expect_false("odds" %in% names(values))
})

test_that("numeric frequencies use exact values without bins or label merging", {
    x <- c(.2, .1, .2, -.5)
    values <- saRa:::.saraFrequencyRows(x, TRUE, numericValues=TRUE)
    expect_identical(values$levels, c("-0.5", "0.1", "0.2", "Missing"))
    expect_equal(values$counts, c(1, 1, 2, 0))
    close <- c(1, 1 + 1e-15)
    values <- saRa:::.saraFrequencyRows(close, TRUE, numericValues=TRUE)
    expect_equal(values$counts, c(1, 1, 0))
    expect_equal(length(unique(values$levels[1:2])), 2L)
    expect_identical(as.numeric(values$levels[1:2]), close)
})

test_that("empty, missing-only and constant numerical frequencies remain defined where possible", {
    for (x in list(numeric(), c(NA_real_, NA_real_))) {
        values <- saRa:::.saraFrequencyRows(x, TRUE, numericValues=TRUE)
        expect_identical(values$levels, "Missing")
        expect_equal(values$counts, length(x))
        expect_true(is.na(values$cumcounts))
        expect_true(is.na(values$prop))
        expect_true(is.na(values$cumprop))
    }
    values <- saRa:::.saraFrequencyRows(c(3, 3, NA), TRUE, numericValues=TRUE)
    expect_equal(values$counts, c(2, 1))
    expect_equal(values$cumprop, c(1, NA))
    expect_false("odds" %in% names(values))
})

test_that("Numerical Data builds one cumulative table per variable and supports the checkbox", {
    data <- data.frame("Numeric score"=c(10, 2, 10, NA),
                       other=c(-1, 0, -1, 2), check.names=FALSE)
    result <- saRa::numSummary(data, vars=c("Numeric score", "other"), freq=TRUE)
    values <- result$frequencies$get("Numeric score")$asDF
    expect_identical(values$levels, c("2", "10", "Missing"))
    expect_equal(values$counts, c(1, 2, 1))
    expect_equal(values$cumprop, c(1/3, 1, NA))
    expect_false("odds" %in% names(values))
    expect_equal(result$frequencies$get("other")$asDF$counts, c(2, 1, 1, 0))
    expect_true(result$frequencies$visible)
    off <- saRa::numSummary(data, vars=c("Numeric score", "other"), freq=FALSE)
    expect_false(off$frequencies$visible)
    expect_equal(result$indicesnum$asDF, off$indicesnum$asDF)
    default <- saRa::numSummary(data, vars="other")
    expect_true(default$frequencies$visible)
})

test_that("repopulating numeric frequency tables removes old rows", {
    result <- saRa::numSummary(data.frame(x=c(1, 2, 3, 4)), vars="x", freq=TRUE)
    saRa:::.saraPopulateFrequencyTables(result$frequencies, data.frame(x=c(8, 8, NA)),
                                        "x", cumulative=TRUE, numericValues=TRUE)
    values <- result$frequencies$get("x")$asDF
    expect_identical(values$levels, c("8", "Missing"))
    expect_equal(values$counts, c(2, 1))
    expect_equal(values$cumcounts, c(2, NA))
    expect_equal(values$cumprop, c(1, NA))
})
