test_that("omitted ordinal Range receives neither values nor cell footnotes", {
    data <- data.frame(text=ordered(c("low", "middle", "high"),
                                    levels=c("low", "middle", "high")))
    for (freq in c(TRUE, FALSE)) {
        result <- saRa::ordSummary(data, vars="text", range=TRUE, freq=freq)
        columns <- vapply(result$indicesord$columns, function(column) column$name, character(1))
        expect_false(any(grepl("[range]", columns, fixed=TRUE)))
        expect_false(any(grepl("text categories", result$indicesord$footnotes, fixed=TRUE)))
        expect_true("text[median]" %in% columns)
    }
})

test_that("mixed ordinal selections retain Range and its text-cell explanation", {
    data <- data.frame(text=ordered(c("low", "middle", "high"),
                                    levels=c("low", "middle", "high")),
                       score=ordered(c("zero", "two", "nine"),
                                     levels=c("zero", "two", "nine")))
    attr(data$score, "values") <- c(0, 2, 9)
    result <- saRa::ordSummary(data, vars=c("text", "score"), range=TRUE, freq=TRUE)
    table <- result$indicesord
    expect_true("text[range]" %in% names(table$asDF))
    expect_equal(table$asDF[["score[range]"]], 9)
    expect_true(any(grepl("text categories", table$getCell(col="text[range]", rowNo=1)$footnotes, fixed=TRUE)))
    expect_length(table$getCell(col="score[range]", rowNo=1)$footnotes, 0L)
    off <- saRa::ordSummary(data, vars=c("text", "score"), range=FALSE, freq=TRUE)
    expect_false(any(grepl("[range]", names(off$indicesord$asDF), fixed=TRUE)))
})
