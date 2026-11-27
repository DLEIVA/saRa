.mixedEffectFixture <- function() {
    x <- c(1, 2, 3, NA, 5, 6, 7, NA, 9, 10, 11, NA)
    data.frame("Group label"=factor(rep(c("A", "B", "C"), each=4)),
               "Score one"=x, "Score two"=2*x+10, check.names=FALSE)
}

test_that("mixed effect-size cells contain one footnote and one marker", {
    data <- .mixedEffectFixture()
    result <- saRa::bivmix(data, vars=c("Score one", "Score two"),
        groupBy="Group label", cohend=TRUE, rbp=TRUE, probsup=TRUE,
        etasq=TRUE, omegasq=TRUE, kwind=TRUE)
    table <- result$esindices
    # Accessing table footnotes computes the display marker indices.
    expect_length(table$footnotes, 1L)
    expect_match(table$footnotes, "exactly two observed groups")
    for (var in c("Score one", "Score two")) {
        for (stat in c("cohend", "rbp", "probsup")) {
            cell <- table$getCell(col=paste0(var, "[", stat, "]"), rowNo=1)
            expect_length(cell$footnotes, 1L)
            expect_equal(cell$sups, 0)
            expect_true(is.nan(cell$value))
        }
        expect_equal(table$asDF[[paste0(var, "[etasq]")]], 16/17)
        expect_equal(table$asDF[[paste0(var, "[omegasq]")]], 94/103)
        expect_equal(table$asDF[[paste0(var, "[kwind]")]], 13/15)
    }
    expect_identical(table$asDF[["es[kwind]"]],
                     "η²<sub>H</sub> (Kruskal-Wallis)")
})

test_that("two observed groups do not receive the three-group explanation", {
    data <- .mixedEffectFixture()
    # Retain the unused C factor level: applicability uses observed groups.
    data <- data[data[["Group label"]] != "C",,drop=FALSE]
    result <- saRa::bivmix(data, vars="Score one", groupBy="Group label",
        cohend=TRUE, rbp=TRUE, probsup=TRUE)
    table <- result$esindices
    expect_length(table$footnotes, 0L)
    for (stat in c("cohend", "rbp", "probsup")) {
        cell <- table$getCell(col=paste0("Score one[", stat, "]"), rowNo=1)
        expect_length(cell$footnotes, 0L)
        expect_true(is.finite(cell$value))
    }
})

test_that("unselected two-level effects add no footnote to mixed tables", {
    result <- saRa::bivmix(.mixedEffectFixture(), vars="Score one",
        groupBy="Group label", cohend=FALSE, rbp=FALSE, probsup=FALSE,
        etasq=TRUE, omegasq=TRUE, kwind=TRUE)
    expect_length(result$esindices$footnotes, 0L)
    expect_equal(result$esindices$asDF[["Score one[etasq]"]], 16/17)
})
