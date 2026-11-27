.nominalRefactorData <- function(unused=TRUE) {
    data.frame("Row score"=factor(c("A","A","A","B","B","B","B","C","C",NA),
        levels=c("A","B","C",if (unused) "unused row")),
        "Column score"=factor(c("U","U","V","U","V","V","V","U","V","U"),
        levels=c("U","V",if (unused) "unused column")), check.names=FALSE)
}

.nominalRefactorRun <- function(data, rows="Row score", cols="Column score") {
    # Resolve string-valued variable names before the public non-standard
    # evaluation interface captures arguments as column symbols.
    do.call(saRa::bivcat, list(data=data, rows=rows, cols=cols, obs=TRUE, exp=TRUE,
        pcRow=TRUE, pcCol=TRUE, pcTot=TRUE, chiSq=TRUE, phiind=TRUE,
        phiSq=TRUE, contCoef=TRUE, craV=TRUE, chuprov=TRUE, sakoda=TRUE,
        Qyule=TRUE, Yyule=TRUE, Vyule=TRUE, lambdaGKab=TRUE, lambdaGKba=TRUE,
        lambdaGKsym=TRUE, tauGKab=TRUE, tauGKba=TRUE,
        theilab=TRUE, theilba=TRUE, theilsym=TRUE))
}

.nominalRefactorCell <- function(result, table, column, row=1L) {
    unname(result[[table]]$getCell(col=column, rowNo=row)$value)
}

test_that("nominal frequencies retain known counts, expected counts and percentages", {
    result <- .nominalRefactorRun(.nominalRefactorData())
    cell <- function(column, row=1L) .nominalRefactorCell(result, "freqs", column, row)
    # Three observed rows: (2,1), (1,3), (1,1), plus unused categories.
    expect_equal(cell("1[count]"), 2)
    expect_equal(cell("2[count]"), 1)
    expect_equal(cell(".total[count]"), 3)
    expect_equal(cell("1[expected]"), 4/3)
    expect_equal(cell("2[expected]"), 5/3)
    expect_equal(cell("1[pcRow]"), 2/3)
    expect_equal(cell("1[pcCol]"), 1/2)
    expect_equal(cell("1[pcTot]"), 2/9)
    expect_equal(cell("3[count]"), 0)
    expect_equal(cell("1[count]", 4L), 0)
    expect_equal(cell("1[count]", 5L), 4)
    expect_equal(cell("2[count]", 5L), 5)
    expect_equal(cell(".total[count]", 5L), 9)
    expect_equal(cell("1[pcTot]", 5L), 4/9)
})

test_that("nominal public output retains the supplied lambda reference example", {
    data <- data.frame("Row score"=factor(c(rep("A",10),rep("B",10))),
        "Column score"=factor(c(rep("U",15),rep("V",5))), check.names=FALSE)
    result <- .nominalRefactorRun(data)
    cell <- function(table, name)
        .nominalRefactorCell(result, table, paste0("v[",name,"]"))
    expect_equal(cell("asocind", "chiSq"), 20/3)
    expect_equal(cell("asocind", "craV"), sqrt(1/3))
    expect_equal(cell("errorind", "lambdaGKab"), 1/2)
    expect_equal(cell("errorind", "lambdaGKba"), 0)
    expect_equal(cell("errorind", "lambdaGKsym"), 1/3)
    tab <- matrix(c(10,0,5,5), nrow=2, byrow=TRUE)
    expect_equal(cell("errorind", "lambdaGKsym"),
        DescTools::Lambda(tab, direction="symmetric"))
})

test_that("unused levels preserve indices and transposition exchanges prediction directions", {
    data <- .nominalRefactorData()
    full <- .nominalRefactorRun(data)
    observed <- .nominalRefactorRun(.nominalRefactorData(FALSE))
    transposed <- .nominalRefactorRun(data, rows="Column score", cols="Row score")
    for (name in c("Qyule","Yyule","Vyule"))
        expect_true(length(full$asocind$getCell(col=paste0("v[",name,"]"), rowNo=1)$footnotes)>0L)
    for (name in c("chiSq","phiind","phiSq","contCoef","craV","chuprov","sakoda")) {
        column <- paste0("v[",name,"]")
        expected <- .nominalRefactorCell(observed, "asocind", column)
        expect_equal(.nominalRefactorCell(full, "asocind", column), expected)
        expect_equal(.nominalRefactorCell(transposed, "asocind", column), expected)
    }
    for (pair in list(c("lambdaGKab","lambdaGKba"), c("tauGKab","tauGKba"),
                      c("theilab","theilba"))) {
        expect_equal(.nominalRefactorCell(full, "errorind", paste0("v[",pair[1],"]")),
            .nominalRefactorCell(transposed, "errorind", paste0("v[",pair[2],"]")))
        expect_equal(.nominalRefactorCell(full, "errorind", paste0("v[",pair[2],"]")),
            .nominalRefactorCell(transposed, "errorind", paste0("v[",pair[1],"]")))
    }
    for (name in c("lambdaGKsym","theilsym"))
        expect_equal(.nominalRefactorCell(full, "errorind", paste0("v[",name,"]")),
            .nominalRefactorCell(transposed, "errorind", paste0("v[",name,"]")))
})

test_that("degenerate nominal data retain frequencies and undefined associations", {
    data <- data.frame("Row score"=factor(rep("A",3),levels=c("A","unused")),
        "Column score"=factor(rep("U",3),levels=c("U","unused")), check.names=FALSE)
    result <- .nominalRefactorRun(data)
    expect_equal(.nominalRefactorCell(result, "freqs", ".total[count]", 3L), 3)
    expect_true(is.nan(.nominalRefactorCell(result, "asocind", "v[chiSq]")))
    expect_true(is.nan(.nominalRefactorCell(result, "errorind", "v[lambdaGKsym]")))
    empty <- data.frame("Row score"=factor(c("A",NA)),
        "Column score"=factor(c(NA,"U")), check.names=FALSE)
    expect_error(.nominalRefactorRun(empty), "No complete observations", fixed=TRUE)
})
