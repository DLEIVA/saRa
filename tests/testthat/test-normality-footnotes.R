test_that("normality notes use consecutive letters and recover after edits", {
  opts <- list(vars=c("Valor","Other"),groupBy="Grupo",chisqtest=FALSE,
    kstest=FALSE,swtest=TRUE,adtest=TRUE,lillietest=TRUE,sftest=FALSE,cvmtest=FALSE)
  tests <- c("swtest","adtest","lillietest")
  columns <- list(list(name="depvar",type="text"),list(name="group",type="text"))
  for (test in tests) for (prefix in c("stat","p"))
    columns <- c(columns,list(list(name=paste0(prefix,"[",test,"]"),type="number")))
  table <- jmvcore::Table$new(options=jmvcore::Options$new(),name="normality",
    columns=columns)
  d <- data.frame(Valor=c(3,5,5,8,9,2,4,5,6,7,10,12),
    Other=c(3,5,NA,NA,NA,2,4,NA,NA,NA,NA,NA),
    Grupo=factor(rep(c("A","B"),c(5,7))))
  context <- new.env(parent=asNamespace("saRa"))
  context$self <- list(options=opts,results=list(normtests=table),data=d)
  context$private <- list(.preparePlots=function() NULL)
  run <- getFromNamespace("testnormClass","saRa")$private_methods$.run
  environment(run) <- context
  notes <- function() {
    all <- table$notes
    keys <- names(all)
    if (is.null(keys)) return(character())
    vapply(all[startsWith(keys,"sara_letter_")],function(x) x$note,
      character(1),USE.NAMES=FALSE)
  }
  expect_error(run(),NA)
  expect_length(notes(),3L)
  patterns <- c("^<sup>a</sup>.*greater than 7",
    "^<sup>b</sup>.*between 3 and 5000","^<sup>c</sup>.*greater than 4")
  for (i in seq_along(patterns)) expect_match(notes()[i],patterns[i])
  for (g in c("A","B")) {
    key <- list(variable="Other",group=g)
    for (i in seq_along(tests)) {
      cell <- table$getCell(col=paste0("stat[",tests[i],"]"),rowKey=key)
      expect_true(is.nan(cell$value))
      expect_length(cell$footnotes,0L)
      expect_equal(cell$symbols,paste0("<sup>",c("b","a","c")[i],"</sup>"))
    }
  }
  context$self$data$Other <- d$Valor
  expect_error(run(),NA)
  expect_length(notes(),1L)
  expect_match(notes(),"greater than 7")
  for (g in c("A","B")) for (test in c("swtest","lillietest")) {
    col <- paste0("stat[",test,"]")
    actual <- table$getCell(col=col,rowKey=list(variable="Other",group=g))
    reference <- table$getCell(col=col,rowKey=list(variable="Valor",group=g))
    expect_equal(actual$value,reference$value)
    expect_length(actual$symbols,0L)
  }
  context$self$options$adtest <- FALSE
  expect_error(run(),NA)
  expect_length(notes(),0L)
})
