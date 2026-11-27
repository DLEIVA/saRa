.mwLetterNotes <- function(table) {
  notes <- table$notes
  if (is.null(names(notes))) return(character())
  vapply(notes[startsWith(names(notes),"sara_letter_")],function(note) note$note,
    character(1),USE.NAMES=FALSE)
}

test_that("group median intervals agree with successful native results", {
  samples <- list(A=c(3,5,5,8,9),B=c(2,4,5,6,7,10,12))
  for (level in c(.90,.95,.99)) {
    actual <- suppressWarnings(saRa:::.saraMedianIntervals(samples,"exact",level,details=TRUE))
    for (g in seq_along(samples)) {
      ref <- try(suppressWarnings(DescTools::MedianCI(samples[[g]],
        conf.level=level,method="exact")),silent=TRUE)
      if (inherits(ref,"try-error")) {
        # Older DescTools errors when an order-statistic endpoint is out of range.
        expect_equal(actual$values[g,],c(-Inf,Inf))
        expect_equal(unname(actual$confidence[g]),1)
      } else {
        expect_equal(actual$values[g,],unname(ref[2:3]))
        metadata <- attr(ref,"conf.level")
        if (length(metadata)==1L && is.finite(metadata) && metadata>0 && metadata<=1)
          expect_equal(unname(actual$confidence[g]),unname(metadata))
        else expect_true(is.finite(actual$confidence[g]))
        expect_null(actual$errors[[g]])
      }
    }
  }
  # An invalid sample must not erase the other group's finite interval.
  actual <- suppressWarnings(saRa:::.saraMedianIntervals(
    list(c(NA_real_,NA_real_),samples$B),"exact",.90,details=TRUE))
  ref <- suppressWarnings(DescTools::MedianCI(samples$B,conf.level=.90,method="exact"))
  expect_equal(actual$values[2,],unname(ref[2:3]))
})

test_that("exact median intervals handle unattainable finite coverage explicitly", {
  samples <- list(A=c(3,5,5,8,9),B=c(2,4,5,6,7,10,12))
  expected <- list(
    list(level=.90,bounds=matrix(c(3,2,9,12),ncol=2),confidence=c(.9375,.984375)),
    list(level=.95,bounds=matrix(c(-Inf,2,Inf,12),ncol=2),confidence=c(1,.984375)),
    list(level=.99,bounds=matrix(c(-Inf,-Inf,Inf,Inf),ncol=2),confidence=c(1,1)))
  for (case in expected) {
    actual <- saRa:::.saraMedianIntervals(samples,"exact",case$level,details=TRUE)
    expect_equal(unname(actual$values),case$bounds)
    expect_equal(unname(actual$confidence),case$confidence)
    expect_true(all(vapply(actual$errors,is.null,logical(1))))
  }
  single <- saRa:::.saraExactMedianCI(22,.95)
  expect_equal(unname(single[2:3]),c(-Inf,Inf))
  expect_equal(attr(single,"conf.level"),1)
  expect_error(saRa:::.saraExactMedianCI(numeric(0)),"finite observations")
  expect_error(saRa:::.saraExactMedianCI(c(1,NA)),"finite observations")
  expect_error(saRa:::.saraExactMedianCI(c(1,2),1),"between zero and one")
})

test_that("Mann-Whitney table retains per-group intervals and notes", {
  d <- data.frame(Value=c(3,5,5,8,9,2,4,5,6,7,10,12),
    Group=factor(rep(c("A","B"),c(5,7))))
  for (level in c(.90,.95,.99)) {
    refs <- suppressWarnings(saRa:::.saraMedianIntervals(
      split(d$Value,d$Group),"exact",level,details=TRUE))
    result <- suppressWarnings(saRa::umw(d,vars="Value",group="Group",
      ciMedians=TRUE,ciWidth=100*level))
    table <- result$cisMed
    for (g in seq_len(2L)) {
      expect_equal(unname(table$getCell(col=paste0("cilMed[",g,"]"),rowKey="Value")$value),
        unname(refs$values[g,1]))
      expect_equal(unname(table$getCell(col=paste0("ciuMed[",g,"]"),rowKey="Value")$value),
        unname(refs$values[g,2]))
    }
    expect_identical(table$getColumn("cilMed[1]")$superTitle,
      saRa:::.saraMedianConfidenceHeading(refs$confidence,level))
    if (any(is.infinite(refs$values)))
      expect_true(any(grepl("unbounded",.mwLetterNotes(table),fixed=TRUE)))
  }
})

test_that("median confidence headings and plot legends distinguish effective levels", {
  expect_identical(saRa:::.saraMedianConfidenceHeading(c(.9375,.984375),.90),
    "Confidence Interval (level reported per group)")
  expect_identical(saRa:::.saraMedianConfidenceHeading(c(1,1),.99),
    "100% Confidence Interval")
  state <- saRa:::.saraMedianPlotData(c("A","B"),c(5,6),
    matrix(c(-Inf,2,Inf,12),ncol=2))
  state$confidence <- c(1,.984375)
  expect_identical(saRa:::.saraMWMedianPlotLabel(state,95),
    "Median (unbounded intervals; see table)")
  state$lower <- c(3,2); state$upper <- c(9,12)
  state$confidence <- c(.9375,.984375)
  expect_identical(saRa:::.saraMWMedianPlotLabel(state,90),
    "Median (confidence intervals reported in table)")
  state$confidence <- c(.95,.95)
  expect_identical(saRa:::.saraMWMedianPlotLabel(state,95),"Median (95% CI)")
})

test_that("median interval notes use one consecutive marker per group", {
  d <- data.frame(Value=c(3,5,5,8,9,2,4,5,6,7,10,12),
    Group=factor(rep(c("A","B"),c(5,7))))
  for (width in c(90,95,99,95,90)) {
    result <- suppressWarnings(saRa::umw(d,vars="Value",group="Group",
      ciMedians=TRUE,ciWidth=width))
    table <- result$cisMed
    notes <- .mwLetterNotes(table)
    markers <- character(0)
    for (g in seq_len(2L)) {
      cell <- table$getCell(col=paste0("cilMed[",g,"]"),rowKey="Value")
      expect_length(cell$footnotes,0L)
      expect_length(cell$symbols,1L)
      markers <- c(markers,cell$symbols)
    }
    expect_equal(sort(unique(markers)),paste0("<sup>",letters[seq_along(notes)],"</sup>"))
    expect_length(notes,if (width==99) 1L else 2L)
    if (width>=95)
      expect_true(any(grepl("unbounded",notes,fixed=TRUE)))
    else expect_false(any(grepl("unbounded",notes,fixed=TRUE)))
  }
})


test_that("Mann-Whitney median plots keep finite intervals and a bottom legend", {
  samples <- list(A=c(3,5,5,8,9),B=c(2,4,5,6,7,10,12))
  for (width in c(90,95,99)) {
    ci <- saRa:::.saraMedianIntervals(samples,"exact",width/100,details=TRUE)
    state <- saRa:::.saraMedianPlotData(names(samples),c(5,6),ci$values)
    state$confidence <- ci$confidence
    label <- saRa:::.saraMWMedianPlotLabel(state,width)
    plot <- saRa:::.saraMedianPlot(state,label,
      ggtheme=ggplot2::theme(legend.position="right"),legendPosition="bottom")
    built <- ggplot2::ggplot_build(plot)
    expect_identical(plot$theme$legend.position,"bottom")
    expect_equal(nrow(built$data[[1]]),switch(as.character(width),
      "90"=2L,"95"=1L,"99"=0L))
    expect_equal(nrow(built$data[[2]]),2L)
    expect_equal(length(unique(built$data[[2]]$x)),2L)
    expect_equal(as.numeric(built$data[[2]]$x),c(1,2))
    expect_identical(built$layout$panel_scales_x[[1]]$get_limits(),c("A","B"))
    if (width==95)
      expect_equal(as.numeric(built$data[[1]]$x),2)
    expect_equal(built$data[[2]]$y,c(5,6))
    expect_identical(plot$scales$get_scales("shape")$labels,
      c(median=paste(strwrap(label,width=42),collapse="\n")))
    expect_error(ggplot2::ggplotGrob(plot),NA)
  }
})


test_that("complete marker alphabet is used with missing groups and multiple variables", {
  expect_identical(vapply(c(1L,2L,3L,4L,26L,27L),saRa:::.saraLetterMarker,
    character(1)),c("a","b","c","d","z","aa"))
  original <- c(3,5,5,8,9,2,4,5,6,7,10,12)
  for (missing in c(TRUE,FALSE,TRUE)) {
    d <- data.frame(Value=original,Other=original,
      Group=factor(rep(c("A","B"),c(5,7))))
    if (missing) d$Other[d$Group=="B"] <- NA_real_
    result <- suppressWarnings(saRa::umw(d,vars=c("Value","Other"),group="Group",
      mwu=TRUE,zstat=TRUE,pSup=TRUE,rankCorr=TRUE,fstat=TRUE,
      ciMedians=TRUE,ciHL=TRUE,ciWidth=90))
    for (table in list(result$mwutest,result$cisMed,result$cisHL)) {
      notes <- .mwLetterNotes(table)
      expect_false(anyDuplicated(notes)>0L)
      symbols <- character()
      for (row in seq_len(table$rowCount)) for (column in table$columns) {
        if (!column$visible) next
        cell <- column$getCell(row)
        expect_length(cell$footnotes,0L)
        symbols <- c(symbols,cell$symbols)
      }
      expected <- paste0("<sup>",letters[seq_along(notes)],"</sup>")
      if (!length(notes)) expected <- character()
      expect_equal(sort(unique(symbols)),expected)
      if (!missing) expect_false(any(grepl("enough|requires finite|observed groups",
        notes)))
    }
    if (missing) {
      expect_length(.mwLetterNotes(result$cisMed),3L)
      expect_identical(result$cisMed$getCell(col="cilMed[2]",rowKey="Other")$symbols,
        "<sup>c</sup>")
      expect_true(length(.mwLetterNotes(result$cisHL))>0L)
    }
  }
})

test_that("all-tied groups give a readable HL explanation and recover", {
  group <- factor(rep(c("A","B"),c(5,7)))
  original <- c(3,5,5,8,9,2,4,5,6,7,10,12)
  for (tied in c(TRUE,FALSE)) {
    d <- data.frame(Value=original,Other=if(tied) rep(5,12) else original,
      Group=group)
    result <- suppressWarnings(saRa::umw(d,vars=c("Value","Other"),group="Group",
      ciMedians=TRUE,ciHL=TRUE,ciWidth=90))
    notes <- .mwLetterNotes(result$cisHL)
    expect_false(any(grepl("TRUE/FALSE",notes,fixed=TRUE)))
    estimate <- result$cisHL$getCell(col="hlestimate",rowKey="Other")
    if (tied) {
      expect_true(any(grepl("all observations are tied",notes,fixed=TRUE)))
      expect_true(is.nan(estimate$value))
      expect_length(estimate$symbols,1L)
      expect_equal(unname(result$cisMed$getCell(col="cilMed[1]",rowKey="Other")$value),5)
      expect_equal(unname(result$cisMed$getCell(col="ciuMed[2]",rowKey="Other")$value),5)
    } else {
      expect_length(notes,0L)
      expect_length(estimate$symbols,0L)
      expect_equal(estimate$value,
        result$cisHL$getCell(col="hlestimate",rowKey="Value")$value)
    }
    expect_true(is.finite(result$cisHL$getCell(col="hlestimate",rowKey="Value")$value))
  }
  expect_error(saRa:::.saraHodgesLehmann(rep(5,12),group=group),
    "all observations are tied",fixed=TRUE)
})

test_that("one observation per group retains HL estimate without false precision", {
  original <- c(3,5,5,8,9,2,4,5,6,7,10,12)
  group <- factor(rep(c("A","B"),c(5,7)))
  for (width in c(90,95,99)) {
    minimal <- rep(NA_real_,12)
    minimal[c(1,6)] <- c(3,2)
    actual <- saRa:::.saraHodgesLehmann(minimal,group=group,
      conf.level=width/100,details=TRUE)
    expect_equal(actual$values,c(1,NaN,NaN))
    expect_null(actual$confidence)
    expect_match(actual$note,"only one observation per group",fixed=TRUE)
    expect_equal(saRa:::.saraHodgesLehmann(minimal,group=group,
      conf.level=width/100),actual$values)
    d <- data.frame(Value=original,Other=minimal,Group=group)
    result <- suppressWarnings(saRa::umw(d,vars=c("Value","Other"),group="Group",
      ciMedians=TRUE,ciHL=TRUE,ciWidth=width))
    expect_equal(unname(result$cisHL$getCell(col="hlestimate",rowKey="Other")$value),1)
    for (column in c("cilHL","ciuHL"))
      expect_true(is.nan(result$cisHL$getCell(col=column,rowKey="Other")$value))
    expect_true(any(grepl("only one observation per group",
      .mwLetterNotes(result$cisHL),fixed=TRUE)))
    expect_true(is.finite(result$cisHL$getCell(col="hlestimate",rowKey="Value")$value))
    d$Other <- original
    restored <- suppressWarnings(saRa::umw(d,vars=c("Value","Other"),group="Group",
      ciMedians=TRUE,ciHL=TRUE,ciWidth=width))
    expect_length(.mwLetterNotes(restored$cisHL),0L)
    expect_length(restored$cisHL$getCell(col="hlestimate",rowKey="Other")$symbols,0L)
    expect_equal(restored$cisHL$getCell(col="hlestimate",rowKey="Other")$value,
      restored$cisHL$getCell(col="hlestimate",rowKey="Value")$value)
  }
  reversed <- factor(c("A","B"),levels=c("B","A"))
  expect_equal(saRa:::.saraHodgesLehmann(c(3,2),group=reversed),c(-1,NaN,NaN))
})
