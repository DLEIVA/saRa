test_that("normal overlays omit degenerate groups and recover", {
  context <- new.env(parent=asNamespace("saRa"))
  d <- data.frame(score=c(rep(5,5),2,4,5,6,7,10,12),
    group=factor(rep(c("A","B"),c(5,7))))
  context$self <- list(options=list(groupBy="group",hist=TRUE,dens=TRUE,norm=TRUE),
    data=d)
  render <- getFromNamespace("testnormClass","saRa")$private_methods$.histogram
  environment(render) <- context
  theme <- list(fill=c("white","#7b9ee6"),color="black")
  image <- list(state=list(names=list(x="x",s1="s1"),
    labels=list(x="score",s1="group")))
  draw <- function() render(image,ggplot2::theme_minimal(),theme)
  plot <- draw()
  normal <- plot$layers[[3]]$data
  expect_equal(unique(as.character(normal$s1)),"B")
  expect_equal(nrow(normal),150L)
  expect_equal(normal$density,dnorm(normal$val,mean(d$score[6:12]),sd(d$score[6:12])))
  expect_match(plot$labels$caption,"Normal curve omitted")
  expect_true(all(nchar(strsplit(plot$labels$caption,"\n",fixed=TRUE)[[1]]) <= 55L))
  expect_error(ggplot2::ggplot_build(plot),NA)
  context$self$data$score[1:5] <- c(3,5,5,8,9)
  plot <- draw()
  expect_equal(unique(as.character(plot$layers[[3]]$data$s1)),c("A","B"))
  expect_null(plot$labels$caption)
  expect_error(ggplot2::ggplot_build(plot),NA)
  context$self$data$score <- rep(5,12)
  plot <- draw()
  expect_length(plot$layers,2L)
  expect_match(plot$labels$caption,"Normal curve omitted")
  expect_error(ggplot2::ggplot_build(plot),NA)
  context$self$options$groupBy <- NULL
  image$state <- list(names=list(x="x"),labels=list(x="score"))
  context$self$data <- data.frame(score=5)
  context$self$options$dens <- FALSE
  plot <- draw()
  expect_length(plot$layers,1L)
  expect_match(plot$labels$caption,"Normal curve omitted")
  expect_error(ggplot2::ggplot_build(plot),NA)
})
