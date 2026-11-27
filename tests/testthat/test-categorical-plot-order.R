test_that("categorical plots preserve configured levels and Pareto ranks counts", {
  context <- new.env(parent=asNamespace("saRa"))
  states <- new.env(parent=emptyenv())
  group <- list(get=function(name) {
    force(name)
    list(setState=function(state) assign(name,state,envir=states))
  })
  context$self <- list(
    options=list(vars="hijos",pie=TRUE,dough=TRUE,lolli=TRUE,
      bar=TRUE,pareto=TRUE,f2p.lolli="absolutefreq",f2p.bar="absolutefreq"),
    data=data.frame(hijos=factor(rep(0:4,c(38,71,69,19,3)),levels=0:4)),
    results=list(plots=list(get=function(name) group)))
  prepare <- getFromNamespace("catSummaryClass","saRa")$private_methods$.preparePlots
  environment(prepare) <- context
  for (configured in list(as.character(0:4),as.character(4:0),c("2","0","4","1","3"))) {
    context$self$data$hijos <- factor(context$self$data$hijos,levels=configured)
    for (freq in c("absolutefreq","relativefreq")) {
      context$self$options$f2p.lolli <- freq
      prepare()
      for (name in c("pie","dough","lolli","bar")) {
        state <- get(name,envir=states)
        expect_identical(levels(state$data$x),configured)
        expect_identical(as.character(state$data$x),configured)
        expected <- as.numeric(table(context$self$data$hijos))
        if (name=="lolli" && freq!="absolutefreq") expected <- expected/2
        expect_equal(as.numeric(state$data$y),expected)
      }
      expect_identical(as.character(states$pareto$data$x),c("1","2","0","3","4"))
      expect_equal(tail(states$pareto$data$cumulative,1),200)
      expect_equal(sum(states$pie$data$pct),100)
      expect_equal(states$dough$data$cumulative,cumsum(states$dough$data$y))
    }
  }
})
