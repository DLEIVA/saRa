test_that("Mann-Whitney superiority counts between-group ties as one half", {
  x <- c(3,5,5,8,9); y <- c(2,4,5,6,7,10,12)
  d <- data.frame(dep=c(x,y),group=factor(rep(c("A","B"),c(5,7))))
  # Independent pairwise definition, including the two between-group ties.
  reference <- mean(outer(x,y,">") + .5*outer(x,y,"=="))
  expect_equal(reference,16/35)
  effects <- saRa:::.saraMWEffects(d$dep,d$group)
  expect_equal(effects$superiority,reference)
  expect_equal(effects$correlation,1-2*reference)
  expect_equal(effects$f,max(reference,1-reference))
  for (alternative in c("two.sided","greater","less")) {
    actual <- saRa:::.saraMWTest(d,"pSup",alternative)
    expect_equal(actual$statistic,reference)
  }
  reversed <- d
  reversed$group <- factor(reversed$group,levels=c("B","A"))
  expect_equal(saRa:::.saraMWTest(reversed,"pSup")$statistic,1-reference)
  d <- rbind(d,data.frame(dep=NA_real_,group="A"),
    data.frame(dep=100,group=NA_character_))
  expect_equal(saRa:::.saraMWTest(d,"pSup")$statistic,reference)
})

test_that("Mann-Whitney superiority handles separation and no between-group ties", {
  for (samples in list(list(c(1,2),c(3,4,5)),list(c(3,4,5),c(1,2)),
                       list(c(1,4),c(2,3,5)))) {
    x <- samples[[1]]; y <- samples[[2]]
    d <- data.frame(dep=c(x,y),group=factor(rep(c("A","B"),c(length(x),length(y)))))
    expect_equal(saRa:::.saraMWTest(d,"pSup")$statistic,
      mean(outer(x,y,">")+.5*outer(x,y,"==")))
  }
})
