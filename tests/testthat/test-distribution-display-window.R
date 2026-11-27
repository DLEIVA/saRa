test_that("long continuous tails leave the centre readable and retain requested cuts", {
    theme <- getFromNamespace("getTheme", "jmvcore")("default", "jmv")
    for (distribution in c("tdist","fdist")) {
        o <- list(cdistros=distribution,tnu=1,f1nu=2,f2nu=2,
                  contppvalue=0,contx1value=0,contx2value=1,
                  contpqvalue=.95,conttail="left")
        d <- saRa:::.saraDistribution(o)
        original <- saRa:::.saraContinuousRange(o)
        bounds <- saRa:::.saraContinuousDisplayRange(o)
        expect_lt(diff(bounds),diff(original)/2)
        expect_true(all(d$q(c(.25,.75)) >= bounds[1]))
        expect_true(all(d$q(c(.25,.75)) <= bounds[2]))
        plot <- saRa:::.saraDistributionPlot(o,"interval",FALSE,ggplot2::theme_minimal(),theme)
        expect_match(plot$labels$caption,"Calculations use the full distribution",fixed=TRUE)
        expect_equal(range(plot$layers[[1]]$data$x),c(0,1))
        expect_warning(expect_error(ggplot2::ggplotGrob(plot),NA),NA)
        distant <- d$q(.9999)
        expanded <- saRa:::.saraContinuousDisplayRange(o,distant)
        expect_gte(expanded[2],distant)
        # Neither densities nor probability calculations depend on the window.
        expect_equal(d$p(1)-d$p(0),if(distribution=="tdist") .25 else .5)
    }
    o <- list(cdistros="norm",mu=0,sigma=1)
    expect_equal(saRa:::.saraContinuousDisplayRange(o),saRa:::.saraContinuousRange(o))
})

test_that("large discrete supports focus on mass and mark the selected probability", {
    theme <- getFromNamespace("getTheme", "jmvcore")("default", "jmv")
    o <- list(distros="binom",binomn=1000,binomp=.5,ppvalue=500,
              x1value=0,x2value=1000,pqvalue=.8,tail="left")
    draw <- function(kind) saRa:::.saraDistributionPlot(o,kind,TRUE,ggplot2::theme_minimal(),theme)
    plot <- draw("density")
    expect_gt(min(plot$data$x),400)
    expect_lt(max(plot$data$x),600)
    expect_equal(sum(plot$data$selected),1L)
    expect_equal(plot$data$y[plot$data$selected],dbinom(500,1000,.5))
    built <- ggplot2::ggplot_build(plot)
    expect_equal(built$data[[3]]$x,500)
    expect_equal(built$data[[3]]$y,dbinom(500,1000,.5))
    expect_match(plot$labels$caption,"probability outside",fixed=TRUE)
    expect_warning(expect_error(ggplot2::ggplotGrob(plot),NA),NA)
    o$ppvalue <- 100
    expect_equal(min(draw("density")$data$x),100)
    full <- draw("interval")
    expect_equal(range(full$data$x),c(0,1000))
    expect_true(all(full$data$selected))
    o$binomn <- 10; o$ppvalue <- 5
    small <- draw("density")
    expect_equal(small$data$x,0:10)
    expect_length(small$layers,1L)
    o$binomp <- 0
    expect_equal(range(draw("density")$data$x),c(0,10))
})
