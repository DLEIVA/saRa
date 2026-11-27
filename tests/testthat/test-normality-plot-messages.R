.normalityPlotCallback <- function(data,variable,kind,groupBy=NULL) {
    context <- new.env(parent=asNamespace("saRa"))
    context$self <- list(data=data,options=list(groupBy=groupBy))
    fun <- getFromNamespace("testnormClass","saRa")$private_methods[[paste0(".",kind)]]
    environment(fun) <- context
    fun(list(state=variable),ggplot2::theme_minimal(),NULL)
}

test_that("normality callbacks render constant and insufficient observations", {
    for (kind in c("qq","ecdf")) {
        for (values in list(rep(5,5),c(5,NA),rep(NA_real_,5))) {
            plot <- .normalityPlotCallback(data.frame(x=values),"x",kind)
            expect_s3_class(plot,"ggplot")
            built <- ggplot2::ggplot_build(plot)
            expected <- if(sum(is.finite(values))<2L) "two finite" else "constant"
            expect_match(built$data[[1]]$label,expected)
            expect_warning(expect_error(ggplot2::ggplotGrob(plot),NA),NA)
        }
    }
})

test_that("a constant variable does not prevent valid plots or recovery", {
    data <- data.frame(x=c(1,2,2,3,4,NA),constant=c(rep(5,5),NA))
    for (kind in c("qq","ecdf")) {
        bad <- .normalityPlotCallback(data,"constant",kind)
        expect_match(ggplot2::ggplot_build(bad)$data[[1]]$label,"constant")
        good <- .normalityPlotCallback(data,"x",kind)
        expect_equal(nrow(good$data),5L)
        expect_warning(expect_error(ggplot2::ggplotGrob(good),NA),NA)
        if (kind=="ecdf") expect_equal(good$data$empirical,c(.2,.6,.6,.8,1))
        else expect_equal(good$data$sample,sort(as.vector(scale(c(1,2,2,3,4)))))
        data$constant <- data$x
        recovered <- .normalityPlotCallback(data,"constant",kind)
        expect_equal(nrow(recovered$data),5L)
        data$constant <- c(rep(5,5),NA)
    }
})

test_that("invalid groups have explanatory facets alongside valid groups", {
    data <- data.frame(x=c(1,2,2,3,4,5,5,5,9,NA),
                       g=c(rep("valid",5),rep("constant",3),"single","empty"))
    for (kind in c("qq","ecdf")) {
        plot <- .normalityPlotCallback(data,"x",kind,"g")
        built <- ggplot2::ggplot_build(plot)
        expect_setequal(as.character(built$layout$layout$group),
                        c("valid","constant","single","empty"))
        expect_equal(nrow(plot$data),5L)
        notes <- built$data[[length(built$data)]]
        expect_equal(nrow(notes),3L)
        expect_equal(sum(grepl("constant",notes$label)),1L)
        expect_equal(sum(grepl("two finite",notes$label)),2L)
        expect_warning(expect_error(ggplot2::ggplotGrob(plot),NA),NA)
    }
})


test_that("group messages have full-width panels and centred coordinates", {
    data <- data.frame(x=c(1,2,2,3,4,5,5,5,9),
                       g=c(rep("A",5),rep("B",3),"C"))
    for (kind in c("qq","ecdf")) {
        plot <- .normalityPlotCallback(data,"x",kind,"g")
        built <- ggplot2::ggplot_build(plot)
        expect_true(all(built$layout$layout$COL==1L))
        expect_equal(length(unique(built$layout$layout$ROW)),3L)
        notes <- plot$layers[[length(plot$layers)]]$data
        horizontal <- if(kind=="qq") plot$data$theoretical else plot$data$z
        expect_equal(notes$x,rep(mean(range(horizontal)),2))
        if(kind=="ecdf") expect_equal(notes$y,c(.5,.5))
        else expect_equal(notes$y,rep(mean(range(c(plot$data$lower,
            plot$data$upper,plot$data$sample))),2))
        expect_warning(expect_error(ggplot2::ggplotGrob(plot),NA),NA)
    }
})
