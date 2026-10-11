
# This file is a generated template, your changes will not be overwritten

testnormClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "testnormClass",
    inherit = testnormBase,
    private = list(
        .init = function() {
          private$.initPlots()

        },
        .run = function() {
          groupName <- self$options$groupBy
          variables <- self$options$vars
          if (!is.null(groupName) && groupName %in% variables)
            jmvcore::reject("Grouping variable must not also be a dependent variable",
                            code="a_is_dependent_variable")
          tests <- list(
            chisqtest=function(x) nortest::pearson.test(x),
            kstest=function(x) stats::ks.test(x, "pnorm", mean=mean(x), sd=sd(x)),
            swtest=function(x) stats::shapiro.test(x),
            adtest=function(x) nortest::ad.test(x),
            lillietest=function(x) nortest::lillie.test(x),
            sftest=function(x) nortest::sf.test(x),
            cvmtest=function(x) nortest::cvm.test(x))
          table <- self$results$normtests
          # Group membership depends on observations; init may receive headers only.
          # Rebuild rows from the full dataset to avoid missing/stale group keys.
          table$deleteRows()
          for (variable in variables) {
            values <- jmvcore::toNumeric(self$data[[variable]])
            groups <- if (is.null(groupName)) rep("", length(values)) else as.character(self$data[[groupName]])
            groupLevels <- if (is.null(groupName)) "" else levels(droplevels(as.factor(self$data[[groupName]])))
            for (group in groupLevels) {
              key <- list(variable=variable, group=group)
              table$addRow(rowKey=key, values=list(depvar=variable, group=group))
              x <- values[!is.na(groups) & groups == group & !is.na(values)]
              for (testName in names(tests)) {
                if (!self$options[[testName]]) next
                result <- tryCatch({
                  if (length(x) < 2L) stop("Requires at least two non-missing observations")
                  if (any(!is.finite(x))) stop("Infinite values are not supported")
                  if (sd(x) == 0) stop("Requires a non-constant variable")
                  suppressWarnings(tests[[testName]](x))
                }, error=function(e) e)
                statColumn <- paste0("stat[", testName, "]")
                pColumn <- paste0("p[", testName, "]")
                if (inherits(result, "error")) {
                  table$setRow(rowKey=key, setNames(list(NaN, NaN), c(statColumn, pColumn)))
                  table$addFootnote(rowKey=key, statColumn, conditionMessage(result))
                } else {
                  table$setRow(rowKey=key, setNames(list(unname(result$statistic), result$p.value), c(statColumn, pColumn)))
                }
                if (testName == "kstest")
                  table$addFootnote(rowKey=key, pColumn,
                    "Normal parameters are estimated from these data; this p-value is uncorrected. Use the Lilliefors test for corrected inference.")
              }
            }
          }
          private$.preparePlots()
        },
        #### Init plots ----    
        .initPlots = function() {
          plots <- self$results$plots
          
          data <- self$data
          vars <- self$options$vars
          
          for (var in vars) {
            
            group <- plots$get(var)
            column <- data[[var]]
            
            if (self$options$hist || self$options$dens  || self$options$norm) {
              
              image <- jmvcore::Image$new(
                options = self$options,
                name = "hist",
                renderFun = ".histogram",
            requiresData = TRUE,
                width = 550,
                height = 550,
                clearWith = list("vars", "groupBy", "hist", "dens", "norm")
              )
              
              group$add(image)
            }
            
            if (self$options$qq) {
              
              image <- jmvcore::Image$new(
                options = self$options,
                name = "qq",
                renderFun = ".qq",
                requiresData = TRUE,
                width = 550,
                height = 550,
                clearWith = list("vars", "groupBy", "qq")
              )
              group$add(image)
            }
            
            if (self$options$ecdf) {
              
              image <- jmvcore::Image$new(
                options = self$options,
                name = "ecdf",
                renderFun = ".ecdf",
                requiresData = TRUE,
                width = 550,
                height = 550,
                clearWith = list("vars", "groupBy", "ecdf")
              )
              group$add(image)
            }            
          }
        },
        #### Plot functions ----
        .preparePlots = function() {
          data <- self$data
          plots <- self$results$plots
          vars <- self$options$vars
          groupBy <- self$options$groupBy      
          
          for (i in seq_along(vars)) {
            var <- vars[i]
            group <- plots$get(var)
            column <- data[[var]]
            
            if (!is.numeric(column) && !is.factor(column)) next
            
            hist  <- group$get('hist')
            qq <- group$get('qq')
            ecdf <- group$get('ecdf')
            
            if (self$options$qq)
              qq$setState(var)
            
            if (self$options$ecdf)
              ecdf$setState(var)            
            
            if (
              self$options$hist ||
              self$options$dens ||
              self$options$norm
            ) {
              
              if (length(na.omit(column)) > 0) {
                columns <- na.omit(c(var, groupBy))
                plotData <- jmvcore::naOmit(data[columns])
                plotData[[var]] <- jmvcore::toNumeric(plotData[[var]])
                names <- if(!is.null(groupBy)){
                  list("x"="x", "s1"="s1") 
                } else{ list("x"="x")}
                labels <- if(!is.null(groupBy)){
                  list("x"=var, "s1"=groupBy) 
                } else{list("x"=var)}
                
                colnames(plotData) <- as.character(unlist(names))
                
                if (self$options$hist || self$options$dens || self$options$norm)
                  hist$setState(list(names=names, labels=labels))
              }
            }
          }          
        },
        .histogram = function(image, ggtheme, theme, ...) {
          
          if (is.null(image$state))
            return(FALSE)
          
          data <- .saraPlotFrame(self$data, image$state)
      if (nrow(data) == 0L) return(FALSE)
          names <- image$state$names
          labels <- image$state$labels
          groupBy <- self$options$groupBy
          
          plotSpecificTheme <- ggplot2::theme(axis.text.y=ggplot2::element_blank(),
                                     axis.ticks.y=ggplot2::element_blank())
          
          if (self$options$hist && self$options$dens)
            alpha <- 0.4
          else
            alpha <- 1
          
          nBins <- 12
          nSplits <- if(!is.null(groupBy)){
            length(unique(groupBy)) 
          } else{ 1 }
          
          fill <- theme$fill[2]
          color <- theme$color[1]
          
          min <- min(data[[names$x]])
          if (is.na(min))
            min <- 0
          
          max <- max(data[[names$x]])
          if (is.na(max))
            max <- 0
          
          range <- max - min
          
          nUniques <- length(unique(data[[names$x]]))
          if (nUniques > nBins)
            binWidth <- if (range > 0) range / nBins else 1
          else
            binWidth <- if (nUniques > 1) range / (nUniques - 1) else 1
          
          if(self$options$norm){
            grids <- seq(min, max, 
                         length.out = 150)
            datos <- data.frame(val=data[[names$x]])
            if(!is.null(groupBy)) datos$s1 <- data[[names$s1]]
            
            .getDensity <- function(x){
              data.frame(
                val = grids,
                density = dnorm(grids, mean(x$val), sd(x$val))
              )
            }
            
            densDAT <- if(!is.null(groupBy)){
              datos |>  dplyr::group_by(s1) |>  dplyr::do(.getDensity(.)) |>  data.frame() 
            } else{ datos |>  dplyr::do(.getDensity(.)) |>  data.frame() }
          }
          
          plot <- ggplot2::ggplot(data=data, ggplot2::aes(x=.data[[names$x]])) +
            ggplot2::labs(x=labels$x, y='density') +
            ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
            {if(self$options$hist) ggplot2::geom_histogram(
              ggplot2::aes(y=ggplot2::after_stat(density)),
              position="identity",
              stat="bin",
              binwidth=binWidth,
              color=color,
              fill=fill
            )}+
            {if(self$options$dens) ggplot2::geom_density(color=color, fill=fill, alpha=alpha)}+
            {if(self$options$norm) ggplot2::geom_line(data = densDAT, ggplot2::aes(x=.data[['val']], y=.data[['density']]),
                                             col='red',lty=2,lwd=1.15)} +
            {if(!is.null(groupBy)) ggplot2::facet_grid(rows=ggplot2::vars(s1))} +
            ggtheme
          
          themeSpec <- ggplot2::theme(legend.position = 'none')
          
          plot <- plot + themeSpec
          return(plot)      
        },
    .qq = function(image, ggtheme, theme, ...) {
      if (is.null(image$state)) return(FALSE)
      .saraQQPlot(self$data, image$state, self$options$groupBy, ggtheme)
    },
        .ecdf = function(image, ggtheme, theme, ...) {
          if (is.null(image$state))
            return(FALSE)
          
          var <- image$state
          data <- self$data
          groupBy <- self$options$groupBy
          
          df <- .saraECDFData(jmvcore::toNumeric(data[[var]]),
                              if (is.null(groupBy)) NULL else data[[groupBy]])
          if (is.null(df) || nrow(df) == 0L) return(FALSE)
          plot <- ggplot2::ggplot(df, ggplot2::aes(x=z)) +
            ggplot2::geom_line(ggplot2::aes(y=normal), colour="#7b9ee6") +
            ggplot2::geom_step(ggplot2::aes(y=empirical)) +
            ggplot2::geom_point(ggplot2::aes(y=empirical)) +
            ggplot2::xlab("Standardised values") +
            ggplot2::ylab("Cumulative probability") + ggtheme
          if (!is.null(groupBy)) plot <- plot + ggplot2::facet_wrap(~ group)

          return(plot)
        }
        )
)
