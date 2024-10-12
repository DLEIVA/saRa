
# This file is a generated template, your changes will not be overwritten

testnormClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "testnormClass",
    inherit = testnormBase,
    private = list(
        .run = function() {
          private$.initPlots()
          groupVarName <- self$options$groupBy
          depVarNames <- self$options$vars
          varNames <- if(is.null(groupVarName) || length(depVarNames) == 0){
            c(depVarNames)
          } else{
            c(groupVarName, depVarNames) 
          }
          
          # if (is.null(groupVarName) || length(depVarNames) == 0)
          #   return()
          
          data <- select(self$data, varNames)
          
          .kstest <- function(x){
            test <- ks.test(x,'pnorm',mean=mean(x),sd=sd(x))
            list(statistic=test$statistic,p.value=test$p.value)
          }
          
          .chisqtest <- function(x){
            test <- nortest::pearson.test(x)
            list(statistic=test$statistic,p.value=test$p.value)
          }
          
          .swtest <- function(x){
            test <- shapiro.test(x)
            list(statistic=test$statistic,p.value=test$p.value)
          }
          
          .adtest <- function(x){
            test <- nortest::ad.test(x)
            list(statistic=test$statistic,p.value=test$p.value)
          }
          
          .lillietest <- function(x){
            test <- nortest::lillie.test(x)
            list(statistic=test$statistic,p.value=test$p.value)
          }
          
          .sftest <- function(x){
            test <- nortest::sf.test(x)
            list(statistic=test$statistic,p.value=test$p.value)
          }
          
          .cvmtest <- function(x){
            test <- nortest::cvm.test(x)
            list(statistic=test$statistic,p.value=test$p.value)
          }          
          
          for (name in depVarNames)
            data[[name]] <- jmvcore::toNumeric(data[[name]])
          if(!is.null(groupVarName))
          data[[groupVarName]] <- droplevels(as.factor(data[[groupVarName]]))
          
          normtestTable <- self$results$normtests
          
          if (any(depVarNames == groupVarName))
            jmvcore::reject(.("Grouping variable '{a}' must not also be a dependent variable"),
                            code="a_is_dependent_variable", a=groupVarName)
          
          # exclude rows with missings in the grouping variable
          if(!is.null(groupVarName))
          data <- data[ ! is.na(data[[groupVarName]]),]
          
          for (depName in depVarNames) {
            if(!is.null(groupVarName)){
              dataNTest <- data.frame(dep=data[[depName]], group=data[[groupVarName]])
            } else{
              dataNTest <- data.frame(dep=data[[depName]])              
            }
            
            if(!is.null(groupVarName)){
              groupLevels <- base::levels(dataNTest$group)
            } else groupLevels <- NULL            

              if(!is.null(groupLevels)){
                chisq <- matrix(unlist(tapply(dataNTest$dep,dataNTest$group,
                                              suppressWarnings(.chisqtest))),nrow=length(levels(dataNTest$group)),byrow=TRUE)
                
                ks <- matrix(unlist(tapply(dataNTest$dep,dataNTest$group,
                                           suppressWarnings(.kstest))),nrow=length(levels(dataNTest$group)),byrow=TRUE)
                
                sw <- matrix(unlist(tapply(dataNTest$dep,dataNTest$group,
                                           suppressWarnings(.swtest))),nrow=length(levels(dataNTest$group)),byrow=TRUE)
                
                ad <- matrix(unlist(tapply(dataNTest$dep,dataNTest$group,
                                           suppressWarnings(.adtest))),nrow=length(levels(dataNTest$group)),byrow=TRUE)
                
                lillie <- matrix(unlist(tapply(dataNTest$dep,dataNTest$group,
                                           suppressWarnings(.lillietest))),nrow=length(levels(dataNTest$group)),byrow=TRUE)
                
                sf <- matrix(unlist(tapply(dataNTest$dep,dataNTest$group,
                                           suppressWarnings(.sftest))),nrow=length(levels(dataNTest$group)),byrow=TRUE)

                cvm <- matrix(unlist(tapply(dataNTest$dep,dataNTest$group,
                                           suppressWarnings(.cvmtest))),nrow=length(levels(dataNTest$group)),byrow=TRUE)
              } else{
                chisq <- .chisqtest(dataNTest$dep)
                ks <- .kstest(dataNTest$dep) 
                sw <- .swtest(dataNTest$dep) 
                ad <- .adtest(dataNTest$dep)
                lillie <- .lillietest(dataNTest$dep)
                sf <- .sftest(dataNTest$dep)
                cvm <- .cvmtest(dataNTest$dep)
              }
                                        
            if(length(groupLevels)>0){
              if(length(groupLevels) > 1){
                for(k in 1:length(groupLevels)){
                  normtestTable$addRow(rowKey=k,values=list(`depvar`=ifelse(k==1,depName,''),
                  `group`=groupLevels[k],`stat`='',`p`='',`stat[chisqtest]`=chisq[k,1],
                  `p[chisqtest]`=chisq[k,2],`stat[kstest]`=ks[k,1],`p[kstest]`=ks[k,2],
                  `stat[swtest]`=sw[k,1],`p[swtest]`=sw[k,2],`stat[adtest]`=ad[k,1],
                  `p[adtest]`=ad[k,2],`stat[lillietest]`=lillie[k,1],
                  `p[lillietest]`=lillie[k,2],`stat[sftest]`=sf[k,1],
                  `p[sftest]`=sf[k,2],`stat[cvmtest]`=cvm[k,1],
                  `p[cvmtest]`=cvm[k,2]))
              }
        }
            } else{
              normtestTable$addRow(rowKey=1,values=list(`depvar`=depName,
              `stat`='',`p`='',`stat[chisqtest]`=chisq[[1]],
              `p[chisqtest]`=chisq[[2]],`stat[kstest]`=ks[[1]],`p[kstest]`=ks[[2]],
              `stat[swtest]`=sw[[1]],`p[swtest]`=sw[[2]],`stat[adtest]`=ad[[1]],
              `p[adtest]`=ad[[2]],`stat[lillietest]`=lillie[[1]],
              `p[lillietest]`=lillie[[2]],`stat[sftest]`=sf[[1]],`p[sftest]`=sf[[2]],
              `stat[cvmtest]`=cvm[[1]],`p[cvmtest]`=cvm[[2]]))  
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
                width = 550,
                height = 550,
                clearWith = list("hist", "dens", "norm")
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
                clearWith = list("qq")
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
                clearWith = list("ecdf")
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
            
            if(!is.numeric(column))
              stop(paste0("The variable ", var,
                          " cannot be treated as numeric. 
                      Plots that expect numeric data will not be created for this variable."))
            
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
                  hist$setState(list(data=plotData, names=names, labels=labels))
              }
            }
          }          
        },
        .histogram = function(image, ggtheme, theme, ...) {
          
          if (is.null(image$state))
            return(FALSE)
          
          data <- image$state$data
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
            binWidth <- range / nBins
          else
            binWidth <- range / (nUniques - 1)
          
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
          
          plot <- ggplot2::ggplot(data=data, ggplot2::aes_string(x=names$x)) +
            ggplot2::labs(x=labels$x, y='density') +
            ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
            {if(self$options$hist) ggplot2::geom_histogram(
              ggplot2::aes(y=..density..),
              position="identity",
              stat="bin",
              binwidth=binWidth,
              color=color,
              fill=fill
            )}+
            {if(self$options$dens) ggplot2::geom_density(color=color, fill=fill, alpha=alpha)}+
            {if(self$options$norm) ggplot2::geom_line(data = densDAT, ggplot2::aes_string(x='val',y = 'density'),
                                             col='red',lty=2,lwd=1.15)} +
            {if(!is.null(groupBy)) ggplot2::facet_grid(rows=ggplot2::vars(s1))} +
            ggtheme
          
          themeSpec <- ggplot2::theme(legend.position = 'none')
          
          plot <- plot + themeSpec
          return(plot)      
        },
        .qq = function(image, ggtheme, theme, ...) {
          if (is.null(image$state))
            return(FALSE)
          
          var <- image$state
          data <- self$data
          groupBy <- self$options$groupBy
          
          y <- data[[var]]
          y <- as.vector(scale(y))
          data <- if(!is.null(groupBy)){
            data.frame(y=y,s1=self$data[[groupBy]]) 
          } else{
            data.frame(y=y)
          }
          
          data <- na.omit(data)
          
          plot <- ggplot2::ggplot(data=data, mapping = ggplot2::aes(sample = y)) +
            qqplotr::stat_qq_band() +
            qqplotr::stat_qq_line() +
            qqplotr::stat_qq_point() +
            ggplot2::xlab("Theoretical Quantiles") +
            ggplot2::ylab("Standardized Residuals") +
            {if(!is.null(groupBy)) ggplot2::facet_grid(cols=ggplot2::vars(s1))} +
            ggtheme
          
          return(plot)
        },
        .ecdf = function(image, ggtheme, theme, ...) {
          if (is.null(image$state))
            return(FALSE)
          
          var <- image$state
          data <- self$data
          groupBy <- self$options$groupBy
          
          y <- data[[var]]
          y <- as.vector(sort(scale(y)))
          Fn <- stats::ecdf(y)          
          
          
          if(!is.null(groupBy)){
            s1 <- self$data[[groupBy]]
            df <- data.frame(y,s1)
            d.f <- dplyr::arrange(df,s1,y)
            d.f <- plyr::ddply(d.f, plyr::.(s1), transform,
                               z=sort(scale(y)),p=pnorm(sort(scale(y))))
            
          } else{
            d.f <- data.frame(z=sort(scale(y)),p=pnorm(sort(scale(y))))
          }
          
          d.f <- na.omit(d.f)
          
          plot <- ggplot2::ggplot(d.f, ggplot2::aes(x=z,y=p)) + ggplot2::geom_line(col="#7b9ee6") +
            ggplot2::geom_point(ggplot2::aes(y=Fn(z))) + ggplot2::geom_line(ggplot2::aes(y=Fn(z))) +
            ggplot2::xlab("Empirical Quantiles") +
            ggplot2::ylab("Cumulative distribution") +            
            {if(!is.null(groupBy)) ggplot2::facet_wrap(d.f$s1)} +   
            ggplot2::guides(fill=FALSE) +
            ggtheme

          return(plot)
        }
        )
)
