
# This file is a generated template, your changes will not be overwritten

testnormClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "testnormClass",
    inherit = testnormBase,
    private = list(
        .init = function(){
          private$.initPlots()           
        },
        .run = function() {

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
            test <- pearson.test(x)
            list(statistic=test$statistic,p.value=test$p.value)
          }
          
          .swtest <- function(x){
            test <- shapiro.test(x)
            list(statistic=test$statistic,p.value=test$p.value)
          }
          
          .adtest <- function(x){
            test <- ad.test(x)
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
              } else{
                chisq <- .chisqtest(dataNTest$dep)
                ks <- .kstest(dataNTest$dep) 
                sw <- .swtest(dataNTest$dep) 
                ad <- .adtest(dataNTest$dep) 
              }
                                        
            if(length(groupLevels)>0){
              if(length(groupLevels) > 1){
                for(k in 1:length(groupLevels)){
                  normtestTable$addRow(rowKey=k,values=list(`depvar`=ifelse(k==1,depName,''),
                  `group`=groupLevels[k],`stat`='',`p`='',`stat[chisqtest]`=chisq[k,1],
                  `p[chisqtest]`=chisq[k,2],`stat[kstest]`=ks[k,1],`p[kstest]`=ks[k,2],
                  `stat[swtest]`=sw[k,1],`p[swtest]`=sw[k,2],`stat[adtest]`=ad[k,1],
                  `p[adtest]`=ad[k,2]))
              }
        }
            } else{
              normtestTable$addRow(rowKey=1,values=list(`depvar`=depName,#`group`=NA,
              `stat`='',`p`='',`stat[chisqtest]`=chisq[[1]],
              `p[chisqtest]`=chisq[[2]],`stat[kstest]`=ks[[1]],`p[kstest]`=ks[[2]],
              `stat[swtest]`=sw[[1]],`p[swtest]`=sw[[2]],`stat[adtest]`=ad[[1]],
              `p[adtest]`=ad[[2]]))  
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
                plotData <- naOmit(data[columns])
                plotData[[var]] <- jmvcore::toNumeric(plotData[[var]])
                names <- list("x"="x", "s1"="s1")
                labels <- list("x"=var, "s1"=groupBy)
                
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
          
          plotSpecificTheme <- theme(axis.text.y=element_blank(),
                                     axis.ticks.y=element_blank())
          
          if (self$options$hist && self$options$dens)
            alpha <- 0.4
          else
            alpha <- 1
          
          nBins <- 12
          nSplits <- length(groupBy)
          
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
            datos$s1 <- data[[names$s1]]
            
            .getDensity <- function(x){
              data.frame(
                val = grids,
                density = dnorm(grids, mean(x$val), sd(x$val))
              )
            }
            
            densDAT <- datos %>% group_by(s1) %>% do(.getDensity(.)) %>% data.frame()
          }
          
          plot <- ggplot(data=data, aes_string(x=names$x)) +
            labs(x=labels$x, y='density') +
            scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
            {if(self$options$hist)geom_histogram(
              aes(y=..density..),
              position="identity",
              stat="bin",
              binwidth=binWidth,
              color=color,
              fill=fill
            )}+
            {if(self$options$dens) geom_density(color=color, fill=fill, alpha=alpha)}+
            {if(self$options$norm) geom_line(data = densDAT, aes_string(x='val',y = 'density'),
                                             col='red',lty=2,lwd=1.15)} +
            facet_grid(rows=vars(s1))
          
          themeSpec <- theme(legend.position = 'none')
          
          plot <- plot + ggtheme + themeSpec
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
          
          plot <- ggplot(data=data, mapping = aes(sample = y)) +
            stat_qq_band() +
            stat_qq_line() +
            stat_qq_point() +
            xlab("Theoretical Quantiles") +
            ylab("Standardized Residuals") +
            {if(!is.null(groupBy))facet_grid(cols=vars(s1))} +
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
            d.f <- arrange(df,s1,y)
            d.f <- plyr::ddply(d.f, .(s1), transform,
                               z=sort(scale(y)),p=pnorm(sort(scale(y))))
            
          } else{
            d.f <- data.frame(z=sort(scale(y)),p=pnorm(sort(scale(y))))
          }
          
          d.f <- na.omit(d.f)
          
          plot <- ggplot(d.f, aes(x=z,y=p)) + geom_line(col="#7b9ee6") +
            geom_point(aes(y=Fn(z))) + geom_line(aes(y=Fn(z))) +
            xlab("Empirical Quantiles") +
            ylab("Cumulative distribution") +            
            {if(!is.null(groupBy))facet_wrap(d.f$s1)} +   
            guides(fill=FALSE) +
            ggtheme

          return(plot)
        }
        )
)
