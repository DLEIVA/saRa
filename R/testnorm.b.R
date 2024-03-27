
# This file is a generated template, your changes will not be overwritten

testnormClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "testnormClass",
    inherit = testnormBase,
    private = list(
        .run = function() {

          groupVarName <- self$options$groupBy
          depVarNames <- self$options$vars
          varNames <- c(groupVarName, depVarNames)
          
          if (is.null(groupVarName) || length(depVarNames) == 0)
            return()
          
          data <- select(self$data, varNames)
          
          for (name in depVarNames)
            data[[name]] <- jmvcore::toNumeric(data[[name]])
          data[[groupVarName]] <- droplevels(as.factor(data[[groupVarName]]))
          
          normtestTable <- self$results$normtests
          
          if (any(depVarNames == groupVarName))
            jmvcore::reject(.("Grouping variable '{a}' must not also be a dependent variable"),
                            code="a_is_dependent_variable", a=groupVarName)
          
          # exclude rows with missings in the grouping variable
          data <- data[ ! is.na(data[[groupVarName]]),]
          
          for (depName in depVarNames) {
            
            dataNTest <- data.frame(dep=data[[depName]], group=data[[groupVarName]])
            groupLevels <- base::levels(dataNTest$group)            
            if(length(groupLevels)>0){
              if(length(groupLevels) > 1){
                for(k in 1:length(groupLevels))
                  normtestTable$addRow(rowKey=k,values=list(`depvar`=ifelse(k==1,depName,''),
                                                            `group`=groupLevels[k],
                                                            `name`='',`stat`='',`p`=''))
              }        
              
              TableRowNo <- 1          

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
            qq    <- group$get('qq')
            
            if (self$options$qq)
              qq$setState(var)
            
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
          data <- data.frame(y=y,s1=self$data[[groupBy]])
          
          data <- na.omit(data)
          
          plot <- ggplot(data=data, mapping = aes(sample = y)) +
            stat_qq_band() +
            stat_qq_line() +
            stat_qq_point() +
            xlab("Theoretical Quantiles") +
            ylab("Standardized Residuals") +
            ggtheme +
            facet_grid(cols=vars(s1))
          
          return(plot)
        },
        .ecdf = function(image, ggtheme, theme, ...) {
          if (is.null(image$state))
            return(FALSE)
          
          var <- image$state
          data <- self$data
          groupBy <- self$options$groupBy
          
          y <- data[[var]]
          y <- as.vector(scale(y))
          data <- data.frame(y=y,s1=self$data[[groupBy]])
          
          data <- na.omit(data)
          
          plot <- ggplot(data=data, mapping = aes(sample = y)) +
            stat_qq_band() +
            stat_qq_line() +
            stat_qq_point() +
            xlab("Theoretical Quantiles") +
            ylab("Standardized Residuals") +
            ggtheme +
            facet_grid(cols=vars(s1))
          
          return(plot)
        }
        )
)
