
# This file is a generated template, your changes will not be overwritten

bivmixClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "bivmixClass",
  inherit = bivmixBase,
  private = list(
    #### Member variables ----
    colArgs = NA,
    colArgs2 = NA,
    .levels = NULL,
    .groupByGrid = NULL,
    #### Init + run functions ----
    .init = function() {
      private$colArgs <- list(
        name = c(
          "n", "missing","mean", "median", "mode", "geommean",
          "trimean", "avquartile", "midrange", "trimmean","winsmean",
          "variance", "stddev", "geomsd", "meandev", "cv",
          "range", "iqr", "qd", "mad", 
          "rcv", "ir", "min", "max", "q1", "q2", "q3","beta1","gamma1",
          "h1","h3","beta2","gamma2","k2","k3"
        ),
        title = c(
          "N", "Missing","Mean", "Median", "Mode", "Geometric mean", "Trimean",
          "Midhinge", "Mid-range", 
          paste0(ifelse(!is.na(private$.getTrim()*100),
                        private$.getTrim()*100,''),'% trimmed mean'),
          paste0(ifelse(!is.na(private$.getWin()*100),
                        private$.getWin()*100,''),'% winsorized mean'),
          "Variance","Standard deviation", "Geometric standard deviation",
          "Mean deviation","CV", "Range", "IQR", "QD", "MAD", 
          "Quartile dispersion", paste0(ifelse(!is.na(private$.getNPerc()*100),
                               private$.getNPerc()*100,''),'% Inner range'),
          "Min", "Max", "Q1", "Q2", "Q3", "Pearson's \u03B2\u2081",
          "Fisher's \u03B3\u2081","H1","H3","Pearson's \u03B2\u2082",
          "Fisher's \u03B3\u2082","K2","K3"
        ),
        superTitle = rep("",35),
        type = c("integer","integer",rep("number",33)),
        format = rep("",35),
        visible = c(
          "(n)","(missing)","(mean)","(median)","(mode)","(geommean)",
          "(trimean)","(avquartile)","(midrange)","(trimmean)","(winsmean)",
          "(variance)","(stddev)","(geomsd)","(meandev)","(cv)","(range)","(iqr)",
          "(qd)","(mad)","(rcv)","(irange)","(min)","(max)","(q1)","(q2)","(q3)",
          "(beta1)","(gamma1)","(h1)","(h3)","(beta2)","(gamma2)","(k2)","(k3)"
        )
      )
      private$colArgs2 <- list(
        name = c(
          "cohend", "rbp", "probsup","etasq","omegasq","kwind"
        ),
        title = c(
          "Cohen's d","Point-biserial Correlation","Probability of superiority",
          "\u03B7\u00B2","\u03C9\u00B2","η²_H (Kruskal–Wallis)"
        ),
        superTitle = rep("",6),
        type = c(rep("number",6)),
        format = rep("",6),
        visible = c("(cohend)","(rbp)","(probsup)","(etasq)","(omegasq)","(kwind)"
        )
      )      
      #if(length(self$options$vars)>0 & !is.null(self$options$groupBy)){
      private$.addQuantiles()
      private$.initIndicesTableNum()
      private$.initEffSTable()
      private$.initPlots()
      #}
    },
    .clear = function(vChanges,...) {
      private$.clearIndicesTableNum(vChanges)
      private$.clearEffSTable(vChanges)      
    },    
    .run = function(){
      if (length(self$options$vars) > 0 && is.null(self$options$groupBy))
        jmvcore::reject("Select a Group by variable", code="missing_group_variable")
      if ( (length(self$options$vars) > 0) & (!is.null(self$options$groupBy)) )  {
        results <- private$.compute()
        private$.populateIndicesTableNum(results)
        resultsES <- private$.computeES()
        private$.populateEffSTable(resultsES)
        private$.preparePlots()
      }
    },
    #### Compute results ----
    .compute = function() {
      data <- self$data
      vars <- self$options$vars
      indic <- list()
      groupBy <- self$options$groupBy
      groups <- data[groupBy]
      for(var in vars){
        column <- data[[var]]
        #if (private$.treatAsFactor(column)) {            
        #}
        indic[[var]] <- tapply(
          column,groups, private$.computeIndicesNum
        )
      }
      
      return(list(indic=indic))
    },
    .computeES = function() {
      data <- self$data
      vars <- self$options$vars
      groupBy <- self$options$groupBy
      groups <- data[[groupBy]]      
      indic <- list()
      for(var in vars){
        column <- data[[var]]
        #if (private$.treatAsFactor(column)) {            
        #}
        dfES <- na.omit(data.frame(c=column,f=groups))
        indic[[var]] <- private$.computeEffSIndices(dfES)
      }
      
      return(list(indic=indic))
    },
    #### Init tables ----
    .initIndicesTableNum = function() {
      table <- self$results$indicesnum
      
      vars <- self$options$vars
      groupBy <- self$options$groupBy
      data <- self$data
      
      grid <- private$.getGroupByGrid()
      colArgs <- private$colArgs
      if(!is.null(groupBy)){
      for (i in seq_along(colArgs$name)) {
        if (private$.skipOption(colArgs$visible[i]))
          next
        
        name <- colArgs$name[i]
        title <- colArgs$title[i]
        format <- colArgs$format[i]
        type <- colArgs$type[i]
        visible <- colArgs$visible[i]
        
          for (j in 1:nrow(grid)) {
            post <- paste0(
              "[", name, paste0(grid[j,], collapse = ""), "]"
            )
            table$addColumn(
              name=paste0("stat", post),
              title="",
              type="text",
              value=title,
              visible=visible,
              combineBelow=TRUE
            )
            
            if (j == 1) {
              table$addFormat(
                rowNo=1, col=paste0("stat", post), jmvcore::Cell.BEGIN_GROUP
              )
            }
            
            for (k in 1:ncol(grid)) {
              table$addColumn(
                name=paste0("var", k,  post),
                title=groupBy[k],
                type="text",
                value=grid[j,k],
                visible=visible,
                combineBelow=TRUE
              )
            }
            
            for (k in seq_along(vars)) {
              subName <- paste0(vars[k], post)
              table$addColumn(
                name=subName,
                title=vars[k],
                type=type,
                format=format,
                visible=visible
              )
            }
          }
        }  
      } else if(is.null(groupBy) & length(colArgs$name)>0){
        for (i in seq_along(colArgs$name)) {
          if (private$.skipOption(colArgs$visible[i]))
            next
          
          name <- colArgs$name[i]
          title <- colArgs$title[i]
          format <- colArgs$format[i]
          type <- colArgs$type[i]
          visible <- colArgs$visible[i]
          
          post <- paste0("[", name, "]")
          table$addColumn(
            name=paste0("stat", post),
            title="",
            type="text",
            value=title,
            visible=visible,
            combineBelow=TRUE
          )
          
          for (k in seq_along(vars)) {
            subName <- paste0(vars[k], post)
            table$addColumn(
              name=subName,
              title=vars[k],
              type=type,
              format=format,
              visible=visible
            )
          }
        }        
      }
    },
    .initEffSTable = function() {
      table <- self$results$esindices
      
      vars <- self$options$vars
      data <- self$data
      
      colArgs2 <- private$colArgs2
      if(!is.null(table)){
        for (i in seq_along(colArgs2$name)) {
          if (private$.skipOption(colArgs2$visible[i]))
            next
          
          name <- colArgs2$name[i]
          title <- colArgs2$title[i]
          format <- colArgs2$format[i]
          type <- colArgs2$type[i]
          visible <- colArgs2$visible[i]
          
          post <- paste0("[", name, "]")
          table$addColumn(
            name=paste0("es", post),
            title="",
            type="text",
            value=title,
            visible=visible,
            combineBelow=TRUE
          )
          
          for (k in seq_along(vars)) {
            subName <- paste0(vars[k], post)
            table$addColumn(
              name=subName,
              title=vars[k],
              type=type,
              format=format,
              visible=visible
            )
          }
        }
      }
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
        
        if (self$options$box || self$options$violin || self$options$dot) {
          
          image <- jmvcore::Image$new(
            options = self$options,
            name = "box",
            renderFun = ".boxPlot",
            requiresData = TRUE,
            width = 550,
            height = 550,
            clearWith = list("vars", "groupBy", "box", "violin", "dot", "dotType", "boxMean", "innerf", "outerf")
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
        box   <- group$get('box')
        qq    <- group$get('qq')
        
        if (self$options$qq)
          qq$setState(var)
        
        if (
          self$options$hist ||
          self$options$dens ||
          self$options$norm ||
          self$options$box ||
          self$options$violin ||
          self$options$dot
        ) {
          
          if (length(na.omit(column)) > 0) {
            columns <- na.omit(c(var, groupBy))
            plotData <- jmvcore::naOmit(data[columns])
            plotData[[var]] <- jmvcore::toNumeric(plotData[[var]])
            names <- list("x"="x", "s1"="s1")
            labels <- list("x"=var, "s1"=groupBy)
            
            colnames(plotData) <- as.character(unlist(names))
          
          if (self$options$hist || self$options$dens || self$options$norm)
            hist$setState(list(names=names, labels=labels))
          
          if (self$options$box || self$options$violin || self$options$dot)
            box$setState(list(names=names, labels=labels))
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
          binWidth <- if (range > 0) range / nBins else 1
        else
          binWidth <- if (nUniques > 1) range / (nUniques - 1) else 1
        
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
          
          densDAT <- datos |>  dplyr::group_by(s1) |>  dplyr::do(.getDensity(.)) |>  data.frame()
          #dens <- plyr::ddply(datos,.(s1), function(x){
          #  data.frame(
          #    y = grids,
          #    density = dnorm(grids, mean(x$val), sd(x$val))
          #  )
          #})
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
          ggplot2::facet_grid(rows=ggplot2::vars(s1))
        
        #data$s1rev <- factor(data$s1, rev(levels(data$s1)))
        #
        #plot <- ggplot(data=data, aes_string(x='x', y='s1rev', fill='s1')) +
        #  labs(x=labels$x, y=labels$s1) +
        #  scale_y_discrete(expand = c(0.05, 0)) +
        #  scale_x_continuous(expand = c(0.01, 0))
        #
        #if (self$options$hist)
        #  plot <- plot + ggridges::geom_density_ridges(stat="binline", bins=nBins, scale=0.9,
        #                                               color=color, fill=fill, alpha=alpha)
        #
        #if (self$options$dens)
        #  plot <- plot + ggridges::geom_density_ridges(scale=0.9, alpha=alpha,
        #                                               color=color, fill=fill)
        #if (self$options$norm){
        #  plot <- plot + stat_function(fun = dnorm, args = list(mean = mean(data), 
        #                                                                  sd = sd(data)),col='red',lty=2,lwd=1.15) 
        #}
        #
        #if (nSplits == 2) {
        #  plot <- plot + facet_grid(cols=vars(s2))
        #} else if (nSplits > 2) {
        #  plot <- plot + facet_grid(cols=vars(s2), rows=vars(s3))
        #}
        
        themeSpec <- ggplot2::theme(legend.position = 'none')
        
        plot <- plot + ggtheme + themeSpec
        return(plot)      
      },
    .boxPlot = function(image, ggtheme, theme, ...) {
      if (is.null(image$state))
        return(FALSE)
      
      data <- .saraPlotFrame(self$data, image$state)
      if (nrow(data) == 0L) return(FALSE)
      type <- image$state$type
      names <- image$state$names
      labels <- image$state$labels
      groupBy <- self$options$groupBy
      
      fill <- theme$fill[2]
      color <- theme$color[2]
      
      themeSpec <- NULL
      
      data[["placeHolder"]] <- rep('var1', nrow(data))
      x <- names$s1
      
      column <- data[[names$x]]
      datos <- data.frame(column=column,grupo=data[[names$s1]])
      outdata <- unlist(tapply(datos$column,datos$grupo,private$.getOUTLIERS))      
      data <- data |>  dplyr::arrange(datos$grupo)
      data$outdata <- outdata
      
      plot <- ggplot2::ggplot(data=data, ggplot2::aes(x=.data[[x]], y=.data[[names$x]])) +
        ggplot2::labs(x=labels$s1, y=labels$x) +
        ggplot2::scale_y_continuous( breaks=scales::pretty_breaks())
      
      if (self$options$violin) {
        plot <- plot +
          ggplot2::geom_violin(
            fill=theme$fill[1], color=theme$color[1], alpha=0.5
          )
      }
      
      if (self$options$dot) {
        if (self$options$dotType == 'jitter') {
          plot <- plot +
            ggplot2::geom_jitter(ggplot2::aes(color=outdata,shape=outdata), width=0.05, alpha=0.6, size=3
            ) +
            ggplot2::guides(color='none',shape='none')
        } else if (self$options$dotType == 'stack') {
          plot <- plot +
            ggplot2::geom_dotplot(ggplot2::aes(color=outdata,fill=outdata),
                                  binaxis="y",
                                  stackdir="center",
                                  alpha=0.5,
                                  stackratio=0.9,
                                  dotsize=0.5
            ) +
            ggplot2::guides(color='none',fill='none')
        }
      }
      
      if (self$options$box & !self$options$dot) {
        plot <- plot +
          ggplot2::geom_boxplot(
            color=theme$color[1],
            fill=theme$color[2],
            width=0.3,
            alpha=0.9,
            coef= self$options$innerf,
            outlier.alpha = 0.4
          )
      } else if (self$options$box & self$options$dot){
        plot <- plot +
          ggplot2::geom_boxplot(
            color=theme$color[1],
            fill=theme$color[2],
            width=0.3,
            alpha=0.9,
            coef= self$options$innerf,
            outlier.alpha = 0.0
          )
      }
      
      if (self$options$boxMean) {
        plot <- plot +
          ggplot2::stat_summary(
            fun=mean,
            geom="point",
            shape=15,
            size=3.5,
            color=theme$color[1]
          )
      }
      
      themeSpec <- list(ggplot2::theme(
                              axis.ticks.x=ggplot2::element_blank(),
                              axis.title.x=ggplot2::element_blank()),
                        ggplot2::scale_colour_manual(name = 'out', 
                                            values = setNames(c('red','blue','grey'),c('extreme','anom','normal'))),
                        ggplot2::scale_shape_manual(name = 'out',
                                           values = setNames(c(8,19,19),c('extreme','anom','normal'))),
                        ggplot2::scale_fill_manual(name = 'out',
                                          values = setNames(c('red','blue','grey'),c('extreme','anom','normal'))))
      
      
      plot <- plot + ggtheme + themeSpec
      
      return(plot)
    },
    .qq = function(image, ggtheme, theme, ...) {
      if (is.null(image$state)) return(FALSE)
      .saraQQPlot(self$data, image$state, self$options$groupBy, ggtheme)
    },
    #### Clear tables ----
    .clearIndicesTableNum = function(vChanges) {

      table <- self$results$indicesnum
      vars <- vChanges
      groupBy <- self$options$groupBy
      
      grid <- private$.getGroupByGrid()
      colNames <- private$colArgs$name
      
      values <- rep(
        NA,
        length(vars) * ifelse(length(groupBy) > 0, nrow(grid), 1) * length(colNames)
      )
      names <- rep(
        '',
        length(vars) * ifelse(length(groupBy) > 0, nrow(grid), 1) * length(colNames)
      )
      iter <- 1
      for (i in seq_along(vars)) {
        if (length(groupBy) > 0) {
          for (j in 1:nrow(grid)) {
            for (k in seq_along(colNames)) {
              name <- colNames[k]
              post <- paste0("[", name, paste0(grid[j,], collapse = ""), "]")
              subName <- paste0(vars[i], post)
              
              names[iter] <- subName
              iter <- iter + 1
            }
          }
        } else {
          for (k in seq_along(colNames)) {
            name <- colNames[k]
            post <- paste0("[", name, "]")
            subName <- paste0(vars[i], post)
            
            names[iter] <- subName
            iter <- iter + 1
          }
        }
      }
      
      names(values) <- names
      table$setRow(rowNo=1, values=values)
    },
    #### Populate tables ----
    .populateIndicesTableNum = function(results) {
      #        if (self$options$indic != "columns")
      #          return()
      
      table <- self$results$indicesnum
      vars <- self$options$vars
      groupBy <-  self$options$groupBy
      
      grid <- private$.getGroupByGrid()
      colNames <- private$colArgs$name
      indic <- results$indic
      
      values <- list(); footnotes <- list(); footnotes2 <- list()
      footnotes3 <- list()
      for (i in seq_along(vars)) {
        
        r <- indic[[vars[i]]]
        if (length(groupBy) > 0) {
          for (j in 1:nrow(grid)) {
            indices <- grid[j,]
            stats <- do.call("[", c(list(r), indices))[[1]]
            
            for (k in seq_along(colNames)) {
              if (private$.skipOption(private$colArgs$visible[k]))
                next
              
              name <- colNames[k]
              post <- paste0("[", name, paste0(grid[j,], collapse = ""), "]")
              subName <- paste0(vars[i], post)
              
              values[[subName]] <- stats[[name]][1]
            }
            
            if (self$options$mode && length(stats[['mode']]) > 1) {
              post <- paste0("[mode", paste0(grid[j,], collapse = ""), "]")
              subName <- paste0(vars[i], post)
              footnotes <- c(footnotes, subName)
            }
            
            if (self$options$geommean && is.na(stats[['geommean']])) {
              post <- paste0("[geommean", paste0(grid[j,], collapse = ""), "]")
              subName <- paste0(vars[i], post)
              footnotes2 <- c(footnotes2, subName)
            }
            
            if (self$options$geomsd && is.na(stats[['geomsd']])) {
              post <- paste0("[geomsd", paste0(grid[j,], collapse = ""), "]")
              subName <- paste0(vars[i], post)
              footnotes3 <- c(footnotes3, subName)
            }
          }
        }
      }
      table$setRow(rowNo=1, values=values)
      
      for (i in seq_along(footnotes)) {
        table$addFootnote(
          rowNo=1,
          footnotes[[i]],
          ('More than one mode exists, only the first is reported')
        )
      }
      
      for (i in seq_along(footnotes2)) {
        table$addFootnote(
          rowNo=1,
          footnotes2[[i]],
          ('Geometric mean can only be computed with positive values (>0)')
        )
      }
      
      for (i in seq_along(footnotes3)) {
        table$addFootnote(
          rowNo=1,
          footnotes3[[i]],
          ('Geometric std. deviation can only be computed with positive values (>0)')
        )
      }
    },
    .populateEffSTable = function(results) {
      
      table <- self$results$esindices
      vars <- self$options$vars
      groupBy <-  self$options$groupBy
      ngroups <- length(levels(self$data[[groupBy]]))
      
      colNames <- private$colArgs2$name
      indic <- results$indic
      
      values <- list(); footnotes <- list(); 
      
      for (i in seq_along(vars)) {
        
        r <- indic[[vars[i]]]
        for (k in seq_along(colNames)) {
          if (private$.skipOption(private$colArgs2$visible[k]))
            next
          
          name <- colNames[k]
          post <- paste0("[", name, "]")
          subName <- paste0(vars[i], post)
          
          values[[subName]] <- r[[name]][1]
        }
        
        if ( self$options$cohend && ngroups > 2 )
          footnotes <- c(footnotes, paste0(vars[i],'[cohend]'))
        
        if (self$options$rbp && ngroups > 2)
          footnotes <- c(footnotes, paste0(vars[i], '[rbp]'))
        
        if (self$options$probsup && ngroups > 2)
          footnotes <- c(footnotes, paste0(vars[i], '[probsup]'))
      }
      table$setRow(rowNo=1, values=values)
      
      for (i in seq_along(footnotes)) {
        table$addFootnote(
          rowNo=1,
          footnotes[[i]],
          ("More than two levels, effect size indicator is not reported")
        )
      }
    },    
    
    #### Helper functions ----
    .errorCheck = function() {
      data <- self$data
      groupBy <- self$options$groupBy
      
      if ( ! is.null(groupBy)) {
        for (item in groupBy) {
          if ( ! is.factor(data[[item]])) {
            jmvcore::reject(
              .('Unable to split by a continuous variable')
            )
          }
        }
      }
      
      for (var in groupBy) {
        if (length(levels(data[[var]])) == 0) {
          jmvcore::reject(
            jmvcore::format(
              .("The 'group by' variable '{var}' contains no data."), var=var
            ),
            code=''
          )
        } else if (is.null(groupBy)) {
          jmvcore::reject(
            jmvcore::format(
              .("The 'group by' variable is required."), var=var
            ),
            code=''
          )
        }
      }
    },    
    .skipOption = function(visible) {
      return(! self$options[[ gsub("[()]", "", visible) ]])
    },
    .getTrim = function(){
      Trim <- self$options$Trim
      if (is.character(Trim))
        Trim <- as.numeric(Trim)
      Trim <- Trim / 100
      Trim[Trim > .50 | Trim < 0.] <- NA
      return(Trim)
    },
    .getWin = function(){
      Win <- self$options$Win
      if (is.character(Win))
        Win <- as.numeric(Win)
      Win <- Win / 100
      Win[Win > .50 | Win < 0.] <- NA
      return(Win)
    },    
    .getPcValues = function(){
      pcValues <- self$options$pcValues
      if (is.character(pcValues))
        pcValues <- as.numeric(unlist(strsplit(pcValues, ",")))
      pcValues <- pcValues / 100
      pcValues[pcValues < 0 | pcValues > 1] <- NA
      pcValues <- pcValues[!is.na(pcValues)]
      
      return(pcValues)
    },
    .getLevels = function() {
      if (is.null(private$.levels)) {
        groupBy <- self$options$groupBy
        levels <- rep(list(NULL), length(groupBy))
        for (i in seq_along(groupBy)) {
          lvls <- levels(self$data[[groupBy[i]]])
          if (length(lvls) == 0) {
            # error
            groupBy <- NULL
            levels <- list()
            break()
          }
          levels[[i]] <- lvls
        }
        private$.levels <- levels
      }
      
      return(private$.levels)
    },      
    .getGroupByGrid = function() {
      if (is.null(private$.groupByGrid)) {
        expandGrid <- function(...) expand.grid(..., stringsAsFactors = FALSE)
        private$.groupByGrid <- rev(do.call(expandGrid, rev(private$.getLevels())))
      }
      return(private$.groupByGrid)
    },      
    .getNPerc = function(){
      NPerc <- self$options$NPerc
      if (is.character(NPerc))
        NPerc <- as.numeric(NPerc)
      NPerc <- NPerc / 100
      NPerc[NPerc < .50 | NPerc > 0.99] <- NA
      return(NPerc)
    },
    .addQuantiles = function(){
      
      if(self$options$pc){
        pcValues <- private$.getPcValues()
        npcValues <- length(pcValues)
        
        if(npcValues > 0){
          colArgs <- private$colArgs
          
          private$colArgs$name <- append(colArgs$name, paste0('perc', 1:npcValues), after = 27)
          private$colArgs$title <- append(colArgs$title, paste0("Percentile ", round(pcValues * 100, 2)), after = 27)
          private$colArgs$type <- append(colArgs$type, rep('number', npcValues), after = 27)
          private$colArgs$visible <- append(colArgs$visible, rep("(pc)", npcValues), after = 27)
        }
      }
    },
    .getOUTLIERS = function(column){
      classout <- function(x,.anom,.extreme){
        if(!x %in%c(.anom,.extrem)) {val <- 'normal'
        } else if(x %in% .anom) {val <- 'anom'
        } else val <- 'extreme'
        return(val)
      }
      
      intervinf <- self$options$innerf
      intervsup <- self$options$outerf
      .bxp1 <- grDevices::boxplot.stats(as.numeric(column), coef = intervinf)
      .bxp2 <- grDevices::boxplot.stats(as.numeric(column), coef = intervsup)
      .selec <- .bxp1$out %in% .bxp2$out
      .anom <- .bxp1$out
      .anom[.selec] <- NA
      .extrem <- .bxp2$out
      
      out <- sapply(column,function(x) classout(x,.anom,.extrem))
      return(out)
    },
    .sourcifyOption = function(option) {
      if (option$name == 'vars' && length(self$options$groupBy) > 0)
        return('')
      if (option$name == 'groupBy')
        return('')
      super$.sourcifyOption(option)
    },  
    .computeIndicesNum = function(column) {
      .saraComputeNumeric(column, self$options, list(
        trim=private$.getTrim, win=private$.getWin,
        inner=private$.getNPerc, percentiles=private$.getPcValues))
    }, 
.computeEffSIndices = function(ESdf) {
  es <- list()
  ESdf <- data.frame(ESdf)
  n <- nrow(ESdf)
  k <- length(unique(ESdf$f))
  if (k < 2L || n < 2L)
    return(setNames(as.list(rep(NaN, 6)), c("cohend", "rbp", "probsup", "etasq", "omegasq", "kwind")))
  if (n > 0) {
    
    if ( self$options$cohend & is.numeric(ESdf$c) & k == 2 ){
      es[['cohend']] <- psych::cohen.d(ESdf$c,ESdf$f)$cohen.d[[1,2]]
    } else if ( self$options$cohend & is.numeric(ESdf$c) & 
                k != 2 ) {es[['cohend']] <- NA}
    
    if ( self$options$rbp & is.numeric(ESdf$c) & k == 2 ){
      es[['rbp']] <- cor(ESdf$c,as.numeric(ESdf$f))
    } else if ( self$options$rbp & is.numeric(ESdf$c) & 
                k != 2 ) {es[['rbp']] <- NA}
  
    if ( self$options$probsup & is.numeric(ESdf$c) & k == 2 ){
      es[['probsup']] <- rcompanion::wilcoxonPS(c~f,data=ESdf)[[1]]
    } else if ( self$options$probsup & is.numeric(ESdf$c) & 
                k != 2 ) {es[['probsup']] <- NA}
    
    if ( self$options$etasq & is.numeric(ESdf$c) ){
      es[['etasq']] <- effectsize::eta_squared(lm(c~f,data=ESdf),partial=FALSE)[[2]]
    } else if ( self$options$etasq & !is.numeric(ESdf$c) ) {es[['omegasq']] <- NA}
    
    if ( self$options$omegasq & is.numeric(ESdf$c) ){
      es[['omegasq']] <- effectsize::omega_squared(lm(c~f,data=ESdf),partial=FALSE)[[2]]
    } else if ( self$options$omegasq & !is.numeric(ESdf$c) ) {es[['omegasq']] <- NA}
    
    if ( self$options$kwind & is.numeric(ESdf$c) ){
      H <- kruskal.test(c~f,data=ESdf)$statistic[[1]]
      es[['kwind']] <- (H-k+1)/(n-k)
    } else if ( self$options$kwind & !is.numeric(ESdf$c) ) {es[['kwind']] <- NA}    
  
  } else if (n==0) {
    l <- list(
      cohend=NaN,
      rbp = NaN,
      probsup = NaN,
      etasq = NaN,
      omegasq = NaN,
      kwind = NaN
    )
  }
  return(es)  
} 
  )
)
