numSummaryClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "numSummaryClass",
  inherit = numSummaryBase,
  private = list(
    #### Member variables ----
    colArgs = NA,
    .levels = NULL,
    #### Init + run functions ----
    .init = function() {
      private$colArgs <- list(
        name = c(
          "n", "missing", "mean", "median", "mode", "geommean",
          "trimean", "avquartile", "midrange", "trimmean","winsmean",
          "variance", "stddev", "geomsd", "meandev", "cv",
          "range", "iqr", "qd", "mad", 
          "rcv", "ir", "min", "max", "q1", "q2", "q3","beta1","gamma1",
          "h1","h3","beta2","gamma2","k2","k3"
        ),
        title = c(
          "N", "Missing", "Mean", "Median", "Mode", "Geometric mean", "Trimean",
          "Midhinge", "Mid-range", 
          paste0(ifelse(!is.na(private$.getTrim()*100),
                        private$.getTrim()*100,''),'% trimmed mean'),
          paste0(ifelse(!is.na(private$.getWin()*100),
                        private$.getWin()*100,''),'% winsorized mean'),
          "Variance","Standard deviation", "Geometric standard deviation",
          "Mean deviation","CV", "Range", "IQR", "QD", "MAD", 
          "Quartile dispersion", paste0(ifelse(!is.na(private$.getNPerc()*100),
                               private$.getNPerc()*100,''),'% inner range'),
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
      private$.addQuantiles()
      private$.initIndicesTableNum()
      private$.initPlots()
    },
    .run = function(){
      if (length(self$options$vars) > 0) {
        results <- private$.compute()
        private$.populateIndicesTableNum(results)
        private$.preparePlots()
      }
    },
    #### Compute results ----
    .compute = function() {
      data <- self$data
      vars <- self$options$vars
      indic <- list()
      freq <- list()
      for(var in vars){
        column <- data[[var]]
        #if (private$.treatAsFactor(column)) {            
        #}
        indic[[var]] <- private$.computeIndicesNum(column)
      }
      
      return(list(indic=indic))
    },
    #### Init tables ----
    .initIndicesTableNum = function() {
      table <- self$results$indicesnum
      
      #        if (self$options$indic != "columns") {
      #          table$setVisible(FALSE)
      #          return()
      #        }
      
      vars <- self$options$vars
      data <- self$data
      
      colArgs <- private$colArgs
      
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
            clearWith = list("vars", "hist", "dens", "norm")
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
            clearWith = list("vars", "box", "violin", "dot", "dotType", "boxMean", "innerf", "outerf")
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
      }
    },
    #### Plot functions ----
    .preparePlots = function() {
      data <- self$data
      plots <- self$results$plots
      vars <- self$options$vars
      
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
            columns <- na.omit(c(var))
            plotData <- na.omit(data[columns])
            names <- list("x"="x")
            labels <- list("x"=var)
          } else {
            plotData <- data.frame(x=character())
            names <- list("x"="x")
            labels <- list("x"=var)
          }
          
          colnames(plotData) <- as.character(unlist(names))
          
          if (self$options$hist || self$options$dens || self$options$norm)
            hist$setState(list(names=names, labels=labels))
          
          if (self$options$box || self$options$violin || self$options$dot)
            box$setState(list(names=names, labels=labels))
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
      
      plotSpecificTheme <- ggplot2::theme(axis.text.y=ggplot2::element_blank(),
                                 axis.ticks.y=ggplot2::element_blank())
      
      if (self$options$hist && self$options$dens)
        alpha <- 0.4
      else
        alpha <- 1
      
      nBins <- 12
      
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
      
      plot <- ggplot2::ggplot(data=data, ggplot2::aes(x=.data[[names$x]])) +
        ggplot2::labs(x=labels$x, y='density') +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
      
      if (self$options$hist)
        plot <- plot + ggplot2::geom_histogram(
          ggplot2::aes(y=ggplot2::after_stat(density)),
          position="identity",
          stat="bin",
          binwidth=binWidth,
          color=color,
          fill=fill
        )
      
      if (self$options$dens)
        plot <- plot + ggplot2::geom_density(color=color, fill=fill, alpha=alpha)
      
      if (self$options$norm){
        datos <- data[[names$x]]
        plot <- plot +
          ggplot2::stat_function(fun = dnorm, args = list(mean = mean(datos), 
                                                 sd = sd(datos)),col='red',lty=2,lwd=1.15)  
      }
      plot <- plot + ggtheme + plotSpecificTheme
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
      
      column <- data[[names$x]]
      outdata <- private$.getOUTLIERS(column)
      
      fill <- theme$fill[2]
      color <- theme$color[2]
      
      themeSpec <- NULL
      
      data[["placeHolder"]] <- rep('var1', nrow(data))
      x <- "placeHolder"
      
      plot <- ggplot2::ggplot(data=data, ggplot2::aes(x=.data[[x]], y=.data[[names$x]])) +
        ggplot2::labs(x='', y=labels$x) +
        ggplot2::scale_y_continuous( breaks=scales::pretty_breaks())
      
      if (self$options$violin) {
        plot <- plot +
          ggplot2::geom_violin(
            fill=theme$fill[1], color=theme$color[1], alpha=0.5
          )
      }
      
      if (self$options$dot) {
        if (self$options$dotType == 'jitter') {
          column <- data[[names$x]]
          outdata <- private$.getOUTLIERS(column)
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
      
      themeSpec <- list(ggplot2::theme(axis.text.x=ggplot2::element_blank(),
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
      .saraQQPlot(self$data, image$state, NULL, ggtheme)
    },
    #### Populate tables ----
    .populateIndicesTableNum = function(results) {
      #        if (self$options$indic != "columns")
      #          return()
      
      table <- self$results$indicesnum
      vars <- self$options$vars
      
      colNames <- private$colArgs$name
      indic <- results$indic
      
      values <- list(); footnotes <- list(); footnotes2 <- list()
      footnotes3 <- list()
      for (i in seq_along(vars)) {
        
        r <- indic[[vars[i]]]
        for (k in seq_along(colNames)) {
          if (private$.skipOption(private$colArgs$visible[k]))
            next
          
          name <- colNames[k]
          post <- paste0("[", name, "]")
          subName <- paste0(vars[i], post)
          
          values[[subName]] <- r[[name]][1]
        }
        
        if (self$options$mode && length(r[['mode']]) > 1)
          footnotes <- c(footnotes, paste0(vars[i], '[mode]'))
        
        if (self$options$geommean && is.na(r[['geommean']]))
          footnotes2 <- c(footnotes2, paste0(vars[i], '[geommean]'))
        
        if (self$options$geomsd && is.na(r[['geomsd']]))
          footnotes3 <- c(footnotes3, paste0(vars[i], '[geomsd]'))
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
    
    #### Helper functions ----
    .skipOption = function(visible) {
      return(! self$options[[ gsub("[()]", "", visible) ]])
    },
    .getTrim = function(){
      .saraPercentages(self$options$Trim,0,50)
    },
    .getWin = function(){
      .saraPercentages(self$options$Win,0,50)
    },    
    .getPcValues = function(){
      .saraPercentages(self$options$pcValues,split=TRUE,drop=TRUE)
    },
    .getNPerc = function(){
      .saraPercentages(self$options$NPerc,50,99)
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
    .computeIndicesNum = function(column) {
      .saraComputeNumeric(column, self$options, list(
        trim=private$.getTrim, win=private$.getWin,
        inner=private$.getNPerc, percentiles=private$.getPcValues))
    }
)
)
