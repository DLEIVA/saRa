
# This file is a generated template, your changes will not be overwritten

ordSummaryClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "ordSummaryClass",
  inherit = ordSummaryBase,
  private = list(
    #### Member variables ----
    colArgs = NA,
    .levels = NULL,
    #### Init + run functions ----
    .init = function() {
      private$colArgs <- list(
        name = c(
          "n", "missing", "median", "mode", "trimean", "avquartile", "midrange",
          "range", "iqr", "qd", "mad", "rcv", "min", "max", "q1", "q2", "q3", "h1","h3","k2","k3"
        ),
        title = c(
          "N", "Missing", "Median", "Mode", "Trimean", "Midhinge", "Mid-range",
          "Range", "IQR", "QD", "MAD", "Quartile coefficient of dispersion", "Min", "Max", "Q1", "Q2", "Q3", "SP50","SP80","K2","K3"
        ),
        superTitle = rep("",21),
        type = c("integer","integer","number","text",rep("number",17)),
        format = rep("",21),
        visible = c(
          "(n)","(missing)","(median)","(mode)","(trimean)","(avquartile)","(midrange)",
          "(range)","(iqr)","(qd)","(mad)","(rcv)","(min)","(max)","(q1)","(q2)","(q3)",
          "(h1)","(h3)","(k2)","(k3)"
        )
      )
      if (self$options$freq)
        .saraInitFrequencyTables(self$results$frequencies, self$data,
                                self$options$vars, cumulative=TRUE)
      private$.addQuantiles()
      private$.initIndicesTableOrd()
      private$.initPlotsOrd()
    },
    .run = function(){
      if (length(self$options$vars) > 0) {
        results <- private$.compute()
        if (self$options$freq)
          .saraPopulateFrequencyTables(self$results$frequencies, self$data,
                                      self$options$vars, cumulative=TRUE)
        private$.populateIndicesTableOrd(results)
        private$.preparePlotsOrd()
      }
    },
    #### Compute results ----
    .compute = function() {
      data <- self$data
      vars <- self$options$vars
      indic <- list()
      for(var in vars){
        column <- data[[var]]
        if (!is.ordered(column)) jmvcore::reject(.("Select ordinal variables with ordered levels"), code="requires_ordinal_variables")
        #if (private$.treatAsFactor(column)) {            
        #}
        indic[[var]] <- private$.computeIndicesOrd(column)
      }
      
      return(list(indic=indic))
    },
    #### Init tables ----
    .initIndicesTableOrd = function() {
      table <- self$results$indicesord
      
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
        if (name == "range" && !.saraFrequencyHasNumeric(data, vars)) next
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
            type=if (name %in% c("median","mode","min","max","q1","q2","q3") || startsWith(name,"perc")) {
              if (is.numeric(tryCatch(.saraNumeric(data[[vars[k]]]),error=function(e) NULL))) "number" else "text"
            } else type,
            format=format,
            visible=visible
          )
        }
      }
    },
    #### Init plots ----
    .initPlotsOrd = function() {
      plots <- self$results$plots
      
      data <- self$data
      vars <- self$options$vars
      
      for (var in vars) {
        
        group <- plots$get(var)
        column <- data[[var]]
        
        if (self$options$bar) {
          df <- data[[var]]
          levels <- levels(df)
          
          image <- jmvcore::Image$new(
            options = self$options,
            name = "bar",
            renderFun = ".barPlot",
            width = 550,
            height=550,
            clearWith=list("bar","f2p.bar")
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
            clearWith = list("vars", "box", "violin", "dot", "dotType", "innerf", "outerf")
          )
          
          group$add(image)
        }
      }
    },
    #### Populate tables ----
    .populateIndicesTableOrd = function(results) {
      #        if (self$options$indic != "columns")
      #          return()
      
      table <- self$results$indicesord
      vars <- self$options$vars
      availableColumns <- vapply(table$columns, function(column) column$name, character(1))
      
      colNames <- private$colArgs$name
      indic <- results$indic
      
      values <- list(); footnotes <- list()
      for (i in seq_along(vars)) {
        
        r <- indic[[vars[i]]]
        for (k in seq_along(colNames)) {
          if (private$.skipOption(private$colArgs$visible[k]))
            next
          
          name <- colNames[k]
          post <- paste0("[", name, "]")
          subName <- paste0(vars[i], post)
          if (!subName %in% availableColumns) next
          
          values[[subName]] <- r[[name]][1]
        }
        
        if (self$options$mode && length(r[['mode']]) > 1 &&
            paste0(vars[i], '[mode]') %in% availableColumns)
          footnotes <- c(footnotes, paste0(vars[i], '[mode]'))
      }
      table$setRow(rowNo=1, values=values)
      
      for (i in seq_along(vars)) {
        errors <- attr(indic[[vars[i]]],"errors")
        for (name in names(errors)) {
          index <- match(name,private$colArgs$name)
          if (!is.na(index) && !private$.skipOption(private$colArgs$visible[index]) &&
              paste0(vars[i],"[",name,"]") %in% availableColumns)
            table$addFootnote(rowNo=1,paste0(vars[i],"[",name,"]"),.saraMessage(errors[[name]],self))
        }
      }
      for (i in seq_along(footnotes)) {
        table$addFootnote(
          rowNo=1,
          footnotes[[i]],
          .('More than one mode exists, only the first is reported')
        )
      }
    },
    #### Plot functions ----
    .preparePlotsOrd = function() {
      data <- self$data
      plots <- self$results$plots
      vars <- self$options$vars
      
      for (i in seq_along(vars)) {
        var <- vars[i]
        group <- plots$get(var)
        column <- data[[var]]
        
        if (!is.numeric(column) && !is.factor(column)) next
        
        
        if (self$options$bar) {
          bar  <- group$get('bar')
          freqtype <- self$options$f2p.bar
          levels <- base::levels(column)
          names <- list("x"="x", "y"="y")
          if (freqtype == 'absolutefreq'){
            plotData <- as.data.frame(table(data[var]))
          } else{
            plotData <- as.data.frame(table(data[var])/
                                        sum(table(data[var]))*100)
          }
          
          colnames(plotData) <- as.character(unlist(names))
          
          labels <- list("x"=var)
          
          type <- `if`(is.factor(column), 'categorical')
          bar$setState(list(data=plotData, names=names, labels=labels, 
                            freqtype=freqtype, type=type))
        }
        box <- group$get('box')
        
        if(
          self$options$box ||
          self$options$violin ||
          self$options$dot
        ){
          numeric <- tryCatch(.saraNumeric(column),error=function(e) NULL)
          if (!is.numeric(numeric)) {
            box$setVisible(FALSE)
          } else {
            box$setVisible(TRUE)
            box$setState(list(names=list(x="x"),labels=list(x=var)))
          }
        }
      }
    },
    .barPlot = function(image, ggtheme, theme, ...) {
      if (is.null(image$state))
        return(FALSE)
      
      data <- image$state$data
      names <- image$state$names
      labels <- image$state$labels
      type <- image$state$type
      freqtype <- image$state$freqtype
      fill <- theme$fill[2]
      color <- theme$color[1]
      pd <- ggplot2::position_dodge(0.85)
      plotSpecificTheme <- NULL
      
      plot <-
        ggplot2::ggplot(data=data, ggplot2::aes(x=.data[[names$x]], y=.data[[names$y]])) +
        ggplot2::geom_bar(
          stat="identity",
          position="dodge",
          width = 0.7,
          fill=fill,
          color=color
        ) +
        ggplot2::labs(x=labels$x, y=if(freqtype=='absolutefreq') 'Counts' else
          'Percentages')
      
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
        ggplot2::scale_y_continuous(breaks=scales::pretty_breaks())
      
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
    #### Helper functions ----
    .skipOption = function(visible) {
      return(! self$options[[ gsub("[()]", "", visible) ]])
    },
    .getPcValues = function(){
      pcValues <- self$options$pcValues
      if (is.character(pcValues))
        pcValues <- as.numeric(unlist(strsplit(pcValues, ",")))
      if (any(!is.finite(pcValues)) || any(pcValues < 0 | pcValues > 100))
        jmvcore::reject("Percentiles must be numbers between 0 and 100", code="invalid_percentiles")
      pcValues <- pcValues / 100
      pcValues <- pcValues[!is.na(pcValues)]
      
      return(pcValues)
    },
    .addQuantiles = function(){
      if(self$options$pc){
        pcValues <- private$.getPcValues()
        npcValues <- length(pcValues)
        
        if(npcValues > 0){
          colArgs <- private$colArgs
          
          private$colArgs$name <- append(colArgs$name, paste0('perc', 1:npcValues), after = 17)
          private$colArgs$title <- append(colArgs$title, paste0("Percentile ", round(pcValues * 100, 2)), after = 17)
          private$colArgs$type <- append(colArgs$type, rep('number', npcValues), after = 17)
          if (!is.null(colArgs$format)) private$colArgs$format <- append(colArgs$format, rep('', npcValues), after=17)
          if (!is.null(colArgs$superTitle)) private$colArgs$superTitle <- append(colArgs$superTitle, rep('', npcValues), after=17)
          private$colArgs$visible <- append(colArgs$visible, rep("(pc)", npcValues), after = 17)
        }
      }
      if(self$options$irange){
        NPerc <- self$options$NPerc
        nNPerc <- NPerc / 100
        
        if(nNPerc > 0){
          colArgs <- private$colArgs
          
          private$colArgs$name <- append(colArgs$name, paste0('ir'), after = 12)
          private$colArgs$title <- append(colArgs$title, paste0(NPerc, ('% inner range')), after = 12)
          private$colArgs$type <- append(colArgs$type, 'number', after = 12)
          if (!is.null(colArgs$format)) private$colArgs$format <- append(colArgs$format, '', after=12)
          if (!is.null(colArgs$superTitle)) private$colArgs$superTitle <- append(colArgs$superTitle, '', after=12)
          private$colArgs$visible <- append(colArgs$visible, "(irange)", after = 12)
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
      .saraValidateFences(intervinf,intervsup)
      .bxp1 <- boxplot.stats(jmvcore::toNumeric(column), coef = intervinf)
      .bxp2 <- boxplot.stats(jmvcore::toNumeric(column), coef = intervsup)
      .selec <- .bxp1$out %in% .bxp2$out
      .anom <- .bxp1$out
      .anom[.selec] <- NA
      .extrem <- .bxp2$out
      
      out <- sapply(column,function(x) classout(x,.anom,.extrem))
      return(out)
    },
    .computeIndicesOrd = function(column) {
      .saraOrdinalSummary(column,self$options,list(
        inner=function() self$options$NPerc/100,
        percentiles=function() private$.getPcValues()))
    }
  )
)
