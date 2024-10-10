
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
          "N", "Missing", "Median", "Mode", "Trimean", "Quartiles' average", "Mid-range",
          "Range", "IQR", "QD", "MAD", "rCV", "Min", "Max", "Q1", "Q2", "Q3", "H1","H3","K2","K3"
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
      private$.addQuantiles()
      private$.initIndicesTableOrd()
      private$.initPlotsOrd()
    },
    .run = function(){
      if (length(self$options$vars) > 0) {
        results <- private$.compute()
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
            width = 550,
            height = 550,
            clearWith = list("box", "violin", "dot", "dotType", "boxMean")
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
          
          values[[subName]] <- r[[name]][1]
        }
        
        if (self$options$mode && length(r[['mode']]) > 1)
          footnotes <- c(footnotes, paste0(vars[i], '[mode]'))
      }
      table$setRow(rowNo=1, values=values)
      
      for (i in seq_along(footnotes)) {
        table$addFootnote(
          rowNo=1,
          footnotes[[i]],
          ('More than one mode exists, only the first is reported')
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
        
        if(!is.ordered(column) & !is.numeric(column))
          stop(paste0("The variable ", var,
                      " cannot be treated as ordinal. 
                      Plots that expect ordinal data will not be created for this variable."))
        
        
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
          if(length(na.omit(column)) > 0){
            columns <- na.omit(c(var))
            plotData <- na.omit(data[columns])
            plotData[[var]] <- as.numeric(plotData[[var]])
            names <- list("x"="x")
            labels <- list("x"=var)
          }
          else{
            plotData <- data.frame(x=character())
            names <- list("x"="x")
            labels <- list("x"=var)
          }
          colnames(plotData) <- as.character(unlist(names))
          
          if (self$options$box || self$options$violin || self$options$dot)
            box$setState(list(data=plotData, names=names, labels=labels))
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
      pd <- position_dodge(0.85)
      plotSpecificTheme <- NULL
      
      plot <-
        ggplot(data=data, aes_string(x=names$x, y=names$y)) +
        geom_bar(
          stat="identity",
          position="dodge",
          width = 0.7,
          fill=fill,
          color=color
        ) +
        labs(x=labels$x, y=if(freqtype=='absolutefreq') 'Counts' else
          'Percentages')
      
      plot <- plot + ggtheme + plotSpecificTheme
      return(plot)
    },
    .boxPlot = function(image, ggtheme, theme, ...) {
      if (is.null(image$state))
        return(FALSE)
      
      data <- image$state$data
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
      
      plot <- ggplot(data=data, aes_string(x=x, y=names$x)) +
        labs(x='', y=labels$x) +
        scale_y_continuous(breaks=pretty_breaks())
      
      if (self$options$violin) {
        plot <- plot +
          geom_violin(
            fill=theme$fill[1], color=theme$color[1], alpha=0.5
          )
      }
      
      if (self$options$dot) {
        if (self$options$dotType == 'jitter') {
          column <- data[[names$x]]
          outdata <- private$.getOUTLIERS(column)
          plot <- plot +
            ggplot2::geom_jitter(aes(color=outdata,shape=outdata), width=0.05, alpha=0.6, size=3
            ) +
            guides(color='none',shape='none')
        } else if (self$options$dotType == 'stack') {
          plot <- plot +
            ggplot2::geom_dotplot(aes(color=outdata,fill=outdata),
                                  binaxis="y",
                                  stackdir="center",
                                  alpha=0.5,
                                  stackratio=0.9,
                                  dotsize=0.5
            ) +
            guides(color='none',fill='none')
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
          stat_summary(
            fun.y=mean,
            geom="point",
            shape=15,
            size=3.5,
            color=theme$color[1]
          )
      }
      
      themeSpec <- list(theme(axis.text.x=element_blank(),
                              axis.ticks.x=element_blank(),
                              axis.title.x=element_blank()),
                        scale_colour_manual(name = 'out', 
                                            values = setNames(c('red','blue','grey'),c('extreme','anom','normal'))),
                        scale_shape_manual(name = 'out',
                                           values = setNames(c(8,19,19),c('extreme','anom','normal'))),
                        scale_fill_manual(name = 'out',
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
          private$colArgs$title <- append(colArgs$title, paste0(round(pcValues * 100, 2), ('th pct')), after = 17)
          private$colArgs$type <- append(colArgs$type, rep('number', npcValues), after = 17)
          private$colArgs$visible <- append(colArgs$visible, rep("(pc)", npcValues), after = 17)
        }
      }
      if(self$options$irange){
        NPerc <- self$options$NPerc
        nNPerc <- length(NPerc)
        
        if(nNPerc > 0){
          colArgs <- private$colArgs
          
          private$colArgs$name <- append(colArgs$name, paste0('ir'), after = 12)
          private$colArgs$title <- append(colArgs$title, paste0(NPerc, ('% Inner range')), after = 12)
          private$colArgs$type <- append(colArgs$type, rep('number', nNPerc), after = 12)
          private$colArgs$visible <- append(colArgs$visible, rep("(irange)", nNPerc), after = 12)
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
      .bxp1 <- boxplot.stats(as.numeric(column), coef = intervinf)
      .bxp2 <- boxplot.stats(as.numeric(column), coef = intervsup)
      .selec <- .bxp1$out %in% .bxp2$out
      .anom <- .bxp1$out
      .anom[.selec] <- NA
      .extrem <- .bxp2$out
      
      out <- sapply(column,function(x) classout(x,.anom,.extrem))
      return(out)
    },
    .computeIndicesOrd = function(column) {
      stats <- list()
      
      total <- length(column)
      column <- jmvcore::naOmit(column)
      n <- length(column)
      stats[['n']] <- n
      stats[['missing']] <- total - n
      
      if (n > 0) {
        ni <- as.numeric(column)
        trimedia <- function(x){
          FN <- fivenum(as.numeric(x)) 
          Finf <- FN[2]
          Md <- FN[3]
          Fsup <- FN[4]
          res <- (Finf+2*Md+Fsup)/4
          res
        }
        promcuar <- function(x){
          FN <- fivenum(as.numeric(x),na.rm=TRUE)
          Q1 <- FN[2]
          Q3 <- FN[4]
          res <- (Q1+Q3)/2
          res
        }
        midR <- function(x){
          QU <- quantile(as.numeric(x),na.rm=TRUE)
          min <- QU[1]
          max <- QU[5]
          res <- (min+max)/2
          res
        }
        
        rango <- function(x){
          QU <- quantile(as.numeric(x),na.rm=TRUE)
          min <- QU[1]
          max <- QU[5]
          res <- max-min
          res
        }
        
        IQR <- function(x){
          QU <- quantile(as.numeric(x),na.rm=TRUE)
          Q1 <- QU[2]
          Q3 <- QU[4]
          res <- Q3-Q1
          res
        }
        
        QD <- function(x){
          QU <- quantile(as.numeric(x),na.rm=TRUE)
          Q1 <- QU[2]
          Q3 <- QU[4]
          res <- (Q3-Q1)/2
          res
        }
        
        MAD <- function(x){
          mediana <- median(as.numeric(x),na.rm=TRUE)
          res <- median(abs(as.numeric(x)-mediana),na.rm=TRUE)
          res
        }
        
        RCV <- function(x){
          FN <- fivenum(as.numeric(x))
          Finf <- FN[2]
          Fsup <- FN[4]
          res <- (Fsup-Finf)/(Finf+Fsup)
          res
        }
        
        H1 <- function(x) {
          FN <- fivenum(as.numeric(x))
          Finf <- FN[2]
          Md <- FN[3]
          Fsup <- FN[4]
          res <- (Finf+Fsup-2*Md)/(2*Md)
          res
        }
        H3 <- function(x) {
          AC90 <- quantile(as.numeric(x),probs=0.9,na.rm=TRUE)
          AC10 <- quantile(as.numeric(x),probs=0.1,na.rm=TRUE)
          Md <- fivenum(as.numeric(x))[3]
          res <- (AC90+AC10-2*Md)/(2*Md)
          res
        }
        K2 <- function(x){
          QU <- quantile(as.numeric(x),na.rm=TRUE) 
          AC90 <- quantile(as.numeric(x),probs=0.9,na.rm=TRUE)
          AC10 <- quantile(as.numeric(x),probs=0.1,na.rm=TRUE)
          Q1 <- QU[2]
          Q3 <- QU[4]
          res <- (AC90-AC10)/(1.9*(Q3-Q1))        
          res
        }
        K3 <- function(x){
          FN <- fivenum(as.numeric(x))
          Einf <- quantile(as.numeric(x),probs=0.125,na.rm=TRUE)
          Esup <- quantile(as.numeric(x),probs=0.875,na.rm=TRUE)
          Finf <- FN[2]
          Fsup <- FN[4]
          res <- (Esup-Einf)/(1.7*(Fsup-Finf))        
          res
        }
        
        
        if ( self$options$median )
          stats[['median']] <- median(as.numeric(ni),na.rm=TRUE)
        
        if ( self$options$mode )
          stats[['mode']] <- 
          names(table(column)[ table(column) == max(table(column)) ])
        
        if ( self$options$trimean )
          stats[['trimean']] <- trimedia(column)
        
        if ( self$options$avquartile )
          stats[['avquartile']] <- promcuar(column)
        
        if ( self$options$midrange )
          stats[['midrange']] <- midR(column)
        
        if ( self$options$range )
          stats[['range']] <- rango(column)
        
        if ( self$options$iqr )
          stats[['iqr']] <- IQR(column)
        
        if (self$options$qd)
          stats[['qd']] <- QD(column)
        
        if (self$options$mad)
          stats[['mad']] <- MAD(column)
        
        if (self$options$rcv)
          stats[['rcv']] <- RCV(column)
        
        if(self$options$min)
          stats[['min']] <- min(as.numeric(column), na.rm=TRUE)
        
        if(self$options$max)
          stats[['max']] <- max(as.numeric(column), na.rm=TRUE)
        
        if (self$options$q1)
          stats[['q1']] <- quantile(as.numeric(column),na.rm=TRUE)[2]
        
        if (self$options$q2)
          stats[['q2']] <- quantile(as.numeric(column),na.rm=TRUE)[3]
        
        if (self$options$q3)
          stats[['q3']] <- quantile(as.numeric(column),na.rm=TRUE)[4]
        
        if(self$options$pc){
          pcValues <- private$.getPcValues()
          npcValues <- length(pcValues)
          
          if( npcValues > 0 ){
            quants <- quantile(as.numeric(column), pcValues)
            for(i in 1:npcValues)
              stats[[paste0('perc', i)]] <- quants[i]
          }
        }
        
        if(self$options$irange){
          NPerc <- self$options$NPerc
          nNPerc <- length(NPerc)
          
          if( nNPerc > 0 ){
            .ACinf <- quantile(as.numeric(column),probs=(1-nNPerc)/2,na.rm=TRUE)
            .ACsup <- quantile(as.numeric(column),probs=1-(1-nNPerc)/2,na.rm=TRUE)
            res <- .ACsup-.ACinf
            stats[[paste0('ir')]] <- res
          }
        }
        
        if (self$options$h1)
          stats[['h1']] <- H1(column)
        
        if (self$options$h3)
          stats[['h3']] <- H3(column)
        
        if (self$options$k2)
          stats[['k2']] <- K2(column)
        
        if (self$options$k3)
          stats[['k3']] <- K3(column)
        
      }
      else if (n==0) {
        l <- list(
          n=NaN, missing=NaN, median=NaN, mode=NaN, trimean=NaN, avquartile=NaN, midrange=NaN,
          range=NaN, iqr=NaN, qd=NaN, mad=NaN, rcv=NaN, min=NaN, max=NaN, q1=NaN, q2=NaN, q3=NaN,
          h1=NaN, h3=NaN, k2=NaN, k3=NaN
        )
      }
      return(stats)  
    }
  )
)
