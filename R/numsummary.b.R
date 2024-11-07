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
          "Variance","Standard Deviation", "Geometric standard deviation",
          "Mean deviation","CV", "Range", "IQR", "QD", "MAD", 
          "rCV", paste0(ifelse(!is.na(private$.getNPerc()*100),
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
            width = 550,
            height = 550,
            clearWith = list("hist", "dens", "norm")
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
        
        if(!is.numeric(column))
          stop(paste0("The variable ", var,
                      " cannot be treated as numeric. 
                      Plots that expect numeric data will not be created for this variable."))
        
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
            hist$setState(list(data=plotData, names=names, labels=labels))
          
          if (self$options$box || self$options$violin || self$options$dot)
            box$setState(list(data=plotData, names=names, labels=labels))
        }
      }
    },
    .histogram = function(image, ggtheme, theme, ...) {
      if (is.null(image$state))
        return(FALSE)
      
      data <- image$state$data
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
        binWidth <- range / nBins
      else
        binWidth <- range / (nUniques - 1)
      
      plot <- ggplot2::ggplot(data=data, ggplot2::aes_string(x=names$x)) +
        ggplot2::labs(x=labels$x, y='density') +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
      
      if (self$options$hist)
        plot <- plot + ggplot2::geom_histogram(
          ggplot2::aes(y=..density..),
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
      
      plot <- ggplot2::ggplot(data=data, ggplot2::aes_string(x=x, y=names$x)) +
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
            fun.y=mean,
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
      if (is.null(image$state))
        return(FALSE)
      
      var <- image$state
      data <- self$data
      
      y <- data[[var]]
      y <- as.vector(scale(y))
      data <- data.frame(y=y)
      
      data <- na.omit(data)
      
      geom_qq_band <- function(
      mapping = NULL,
      data = NULL,
      stat = "qq_band",
      position = "identity",
      na.rm = TRUE,
      show.legend = NA,
      inherit.aes = TRUE,
      distribution = "norm",
      dparams = list(),
      qtype = 7,
      qprobs = c(.25, .75),
      conf = .95,
      mu = NULL,
      sigma = NULL,
      ...
        ) {
          ggplot2::layer(
            data = data,
            mapping = mapping,
            stat = stat,
            geom = GeomQqBand,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
              na.rm = na.rm,
              distribution = distribution,
              dparams = dparams,
              qtype = qtype,
              qprobs = qprobs,
              conf = conf,
              mu = mu,
              sigma = sigma,
              ...
            )
          )
        }
        
        #' GeomQqBand

        GeomQqBand <- ggplot2::ggproto(
          `_class` = "GeomQqBand",
          `_inherit` = ggplot2::Geom,
          
          default_aes = ggplot2::aes(
            width = 0.75,
            linetype = "solid",
            fontsize = 5,
            shape = 19,
            colour = NA,
            size = .1,
            fill = "blue",
            alpha = .8,
            stroke = 0.1,
            linewidth = .1,
            weight = 1,
            x = NULL,
            y = NULL,
            conds = NULL
          ),
          
          required_aes = c("x", "ymin", "ymax"),
          
          setup_data = function(data, params) {
            data
          },
          
          draw_group = ggplot2::GeomRibbon$draw_group,
          
          draw_key = ggplot2::draw_key_polygon
        )      
      
      #' Quantile-quantile points
      #'
      #' Draws quantile-quantile points, functions and stats taken from qqplotr package.
      
    .stat_qq_point = function(
    mapping = NULL,
    data = NULL,
    geom = "point",
    position = "identity",
    na.rm = TRUE,
    show.legend = NA,
    inherit.aes = TRUE,
    distribution = "norm",
    dparams = list(),
    qtype = 7,
    qprobs = c(.25, .75),
    ...
      ) {
        ggplot2::layer(
          data = data,
          mapping = mapping,
          stat = .StatQqPoint,
          geom = geom,
          position = position,
          show.legend = show.legend,
          inherit.aes = inherit.aes,
          params = list(
            na.rm = na.rm,
            distribution = distribution,
            dparams = dparams,
            qtype = qtype,
            qprobs = qprobs,
            ...
          )
        )
      }
      #' StatQqPoint
      
      .StatQqPoint = ggplot2::ggproto(
        `_class` = ".StatQqPoint",
        `_inherit` = ggplot2::Stat,
        
        default_aes = ggplot2::aes(
          x = ..theoretical..,
          y = ..sample..
        ),
        
        required_aes = c("sample"),
        
        optional_aes = c("label"),
        
        compute_group = function(data,
                                 self,
                                 scales,
                                 distribution = "norm",
                                 dparams = list(),
                                 qtype = 7,
                                 qprobs = c(.25)) {
          # distributional function
          qFunc <- eval(parse(text = paste0("q", distribution)))
          
          samp <- data$sample
          
          oidx <- order(samp)
          smp <- samp[oidx]
          n <- length(smp)
          quantiles <- ppoints(n)
          
          dparams <- MASS::fitdistr(x = smp, densfun = "normal")$estimate
          
          theoretical <- do.call(qFunc, c(list(p = quantiles), dparams))
          
          out <- data.frame(sample = smp, theoretical = theoretical)
          
          if (!is.null(data$label)) out$label <- data$label[oidx]
          out
        }
      )
      #' Quantile-quantile lines
      #'
      #' Draws a quantile-quantile line, functions and stats taken from qqplotr package.
      
      .stat_qq_line = function(
    mapping = NULL,
    data = NULL,
    geom = "path",
    position = "identity",
    na.rm = TRUE,
    show.legend = NA,
    inherit.aes = TRUE,
    distribution = "norm",
    dparams = list(),
    qtype = 7,
    qprobs = c(.25, .75),
    ...
      ){
        ggplot2::layer(
          data = data,
          mapping = mapping,
          stat = .StatQqLine,
          geom = geom,
          position = position,
          show.legend = show.legend,
          inherit.aes = inherit.aes,
          params = list(
            na.rm = na.rm,
            distribution = distribution,
            dparams = dparams,
            qtype = qtype,
            qprobs = qprobs,
            ...
          )
        )
      }
      
      #' StatQqLine
      
      .StatQqLine = ggplot2::ggproto(
        `_class` = ".StatQqLine",
        `_inherit` = ggplot2::Stat,
        
        required_aes = c("sample"),
        
        dropped_aes = c("sample"),
        
        default_aes = ggplot2::aes(
          x = ..xline..,
          y = ..yline..
        ),
        
        compute_group = {
          function(data,
                   self,
                   scales,
                   distribution,
                   dparams,
                   qtype,
                   qprobs) {
            # distributional function
            qFunc <- eval(parse(text = paste0("q", distribution)))
            
            smp <- sort(data$sample)
            n <- length(smp)
            quantiles <- ppoints(n)
            
            dparams <- MASS::fitdistr(x = smp, densfun = "normal")$estimate
            
            theoretical <- do.call(qFunc, c(list(p = quantiles), dparams))
            
            xCoords <- do.call(qFunc, c(list(p = qprobs), dparams))
            yCoords <- do.call(quantile, list(x = smp, probs = qprobs, type = qtype))
            slope <- diff(yCoords) / diff(xCoords)
            intercept <- yCoords[1] - slope * xCoords[1]
            
            out <- data.frame(xline = c(min(theoretical), max(theoretical)))
            out$yline <- slope * out$xline + intercept
            
            out$size <- .8
            
            out
          }
        }
      )
      
      #' Quantile-quantile confidence bands
      #'
      #' Draws quantile-quantile confidence bands, functions and stats taken from qqplotr package.
      
      .stat_qq_band = function(
    mapping = NULL,
    data = NULL,
    geom = "qq_band",
    position = "identity",
    na.rm = TRUE,
    show.legend = NA,
    inherit.aes = TRUE,
    distribution = "norm",
    dparams = list(),
    qtype = 7,
    qprobs = c(.25, .75),
    conf = .95,
    mu = NULL,
    sigma = NULL,
    ...
      ) {
        
        ggplot2::layer(
          data = data,
          mapping = mapping,
          stat = .StatQqBand,
          geom = geom,
          position = position,
          show.legend = show.legend,
          inherit.aes = inherit.aes,
          params = list(
            na.rm = na.rm,
            distribution = distribution,
            dparams = dparams,
            qtype = qtype,
            qprobs = qprobs,
            conf = conf,
            mu = mu,
            sigma = sigma,
            ...
          )
        )
      }
      
      #' StatQqBand
      
      .StatQqBand = ggplot2::ggproto(
        `_class` = ".StatQqBand",
        `_inherit` = .StatQqLine,
        
        default_aes = ggplot2::aes(
          x = ..x..,
          ymin = ..lower..,
          ymax = ..upper..
        ),
        
        required_aes = c("sample"),
        
        dropped_aes = c("sample"),
        
        compute_group = {
          function(data,
                   self,
                   scales,
                   distribution,
                   dparams,
                   qtype,
                   qprobs,
                   conf,
                   mu,
                   sigma) {
            
            # distributional functions
            dFunc <- eval(parse(text = paste0("d", distribution)))
            qFunc <- eval(parse(text = paste0("q", distribution)))
            rFunc <- eval(parse(text = paste0("r", distribution)))
            
            smp <- sort(data$sample)
            n <- length(smp)
            quantiles <- ppoints(n)
            
            dparams <- MASS::fitdistr(x = smp, densfun = "normal")$estimate
            
            theoretical <- do.call(qFunc, c(list(p = quantiles), dparams))
            
            # inherit from StatQqLine
            xline <- self$super()$compute_group(data = data,
                                    distribution = distribution,
                                    dparams = dparams,
                                    qtype = qtype,
                                    qprobs = qprobs)$xline
            yline <- self$super()$compute_group(data = data,
                                    distribution = distribution,
                                    dparams = dparams,
                                    qtype = qtype,
                                    qprobs = qprobs)$yline
            
            slope <- diff(yline) / diff(xline)
            intercept <- yline[1L] - slope * xline[1L]
            
            fittedValues <- (slope * theoretical) + intercept
            
            
            probs <- ppoints(n)
            stdErr <- (slope / do.call(dFunc, c(list(x = theoretical), dparams))) * sqrt(probs * (1 - probs) / n)
            zCrit <- qnorm(p = (1 - (1 - conf) / 2))
            
            upper <- fittedValues + (stdErr * zCrit)
            lower <- fittedValues - (stdErr * zCrit)
            
            out <- data.frame(
              x = theoretical,
              upper = upper,
              lower = lower,
              fill = if (is.null(data$fill)) rgb(.6, .6, .6, .5) else data$fill
            )
            
            out
          }
        }
      )      
      
      plot <- ggplot2::ggplot(data=data, mapping = ggplot2::aes(sample = y)) +
        #geom_abline(slope=1, intercept=0, colour=theme$color[1]) +
        #stat_qq(aes(sample=y), size=2, colour=theme$color[1]) +
        .stat_qq_band() +        
        .stat_qq_line() +
        .stat_qq_point() +
        ggplot2::xlab("Theoretical Quantiles") +
        ggplot2::ylab("Standardized Residuals") +
        ggtheme
      
      return(plot)
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
          private$colArgs$title <- append(colArgs$title, paste0(round(pcValues * 100, 2), ('th Pct')), after = 27)
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
      stats <- list()
      
      total <- length(column)
      column <- jmvcore::naOmit(column)
      n <- length(column)
      stats[['n']] <- n
      stats[['missing']] <- total - n
      
      if (n > 0) {
        mediageom <- function(x) {
          if(is.factor(x)){ res <- NA} else{
            res <- ifelse(sum(x<=0,na.rm = TRUE)==0,round(mean(log(x), 
                                                               na.rm = TRUE), 2),NA)
          }
          res
        }
        
        trimedia <- function(x){
          if(is.factor(x)){ res <- NA} else{
            FN <- fivenum(x)
            Finf <- FN[2]
            Md <- FN[3]
            Fsup <- FN[4]
            res <- (Finf+2*Md+Fsup)/4
          }
          res
        }
        
        promcuar <- function(x){
          if(is.factor(x)){ res <- NA} else{
            FN <- fivenum(x,na.rm=TRUE)
            Q1 <- FN[2]
            Q3 <- FN[4]
            res <- (Q1+Q3)/2
          }
          res
        }
        midR <- function(x){
          if(is.factor(x)){ res <- NA} else{
            res <- (min(x)+max(x))/2
          }
          res
        }
        
        sdgeom <- function(x) {
          if(is.factor(x)){ res <- NA} else{
            res <- ifelse(sum(x<=0,na.rm = TRUE)==0,sd(log(x), 
                                                       na.rm = TRUE),NA)
          }
          res
        } 
        
        desvmed <- function(x) {
          if(is.factor(x)){ res <- NA} else{
            .media <- mean(x, na.rm = TRUE)
            res <- sum(abs(x - .media), na.rm = TRUE)/length(na.omit(x))
          }
          res
        }
        
        CV <- function(x) {
          if(is.factor(x)){ res <- NA} else{
            .media <- mean(x, na.rm = TRUE)
            .dt <- sd(x, na.rm = TRUE)
            res <- .dt/.media
          }
          res
        }        
        
        rango <- function(x){
          if(is.factor(x)){ res <- NA} else{
            res <- max(x)-min(x)
          }
          res
        }
        
        IQR <- function(x){
          if(is.factor(x)){ res <- NA} else{
            qtls <- quantile(x,na.rm=TRUE) 
            Q1 <- qtls[2]
            Q3 <- qtls[4]
            res <- Q3-Q1
          }
          res
        }
        
        QD <- function(x){
          if(is.factor(x)){ res <- NA} else{
            qtls <- quantile(x,na.rm=TRUE) 
            Q1 <- qtls[2]
            Q3 <- qtls[4]
            res <- (Q3-Q1)/2
          }
          res
        }
        
        MAD <- function(x){
          if(is.factor(x)){ res <- NA} else{
            mediana <- median(x,na.rm=TRUE)
            res <- median(abs(x-mediana),na.rm=TRUE)
          }
          res
        }
        
        RCV <- function(x){
          if(is.factor(x)){ res <- NA} else{
            FN <- fivenum(x)
            Finf <- FN[2]
            Fsup <- FN[4]
            res <- (Fsup-Finf)/(Finf+Fsup)
          }
          res
        }
        
        beta1 <- function(x) {
          if(is.factor(x)){ res <- NA} else{
            .media <- mean(x, na.rm = TRUE)
            .n <- length(na.omit(x))
            res <- (sum((x - .media)^3, na.rm = TRUE)/.n)^2/(sum((x -.media)^2,
                                                                 na.rm = TRUE)/.n)^3
          }
          res
        }
        
        gamma1 <- function(x) {
          if(is.factor(x)){ res <- NA} else{
            .media <- mean(x, na.rm = TRUE)
            .dt <- sd(x, na.rm = TRUE)
            .n <- length(na.omit(x))
            res <- .n * sum((x - .media)^3, na.rm = TRUE)/((.n - 1) * (.n - 2))/(.dt^3)
          }
          res
        }
        
        beta2 <- function(x) {
          if(is.factor(x)){ res <- NA} else{
            .media <- mean(x, na.rm = TRUE)
            .n <- length(na.omit(x))
            res <- sum((x - .media)^4, na.rm = TRUE)/.n/(sum((x - .media)^2,
                                                             na.rm = TRUE)/.n)^2
          }
          res
        }
        
        gamma2 <- function(x) {
          if(is.factor(x)){ res <- NA} else{
            .media <- mean(x, na.rm = TRUE)
            .dt <- sd(x, na.rm = TRUE)
            .n <- length(na.omit(x))
            res <- (.n * (.n + 1) * sum((x - .media)^4, na.rm = TRUE)/
                      ((.n - 1) * (.n - 2) * (.n - 3)) - 3 * sum((x - .media)^2,
                                                                 na.rm = TRUE)^2/((.n - 2) * (.n - 3)))/(.dt^4)
          }
          res
        }        
        
        H1 <- function(x) {
          if(is.factor(x)){ res <- NA} else{
            FN <- fivenum(as.numeric(x))
            Finf <- FN[2]
            Md <- FN[3]
            Fsup <- FN[4]
            res <- (Finf+Fsup-2*Md)/(2*Md)
          }
          res
        }
        
        H3 <- function(x) {
          if(is.factor(x)){ res <- NA} else{
            AC90 <- quantile(as.numeric(x),probs=0.9,na.rm=TRUE)
            AC10 <- quantile(as.numeric(x),probs=0.1,na.rm=TRUE)
            Md <- fivenum(as.numeric(x))[3]
            res <- (AC90+AC10-2*Md)/(2*Md)
          }
          res
        }
        
        K2 <- function(x){
          if(is.factor(x)){ res <- NA} else{
            QU <- quantile(as.numeric(x),na.rm=TRUE) 
            AC90 <- quantile(as.numeric(x),probs=0.9,na.rm=TRUE)
            AC10 <- quantile(as.numeric(x),probs=0.1,na.rm=TRUE)
            Q1 <- QU[2]
            Q3 <- QU[4]
            res <- (AC90-AC10)/(1.9*(Q3-Q1))
          }
          res
        }
        
        K3 <- function(x){
          if(is.factor(x)){ res <- NA} else{
            FN <- fivenum(as.numeric(x))
            Einf <- quantile(as.numeric(x),probs=0.125,na.rm=TRUE)
            Esup <- quantile(as.numeric(x),probs=0.875,na.rm=TRUE)
            Finf <- FN[2]
            Fsup <- FN[4]
            res <- (Esup-Einf)/(1.7*(Fsup-Finf))   
          }
          res
        }
        
        if ( self$options$mean )
          stats[['mean']] <- ifelse(is.numeric(column),mean(column,na.rm=TRUE),
                                    NA)
        
        if ( self$options$median )
          stats[['median']] <- ifelse(is.numeric(column),median(column,
                                                                na.rm=TRUE),NA)
        
        if ( self$options$mode )
          stats[['mode']] <- 
          if(is.numeric(column)) {
            names(table(column)[ table(column) == max(table(column)) ])
          } else NA
        
        if ( self$options$geommean )
          stats[['geommean']] <- mediageom(column)          
        
        if ( self$options$trimean )
          stats[['trimean']] <- trimedia(column)
        
        if ( self$options$avquartile )
          stats[['avquartile']] <- promcuar(column)
        
        if ( self$options$midrange )
          stats[['midrange']] <- midR(column)
        
        if(self$options$trimmean){
          Trim <- private$.getTrim()
          nTrim <- sum(!is.na(Trim))
          ifelse(is.numeric(column) && nTrim>0,
                 if( nTrim > 0 ){
                   res <- mean(column, trim = Trim, na.rm = TRUE)
                   stats[[paste0('trimmean')]] <- res
                 },NA)
        }
        
        if(self$options$winsmean){
          Win <- private$.getWin()
          nWin <- sum(!is.na(Win))
          ifelse(is.numeric(column) && nWin>0,
                 if( nWin > 0 ){
                   res <- WRS2::winmean(column, tr = Win, na.rm = FALSE)
                   stats[[paste0('winsmean')]] <- res
                 },NA)
        }        
        
        if ( self$options$variance )
          stats[['variance']] <- ifelse(is.numeric(column),var(column,na.rm=TRUE),
                                        NA)   
        
        if ( self$options$stddev )
          stats[['stddev']] <- ifelse(is.numeric(column),sd(column,na.rm=TRUE),
                                      NA)
        
        if ( self$options$cv )
          stats[['cv']] <- CV(column)
        
        if ( self$options$geomsd )
          stats[['geomsd']] <- sdgeom(column)  
        
        if ( self$options$meandev )
          stats[['meandev']] <- desvmed(column)  
        
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
        
        if(self$options$irange){
          NPerc <- private$.getNPerc()
          nNPerc <- sum(!is.na(NPerc))
          ifelse(is.numeric(column) && nNPerc>0,
                 if( nNPerc > 0 ){
                   .ACinf <- quantile(column,probs=(1-NPerc)/2,na.rm=TRUE)
                   .ACsup <- quantile(column,probs=1-(1-NPerc)/2,na.rm=TRUE)
                   res <- .ACsup-.ACinf
                   stats[[paste0('ir')]] <- res
                 },NA)
        }        
        
        if(self$options$min)
          stats[['min']] <- ifelse(is.numeric(column),min(column, na.rm=TRUE),
                                   NA)
        
        if(self$options$max)
          stats[['max']] <- ifelse(is.numeric(column),max(column, na.rm=TRUE),
                                   NA)
        
        if (self$options$q1)
          stats[['q1']] <- ifelse(is.numeric(column),quantile(column,
                                                              na.rm=TRUE)[2],NA)
        
        if (self$options$q2)
          stats[['q2']] <- ifelse(is.numeric(column),quantile(column,
                                                              na.rm=TRUE)[3],NA)
        
        if (self$options$q3)
          stats[['q3']] <- ifelse(is.numeric(column),quantile(column,
                                                              na.rm=TRUE)[4],NA)
        
        if(self$options$pc){
          pcValues <- private$.getPcValues()
          npcValues <- length(pcValues)
          
          if( npcValues > 0 ){
            if(is.factor(column)){ 
              for(i in 1:npcValues)
                stats[[paste0('perc', i)]] <- NA } else{
                  quants <- quantile(column, pcValues)
                  for(i in 1:npcValues)
                    stats[[paste0('perc', i)]] <- quants[i]
                }
          }
        }
        
        if (self$options$beta1)
          stats[['beta1']] <- beta1(column)
        
        if (self$options$gamma1)
          stats[['gamma1']] <- gamma1(column)
        
        if (self$options$h1)
          stats[['h1']] <- H1(column)
        
        if (self$options$h3)
          stats[['h3']] <- H3(column)        
        
        if (self$options$beta2)
          stats[['beta2']] <- beta2(column)
        
        if (self$options$gamma2)
          stats[['gamma2']] <- gamma2(column)        
        
        if (self$options$k2)
          stats[['k2']] <- K2(column)
        
        if (self$options$k3)
          stats[['k3']] <- K3(column)
        
      }
      else if (n==0) {
        l <- list(
          n=NaN, missing=NaN, mean=NaN, median=NaN, mode=NaN, geommean=NaN, 
          trimean=NaN, avquartile=NaN, midrange=NaN, trimmean=NaN, winsmean= NaN,
          variance=NaN,stddev=NaN,geomsd=NaN,meandev=NaN,cv=NaN,
          range=NaN, iqr=NaN, qd=NaN, mad=NaN, rcv=NaN, ir= NaN, min=NaN, 
          max=NaN, q1=NaN, q2=NaN, q3=NaN,beta1=NaN,gamma1=NaN,
          h1=NaN, h3=NaN,beta2=NaN,gamma2=NaN, k2=NaN, k3=NaN
        )
      }
      return(stats)  
    }
)
)
