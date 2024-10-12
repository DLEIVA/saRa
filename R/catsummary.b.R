
# This file is a generated template, your changes will not be overwritten

catSummaryClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "catSummaryClass",
    inherit = catSummaryBase,
    private = list(
      #### Member variables ----
      colArgs = NA,
      .levels = NULL,
      #### Init + run functions ----
      .init = function() {
        private$colArgs <- list(
          name = c(
            "mode","VR","Blau","Teachman","IQV"
          ),
          title = c(
            "Mode","Variation ratio","Blau","Teachman","IQV"
          ),
          superTitle = c(
            "","","","",""
          ),
          type = c("text","number","number","number","number"),
          format = c("","","","",""),
          visible = c(
            "(mode)","(VR)","(Blau)","(Teachman)","(IQV)"
          )
        )
        private$.initFreqTable()
        private$.initIndicesTable()
        private$.initPlots()
      },
      .run = function(){
        if (length(self$options$vars) > 0) {
          results <- private$.compute()
          private$.populateFreqTable(results)
          private$.populateIndicesTable(results)
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
              freq[[var]] <- table(column,useNA='always')
            #}
              indic[[var]] <- private$.computeIndices(column)
          }
          
          return(list(indic=indic, freq=freq))
        },
      #### Init tables ----
      .initFreqTable = function() {
        if ( ! self$options$freq)
          return()
        
        tables <- self$results$frequencies
        vars <- self$options$vars
        
        for (i in seq_along(vars)) {
          var <- vars[i]
          column <- self$data[[var]]
          
          if (private$.treatAsFactor(column)) {
            table <- tables$get(var)
            levels <- c(base::levels(column),'Missings')
            if (is.null(levels)) {
              levels <- c(levels(factor(naOmit(column))),'Missings')
              table$setVisible(TRUE)
            }
            
              table$addColumn(
                name='levels', title='Levels', type='text'
              )
              table$addColumn(
                name='counts', title='Counts', type='integer'
              )
              table$addColumn(
                name='cumcounts', title='Cumulative Counts', type='integer'
              )
              table$addColumn(
                name='prop', title='Proportions', type='number'
              )
              table$addColumn(
                name='cumprop', title='Cumulative Proportions', type='number'
              )              
              table$addColumn(
                name='pc', title='% of Total', type='number', format='pc'
              )
              table$addColumn(
                name='cumpc', title='Cumulative %', type='number', format='pc'
              )
              table$addColumn(
                name='odds', title='Odds', type='number', format='number'
              )
              
              for (k in seq_along(levels)) {
                table$addRow(
                  levels[k], values = list(levels = levels[k])
                )
              }
            }
          }
        },
      .initIndicesTable = function() {
        table <- self$results$indices
        
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
        
        if (self$options$pie) {

          image <- jmvcore::Image$new(
            options = self$options,
            name = "pie",
            renderFun = ".piePlot",
            width = 600,
            height= 600,
            clearWith=list("pie")
          )
          
          group$add(image)
        }
        
        if (self$options$dough) {
          
          image <- jmvcore::Image$new(
            options = self$options,
            name = "dough",
            renderFun = ".doughnutPlot",
            width = 600,
            height= 600,
            clearWith=list("dough")
          )
          
          group$add(image)
        }
        
        if (self$options$lolli) {
          
          image <- jmvcore::Image$new(
            options = self$options,
            name = "lolli",
            renderFun = ".lolliPlot",
            width = 600,
            height= 600,
            clearWith=list("lolli","f2p.lolli")
          )
          
          group$add(image)
        }
        
        if (self$options$pareto) {
          
          image <- jmvcore::Image$new(
            options = self$options,
            name = "pareto",
            renderFun = ".paretoPlot",
            width = 600,
            height= 600,
            clearWith=list("pareto")
          )
          
          group$add(image)
        }           
        
        if (self$options$bar) {
          
          image <- jmvcore::Image$new(
            options = self$options,
            name = "bar",
            renderFun = ".barPlot",
            width = 600,
            height= 600,
            clearWith=list("bar","f2p.bar")
          )
          
          group$add(image)
        }
        }
        },
      #### Populate tables ----
      .populateFreqTable = function(results) {
        if ( ! self$options$freq)
          return()
        
        tables <- self$results$frequencies
        vars <- self$options$vars
        freqs <- results$freq
        
        for (i in seq_along(vars)) {
          var <- vars[i]
          column <- self$data[[var]]
          
          if (private$.treatAsFactor(column)) {
            table <- tables$get(var)
            
            if ( ! table$visible)
              next()
            
            levels <- c(base::levels(column),'Missings')
            if (is.null(levels)) {
              levels <- c(levels(factor(naOmit(column)),'Missings'))
              table$setVisible(TRUE)
            }
            freq <- freqs[[var]]
            
            n <- sum(freq)
            cumcounts <- 0
            cumprop <- 0
            cumpc <- 0
            for (k in seq_along(levels)) {
              counts <- as.numeric(freq[[k]])
              cumcounts <- cumcounts + counts
              prop <- counts/n
              cumprop <- cumprop + prop 
              pc <- prop
              cumpc <- cumpc + counts/n
              odds <- prop/(1-prop)
              #if (is.na(pc)) pc <- 0
              #if (is.na(cumpc)) cumpc <- 0
              table$setRow(
                rowNo=k,
                values=list(
                  counts=counts,
                  cumcounts=cumcounts,
                  prop=prop,
                  cumprop=cumprop,
                  pc=pc,
                  cumpc=cumpc,
                  odds=odds
                  )
                )
              }
          }
        }
      },
      .populateIndicesTable = function(results) {
#        if (self$options$indic != "columns")
#          return()
        
        table <- self$results$indices
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
      .preparePlots = function() {
        data <- self$data
        plots <- self$results$plots
        vars <- self$options$vars
        
          for (i in seq_along(vars)) {
            var <- vars[i]
            group <- plots$get(var)
            column <- data[[var]]

            if (self$options$pie) {
              pie  <- group$get('pie')
              levels <- base::levels(column)
              names <- list("x"="x", "y"="y","cumulative"="cumulative",
                            "pct"="pct","pos"="pos", "label"="label")
              
              lbls <- names(table(column))
              
              cts <- unlist(table(column))  
              
              plotData <- data.frame(cts, stringsAsFactors = FALSE)
              colnames(plotData) <- c('x','y')
              plotData$label <- paste0(cts,' (',round(cts/sum(cts)*100,2),
                                       "%)")
              plotData <- plotData[order(plotData$y,decreasing=TRUE), ]
              plotData$x <- factor(plotData$x, levels=plotData$x)
              plotData$cumulative <-  cumsum(plotData$y)
              plotData$pct <-  round(plotData$y/sum(plotData$y)*100,2)
              
              plotData <- plotData |> 
                dplyr::mutate(csum = rev(cumsum(rev(y))), 
                       pos = y/2 + dplyr::lead(csum, 1),
                       pos = dplyr::if_else(is.na(pos), y/2, pos))
              
              labels <- list("x"=var)
              
              type <- `if`(is.factor(column), 'categorical')
              pie$setState(list(data=plotData, names=names, labels=labels, 
                                type=type))
            }
            
            if (self$options$dough) {
              dough  <- group$get('dough')
              levels <- base::levels(column)
              names <- list("x"="x", "y"="y","cumulative"="cumulative",
                            "pct"="pct","ymin"="ymin", "label"="label",
                            "labelPosition"="labelPosition")
              
              lbls <- names(table(column))
              
              cts <- unlist(table(column))  
              
              plotData <- data.frame(cts, stringsAsFactors = FALSE)
              colnames(plotData) <- c('x','y')
              plotData <- plotData[order(plotData$y,decreasing=TRUE), ]
              plotData$x <- factor(plotData$x, levels=plotData$x)
              plotData$cumulative <-  cumsum(plotData$y)
              plotData$pct <-  round(plotData$y/sum(plotData$y)*100,2)
              
              plotData$ymin <- c(0, head(plotData$cumulative, n=-1))
              plotData$labelPosition <- (plotData$cumulative + plotData$ymin) /2
              plotData$label <- paste0(plotData$x, "\n ", plotData$y,
                                       ' (',plotData$pct,'%)')
              
              labels <- list("x"=var)
              
              type <- `if`(is.factor(column), 'categorical')
              dough$setState(list(data=plotData, names=names, labels=labels, 
                                  type=type))
            }
            
            if (self$options$lolli) {
              lolli  <- group$get('lolli')
              freqtype <- self$options$f2p.lolli
              levels <- base::levels(column)
              names <- list("x"="x", "y"="y")
              if(freqtype=='absolutefreq'){
                cts <- unlist(table(column))  
              } else{
                cts <- unlist(table(column)/sum(table(column))*100)
              }
              
              lbls <- names(table(column))
              plotData <- data.frame(cts, stringsAsFactors = FALSE)
              colnames(plotData) <- c('x','y')
              plotData <- plotData[order(plotData$y,decreasing=TRUE), ]
              plotData$x <- factor(plotData$x, levels=plotData$x)
              
              plotData <- plotData |> 
                dplyr::arrange(y) |> 
                dplyr::mutate(x=factor(x, x))
              
              labels <- list("x"=var)
              
              type <- `if`(is.factor(column), 'categorical')
              lolli$setState(list(data=plotData, names=names, labels=labels, 
                                freqtype=freqtype, type=type))
            }
            
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

            if (self$options$pareto) {
              pareto  <- group$get('pareto')
              levels <- base::levels(column)
              names <- list("x"="x", "y"="y","cumulative"="cumulative",
                            "pct"="pct","pos"="pos")
              
              lbls <- names(table(column))
              
              cts <- unlist(table(column))  
              
              plotData <- data.frame(cts, stringsAsFactors = FALSE)
              colnames(plotData) <- c('x','y')

              plotData <- plotData[order(plotData$y,decreasing=TRUE), ]
              plotData$x <- factor(plotData$x, levels=plotData$x)
              plotData$cumulative <-  cumsum(plotData$y)
              plotData$pct <-  round(plotData$y/sum(plotData$y)*100,2)
              
              scaleRight <- 100/sum(plotData$y)
              
              labels <- list("x"=var)
              
              type <- `if`(is.factor(column), 'categorical')
              pareto$setState(list(data=plotData, names=names, labels=labels, 
                                type=type, scaleRight=scaleRight))
            }            
        }
        },

      .piePlot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        data <- image$state$data
        names <- image$state$names
        labels <- image$state$labels
        type <- image$state$type
        fill <- theme$fill[2]
        color <- theme$color[1]
        plotSpecificTheme <- ggplot2::theme_void() +
          ggplot2::theme(
            axis.title.x = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_blank(),
            panel.border = ggplot2::element_blank(),
            panel.grid=ggplot2::element_blank(),
            legend.title = ggplot2::element_blank(),
            legend.key.size = grid::unit(1, 'cm'),
            legend.text = ggplot2::element_text(size=14),
            axis.ticks = ggplot2::element_blank(),
            plot.title=ggplot2::element_text(size=14, face="bold")
          )
        
        plot <- ggplot2::ggplot(data, ggplot2::aes_string(x=1, y=names$y, fill=names$x))+
          ggplot2::geom_bar(stat='identity',width = 1)+
          ggplot2::coord_polar("y") +
          ggplot2::scale_fill_brewer(palette=1) +
          ggplot2::theme(axis.text.x=ggplot2::element_blank())+
          ggplot2::guides(fill=ggplot2::guide_legend(override.aes=ggplot2::aes(label = ''))) +
          ggrepel::geom_label_repel(data = data,
                           ggplot2::aes_string(y = names$pos, label = names$label),
                                      nudge_x = .6, show.legend = FALSE, size = 3.5)
        
        plot <- plot + ggtheme + plotSpecificTheme
        return(plot)
      },
      .doughnutPlot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        data <- image$state$data
        names <- image$state$names
        labels <- image$state$labels
        type <- image$state$type
        fill <- theme$fill[2]
        color <- theme$color[1]
        plotSpecificTheme <- ggplot2::theme_void() + ggplot2::theme(legend.position='none')
        
        plot <- ggplot2::ggplot(data, ggplot2::aes_string(ymax=names$cumulative, 
                                        ymin=names$ymin, xmax=4, xmin=3, 
                                        fill=names$x)) +
          ggplot2::geom_rect() +
          ggplot2::geom_text( x=1.5, ggplot2::aes_string(y=names$labelPosition, label=names$label,
                                       color=names$x), size=4.5) +
          ggplot2::coord_polar(theta="y") +
          ggplot2::xlim(c(-1, 4)) +
          ggplot2::scale_fill_brewer(palette=1) +
          ggplot2::scale_color_brewer(palette=1)
        
        plot <- plot + ggtheme + plotSpecificTheme
        return(plot)
      },
      .lolliPlot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
        return(FALSE)
  
        data <- image$state$data
        names <- image$state$names
        labels <- image$state$labels
        type <- image$state$type
        freqtype <- image$state$freqtype
        #fill <- theme$fill[2]
        #color <- theme$color[1]
        #pd <- position_dodge(0.85)
        plotSpecificTheme <- NULL
  
        plot <- ggplot2::ggplot(data=data,ggplot2::aes_string(x=names$x,y=names$y)) +
          ggplot2::geom_segment(ggplot2::aes_string(x=names$x, xend=names$x,y=0,yend=names$y), color='grey') +
          ggplot2::geom_point(size=3,color='blue') +
          ggplot2::coord_flip() +
          ggplot2::theme(
            panel.grid.minor.y = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_blank(),
            legend.position="none",
          ) +
          ggplot2::xlab("") +
          ggplot2::ylab(if(freqtype=='absolutefreq') 'Counts' else
            'Percentages')
        
        
          plot <- plot + ggtheme + plotSpecificTheme
          return(plot)
      },
      .paretoPlot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        data <- image$state$data
        names <- image$state$names
        labels <- image$state$labels
        type <- image$state$type
        scaleRight <- image$state$scaleRight
        fill <- theme$fill[2]
        color <- theme$color[1]
        plotSpecificTheme <- ggplot2::theme_classic() + 
          ggplot2::theme(axis.text=ggplot2::element_text(size=12),
                axis.title=ggplot2::element_text(size=14),
                legend.position = 'none')
        
        plot <- ggplot2::ggplot(data, ggplot2::aes_string(x=names$x)) +
          ggplot2::geom_bar(ggplot2::aes_string(y=names$y),fill=fill,
                   color=color,stat='identity') +
          ggplot2::geom_point(ggplot2::aes_string(y=names$cumulative),col='grey',pch=16,size=4) +
          ggplot2::geom_path(ggplot2::aes_string(y=names$cumulative,group=1),lty=2,col='grey',
                    size=0.7) +
          ggplot2::scale_y_continuous(sec.axis=ggplot2::sec_axis(~.*scaleRight, 
                                               name = "Cumulative percent (%)",
                                               breaks=seq(0,100,20))) +
          ggplot2::ylab('Counts') + ggplot2::xlab('')
          
        plot <- plot + ggtheme + plotSpecificTheme
        return(plot)
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
          ggplot2::ggplot(data=data, ggplot2::aes_string(x=names$x, y=names$y)) +
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
      #### Helper functions ----
      .treatAsFactor = function(column) {
        if (is.factor(column))
          return(TRUE)
        
        nUniques <- length(unique(column))
        if (nUniques > 0 && nUniques <= 10)
          return(TRUE)
        else
          return(FALSE)
      },
      .skipOption = function(visible) {
        return(! self$options[[ gsub("[()]", "", visible) ]])
      },
      .computeIndices = function(column) {
        stats <- list()
        
        total <- length(column)
        column <- jmvcore::naOmit(column)
        n <- length(column)
        
        if (n > 0) {
          ni <- table(column)
          if ( self$options$VR )
            stats[['VR']] <- 1 - max(ni, na.rm = TRUE)/sum(ni, na.rm = TRUE)

          if ( self$options$mode )
            stats[['mode']] <- 
              names(table(column)[ table(column) == max(table(column)) ])
          
          if ( self$options$Blau )
            stats[['Blau']] <- 1 - sum(prop.table(ni)^2)
          
          if ( self$options$Teachman)
            stats[['Teachman']] <- -sum(prop.table(ni)[prop.table(ni)!=0]*log(prop.table(ni)[prop.table(ni)!=0]))
          
          if ( self$options$IQV){
            NCat <- self$options$NCat
            if (NCat == 0) {
              NCat <- length(ni)
            } else{
              if(NCat < length(ni)){
                NCat <- length(ni)
              }
              if(is.na(NCat | NCat < 1)){
                NCat <- length(ni)
              }
            }
            
            stats[['IQV']] <- (1 - sum(prop.table(ni)^2))/((NCat - 1)/NCat)        
          }
            
        } else if (n==0) {
          l <- list(
            VR=NaN, mode=NaN, Blau=NaN, IQV=NaN
          )
        }
        
        #stats <- append(stats, l)
        return(stats)
      }
      )
)