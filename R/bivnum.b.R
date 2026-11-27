
# This file is a generated template, your changes will not be overwritten

bivnumClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "bivnumClass",
    inherit = bivnumBase,
    private = list(
      .init=function() {
        private$.initASSOCTable()
        private$.initScatterPlot()
      },      
      .run = function() {
          xVarName <- self$options$xvar
          yVarName <- self$options$yvar
          
          data <- self$data
          
          if (is.null(xVarName) || is.null(yVarName))
            return()
          asocind <- self$results$asocind

          othRowNo <- 1  
          
          datos <- data[c(xVarName,yVarName)]
          datos[] <- lapply(datos,.saraNumeric)
          datos <- datos[complete.cases(datos),,drop=FALSE]
          if(nrow(datos)<2L) {
            asocind$setNote("observations",.("At least two complete observations are required"))
            return()
          }
          
          x <- datos[[1L]]
          y <- datos[[2L]]
          
          constant <- length(unique(x)) < 2L ||
            length(unique(y)) < 2L
          
          covar <- stats::cov(x, y)
          
          if (constant) {
            pearsonr <- spearmanr <- tauk <- NaN
          } else {
            pearsonr <- stats::cor(x, y, method = "pearson")
            spearmanr <- stats::cor(x, y, method = "spearman")
            tauk <- stats::cor(x, y, method = "kendall")
          }
          
          values <- list(
            `v[covar]` = covar,
            `v[pearsonR]` = pearsonr,
            `v[spearmanR]` = spearmanr,
            `v[tauK]` = tauk,
            `v[pearsonR2]` = pearsonr^2,
            `v[spearmanR2]` = spearmanr^2,
            `v[kendallR2]` = tauk^2
          )
          
          asocind$setRow(rowNo = othRowNo, values = values)
          
          correlationColumns <- setdiff(names(values), "v[covar]")
          
          for (column in correlationColumns) {
            cell <- asocind$getCell(
              rowNo = othRowNo,
              col = column
            )
            cell$footnotes <- character(0)
          }
          
          if (constant) {
            note <- .(
              "Correlation is undefined because at least one variable is constant."
            )
            
            for (column in correlationColumns) {
              asocind$addFootnote(
                rowNo = othRowNo,
                col = column,
                note = note
              )
            }
          }  
        },
        #### Init tables ----
        .initASSOCTable = function(){
          asocind <- self$results$asocind
          asocind$addRow(rowKey=1, values=list())
          if((self$options$covar | self$options$pearsonR | self$options$spearmanR |
              self$options$tauK | self$options$pearsonR2 | self$options$spearmanR2 |
              self$options$kendallR2) == FALSE){
            asocind <- self$results$asocind
            asocind$addColumn(
              name=' ',
              title=' ',
              type='text')
          }      
        },    
        #### Plot functions ---- Adapted from scat.b.R in scatr package
        .initScatterPlot = function() {
          image <- self$results$get('scat')
          
          width <- 450
          height <- 450
          
          image$setSize(width * 2, height)
        },        
        .scat = function(image, ggtheme, ...) {
          
          if (! self$options$scat)
            return()
          
          xVarName <- self$options$xvar
          yVarName <- self$options$yvar
          
          if (is.null(xVarName) || is.null(yVarName))
            return()
          
          data <- self$data
          
          data <- data[c(xVarName,yVarName)]
          data[] <- lapply(data,.saraNumeric)
          data <- data[complete.cases(data),,drop=FALSE]
          if(nrow(data)<2L) return(FALSE)
          
          marg <- self$options$marg
          line <- self$options$line
          method <- if (line == 'linear') 'lm' else 'auto'          
          
          p <- ggplot2::ggplot(
            data, ggplot2::aes(x=.data[[xVarName]], y=.data[[yVarName]])) + 
            ggplot2::geom_point(alpha=.8, size=2.5) +
            ggplot2::labs(x=xVarName, y=yVarName) + ggtheme
          
          if (line != 'none') {
            p <- p + ggplot2::geom_smooth(method = method, se = self$options$se
            )
          }
          
          p <- p + ggplot2::theme(legend.position = 'none')
          
          if (marg == 'dens') {
            xdens <- cowplot::axis_canvas(p, axis='x') +
              ggridges::geom_ridgeline(
                data=data, 
                ggplot2::aes(x=.data[[xVarName]], y=0, height=ggplot2::after_stat(density)),
                stat='density', 
                alpha=0.5, 
                linewidth=.2,
                trim=FALSE,
                fill="#ADD8E6"
              ) + ggtheme
            
            ydens <- cowplot::axis_canvas(p, axis='y') +
              ggridges::geom_vridgeline(
                data=data, 
                ggplot2::aes(x=0, y=.data[[yVarName]], width=ggplot2::after_stat(density)),
                stat='ydensity', 
                alpha=0.5, 
                linewidth=.2, 
                trim=FALSE,
                fill ="#ADD8E6"
              ) + ggtheme
            
            p <- cowplot::insert_xaxis_grob(
              p, xdens, grid::unit(.2, "null"), position="top"
            )
            p <- cowplot::insert_yaxis_grob(
              p, ydens, grid::unit(.2, "null"), position="right"
            )
            
            p <- cowplot::ggdraw(p)    
            
          } else if (marg == 'box') {          
            themeBox <- ggplot2::theme(
              panel.grid.major = ggplot2::element_blank(),
              panel.grid.minor = ggplot2::element_blank(),
              strip.background = ggplot2::element_rect(
                fill='transparent', color=NA
              ),
              panel.background=ggplot2::element_rect(
                fill='transparent', color=NA)
            )
            
            xbox <- ggplot2::ggplot(
              data,
              ggplot2::aes(
                x = .data[[xVarName]],
                y = 1,
                group = 1
              )
            ) +
              ggplot2::geom_boxplot(
                orientation = "y",
                width = 0.5,
                alpha = 0.9,
                notch = FALSE,
                fill = "#ADD8E6"
              ) +
              ggtheme +
              themeBox
            
            ybox <- ggplot2::ggplot(
              data,
              ggplot2::aes(
                x = 1,
                y = .data[[yVarName]],
                group = 1
              )
            ) +
              ggplot2::geom_boxplot(
                orientation = "x",
                width = 0.5,
                alpha = 0.9,
                notch = FALSE,
                fill = "#ADD8E6"
              ) +
              ggtheme +
              themeBox
            
            p <- cowplot::insert_xaxis_grob(
              p, 
              xbox,
              position="top"
            )
            p <- cowplot::insert_yaxis_grob(
              p, 
              ybox, 
              position="right"
            )
            
            p <- cowplot::ggdraw(p)          
          }  else if (marg == 'hist') {          
            themeHist <- ggplot2::theme(
              panel.grid.major = ggplot2::element_blank(),
              panel.grid.minor = ggplot2::element_blank(),
              strip.background = ggplot2::element_rect(
                fill='transparent', color=NA
              ),
              panel.background=ggplot2::element_rect(
                fill='transparent', color=NA)
            )
            
            xhist <- ggplot2::ggplot() +
              ggplot2::geom_histogram(
                data = data,
                ggplot2::aes(x = .data[[xVarName]]),
                alpha = 0.9,
                fill = "#ADD8E6"
              ) +
              ggtheme +
              themeHist
            
            yhist <- ggplot2::ggplot() +
              ggplot2::geom_histogram(
                data = data,
                ggplot2::aes(x = .data[[yVarName]]),
                alpha = 0.9,
                fill = "#ADD8E6"
              ) +
              ggtheme +
              themeHist +
              ggplot2::coord_flip()
            
            p <- cowplot::insert_xaxis_grob(
              p, 
              xhist,
              position="top"
            )
            p <- cowplot::insert_yaxis_grob(
              p, 
              yhist, 
              position="right"
            )
            
            p <- cowplot::ggdraw(p)          
          }
          return(p)    
        }
        )
)
