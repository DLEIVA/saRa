
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
          
          covar <- cov(datos,use='na.or.complete')[1,2]
          pearsonr <- cor(datos,use='na.or.complete')[1,2]
          spearmanr <- cor(datos,method='spearman',use='na.or.complete')[1,2]
          tauk <- cor(datos,method='kendall',use='na.or.complete')[1,2]

          values <- list(`v[covar]` = covar,
                         `v[pearsonR]` = pearsonr,
                         `v[spearmanR]` = spearmanr,
                         `v[tauK]` = tauk,
                         `v[pearsonR2]` = pearsonr^2,
                         `v[spearmanR2]` = spearmanr^2,
                         `v[kendallR2]` = tauk^2)
          asocind$setRow(rowNo=othRowNo, values=values)  
        },
        #### Init tables ----
        .initASSOCTable = function(){
          asocind <- self$results$asocind
          asocind$addRow(rowKey=1, values=list())
          if((self$options$pearsonR | self$options$pearsonR | self$options$spearmanR |
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
          
          data <- na.omit(data)
          
          marg <- self$options$marg
          line <- self$options$line
          method <- if (line == 'linear') 'lm' else 'auto'          
          
          p <- ggplot(
            data, aes_string(x=xVarName, y=yVarName)) + 
            geom_point(alpha=.8, size=2.5) +
            labs(x=xVarName, y=yVarName) + ggtheme
          
          if (line != 'none') {
            p <- p + geom_smooth(method = method, se = self$options$se
            )
          }
          
          p <- p + theme(legend.position = 'none')
          
          if (marg == 'dens') {
            xdens <- cowplot::axis_canvas(p, axis='x') +
              ggridges::geom_ridgeline(
                data=data, 
                ggplot2::aes_string(
                  xVarName, y=0, height="..density.."
                ),
                stat='density', 
                alpha=0.5, 
                size=.2,
                trim=FALSE,
                fill="#ADD8E6"
              ) + ggtheme
            
            ydens <- cowplot::axis_canvas(p, axis='y') +
              ggridges::geom_vridgeline(
                data=data, 
                ggplot2::aes_string(
                  x=0, y=yVarName, width="..density.."
                ),
                stat='ydensity', 
                alpha=0.5, 
                size=.2, 
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
            
            xbox <- ggplot2::ggplot() +
              ggplot2::geom_boxplot(
                data=data, 
                ggplot2::aes_string(y=xVarName), 
                position=ggplot2::position_dodge(0.8),
                width=0.5, 
                alpha=0.9, 
                notch=TRUE,
                fill ="#ADD8E6"
              ) + 
              ggtheme + 
              themeBox + 
              ggplot2::coord_flip()
            
            ybox <- ggplot2::ggplot() +
              ggplot2::geom_boxplot(
                data=data, 
                ggplot2::aes_string(y=yVarName), 
                position=ggplot2::position_dodge(0.8),
                width=0.5, 
                alpha=0.9, 
                notch=TRUE,
                fill ="#ADD8E6"
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
                data=data, 
                ggplot2::aes_string(y=xVarName), 
#                position=ggplot2::position_dodge(0.8),
                alpha=0.9,
                fill ="#ADD8E6"
              ) + 
              ggtheme + 
              themeHist + 
              ggplot2::coord_flip()
            
            yhist <- ggplot2::ggplot() +
              ggplot2::geom_histogram(
                data=data, 
                ggplot2::aes_string(y=yVarName), 
#                position=ggplot2::position_dodge(0.8),
                alpha=0.9, 
                fill ="#ADD8E6"
              ) + 
              ggtheme + 
              themeHist
            
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