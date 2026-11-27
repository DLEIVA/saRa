
# This file is a generated template, your changes will not be overwritten

bivcatClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "bivcatClass",
  inherit = bivcatBase,
  private = list(
    #### Member variables ----    
    #colArgs = NA,
    #### Init + run functions ----
    .init=function() {
      private$.FREQSTable()
      private$.initASSOCTable()
      private$.initERRORTable()
      private$.initBarPlot()      
      private$.initMosaicPlot()      
      private$.setTableVisibility()
    },
    
    .run=function() {
      private$.setTableVisibility()
      rowVarName <- self$options$rows
      colVarName <- self$options$cols
      if (is.null(rowVarName) || is.null(colVarName)) return()
      data <- self$data
      if (!any(complete.cases(data[c(rowVarName,colVarName)])))
        jmvcore::reject(.("No complete observations are available for the selected variables"),
          code="no_complete_observations")
      tabla <- table(data[[rowVarName]],data[[colVarName]])
      private$.populateFrequencyRows(tabla)
      observed <- private$.populateAssociationIndices(tabla)
      private$.addYuleNotes(observed)
      private$.populatePredictionIndices(observed)
      private$.setTableVisibility()
    },

    .populateFrequencyRows=function(mat) {
      total <- sum(mat)
      colTotals <- apply(mat,2,sum)
      rowTotals <- apply(mat,1,sum)
      exp <- matrix(kronecker(rowTotals,colTotals),length(rowTotals),
        length(colTotals),byrow=TRUE)/total
      for (rowNo in seq_len(nrow(mat)))
        self$results$freqs$setRow(rowNo=rowNo,
          values=private$.frequencyRow(mat,exp,rowNo,rowTotals,colTotals,total))
      self$results$freqs$setRow(rowNo=nrow(mat)+1L,
        values=private$.frequencyTotal(mat,exp,total))
    },

    .frequencyRow=function(mat,exp,rowNo,rowTotals,colTotals,total) {
      nCols <- ncol(mat)
      values <- mat[rowNo,]
      rowTotal <- rowTotals[rowNo]
      pcRow <- values / rowTotal

      values <- as.list(values)
      names(values) <- paste0(1:nCols, '[count]')
      values[['.total[count]']] <- rowTotal

      expValues <- exp[rowNo,]
      expValues <- as.list(expValues)
      names(expValues) <- paste0(1:nCols, '[expected]')
      expValues[['.total[exp]']] <- sum(exp[rowNo,])

      pcRow <- as.list(pcRow)
      names(pcRow) <- paste0(1:nCols, '[pcRow]')
      pcRow[['.total[pcRow]']] <- 1

      pcCol <- as.list(mat[rowNo,] / colTotals)
      names(pcCol) <- paste0(1:nCols, '[pcCol]')
      pcCol[['.total[pcCol]']] <- unname(rowTotals[rowNo] / total)

      pcTot <- as.list(mat[rowNo,] / total)
      names(pcTot) <- paste0(1:nCols, '[pcTot]')
      pcTot[['.total[pcTot]']] <- sum(mat[rowNo,] / total)

      c(values, expValues, pcRow, pcCol, pcTot)
    },

    .frequencyTotal=function(mat,exp,total) {
      nCols <- ncol(mat)
      values <- apply(mat, 2, sum)
      rowTotal <- sum(values)
      values <- as.list(values)
      names(values) <- paste0(1:nCols, '[count]')
      values[['.total[count]']] <- rowTotal
      expValues <- apply(exp, 2, sum)
      expValues <- as.list(expValues)
      names(expValues) <- paste0(1:nCols, '[expected]')
      pcRow <- apply(mat, 2, sum) / rowTotal
      pcRow <- as.list(pcRow)
      names(pcRow) <- paste0(1:nCols, '[pcRow]')
      pcCol <- rep(1, nCols)
      pcCol <- as.list(pcCol)
      names(pcCol) <- paste0(1:nCols, '[pcCol]')
      pcTot <- apply(mat, 2, sum) / total
      pcTot <- as.list(pcTot)
      names(pcTot) <- paste0(1:nCols, '[pcTot]')
      expValues[['.total[exp]']] <- total
      pcRow[['.total[pcRow]']] <- 1
      pcCol[['.total[pcCol]']] <- 1
      pcTot[['.total[pcTot]']] <- 1
      c(values, expValues, pcRow, pcCol, pcTot)
    },

    .populateAssociationIndices=function(tabla) {
      asocind <- self$results$asocind
      errorind <- self$results$errorind
      emptyLevels <- any(rowSums(tabla)==0) || any(colSums(tabla)==0)
      emptyNote <- if(emptyLevels) .("Empty defined categories are retained in frequencies and excluded from association calculations.") else NULL
      asocind$setNote("empty_levels",emptyNote)
      errorind$setNote("empty_levels",emptyNote)
      tabla <- .saraObservedTable(tabla)
      n <- sum(tabla)
      valid <- n > 0 && all(rowSums(tabla) > 0) && all(colSums(tabla) > 0) && all(dim(tabla) > 1)
      CHI <- if (valid) suppressWarnings(chisq.test(tabla,correct=FALSE)$statistic) else NaN
      if (!valid) {
        message <- .("Association indices require at least two observed categories in each variable and complete observations")
        asocind$setNote("observed_categories",message)
        errorind$setNote("observed_categories",message)
      }
      yule <- .saraYulePhi(tabla)

      values <- list(
        `v[chiSq]`=CHI,
        `v[phiind]`=sqrt(CHI/n),
        `v[phiSq]`=CHI/n,
        `v[contCoef]`=.saraContingency(CHI,n),
        `v[craV]`=.saraCramerV(CHI,n,nrow(tabla),ncol(tabla)),
        `v[chuprov]`=.saraChuprov(CHI, n, nrow(tabla), ncol(tabla)),
        `v[sakoda]`=.saraSakoda(CHI,n,nrow(tabla),ncol(tabla)),
        `v[Qyule]` = yule$Q,
        `v[Yyule]` = yule$Y,
        `v[Vyule]` = yule$V
        )
      asocind$setRow(rowNo=1, values=values)
      tabla
    },

    .addYuleNotes=function(tabla) {
      if (nrow(tabla)>2L || ncol(tabla)>2L) {
        for (option in c("Qyule", "Yyule", "Vyule"))
          if (isTRUE(self$options[[option]]))
            self$results$asocind$addFootnote(rowNo=1, paste0("v[",option,"]"),
              .("Indices can only be computed for 2x2 tables."))
      }
    },

    .populatePredictionIndices=function(tabla) {
      errorind <- self$results$errorind
      prediction <- .saraNominalPrediction(tabla)
      values <- setNames(prediction,paste0("v[",names(prediction),"]"))
      errorind$setRow(rowNo=1, values=values)
    },

    #### Init tables ----
    .FREQSTable = function(){
      rowVarName <- self$options$rows
      colVarName <- self$options$cols
      
      freqs <- self$results$freqs
      
      data <- self$data
      
      subNames  <- c('[count]', '[expected]', '[pcRow]', '[pcCol]', '[pcTot]')
      subTitles <- c(.('Observed'), .('Expected'), .('% within row'), .('% within column'), .('% of total'))
      visible   <- c('(obs)', '(exp)', '(pcRow)', '(pcCol)', '(pcTot)')
      types     <- c('integer', 'number', 'number', 'number', 'number')
      formats   <- c('', '', 'pc', 'pc', 'pc')      
      
      if( ! is.null(rowVarName) & ! is.null(colVarName) &
          (self$options$obs | self$options$exp |
           self$options$pcRow | self$options$pcCol |
           self$options$pcTot)){
        title <- rowVarName
        
        freqs$addColumn(
          name=title,
          title=title,
          type='text')
        
        superTitle <- colVarName
        levels <- base::levels(data[[colVarName]])
        
        # iterate over the sub rows
        
        for (j in seq_along(subNames)) {
          subName <- subNames[[j]]
          if(subName == '[count]')
            v <- '(obs && (exp || pcRow || pcCol || pcTot))'
          else
            v <- visible[j]
          
          freqs$addColumn(
            name=paste0('type', subName),
            title=' ',
            type='text',
            visible=v)
        }
        
        for (i in seq_along(levels)) {
          level <- levels[[i]]
          
          for (j in seq_along(subNames)) {
            subName <- subNames[[j]]
            freqs$addColumn(
              name=paste0(i, subName),
              title=level,
              superTitle=superTitle,
              type=types[j],
              format=formats[j],
              visible=visible[j])
          }
        }
      } else if((self$options$obs | self$options$exp |
                 self$options$pcRow | self$options$pcCol |
                 self$options$pcTot) == FALSE){
        if(! is.null(rowVarName)){   
          title <- rowVarName
        } else{
          title <- '.'
        }
        
        freqs$addColumn(
          name=title,
          title=title,
          type='text')
        
        if(! is.null(colVarName)){
          superTitle <- colVarName
          levels <- base::levels(data[[colVarName]])
          
          for (i in seq_along(levels)) {
            level <- levels[[i]]
            
            for (j in seq_along(subNames)) {
              freqs$addColumn(
                name=paste0(i),
                title=level,
                superTitle=superTitle,
                type='text')
            }
          }
        } else{
          superTitle <- '.'
          levels <- c('.', '.')
          
          for (j in seq_along(subNames)) {
            subName <- subNames[[j]]
            if(subName == '[count]')
              v <- '(obs && (exp || pcRow || pcCol || pcTot))'
            else
              v <- visible[j]
            
            freqs$addColumn(
              name=paste0('type', subName),
              title=' ',
              type='text',
              visible=v)
          }
          
          for (i in seq_along(levels)) {
            level <- levels[[i]]
            
            for (j in seq_along(subNames)) {
              subName <- subNames[[j]]
              freqs$addColumn(
                name=paste0(i, subName),
                title=level,
                superTitle=superTitle,
                type=types[j],
                format=formats[j],
                visible=visible[j])
            }
          }        
        }
      }
      
      # add the Total column
      
      if (self$options$obs ){
        freqs$addColumn(
          name='.total[count]',
          title=.('Total'),
          type='integer')}
      
      if (self$options$exp) {
        freqs$addColumn(
          name='.total[exp]',
          title=.('Total'),
          type='number')
      }
      
      if (self$options$pcRow) {
        freqs$addColumn(
          name='.total[pcRow]',
          title=.('Total'),
          type='number',
          format='pc')
      }
      
      if (self$options$pcCol) {
        freqs$addColumn(
          name='.total[pcCol]',
          title=.('Total'),
          type='number',
          format='pc')
      }
      
      if (self$options$pcTot) {
        freqs$addColumn(
          name='.total[pcTot]',
          title=.('Total'),
          type='number',
          format='pc')
      }
      
      # populate the first column with the levels of the row variable
      
      values <- list()
      for (i in seq_along(subNames))
        values[[paste0('type', subNames[i])]] <- subTitles[i]
      
      rows <- private$.grid(data=data, incRows=TRUE)
      
      nextIsNewGroup <- TRUE
      
      for (i in seq_len(nrow(rows))) {
        
        for (name in colnames(rows)) {
          value <- as.character(rows[i, name])
          if (value == '.total')
            value <- .('Total')
          values[[name]] <- value
        }
        
        key <- paste0(rows[i,], collapse='`')
        freqs$addRow(rowKey=key, values=values)
        
      }
    },
    .initASSOCTable = function(){
      self$results$asocind$addRow(rowKey=1, values=list())
    },
    .initERRORTable = function(){
      self$results$errorind$addRow(rowKey=1, values=list())
    },
    .setTableVisibility = function() {
      selected <- function(names) any(vapply(names, function(name)
        isTRUE(self$options[[name]]), logical(1)))
      ready <- !is.null(self$options$rows) && !is.null(self$options$cols)
      self$results$freqs$setVisible(ready &&
        selected(c("obs", "exp", "pcRow", "pcCol", "pcTot")))
      self$results$asocind$setVisible(ready && selected(c(
        "chiSq", "phiind", "phiSq", "contCoef", "craV", "chuprov",
        "sakoda", "Qyule", "Yyule", "Vyule")))
      self$results$errorind$setVisible(ready && selected(c(
        "lambdaGKab", "lambdaGKba", "lambdaGKsym", "tauGKab", "tauGKba",
        "theilab", "theilba", "theilsym")))
    },
    #### Plot functions ---- Taken from conttables.b.R in jmv package
    .initBarPlot = function() {
      image <- self$results$get('barplot')
      
      width <- 450
      height <- 400
      
      image$setSize(width * 2, height)
    },
    .initMosaicPlot = function() {
      image <- self$results$get('mosaicplot')
      
      width <- 450
      height <- 400
      
      image$setSize(width * 1.5, height*1.5)
    },    
    .barPlotObject = function(image, ggtheme, theme, ...) {
      
      if (! self$options$barplot)
        return()
      
      rowVarName <- self$options$rows
      colVarName <- self$options$cols

      if (is.null(rowVarName) || is.null(colVarName))
        return()
      
      data <- self$data
      
      data <- data[stats::complete.cases(data[c(rowVarName,colVarName)]),,drop=FALSE]
      
      counts <- table(data[[rowVarName]],data[[colVarName]],dnn=c(rowVarName,colVarName))
      d <- dim(counts)
      
      tab <- as.data.frame(counts,responseName="Counts")
      names(tab) <- c(rowVarName,colVarName,"Counts")

      if (self$options$yaxis == "ypc") { # percentages
        props <- counts
        
        if (self$options$yaxisPc == "column_pc") {
          pctVarName <- colVarName
        } else if (self$options$yaxisPc == "row_pc") {
          pctVarName <- rowVarName
        } else { # total
          pctVarName <- NULL
        }
        
        props <- proportions(counts, pctVarName)

        tab$Percentages <- as.numeric(props) * 100
        tab$Perc <- paste0(round(as.numeric(tab$Percentages),2),'%')
      }
      
      if (self$options$xaxis == "xcols") {
        xVarName <- colVarName
        zVarName <- rowVarName
      } else {
        xVarName <- rowVarName
        zVarName <- colVarName
      }
      
      position <- self$options$bartype
      
      if (self$options$yaxis == "ycounts") {
        if (position!='dodge') {
        p <- ggplot2::ggplot(data=tab, ggplot2::aes(y=.data[["Counts"]], x=.data[[xVarName]], fill=.data[[zVarName]])) +
          ggplot2::geom_col(position=position, width = 0.7)
        } else {
          p <- ggplot2::ggplot(data=tab, ggplot2::aes(y=.data[["Counts"]], x=.data[[xVarName]], fill=.data[[zVarName]], label=.data[['Counts']])) +
            ggplot2::geom_col(position=position, width = 0.7)  +
            ggplot2::geom_text(position = ggplot2::position_dodge(.7), 
                      vjust = -0.5, 
                      size = 4)         
        }
      } else {
        if (position!='dodge') {p <- ggplot2::ggplot(data=tab, ggplot2::aes(y=.data[["Percentages"]], x=.data[[xVarName]], fill=.data[[zVarName]])) +
          ggplot2::geom_col(position=position, width = 0.7)} else {
                p <- ggplot2::ggplot(data=tab, ggplot2::aes(y=.data[["Percentages"]], x=.data[[xVarName]], fill=.data[[zVarName]], label=.data[['Perc']])) +
                  ggplot2::geom_col(position=position, width = 0.7) +
                  ggplot2::geom_text(position = ggplot2::position_dodge(.7), 
                    vjust = -0.5, 
                    size = 4)}
      
        if (self$options$yaxisPc == "total_pc") {
          p <- p + ggplot2::labs(y = .("Percentages of total"))
        } else {
          p <- p + ggplot2::labs(y = jmvcore::format(.("Percentages within {}"), pctVarName))
        }
      }
      
      if (self$options$yaxis == "ycounts") p <- p + ggplot2::labs(y=.("Counts"))
      p <- p + ggtheme
      
      return(p)
    },    
    .mosaicPlot = function(image, ...) {
      
      if (! self$options$mosaicplot)
        return()
      
      rowVarName <- self$options$rows
      colVarName <- self$options$cols
      
      if (is.null(rowVarName) || is.null(colVarName))
        return()
      
      data <- self$data
      
      data <- data[stats::complete.cases(data[c(rowVarName,colVarName)]),,drop=FALSE]
      
      if (self$options$xaxis == "xcols") {
        xVarName <- colVarName
        zVarName <- rowVarName
      } else {
        xVarName <- rowVarName
        zVarName <- colVarName
      }
      par(mar=c(0,0,0,0))
      vcd::mosaic(table(data[[xVarName]], data[[zVarName]], dnn=c(xVarName, zVarName)),
                       gp= vcd::shading_Friendly2(interpolate = c(-4,-2,2,4)),
                       split_vertical=FALSE)
    },        
    
    .barPlot = function(image,ggtheme,theme,...) {
      .saraDrawBivariatePlot(function() private$.barPlotObject(image,ggtheme,theme,...))
    },
    #### Helper functions ----
    
    .grid=function(data, incRows=FALSE) {
      
      rowVarName <- self$options$rows
      
      expand <- list()
      
      if (incRows) {
        if (is.null(rowVarName))
          expand[['.']] <- c('.', '. ', ('Total'))
        else
          expand[[rowVarName]] <- c(base::levels(data[[rowVarName]]), '.total')
      }
      
      rows <- rev(expand.grid(expand))
      
      rows
    },
    .skipOption = function(visible) {
      return(! self$options[[ gsub("[()]", "", visible) ]])
    }
  )
)

