
# This file is a generated template, your changes will not be overwritten

bivcatClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "bivcatClass",
  inherit = bivcatBase,
  private = list(
    #### Member variables ----    
    colArgs = NA,
    #### Init + run functions ----
    .init=function() {
      private$.FREQSTable()
      #table <- self$results$asocind
      if(self$options$chiSq | self$options$phiind) {
       private$colArgs <- list(
         name = c(
           "chiSq", "phiind"
         ),
         title = c(
           "\u03c7\u00B2", "\u03c6"
         ),
         superTitle = rep("",2),
         type = c(rep("number",2)),
         format = rep("",2),
         visible = c("(chiSq)", "(phiind)"
         )
       )
       private$.initASOCTable()
      }
    },
    
    .run=function() {

      rowVarName <- self$options$rows
      colVarName <- self$options$cols
      
      if (is.null(rowVarName) || is.null(colVarName))
        return()
      
      data <- self$data
      
      freqs <- self$results$freqs
      
      freqRowNo <- 1
      
      mats <- list(table(data[[rowVarName]],data[[colVarName]]))
  
      nRows  <- base::nlevels(data[[rowVarName]])
      nCols  <- base::nlevels(data[[colVarName]])
      nCells <- nRows * nCols
      
      for (mat in mats) {
        total <- sum(mat)
        colTotals <- apply(mat, 2, sum)
        rowTotals <- apply(mat, 1, sum)
        exp <- matrix(kronecker(rowTotals,colTotals),
                          length(rowTotals),length(colTotals),
                          byrow = TRUE)/total
        
        
        for (rowNo in seq_len(nRows)) {
          
          values <- mat[rowNo,]
          rowTotal <- rowTotals[rowNo]
          colTotal <- colTotals[rowNo]          
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
          
          values <- c(values, expValues, pcRow, pcCol, pcTot)
          
          freqs$setRow(rowNo=freqRowNo, values=values)
          freqRowNo <- freqRowNo + 1
        }
        
        values <- apply(mat, 2, sum)
        rowTotal <- sum(values)
        values <- as.list(values)
        names(values) <- paste0(1:nCols, '[count]')
        values[['.total[count]']] <- rowTotal
       # 
        expValues <- apply(exp, 2, sum)
        expValues <- as.list(expValues)
        names(expValues) <- paste0(1:nCols, '[expected]')
       # 
        pcRow <- apply(mat, 2, sum) / rowTotal
        pcRow <- as.list(pcRow)
        names(pcRow) <- paste0(1:nCols, '[pcRow]')
       # 
        pcCol <- rep(1, nCols)
        pcCol <- as.list(pcCol)
        names(pcCol) <- paste0(1:nCols, '[pcCol]')
       # 
        pcTot <- apply(mat, 2, sum) / total
        pcTot <- as.list(pcTot)
        names(pcTot) <- paste0(1:nCols, '[pcTot]')
       # 
        expValues[['.total[exp]']] <- total
        pcRow[['.total[pcRow]']] <- 1
        pcCol[['.total[pcCol]']] <- 1
        pcTot[['.total[pcTot]']] <- 1
       # 
        values <- c(values, expValues, pcRow, pcCol, pcTot) #expValues,
       # 
        freqs$setRow(rowNo=freqRowNo, values=values)
        freqRowNo <- freqRowNo + 1
      }
      if( self$options$chiSq | self$options$phiind){
      resultsASOC <- private$.computeASOC()      
      private$.populateASOCTable(resultsASOC)
      }
    },
    #### Compute indices ----
    .computeASOC = function() {
      data <- self$data
      vars <- 'Value'
      indic <- list()
      for(var in vars){
        indic[[var]] <- private$.computeASOCIndices()
      }
      
      return(list(indic=indic))
    },    
    #### Init tables ----
    .FREQSTable = function(){
      rowVarName <- self$options$rows
      colVarName <- self$options$cols
      
      freqs <- self$results$freqs
      
      data <- self$data
      
      subNames  <- c('[count]', '[expected]', '[pcRow]', '[pcCol]', '[pcTot]')
      subTitles <- c(('Observed'), ('Expected'), ('% within row'), ('% within column'), ('% of total'))
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
          title=('Total'),
          type='integer')}
      
      if (self$options$exp) {
        freqs$addColumn(
          name='.total[exp]',
          title=('Total'),
          type='number')
      }
      
      if (self$options$pcRow) {
        freqs$addColumn(
          name='.total[pcRow]',
          title=('Total'),
          type='number',
          format='pc')
      }
      
      if (self$options$pcCol) {
        freqs$addColumn(
          name='.total[pcCol]',
          title=('Total'),
          type='number',
          format='pc')
      }
      
      if (self$options$pcTot) {
        freqs$addColumn(
          name='.total[pcTot]',
          title=('Total'),
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
            value <- ('Total')
          values[[name]] <- value
        }
        
        key <- paste0(rows[i,], collapse='`')
        freqs$addRow(rowKey=key, values=values)
        
      }
    },
    .initASOCTable = function() {
      
      table <- self$results$asocind
      
      vars <- 'Value'        

      colArgs <- private$colArgs
      if( self$options$chiSq | self$options$phiind ){
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
            name=paste0("asoc", post),
            title="",
            type="text",
            value=title,
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
      } #else private$.clearASOCTable()
    },
    #### Populate tables ----
    .populateASOCTable = function(results) {
      table <- self$results$asocind
      
      indic <- results$indic
      
      table$setRow(rowNo=1, values=list(
        `asocind[chiSq]`=indic$chiSq, `asocind[phiind]`=indic$phiind))
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
    },    
    .computeASOCIndices = function() {
      asoc <- list()
      rowVarName <- self$options$rows
      colVarName <- self$options$cols
      
      data <- self$data
      
      tabla <- table(data[[rowVarName]],data[[colVarName]])
      n <- sum(tabla)
      if (n > 0) {
        
        if ( self$options$chiSq ){
          asoc[['chiSq']] <- chisq.test(tabla,correct=FALSE)$statistic
        } else NA
        
        if ( self$options$phiind ){
          CHI <- chisq.test(tabla,correct=FALSE)$statistic
          asoc[['phiind']] <- sqrt(CHI/n) 
        } else NA
        
      } else if (n==0) {
        l <- list(
          chiSq=NaN,
          phiind=NaN
        )
      }
      return(asoc)  
    }    
  )
)
