
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
      
      asocind <- self$results$asocind
      errorind <- self$results$errorind
      
      othRowNo <- 1
      
      tabla <- table(data[[rowVarName]],data[[colVarName]])
      n <- sum(tabla)
      CHI <- chisq.test(tabla,correct=FALSE)$statistic
      
      if(dim(tabla)[1]==2 & dim(tabla)[2]==2){
        a <- tabla[1,1]
        b <- tabla[1,2]
        c <- tabla[2,1]
        d <- tabla[2,2] 
        QY <- (a*d-b*c)/(a*d+b*c)
        YY <- (sqrt(a*d)-sqrt(b*c))/(sqrt(a*d)+sqrt(b*c))
        VY <- (a*d-b*c)/((a+b)*(a+c)*(b+d)*(c+d))
      } else{
        QY <- NA
        YY <- NA
        VY <- NA
      }
      
      values <- list(
        `v[chiSq]`=CHI,
        `v[phiind]`=sqrt(CHI/n),
        `v[phiSq]`=CHI/n,
        `v[contCoef]`=sqrt(CHI/(n+CHI)),
        `v[craV]`=sqrt(CHI/((min(dim(tabla))-1)*n)),        
        `v[chuprov]`=sqrt(CHI/(n*(dim(tabla)[1]-1)*(dim(tabla)[2]-1))),
        `v[sakoda]`=sqrt(min(dim(tabla))*CHI/((min(dim(tabla))-1)*(n+CHI))),
        `v[Qyule]` = QY,
        `v[Yyule]` = YY,
        `v[Vyule]` = VY
        )
      asocind$setRow(rowNo=othRowNo, values=values)
      
      if ( self$options$Qyule & (dim(tabla)[1]>2 | dim(tabla)[2]>2) ){
        asocind$addFootnote(rowNo=1,'v[Qyule]',
                            "Indices can only be computed for 2x2 tables.")
      }
      
      if ( self$options$Yyule & (dim(tabla)[1]>2 | dim(tabla)[2]>2) ){
        asocind$addFootnote(rowNo=1,'v[Yyule]',
                            "Indices can only be computed for 2x2 tables.")
      }
      
      if ( self$options$Vyule & (dim(tabla)[1]>2 | dim(tabla)[2]>2) ){
        asocind$addFootnote(rowNo=1,'v[Vyule]',
                            "Indices can only be computed for 2x2 tables.")
      }
      
      lambda.a.b <- (sum(apply(tabla,2,max)/sum(tabla)) - max(rowSums(tabla))/
                       sum(tabla))/(1 - max(rowSums(tabla))/sum(tabla))
      lambda.b.a <- (sum(apply(tabla,1,max)/sum(tabla)) - max(colSums(tabla))/
                       sum(tabla))/(1 - max(colSums(tabla))/sum(tabla))
      .lambda <- (lambda.a.b + lambda.b.a)/2
      
      tau.a.b <- (sum((tabla/n)^2/matrix(colSums(tabla)[col(tabla)]/n,
                  nrow=nrow(tabla)))-sum((rowSums(tabla)/n)^2))/(1-sum((rowSums(tabla)/n)^2))
      tau.b.a <- (sum((tabla/n)^2/matrix(rowSums(tabla)[row(tabla)]/n,
                  nrow=nrow(tabla)))-sum((colSums(tabla)/n)^2))/(1-sum((colSums(tabla)/n)^2))      
      .tau <- (tau.a.b + tau.b.a)/2
      
      values <- list(`v[lambdaGKab]` =lambda.a.b,
                     `v[lambdaGKba]` =lambda.b.a,
                     `v[lambdaGKsym]` =.lambda,
                     `v[tauGKab]` =tau.a.b,
                     `v[tauGKba]` =tau.b.a,
                     `v[tauGKsym]` =.tau)
      
      errorind$setRow(rowNo=othRowNo, values=values)      
      
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
    .initASSOCTable = function(){
      asocind <- self$results$asocind
      asocind$addRow(rowKey=1, values=list())
      if((self$options$chiSq | self$options$phiind | self$options$phiSq |
          self$options$contCoef | self$options$craV | self$options$chuprov | 
          self$options$sakoda | self$options$Qyule |
          self$options$Yyule | self$options$Vyule) == FALSE){
        asocind <- self$results$asocind
        asocind$addColumn(
          name=' ',
          title=' ',
          type='text')

      }      
    },
    .initERRORTable = function(){
      errorind <- self$results$errorind
      errorind$addRow(rowKey=1, values=list())
      if((self$options$lambdaGKab || self$options$lambdaGKba ||
          self$options$lambdaGKsym || self$options$tauGKab ||
          self$options$tauGKba || self$options$tauGKsym) == FALSE){
        errorind <- self$results$errorind
        errorind$addColumn(
          name=' ',
          title=' ',
          type='text')
        
      }      
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

