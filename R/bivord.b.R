
# This file is a generated template, your changes will not be overwritten

bivordClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "bivordClass",
    inherit = bivordBase,
    private = list(    
    #### Member variables ----    
    #colArgs = NA,
    #### Init + run functions ----
    .init=function() {
      private$.FREQSTable()
      private$.initCOMPTable()      
      private$.initASSOCTable()
      private$.initBarPlot()
      private$.initHeatMap()
      private$.initAlluvial()  
    },
    
    .run=function() {
      
      rowVarName <- self$options$rows
      colVarName <- self$options$cols
      
      data <- self$data
      
      if (is.null(rowVarName) || is.null(colVarName)){
        return() 
      } else{
        if(!is.ordered(data[[rowVarName]]))
          stop(paste0("The variable ", rowVarName,
                      " cannot be treated as ordinal.Please select only ordinal variables."))
        if(!is.ordered(data[[colVarName]]))
          stop(paste0("The variable ", colVarName,
                      " cannot be treated as ordinal.Please select only ordinal variables."))
      }
      
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
        
        for (rowNo in seq_len(nRows)) {
          
          values <- mat[rowNo,]
          rowTotal <- rowTotals[rowNo]
          colTotal <- colTotals[rowNo]          
          pcRow <- values / rowTotal
          
          values <- as.list(values)
          names(values) <- paste0(1:nCols, '[count]')
          values[['.total[count]']] <- rowTotal
          
          pcRow <- as.list(pcRow)
          names(pcRow) <- paste0(1:nCols, '[pcRow]')
          pcRow[['.total[pcRow]']] <- 1
          
          pcCol <- as.list(mat[rowNo,] / colTotals)
          names(pcCol) <- paste0(1:nCols, '[pcCol]')
          pcCol[['.total[pcCol]']] <- unname(rowTotals[rowNo] / total)
          
          pcTot <- as.list(mat[rowNo,] / total)
          names(pcTot) <- paste0(1:nCols, '[pcTot]')
          pcTot[['.total[pcTot]']] <- sum(mat[rowNo,] / total)
          
          values <- c(values, pcRow, pcCol, pcTot)
          
          freqs$setRow(rowNo=freqRowNo, values=values)
          freqRowNo <- freqRowNo + 1
        }
        
        values <- apply(mat, 2, sum)
        rowTotal <- sum(values)
        values <- as.list(values)
        names(values) <- paste0(1:nCols, '[count]')
        values[['.total[count]']] <- rowTotal
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
        pcRow[['.total[pcRow]']] <- 1
        pcCol[['.total[pcCol]']] <- 1
        pcTot[['.total[pcTot]']] <- 1
        # 
        values <- c(values, pcRow, pcCol, pcTot)
        # 
        freqs$setRow(rowNo=freqRowNo, values=values)
        freqRowNo <- freqRowNo + 1
      }
      
      asocind <- self$results$asocind
      desc <- self$results$desc
      
      othRowNo <- 1
      
      tabla <- table(data[[rowVarName]],data[[colVarName]])
      n <- sum(tabla)
      
      filas <- row(tabla)
      columnas <- col(tabla)
      n <- sum(tabla)
      q <- min(dim(tabla))
      C <- sum(tabla * mapply(function(f, c){sum(tabla[(filas > f) &
                                                           (columnas > c)])}, f = filas, c = columnas))
      D <- sum(tabla * mapply(function(f, c){sum(tabla[(filas > f) &
                                                           (columnas < c)])}, f = filas, c = columnas))
      E.X <- (sum(apply(tabla,1,sum)^2)-n)/2
      E.Y <- (sum(apply(tabla,2,sum)^2)-n)/2
      E.XY <- (sum(tabla^2)-n)/2
      
      res <- c(C,D,E.X,E.Y,E.XY)
      resnames <- c('Concordant pairs','Discordant pairs',paste0('Ties ',rowVarName),
                         paste0('Ties ',colVarName),paste0('Ties ',rowVarName,' and ',colVarName))
      
      for(k in 1:length(res)){
        values <- list('names' = resnames[k], 'values' = res[k])
        desc$setRow(rowNo=k, values=values)
      }
      gammaGK <- (C-D)/(C+D)        
      tau.a <- (C-D)/choose(n,2)
      tau.b <- (C-D)/sqrt((C+D+E.X-E.XY)*(C+D+E.Y-E.XY))
      tau.c <- 2*q*(C-D)/(n^2*(q-1))
      sommers.x.y <- (C-D)/(C+D+E.X-E.XY)
      sommers.y.x <- (C-D)/(C+D+E.Y-E.XY)
      .sommers <- (C-D)/(C+D+(E.X+E.Y)/2-E.XY)      
      .wilson <- 2*(C-D)/(choose(n,2)-E.XY)
      
      values <- list(`v[gammaGK]` = gammaGK,
                     `v[tauKa]` = tau.a,
                     `v[tauKb]` = tau.b,
                     `v[tauKc]` = tau.c,
                     `v[dSommerR]` = sommers.x.y,
                     `v[dSommerC]` = sommers.y.x,
                     `v[dSommerS]` = .sommers,
                     `v[eWilson]` = .wilson)
      asocind$setRow(rowNo=othRowNo, values=values)

    },
    
    #### Init tables ----
    .FREQSTable = function(){
      rowVarName <- self$options$rows
      colVarName <- self$options$cols

      freqs <- self$results$freqs
      
      data <- self$data
      
      subNames  <- c('[count]', '[pcRow]', '[pcCol]', '[pcTot]')
      subTitles <- c(('Observed'), ('% within row'), ('% within column'), ('% of total'))
      visible   <- c('(obs)', '(pcRow)', '(pcCol)', '(pcTot)')
      types     <- c('integer', 'number', 'number', 'number')
      formats   <- c('', 'pc', 'pc', 'pc')      
      
      if( ! is.null(rowVarName) & ! is.null(colVarName) &
          (self$options$obs | self$options$pcRow | self$options$pcCol |
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
            v <- '(obs && (pcRow || pcCol || pcTot))'
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
      } else if((self$options$obs |
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
              v <- '(obs && (pcRow || pcCol || pcTot))'
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
      if((self$options$gammaGK | self$options$tauKa | self$options$tauKb |
          self$options$tauKc | self$options$dSommerR | self$options$dSommerC |
          self$options$dSommerS | self$options$eWilson) == FALSE){
        asocind <- self$results$asocind
        asocind$addColumn(
          name=' ',
          title=' ',
          type='text')
      }      
    },
    .initCOMPTable = function(){
      desc <- self$results$desc
      desc$addRow(rowKey=1, values=list())
      if(is.null(self$options$rows) | is.null(self$options$cols) ){
        desc <- self$results$desc
        resnames <- c(' ',' ',
                      ' ',' ',' ')
        
        for(k in 1:length(resnames)){
          values <- list('names' = resnames[k], 'values' = NA)
          desc$setRow(rowNo=k, values=values)
        }
      }      
    },
    #### Plot functions ---- Taken from conttables.b.R in jmv package
    .initBarPlot = function() {
      image <- self$results$get('barplot')
      
      width <- 450
      height <- 400
      
      image$setSize(width * 2, height)
    },
    .initHeatMap = function() {
      image <- self$results$get('heatmap')
      
      width <- 450
      height <- 400
      
      image$setSize(width * 2, height)
    },
    .initAlluvial = function() {
      image <- self$results$get('alluvial')
      
      width <- 450
      height <- 400
      
      image$setSize(width * 2, height)
    },    
    .barPlot = function(image, ggtheme, theme, ...) {
      
      if (! self$options$barplot)
        return()
      
      rowVarName <- self$options$rows
      colVarName <- self$options$cols
      
      if (is.null(rowVarName) || is.null(colVarName))
        return()
      
      data <- self$data
      
      data <- na.omit(data)
      
      formula <- jmvcore::composeFormula(NULL, c(rowVarName, colVarName))
      counts <- xtabs(formula, data)
      d <- dim(counts)
      
      expand <- list() 
      for (i in c(rowVarName, colVarName))
        expand[[i]] <- base::levels(data[[i]])
      tab <- expand.grid(expand)
      tab$Counts <- as.numeric(counts)
      
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
          p <- ggplot(data=tab, aes_string(y="Counts", x=xVarName, fill=zVarName)) +
            geom_col(position=position, width = 0.7)
        } else {
          p <- ggplot(data=tab, aes_string(y="Counts", x=xVarName, fill=zVarName,
                                           label='Counts')) +
            geom_col(position=position, width = 0.7)  +
            geom_text(position = position_dodge(.7), 
                      vjust = -0.5, 
                      size = 4)         
        }
      } else {
        if (position!='dodge') {p <- ggplot(data=tab, aes_string(y="Percentages", x=xVarName, fill=zVarName)) +
          geom_col(position=position, width = 0.7)} else {
            p <- ggplot(data=tab, aes_string(y="Percentages", x=xVarName, fill=zVarName,label='Perc')) +
              geom_col(position=position, width = 0.7) +
              geom_text(position = position_dodge(.7), 
                        vjust = -0.5, 
                        size = 4)}
        
        if (self$options$yaxisPc == "total_pc") {
          p <- p + labs(y = "Percentages of total")
        } else {
          p <- p + labs(y = paste0("Percentages within ", pctVarName))
        }
      }
      
      p <- p + ggtheme
      
      return(p)
    },  
    .heatmap = function(image, ...) {
      
      if (! self$options$heatmap)
        return()
      
      rowVarName <- self$options$rows
      colVarName <- self$options$cols
      
      if (is.null(rowVarName) || is.null(colVarName))
        return()
      
      data <- self$data
      
      data <- na.omit(data)
      
      # formula <- jmvcore::composeFormula(NULL, c(rowVarName, colVarName))
      # counts <- xtabs(formula, data)
      # d <- dim(counts)
      # 
      # expand <- list() 
      # for (i in c(rowVarName, colVarName))
      #   expand[[i]] <- base::levels(data[[i]])
      # tab <- expand.grid(expand)
      # tab$Counts <- as.numeric(counts)
      
      if (self$options$xaxis == "xcols") {
        xVarName <- colVarName
        zVarName <- rowVarName
      } else {
        xVarName <- rowVarName
        zVarName <- colVarName
      }
      
      tabla <- as.data.frame(with(data,xtabs(as.formula(paste0('~',xVarName,'+',zVarName)))))
      p <- ggplot(tabla,aes_string(xVarName,zVarName)) +
        geom_tile(aes(fill = Freq), colour ="black") +
        scale_fill_gradient(low='white',high='steelblue') + theme_bw() +
        theme(axis.text.x=element_text(size=13),
              axis.text.y=element_text(size=13),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))
      return(p)
    },
    .alluvial =  function(image, ...) {
      library(ggalluvial)
      if (! self$options$alluvial)
        return()
      
      rowVarName <- self$options$rows
      colVarName <- self$options$cols
      
      if (is.null(rowVarName) || is.null(colVarName))
        return()
      
      data <- self$data
      
      data <- na.omit(data)
      
      if (self$options$xaxis == "xcols") {
        xVarName <- colVarName
        zVarName <- rowVarName
      } else {
        xVarName <- rowVarName
        zVarName <- colVarName
      }
      
      tabla <- as.data.frame(with(data,xtabs(as.formula(paste0('~',xVarName,'+',zVarName)))))
      p <- ggplot(data = tabla,
             aes_string(axis1 = xVarName,axis2 = zVarName, y = "Freq")) +
        scale_x_discrete(limits = c(xVarName, zVarName), expand = c(.2, .05)) +
        scale_fill_gradient(low = "white", high = "steelblue") +
        geom_alluvium(aes(fill = Freq)) +
        geom_stratum() +
        geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
        theme_bw() +
        theme(axis.text.x=element_text(size=13),
              axis.text.y=element_text(size=13),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))
      return(p)
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