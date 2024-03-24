
# This file is a generated template, your changes will not be overwritten

umwClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "umwClass",
    inherit = umwBase,
    private = list(
      .run=function() {
        
        groupVarName <- self$options$group
        depVarNames <- self$options$vars
        varNames <- c(groupVarName, depVarNames)
        
        if (is.null(groupVarName) || length(depVarNames) == 0)
          return()
        
        data <- select(self$data, varNames)
        
        for (name in depVarNames)
          data[[name]] <- jmvcore::toNumeric(data[[name]])
        data[[groupVarName]] <- droplevels(as.factor(data[[groupVarName]]))
        
        mwtestTable <- self$results$mwutest
        cismedTable <- self$results$cisMed
        cishlTable <- self$results$cisHL
        descTable <- self$results$desc

        confInt <- self$options$ciWidth / 100

        if (any(depVarNames == groupVarName))
          jmvcore::reject(.("Grouping variable '{a}' must not also be a dependent variable"),
                          code="a_is_dependent_variable", a=groupVarName)
        
        # exclude rows with missings in the grouping variable
        data <- data[ ! is.na(data[[groupVarName]]),]
        
        groupLevels <- base::levels(data[[groupVarName]])
        
        if (length(groupLevels) != 2)
          jmvcore::reject(.("Grouping variable '{a}' must have exactly 2 levels"),
                          code="grouping_var_must_have_2_levels", a=groupVarName)
        
        if (self$options$miss == "listwise") {
          data <- naOmit(data)
          if (dim(data)[1] == 0)
            jmvcore::reject(.("Grouping variable '{a}' has less than 2 levels after missing values are excluded"),
                            code="grouping_var_must_have_2_levels", a=groupVarName)
        }
        
        ## Hypothesis options checking
        if (self$options$hypothesis == 'oneGreater')
          Ha <- "greater"
        else if (self$options$hypothesis == 'twoGreater')
          Ha <- "less"
        else
          Ha <- "two.sided"

        ## Additional functions                
        .zStattest <- function(dep,group,Ha){
          df <- data.frame(dep,group)
          n <- as.numeric(table(df$group))
          N <- sum(n)
          t <- table(df$dep)
          mu <- prod(n)/2
          sigma <- if(any(t>1)){
            sqrt(prod(n)/12*(N-1/(N*(N-1))*sum(t^3-t)))
          } else{ sqrt(prod(n)*(N+1)/12)}
          med <- tapply(df$dep,df$group,median)
          U <- wilcox.test(df$dep~df$group)$statistic
          statistic <- (U-mu)/sigma
          names(statistic) <- NULL
          p.value <- min(1-pnorm(abs(statistic)),pnorm(abs(statistic)))
          p.value <- if(Ha=='two.sided'){(p.value)*2
          } else if((Ha=='greater' & med[1]<med[2]) | (Ha=='less' & med[1]>med[2])){
            1-p.value
          } else{p.value}
          names(p.value) <- NULL
          list(statistic=statistic,p.value=p.value,mu=mu,sigma=sigma,n=n,t=t)
        } 
        
        .bisrankr <- function(dep,group){
          df <- data.frame(dep,group)
          n <- as.numeric(table(df$group))
          U <- wilcox.test(df$dep~df$group)$statistic
          statistic <- 1 - (2 * U/prod(n))
          list(statistic=statistic)
        }
        
        .fstat <- function(dep,group){
          df <- data.frame(dep,group)
          n <- as.numeric(table(df$group))
          rangos <- tryNaN(rank(unlist(split(df$dep,df$group))))
          R1 <- sum(rangos[1:n[1]])
          R2 <- sum(rangos[(n[1]+1):length(rangos)])
          R <- c(R1,R2)  
          U <- R-(n*(n+1)/2)
          statistic <- max(U)/prod(n)
          list(statistic=statistic)
        }
        
        .hl <- function(dep,group,conflevel){
          df <- data.frame(dep,group)
          test <- wilcox.test(df$dep~df$group,conf.int=TRUE,conf.level=conflevel)
          results <- c(test$estimate,test$conf.int)
          names(results) <- NULL
          results
        }          
        
        for (depName in depVarNames) {
          
          dataMWTest <- data.frame(dep=data[[depName]], group=data[[groupVarName]])
          
          if (self$options$miss == "perAnalysis")
            dataMWTest <- naOmit(dataMWTest)  
          
          groupLevels <- base::levels(dataMWTest$group)
          n <- tapply(dataMWTest$dep, dataMWTest$group, length)
          med <- tapply(dataMWTest$dep, dataMWTest$group, function(x) tryNaN(median(x)))
          rangos <- tryNaN(rank(unlist(split(dataMWTest$dep,dataMWTest$group))))
          R1 <- sum(rangos[1:n[1]])
          R2 <- sum(rangos[(n[1]+1):length(rangos)])
          R <- c(R1,R2)
          avgR1 <- R1/n[1]
          avgR2 <- R2/n[1]
          avgR <- c(avgR1,avgR2)
          
          n[is.na(n)] <- 0
          med[is.na(med)] <- NaN
          R[is.na(R)] <- NaN
          avgR[is.na(avgR)] <- NaN
          
          if(self$options$ciMethod=='exact'){
            cimed <- matrix(unlist(tapply(dataMWTest$dep,dataMWTest$group,
                          function(x) DescTools::MedianCI(x,method='exact'))),
            nrow=2,ncol=3,byrow=TRUE)[,2:3]
          } else if(self$options$ciMethod=='boot'){
            cimed <- matrix(unlist(tapply(dataMWTest$dep,dataMWTest$group,
                                 function(x) DescTools::MedianCI(x,method='boot',R=self$options$numR))),
                   nrow=2,ncol=3,byrow=TRUE)[,2:3]            
          }
          
          cimed[is.na(cimed)] <- NaN
          
          if(self$options$ciHL){
            cihl <- .hl(dataMWTest$dep,dataMWTest$group,confInt)
          }
          
          cihl[is.na(cihl)] <- NaN
          
          if (self$options$mwu) {
            
            if (is.factor(dataMWTest$dep))
              res <- createError(.('Variable is not numeric'))
            else if (any(is.infinite(dataMWTest$dep)))
              res <- createError(.('Variable contains infinite values'))
            else
              res <- try(suppressWarnings(wilcox.test(dep ~ group, data=dataMWTest, paired=FALSE,
                                alternative=Ha, conf.level=confInt)), silent=TRUE)
            
            if (isError(res)) {
              
              mwtestTable$setRow(rowKey=depName, list(
                "stat[mwu]"=NaN,
                "p[mwu]"=''))
              
              message <- extractErrorMessage(res)
              if (message == 'grouping factor must have exactly 2 levels')
                message <- .('One or both groups do not contain enough observations')
              else if (message == 'not enough observations')
                message <- .('One or both groups do not contain enough observations')
              else if (message == 'data are essentially constant')
                message <- .('All observations are tied')
              
              mwtestTable$addFootnote(rowKey=depName, 'stat[mwu]', message)
              
            } else {
              
              mwtestTable$setRow(rowKey=depName, list(
                "stat[mwu]"=res$statistic,
                "p[mwu]"=res$p.value))
            }
          }

          if (self$options$zstat) {
            
            if (is.factor(dataMWTest$dep))
              res <- createError(.('Variable is not numeric'))
            else if (any(is.infinite(dataMWTest$dep)))
              res <- createError(.('Variable contains infinite values'))
            else
              res <- try(suppressWarnings(.zStattest(dep=dataMWTest$dep,group=dataMWTest$group,Ha=Ha)), silent=TRUE)
            
            if (isError(res)) {
              
              mwtestTable$setRow(rowKey=depName, list(
                "stat[zstat]"=NaN,
                "p[zstat]"=''))
              
              message <- extractErrorMessage(res)
              if (message == 'grouping factor must have exactly 2 levels')
                message <- .('One or both groups do not contain enough observations')
              else if (message == 'not enough observations')
                message <- .('One or both groups do not contain enough observations')
              else if (message == 'data are essentially constant')
                message <- .('All observations are tied')
              
              mwtestTable$addFootnote(rowKey=depName, 'stat[zstat]', message)
              
            } else {
              
              mwtestTable$setRow(rowKey=depName, list(
                "stat[zstat]"=res$statistic,
                "p[zstat]"=res$p.value))
              
              if(any(res$n<20) & any(res$t>1)){
                message2 <- .('Less than 20 observations and ties in the groups: p-value might not be adequate')
                mwtestTable$addFootnote(rowKey=depName, 'p[zstat]', message2)
              }              
            }
          }
          
          if (self$options$pSup) {
            if (is.factor(dataMWTest$dep))
              res <- createError(.('Variable is not numeric'))
            else if (any(is.infinite(dataMWTest$dep)))
              res <- createError(.('Variable contains infinite values'))
            else
              res <- try(suppressWarnings(rcompanion::wilcoxonPS(dep~group,data=dataMWTest)[[1]]), silent=TRUE)
            
            if (isError(res)) {
              
              mwtestTable$setRow(rowKey=depName, list(
                "stat[pSup]"=NaN))
              
              message <- extractErrorMessage(res)
              if (message == 'grouping factor must have exactly 2 levels')
                message <- .('One or both groups do not contain enough observations')
              else if (message == 'not enough observations')
                message <- .('One or both groups do not contain enough observations')
              else if (message == 'data are essentially constant')
                message <- .('All observations are tied')
              
              mwtestTable$addFootnote(rowKey=depName, 'stat[pSup]', message)
              
            } else {
              
              mwtestTable$setRow(rowKey=depName, list(
                "stat[pSup]"=res))
            }            
          }
          
          if (self$options$rankCorr) {
            if (is.factor(dataMWTest$dep))
              res <- createError(.('Variable is not numeric'))
            else if (any(is.infinite(dataMWTest$dep)))
              res <- createError(.('Variable contains infinite values'))
            else
              res <- try(suppressWarnings(.bisrankr(dep=dataMWTest$dep,group=dataMWTest$group)), silent=TRUE)
            
            if (isError(res)) {
              
              mwtestTable$setRow(rowKey=depName, list(
                "stat[rankCorr]"=NaN))
              
              message <- extractErrorMessage(res)
              if (message == 'grouping factor must have exactly 2 levels')
                message <- .('One or both groups do not contain enough observations')
              else if (message == 'not enough observations')
                message <- .('One or both groups do not contain enough observations')
              else if (message == 'data are essentially constant')
                message <- .('All observations are tied')
              
              mwtestTable$addFootnote(rowKey=depName, 'stat[rankCorr]', message)
              
            } else {
              
              mwtestTable$setRow(rowKey=depName, list(
                "stat[rankCorr]"=res$statistic))
            }            
          }
          
          if (self$options$fstat) {
            if (is.factor(dataMWTest$dep))
              res <- createError(.('Variable is not numeric'))
            else if (any(is.infinite(dataMWTest$dep)))
              res <- createError(.('Variable contains infinite values'))
            else
              res <- try(suppressWarnings(.fstat(dep=dataMWTest$dep,group=dataMWTest$group)), silent=TRUE)
            
            if (isError(res)) {
              
              mwtestTable$setRow(rowKey=depName, list(
                "stat[fstat]"=NaN))
              
              message <- extractErrorMessage(res)
              if (message == 'grouping factor must have exactly 2 levels')
                message <- .('One or both groups do not contain enough observations')
              else if (message == 'not enough observations')
                message <- .('One or both groups do not contain enough observations')
              else if (message == 'data are essentially constant')
                message <- .('All observations are tied')
              
              mwtestTable$addFootnote(rowKey=depName, 'stat[fstat]', message)
              
            } else {
              
              mwtestTable$setRow(rowKey=depName, list(
                "stat[fstat]"=res$statistic))
            }            
          }          
          
          if (self$options$desc) {
            
            descTable$setRow(rowKey=depName, list(
              "dep"=depName,
              "group[1]"=groupLevels[1],
              "num[1]"=n[1],
              "median[1]"=med[1],
              "rankA[1]"=avgR[1],
              "rankS[1]"=R[1],              
              "group[2]"=groupLevels[2],
              "num[2]"=n[2],
              "median[2]"=med[2],
              "rankA[2]"=avgR[2],
              "rankS[2]"=R[2]            
            ))
          }
          
          if (self$options$ciMedians) {
            
            cismedTable$setRow(rowKey=depName, list(
              "dep"=depName,
              "group[1]"=groupLevels[1],
              "median[1]"=med[1],
              "cilMed[1]"=cimed[1,1],
              "ciuMed[1]"=cimed[1,2],              
              "group[2]"=groupLevels[2],
              "num[2]"=n[2],
              "median[2]"=med[2],
              "cilMed[2]"=cimed[2,1],
              "ciuMed[2]"=cimed[2,2]              
            ))
          }
          
          if (self$options$ciHL) {
            
            cishlTable$setRow(rowKey=depName, list(
              "dep"=depName,
              "hlestimate"=cihl[1],
              "cilHL"=cihl[2],
              "ciuHL"=cihl[3]           
            ))
          }          
        }
        },
      .init=function() {
        
        hypothesis <- self$options$hypothesis
        groupName <- self$options$group
        reps <- self$options$numR
        
        groups <- NULL
        if ( ! is.null(groupName))
          groups <- base::levels(self$data[[groupName]])
        if (length(groups) != 2)
          groups <- c('Group 1', 'Group 2')
        
        table <- self$results$mwutest
        cisMed <- self$results$cisMed
        cisHL <- self$results$cisHL
        
         ciTitleString <- .('{ciWidth}% Confidence Interval')
         
         ciTitle <- jmvcore::format(ciTitleString, ciWidth=self$options$ciWidth)
         cisMed$getColumn('cilMed[1]')$setSuperTitle(ciTitle)
         cisMed$getColumn('ciuMed[1]')$setSuperTitle(ciTitle)
         cisMed$getColumn('cilMed[2]')$setSuperTitle(ciTitle)
         cisMed$getColumn('ciuMed[2]')$setSuperTitle(ciTitle)
         cisHL$getColumn('cilHL')$setSuperTitle(ciTitle)
         cisHL$getColumn('ciuHL')$setSuperTitle(ciTitle)
        
        if (hypothesis == 'oneGreater')
          table$setNote("hyp", jmvcore::format("H\u2090 \u03BE\u2009<sub>{}</sub> > \u03BE\u2009<sub>{}</sub>", groups[1], groups[2]))
        else if (hypothesis == 'twoGreater')
          table$setNote("hyp", jmvcore::format("H\u2090 \u03BE\u2009<sub>{}</sub> < \u03BE\u2009<sub>{}</sub>", groups[1], groups[2]))
        else
          table$setNote("hyp", jmvcore::format("H\u2090 \u03BE\u2009<sub>{}</sub> \u2260 \u03BE\u2009<sub>{}</sub>", groups[1], groups[2]))
         
        if(self$options$ciMedians & self$options$ciMethod=='boot')
          cisMed$setNote("numR", jmvcore::format(paste0("Number of replicates in Bootstrap CI(s): ",reps)))
        
      }   
      )
)
