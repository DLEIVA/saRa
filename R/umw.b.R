
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
        
        data <- jmvcore::select(self$data, varNames)
        
        for (name in depVarNames)
          data[[name]] <- jmvcore::toNumeric(data[[name]])
        data[[groupVarName]] <- droplevels(as.factor(data[[groupVarName]]))
        
        mwtestTable <- self$results$mwutest
        cismedTable <- self$results$cisMed
        cishlTable <- self$results$cisHL
        descTable <- self$results$desc

        confInt <- self$options$ciWidth / 100

        if (any(depVarNames == groupVarName))
          jmvcore::reject("Grouping variable '{a}' must not also be a dependent variable",
                          code="a_is_dependent_variable", a=groupVarName)
        
        # exclude rows with missings in the grouping variable
        data <- data[ ! is.na(data[[groupVarName]]),]
        
        groupLevels <- base::levels(data[[groupVarName]])
        
        if (length(groupLevels) != 2)
          jmvcore::reject("Grouping variable '{a}' must have exactly 2 levels",
                          code="grouping_var_must_have_2_levels", a=groupVarName)
        
        if (self$options$miss == "listwise") {
          data <- jmvcore::naOmit(data)
          if (dim(data)[1] == 0)
            jmvcore::reject("Grouping variable '{a}' has less than 2 levels after missing values are excluded",
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
        .zStattest <- .saraMWZ

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
          rangos <- jmvcore::tryNaN(rank(unlist(split(df$dep,df$group))))
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
        
        # Adapted from DescTools::MedianCI func
        .mybootMED <- .saraBootstrapMedian

        for (depName in depVarNames) {
          
          dataMWTest <- data.frame(dep=data[[depName]], group=data[[groupVarName]])
          
          if (self$options$miss == "perAnalysis")
            dataMWTest <- jmvcore::naOmit(dataMWTest)  
          
          groupLevels <- base::levels(dataMWTest$group)
          n <- tapply(dataMWTest$dep, dataMWTest$group, length)
          med <- tapply(dataMWTest$dep, dataMWTest$group, function(x) jmvcore::tryNaN(median(x)))
          rangos <- rank(dataMWTest$dep)
          R1 <- sum(rangos[dataMWTest$group == groupLevels[1]])
          R2 <- sum(rangos[dataMWTest$group == groupLevels[2]])
          R <- c(R1,R2)
          avgR1 <- R1/n[1]
          avgR2 <- R2/n[2]
          avgR <- c(avgR1,avgR2)
          
          n[is.na(n)] <- 0
          med[is.na(med)] <- NaN
          R[is.na(R)] <- NaN
          avgR[is.na(avgR)] <- NaN
          
          cimed <- matrix(NaN, nrow=2, ncol=2)
          if (self$options$ciMedians || self$options$plots) {
            ciResult <- try(suppressWarnings(t(vapply(split(dataMWTest$dep, dataMWTest$group), function(x) {
              if (self$options$ciMethod == 'exact')
                DescTools::MedianCI(x, conf.level=confInt, method='exact')[2:3]
              else .mybootMED(x, conf.level=confInt, R=self$options$numR)[2:3]
            }, numeric(2)))), silent=TRUE)
            if (!jmvcore::isError(ciResult)) cimed <- ciResult
            else cismedTable$addFootnote(rowKey=depName, 'cilMed[1]', jmvcore::extractErrorMessage(ciResult))
          }
          cihl <- rep(NaN, 3)
          if (self$options$ciHL) {
            hlResult <- try(suppressWarnings(.hl(dataMWTest$dep, dataMWTest$group, confInt)), silent=TRUE)
            if (!jmvcore::isError(hlResult)) cihl <- hlResult
            else cishlTable$addFootnote(rowKey=depName, 'hlestimate', jmvcore::extractErrorMessage(hlResult))
          }

          
          
          if (self$options$mwu) {
            
            if (is.factor(dataMWTest$dep))
              res <- jmvcore::createError(.('Variable is not numeric'))
            else if (any(is.infinite(dataMWTest$dep)))
              res <- jmvcore::createError(.('Variable contains infinite values'))
            else
              res <- try(suppressWarnings(wilcox.test(dep ~ group, data=dataMWTest,
                                alternative=Ha, conf.level=confInt)), silent=TRUE)
            
            if (jmvcore::isError(res)) {
              
              mwtestTable$setRow(rowKey=depName, list(
                "stat[mwu]"=NaN,
                "p[mwu]"=''))
              
              message <- .saraRankTestError(jmvcore::extractErrorMessage(res))
              
              mwtestTable$addFootnote(rowKey=depName, 'stat[mwu]', message)
              
            } else {
              
              mwtestTable$setRow(rowKey=depName, list(
                "stat[mwu]"=res$statistic,
                "p[mwu]"=res$p.value))
            }
          }

          if (self$options$zstat) {
            
            if (is.factor(dataMWTest$dep))
              res <- jmvcore::createError(.('Variable is not numeric'))
            else if (any(is.infinite(dataMWTest$dep)))
              res <- jmvcore::createError(.('Variable contains infinite values'))
            else
              res <- try(suppressWarnings(.zStattest(dep=dataMWTest$dep,group=dataMWTest$group,alternative=Ha)), silent=TRUE)
            
            if (jmvcore::isError(res)) {
              
              mwtestTable$setRow(rowKey=depName, list(
                "stat[zstat]"=NaN,
                "p[zstat]"=''))
              
              message <- .saraRankTestError(jmvcore::extractErrorMessage(res))
              
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
              res <- jmvcore::createError(.('Variable is not numeric'))
            else if (any(is.infinite(dataMWTest$dep)))
              res <- jmvcore::createError(.('Variable contains infinite values'))
            else
              res <- try(suppressWarnings(rcompanion::wilcoxonPS(dep~group,data=dataMWTest)[[1]]), silent=TRUE)
            
            if (jmvcore::isError(res)) {
              
              mwtestTable$setRow(rowKey=depName, list(
                "stat[pSup]"=NaN))
              
              message <- .saraRankTestError(jmvcore::extractErrorMessage(res))
              
              mwtestTable$addFootnote(rowKey=depName, 'stat[pSup]', message)
              
            } else {
              
              mwtestTable$setRow(rowKey=depName, list(
                "stat[pSup]"=res))
            }            
          }
          
          if (self$options$rankCorr) {
            if (is.factor(dataMWTest$dep))
              res <- jmvcore::createError(.('Variable is not numeric'))
            else if (any(is.infinite(dataMWTest$dep)))
              res <- jmvcore::createError(.('Variable contains infinite values'))
            else
              res <- try(suppressWarnings(.bisrankr(dep=dataMWTest$dep,group=dataMWTest$group)), silent=TRUE)
            
            if (jmvcore::isError(res)) {
              
              mwtestTable$setRow(rowKey=depName, list(
                "stat[rankCorr]"=NaN))
              
              message <- .saraRankTestError(jmvcore::extractErrorMessage(res))
              
              mwtestTable$addFootnote(rowKey=depName, 'stat[rankCorr]', message)
              
            } else {
              
              mwtestTable$setRow(rowKey=depName, list(
                "stat[rankCorr]"=res$statistic))
            }            
          }
          
          if (self$options$fstat) {
            if (is.factor(dataMWTest$dep))
              res <- jmvcore::createError(.('Variable is not numeric'))
            else if (any(is.infinite(dataMWTest$dep)))
              res <- jmvcore::createError(.('Variable contains infinite values'))
            else
              res <- try(suppressWarnings(.fstat(dep=dataMWTest$dep,group=dataMWTest$group)), silent=TRUE)
            
            if (jmvcore::isError(res)) {
              
              mwtestTable$setRow(rowKey=depName, list(
                "stat[fstat]"=NaN))
              
              message <- .saraRankTestError(jmvcore::extractErrorMessage(res))
              
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
          
          if (self$options$plots) {
            
            image <- self$results$plots$get(key=depName)$desc
            
            if (nrow(dataMWTest) > 0) {
              
              ciWidth <- self$options$ciWidth
              tail <- qnorm(1 - (100 - ciWidth) / 200)
              
              cies  <- cimed
              medians <- aggregate(dataMWTest$dep, by=list(dataMWTest$group),
                                   function(x) jmvcore::tryNaN(median(x)), simplify=FALSE)
              
              medianPlotData <- data.frame(group=medians$Group.1)
              medianPlotData <- cbind(medianPlotData, stat=unlist(medians$x))
              medianPlotData <- cbind(medianPlotData, stat=cimed)
              medianPlotData <- cbind(medianPlotData, type='median')
              
              plotData <- medianPlotData
              
              if (all(is.na(plotData$stat)))
                image$setState(NULL)
              else
                image$setState(plotData)
              
            } else {
              
              image$setState(NULL)
            }
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
        
      },
      .desc=function(image, ggtheme, theme, ...) {
        
        if (is.null(image$state))
          return(FALSE)
        
        groupName <- self$options$group
        
        ciw <- self$options$ciWidth
        
        pd <- ggplot2::position_dodge(0.2)
        
        plot <- ggplot2::ggplot(data=image$state, ggplot2::aes(x=group, y=stat, shape=type)) +
          ggplot2::geom_errorbar(ggplot2::aes(x=group, ymin=stat.1, ymax=stat.2, width=.1),
                        linewidth=.8, colour=theme$color[2], position=pd) +
          ggplot2::geom_point(ggplot2::aes(x=group, y=stat, shape=type), color=theme$color[1],
                     fill=theme$fill[1], size=3, position=pd) +
          ggplot2::labs(x=groupName, y=image$key) +
          ggplot2::scale_shape_manual(
            name='',
            values=c(median=22),
            labels=c(
              median=jmvcore::format(.('Median ({ciWidth}% CI)'), ciWidth=ciw)
            )
          ) +
          ggtheme +
          ggplot2::theme(
            plot.title=ggplot2::element_text(margin=ggplot2::margin(b = 5.5 * 1.2)),
            plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5)
          )
        
        return(plot)
      }      
      )
)
