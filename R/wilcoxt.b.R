
# This file is a generated template, your changes will not be overwritten

wilcoxTClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "wilcoxTClass",
    inherit = wilcoxTBase,
    private = list(
      .run=function() {
        
        data <- self$data
        selectedPairs <- self$options$pairs
        selectedVariables <- unique(unlist(lapply(selectedPairs, function(pair) c(pair$i1, pair$i2))))
        if (length(selectedVariables) > 0) data <- data[selectedVariables]
        
        wilcoxttestTable <- self$results$wilcoxttest
        cismedTable <- self$results$cisMedps
        cishlTable <- self$results$cisHLps
        descTable <- self$results$descps
        
        confInt <- self$options$ciWidthps / 100
        
        if (self$options$get('missps') == 'listwise')
          data <- jmvcore::naOmit(data)
        
        confInt <- self$options$get("ciWidthps") / 100
        
        pairs <- self$options$get('pairs')        
        
        Ha <- .saraAlternative(self$options$hypothesisps)

        for (i in seq_along(pairs)) {
          pair <- pairs[[i]]
          
          name1 <- pair$i1
          name2 <- pair$i2
          
          if (is.null(name1) || is.null(name2))
            next()
          
          data[[name1]] <- jmvcore::toNumeric(data[[name1]])
          data[[name2]] <- jmvcore::toNumeric(data[[name2]])
          
          if (self$options$get('missps') == "perAnalysis") {
            pairsData <- jmvcore::naOmit(data[c(name1, name2)])
            column1 <- pairsData[[name1]]
            column2 <- pairsData[[name2]]
          } else {
            pairsData <- data[c(name1, name2)]
            column1 <- pairsData[[name1]]
            column2 <- pairsData[[name2]]
          }
          
          med1 <- jmvcore::tryNaN(median(column1))
          med2 <- jmvcore::tryNaN(median(column2))
          rankSummary <- .saraPairedRankSummary(column1,column2)
          nTies <- nt <- rankSummary$tied
          np <- rankSummary$positive; nn <- rankSummary$negative
          RankSp <- rankSummary$sumPositive; RankSn <- rankSummary$sumNegative
          RankAp <- rankSummary$meanPositive; RankAn <- rankSummary$meanNegative

          cimed <- matrix(NaN, nrow=2, ncol=2)
          if (self$options$ciMediansps || self$options$plotsps) {
            ciResult <- try(suppressWarnings(.saraMedianIntervals(
              pairsData,self$options$ciMethodps,confInt,self$options$numR)),silent=TRUE)
            if (!jmvcore::isError(ciResult)) cimed <- ciResult
            else cismedTable$addFootnote(rowKey=pair, 'cilMed[1]', jmvcore::extractErrorMessage(ciResult))
          }
          cihl <- rep(NaN, 3)
          if (self$options$ciHLps) {
            hlResult <- try(suppressWarnings(.saraHodgesLehmann(column1,column2,paired=TRUE,conf.level=confInt)), silent=TRUE)
            if (!jmvcore::isError(hlResult)) cihl <- hlResult
            else cishlTable$addFootnote(rowKey=pair, 'hlestimate', jmvcore::extractErrorMessage(hlResult))
          }

          if (is.factor(column1) || is.factor(column2)) {
            wilc <- jmvcore::createError(.('One or both variables are not numeric'))
          }
          else {
            wilc <- try(suppressWarnings(wilcox.test(column1, column2,
                    alternative=Ha, paired=TRUE, conf.int=FALSE)), silent=TRUE)
            zstat <- try(.saraSignedRanks(column1,column2,Ha), silent=TRUE)
            rankCorr <- try(list(statistic=.saraSignedRanks(column1,column2)$correlation), silent=TRUE)
          }
          
          if ( ! jmvcore::isError(wilc)) {
            if (self$options$wilcoxon) {            
            wilcoxttestTable$setRow(rowKey=pair, list(
              'stat[wilcoxon]'=wilc$statistic,
              'p[wilcoxon]'=wilc$p.value))}
            if (self$options$zstatps && !jmvcore::isError(zstat)) {
              wilcoxttestTable$setRow(rowKey=pair, list(
                'stat[zstatps]'=zstat$statistic,
                'p[zstatps]'=zstat$p.value))              
            }
            if (self$options$rankCorrps && !jmvcore::isError(rankCorr)) {
              wilcoxttestTable$setRow(rowKey=pair, list(
                'stat[rankCorrps]'=rankCorr$statistic))              
            }
            
            if (nTies > 0) {
              message <- jmvcore::format(.('{n} pair(s) of values were tied'), n=nTies)
              wilcoxttestTable$addFootnote(rowKey=pair, 'stat[wilcoxon]', message)
            }
            
          } else {
            
            wilcoxttestTable$setRow(rowKey=pair, list(
              'stat[wilcoxon]'=NaN,
              'p[wilcoxon]'='',
              'stat[zstatps]'=NaN,
              'p[zstatps]'='',
              'stat[rankCorrps]'=NaN))
            
            message <- jmvcore::extractErrorMessage(wilc)
            if (message == "not enough 'x' observations")
              message <- .('One or both variables do not contain enough observations')
            else if (message == 'missing value where TRUE/FALSE needed')
              message <- .('One or both variables contain infinite values')
            else if (message == 'cannot compute confidence interval when all observations are tied')
              message <- .('All observations are tied')
            else if (message == "'y' must be numeric")
              message <- .('One or both variables contain no observations')
            
            wilcoxttestTable$addFootnote(rowKey=pair, 'stat[wilcoxon]', message)
          }
          
          if (self$options$ciMediansps) {
            
            cismedTable$setRow(rowKey=pair, list(
              "var[1]"=name1,
              "median[1]"=med1,
              "cilMed[1]"=cimed[1,1],
              "ciuMed[1]"=cimed[1,2], 
              "var[2]"=name2,
              "median[2]"=med2,
              "cilMed[2]"=cimed[2,1],
              "ciuMed[2]"=cimed[2,2]              
            ))
          }  
          
          if (self$options$ciHLps) {
            
            cishlTable$setRow(rowKey=pair, list(
              "hlestimate"=cihl[1],
              "cilHL"=cihl[2],
              "ciuHL"=cihl[3]           
            ))
          }          
          
          if (self$options$descps) {
            categories <- c("Positive differences", "Negative differences", "Tied pairs")
            counts <- c(np, nn, nt)
            averages <- c(RankAp, RankAn, NaN)
            sums <- c(RankSp, RankSn, NaN)
            for (category in seq_along(categories))
              descTable$setRow(rowKey=list(pair=pair, category=category), list(
                var1=name1, var2=name2, sign=categories[category],
                num=counts[category], rankA=averages[category], rankS=sums[category]))
          }

          if (self$options$plotsps) {
            
            image <- self$results$plotsps$get(key=pair)$descps
            
            if (nrow(pairsData) > 0) {
              
              plotData <- .saraMedianPlotData(c(name1,name2),c(med1,med2),cimed)

              if (all(is.na(plotData$median)))
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
        
        hypothesis <- self$options$hypothesisps
        reps <- self$options$numR
        
        table <- self$results$wilcoxttest
        cisMed <- self$results$cisMedps
        cisHL <- self$results$cisHLps
        
        ciTitleString <- .('{ciWidth}% Confidence Interval')
        
        ciTitle <- jmvcore::format(ciTitleString, ciWidth=self$options$ciWidthps)
        cisMed$getColumn('cilMed[1]')$setSuperTitle(ciTitle)
        cisMed$getColumn('ciuMed[1]')$setSuperTitle(ciTitle)
        cisMed$getColumn('cilMed[2]')$setSuperTitle(ciTitle)
        cisMed$getColumn('ciuMed[2]')$setSuperTitle(ciTitle)
        cisHL$getColumn('cilHL')$setSuperTitle(ciTitle)
        cisHL$getColumn('ciuHL')$setSuperTitle(ciTitle)
        
        if (hypothesis == 'oneGreater')
          table$setNote("hyp", jmvcore::format("H\u2090 \u03BE\u2009<sub>{}</sub> > 0", .("Measure 1 - Measure 2")))
        else if (hypothesis == 'twoGreater')
          table$setNote("hyp", jmvcore::format("H\u2090 \u03BE\u2009<sub>{}</sub> < 0", .("Measure 1 - Measure 2")))
        else
          table$setNote("hyp", jmvcore::format("H\u2090 \u03BE\u2009<sub>{}</sub> \u2260 0", .("Measure 1 - Measure 2")))
        
        if(self$options$ciMediansps & self$options$ciMethodps=='boot')
          cisMed$setNote("numR", jmvcore::format(paste0("Number of replicates in Bootstrap CI(s): ",reps)))
        
        pairs <- self$options$pairs
        descTable <- self$results$descps
        plots <- self$results$plotsps
        
        for (i in seq_along(pairs)) {
          
          pair <- pairs[[i]]
          
          table$setRow(rowKey=pair, list(
            `var1`=pair$i1,
            `var2`=pair$i2))
          
          for (category in seq_len(3L))
            descTable$addRow(rowKey=list(pair=pair, category=category),
                             values=list(var1=pair$i1, var2=pair$i2))

          cisMed$setRow(rowKey=pair, list(
            `vars`=paste0(pair, collapse=' - ')))
          
          cisHL$setRow(rowKey=pair, list(
            `vars`=paste0(pair, collapse=' - ')))          
          
          plots$get(pair)$setTitle(paste0(pair, collapse=' - '))
        }        
      },
      .desc=function(image, ggtheme, theme, ...) {
        label <- jmvcore::format(.("Median ({ciWidth}% CI)"),ciWidth=self$options$ciWidthps)
        .saraDrawMedianPlot(image$state,label,ggtheme=ggtheme,theme=theme)
      }
    )
)
