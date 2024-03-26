
# This file is a generated template, your changes will not be overwritten

wilcoxTClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "wilcoxTClass",
    inherit = wilcoxTBase,
    private = list(
      .run=function() {
        
        data <- self$data
        
        wilcoxttestTable <- self$results$wilcoxttest
        cismedTable <- self$results$cisMedps
        cishlTable <- self$results$cisHLps
        descTable <- self$results$descps
        
        confInt <- self$options$ciWidthps / 100
        
        if (self$options$get('missps') == 'listwise')
          data <- naOmit(data)
        
        confInt <- self$options$get("ciWidthps") / 100
        
        pairs <- self$options$get('pairs')        
        
        ## Hypothesis options checking
        if (self$options$hypothesisps == 'oneGreater')
          Ha <- "greater"
        else if (self$options$hypothesisps == 'twoGreater')
          Ha <- "less"
        else
          Ha <- "two.sided"
        
        ## Additional functions                
        .zStattest <- function(column1,column2,Ha){
          df <- data.frame(column1,column2)
          med <- c(median(df$column1),median(df$column2))
          diffs <- df$column1-df$column2
          diffs <- diffs[diffs!=0]
          n <- length(diffs)
          t <- table(abs(diffs))
          mu <- n*(n+1)/4
          sigma <- if(any(t>1)){
            sqrt(n*(n+1)*(2*n+1)/24-1/48*sum(t^3-t))
          } else{sqrt(n*(n+1)*(2*n+1)/24)}
          T <- wilcox.test(df$column1,df$column2)$statistic
          statistic <- (T-mu)/sigma
          names(statistic) <- NULL
          p.value <- min(1-pnorm(abs(statistic)),pnorm(abs(statistic)))
          p.value <- if(Ha=='two.sided'){(p.value)*2
          } else if((Ha=='greater' & med[1]<med[2]) | (Ha=='less' & med[1]>med[2])){
            1-p.value
          } else{p.value}
          names(p.value) <- NULL
          list(statistic=statistic,p.value=p.value,mu=mu,sigma=sigma,n=n,t=t)
        } 
        
        .bisrankr <- function(column1,column2){
          df <- data.frame(column1,column2)
          diffs <- df$column1-df$column2
          nTies <- sum(diffs==0)
          diffs <- diffs[diffs!=0]
          n <- length(diffs)
          T <- wilcox.test(df$column1,df$column2,paired=TRUE)$statistic
          totalRankSum <- ((n-nTies) * ((n-nTies) + 1)) / 2
          statistic <- (2 * (T / totalRankSum)) - 1
          list(statistic=statistic)
        }
        
        .hl <- function(column1,column2,conflevel){
          df <- data.frame(column1,column2)
          test <- wilcox.test(df$column1,df$column2,conf.int=TRUE,conf.level=conflevel)
          results <- c(test$estimate,test$conf.int)
          names(results) <- NULL
          results
        }          
        
        for (i in seq_along(pairs)) {
          pair <- pairs[[i]]
          
          name1 <- pair$i1
          name2 <- pair$i2
          
          if (is.null(name1) || is.null(name2))
            next()
          
          data[[name1]] <- jmvcore::toNumeric(data[[name1]])
          data[[name2]] <- jmvcore::toNumeric(data[[name2]])
          
          if (self$options$get('missps') == "perAnalysis") {
            pairsData <- naOmit(data[c(name1, name2)])
            column1 <- pairsData[[name1]]
            column2 <- pairsData[[name2]]
          } else {
            column1 <- data[[name1]]
            column2 <- data[[name2]]
          }
          
          med1 <- tryNaN(median(column1))
          med2 <- tryNaN(median(column2))
          nTies <- sum((column1 - column2) == 0, na.rm=TRUE) 
          
          diffs <- column1-column2
          diffs <- diffs[diffs!=0]
          rangos <- rank(abs(diffs))
          n <- length(diffs)
          np <- sum((column1 - column2) > 0, na.rm=TRUE)
          nn <- sum((column1 - column2) < 0, na.rm=TRUE)
          nt <- nTies
          RankSp <- sum(rangos[sign(diffs)>0]) 
          RankSn <- sum(rangos[sign(diffs)<0]) 
          RankSt <- ''     
          RankAp <- RankSp/np
          RankAn <- RankSn/nn
          RankAt <- ''    
          
          if(self$options$ciMethodps=='exact'){
            cimed <- t(apply(pairsData,2,
                             function(x) DescTools::MedianCI(x,method='exact')))[,2:3]
          } else if(self$options$ciMethodps=='boot'){
            cimed <- t(apply(pairsData,2,
                             function(x) DescTools::MedianCI(x,method='boot',R=self$options$numR)))[,2:3]
          }
          
          cimed[is.na(cimed)] <- NaN
          
          if(self$options$ciHLps){
            cihl <- .hl(column1,column2,confInt)
            cihl[is.na(cihl)] <- NaN
          }
          
          
          if (is.factor(column1) || is.factor(column2)) {
            wilc <- createError(.('One or both variables are not numeric'))
          }
          else {
            wilc <- try(suppressWarnings(wilcox.test(column1, column2,
                    alternative=Ha, paired=TRUE, conf.int=TRUE, conf.level=confInt)), silent=TRUE)
            zstat <- .zStattest(column1,column2,Ha)
            rankCorr <- .bisrankr(column1,column2)
          }
          
          if ( ! isError(wilc)) {
            if (self$options$wilcoxon) {            
            wilcoxttestTable$setRow(rowKey=pair, list(
              'stat[wilcoxon]'=wilc$statistic,
              'p[wilcoxon]'=wilc$p.value))}
            if (self$options$zstatps) {
              wilcoxttestTable$setRow(rowKey=pair, list(
                'stat[zstatps]'=zstat$statistic,
                'p[zstatps]'=zstat$p.value))              
            }
            if (self$options$rankCorrps) {
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
            
            message <- extractErrorMessage(wilc)
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
              "var[1]"=name2,
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
            
            descTable$setRow(rowKey=pair, list(
              "name[1]"='Positive differences',
              "num[1]"=np,
              "rankA[1]"=RankAp,
              "rankS[1]"=RankSp,
              "name[2]"='Negative differences',              
              "num[2]"=nn,
              "rankA[2]"=RankAn,
              "rankS[2]"=RankSn,
              "name[3]"='Tied pairs',
              "num[3]"= nt,
              "rankA[3]"=RankAt,
              "rankS[3]"=RankSt            
            ))
          }
          
          if (self$options$plotsps) {
            
            image <- self$results$plotsps$get(key=pair)$descps
            
            if (nrow(pairsData) > 0) {
              
              ciWidth <- self$options$ciWidthps
              tail <- qnorm(1 - (100 - ciWidth) / 200)
              
              cies  <- cimed
              medianPlotData <- data.frame(group=c(name1,name2),
                                    stat=matrix(apply(pairsData,2,median),nrow=2,ncol=1))
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
          table$setNote("hyp", jmvcore::format("H\u2090 \u03BC\u2009<sub>{}</sub> > 0", .("Measure 1 - Measure 2")))
        else if (hypothesis == 'twoGreater')
          table$setNote("hyp", jmvcore::format("H\u2090 \u03BC\u2009<sub>{}</sub> < 0", .("Measure 1 - Measure 2")))
        else
          table$setNote("hyp", jmvcore::format("H\u2090 \u03BC\u2009<sub>{}</sub> \u2260 0", .("Measure 1 - Measure 2")))
        
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
          
          descTable$setRow(rowKey=pair, list(
            `var1[1]`=pair$i1,
            `var2[1]`=pair$i2))
          
          cisMed$setRow(rowKey=pair, list(
            `vars`=paste0(pair, collapse=' - ')))
          
          cisHL$setRow(rowKey=pair, list(
            `vars`=paste0(pair, collapse=' - ')))          
          
          plots$get(pair)$setTitle(paste0(pair, collapse=' - '))
        }        
      },
      .desc=function(image, ggtheme, theme, ...) {
        
        if (is.null(image$state))
          return(FALSE)
        
        groupName <- image$state['group']
        
        ciw <- self$options$ciWidthps
        
        pd <- position_dodge(0.2)
        
        plot <- ggplot(data=image$state, aes(x=group, y=stat, shape=type)) +
          geom_errorbar(aes(x=group, ymin=stat.lwr.ci, ymax=stat.upr.ci, width=.1),
                        size=.8, colour=theme$color[2], position=pd) +
          geom_point(aes(x=group, y=stat, shape=type), color=theme$color[1],
                     fill=theme$fill[1], size=3, position=pd) +
          labs(x=NULL, y=NULL) +
          scale_shape_manual(
            name='',
            values=c(median=22),
            labels=c(
              median=jmvcore::format(.('Median ({ciWidth}% CI)'), ciWidth=ciw)
            )
          ) +
          ggtheme +
          theme(
            plot.title=ggplot2::element_text(margin=ggplot2::margin(b = 5.5 * 1.2)),
            plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5)
          )
        
        return(plot)
      }      
    )
)
