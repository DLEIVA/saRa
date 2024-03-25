
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
          T <- wilcox.test(df$dep~df$group)$statistic
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
          np <- sum((column1 - column2) > 0, na.rm=TRUE)
          nn <- sum((column1 - column2) < 0, na.rm=TRUE)
          nt <- nTies
          RankSp <- sum(rangos[sign(rangos)>0]) 
          RankSn <- sum(rangos[sign(rangos)<0]) 
          RankSt <- ''     
          RankAp <- RankSp/np
          RankAn <- RankSn/nn
          RankAt <- ''          
          
        }
      }
    )
)
