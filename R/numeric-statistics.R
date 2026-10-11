# Shared by univariate and mixed-data analyses.
.saraComputeNumeric <- function(column, options, accessors) {
      stats <- list()
      
      total <- length(column)
      column <- jmvcore::naOmit(column)
      n <- length(column)
      stats[['n']] <- n
      stats[['missing']] <- total - n
      
      if (n > 0) {
        mediageom <- function(x) {
          if(is.factor(x)){ res <- NA} else{
            res <- ifelse(sum(x<=0,na.rm = TRUE)==0,exp(mean(log(x), na.rm = TRUE)),NA)
          }
          res
        }
        
        trimedia <- function(x){
          if(is.factor(x)){ res <- NA} else{
            FN <- fivenum(x)
            Finf <- FN[2]
            Md <- FN[3]
            Fsup <- FN[4]
            res <- (Finf+2*Md+Fsup)/4
          }
          res
        }
        
        promcuar <- function(x){
          if(is.factor(x)){ res <- NA} else{
            FN <- fivenum(x,na.rm=TRUE)
            Q1 <- FN[2]
            Q3 <- FN[4]
            res <- (Q1+Q3)/2
          }
          res
        }
        midR <- function(x){
          if(is.factor(x)){ res <- NA} else{
            res <- (min(x)+max(x))/2
          }
          res
        }
        
        sdgeom <- function(x) {
          if(is.factor(x)){ res <- NA} else{
            res <- ifelse(sum(x<=0,na.rm = TRUE)==0,exp(sd(log(x), na.rm = TRUE)),NA)
          }
          res
        } 
        
        desvmed <- function(x) {
          if(is.factor(x)){ res <- NA} else{
            .media <- mean(x, na.rm = TRUE)
            res <- sum(abs(x - .media), na.rm = TRUE)/length(na.omit(x))
          }
          res
        }
        
        CV <- function(x) {
          if(is.factor(x)){ res <- NA} else{
            .media <- mean(x, na.rm = TRUE)
            .dt <- sd(x, na.rm = TRUE)
            res <- .dt/.media
          }
          res
        }        
        
        rango <- function(x){
          if(is.factor(x)){ res <- NA} else{
            res <- max(x)-min(x)
          }
          res
        }
        
        IQR <- function(x){
          if(is.factor(x)){ res <- NA} else{
            qtls <- quantile(x,na.rm=TRUE) 
            Q1 <- qtls[2]
            Q3 <- qtls[4]
            res <- Q3-Q1
          }
          res
        }
        
        QD <- function(x){
          if(is.factor(x)){ res <- NA} else{
            qtls <- quantile(x,na.rm=TRUE) 
            Q1 <- qtls[2]
            Q3 <- qtls[4]
            res <- (Q3-Q1)/2
          }
          res
        }
        
        MAD <- function(x){
          if(is.factor(x)){ res <- NA} else{
            mediana <- median(x,na.rm=TRUE)
            res <- median(abs(x-mediana),na.rm=TRUE)
          }
          res
        }
        
        RCV <- function(x){
          if(is.factor(x)){ res <- NA} else{
            FN <- fivenum(x)
            Finf <- FN[2]
            Fsup <- FN[4]
            res <- (Fsup-Finf)/(Finf+Fsup)
          }
          res
        }
        
        beta1 <- function(x) {
          if(is.factor(x)){ res <- NA} else{
            .media <- mean(x, na.rm = TRUE)
            .n <- length(na.omit(x))
            res <- (sum((x - .media)^3, na.rm = TRUE)/.n)^2/(sum((x -.media)^2,
                                                                 na.rm = TRUE)/.n)^3
          }
          res
        }
        
        gamma1 <- function(x) {
          if(is.factor(x)){ res <- NA} else{
            .media <- mean(x, na.rm = TRUE)
            .dt <- sd(x, na.rm = TRUE)
            .n <- length(na.omit(x))
            res <- .n * sum((x - .media)^3, na.rm = TRUE)/((.n - 1) * (.n - 2))/(.dt^3)
          }
          res
        }
        
        beta2 <- function(x) {
          if(is.factor(x)){ res <- NA} else{
            .media <- mean(x, na.rm = TRUE)
            .n <- length(na.omit(x))
            res <- sum((x - .media)^4, na.rm = TRUE)/.n/(sum((x - .media)^2,
                                                             na.rm = TRUE)/.n)^2
          }
          res
        }
        
        gamma2 <- function(x) {
          if(is.factor(x)){ res <- NA} else{
            .media <- mean(x, na.rm = TRUE)
            .dt <- sd(x, na.rm = TRUE)
            .n <- length(na.omit(x))
            res <- (.n * (.n + 1) * sum((x - .media)^4, na.rm = TRUE)/
                      ((.n - 1) * (.n - 2) * (.n - 3)) - 3 * sum((x - .media)^2,
                                                                 na.rm = TRUE)^2/((.n - 2) * (.n - 3)))/(.dt^4)
          }
          res
        }        
        
        H1 <- function(x) {
          if(is.factor(x)){ res <- NA} else{
            FN <- fivenum(as.numeric(x))
            Finf <- FN[2]
            Md <- FN[3]
            Fsup <- FN[4]
            res <- (Finf+Fsup-2*Md)/(2*Md)
          }
          res
        }
        
        H3 <- function(x) {
          if(is.factor(x)){ res <- NA} else{
            AC90 <- quantile(as.numeric(x),probs=0.9,na.rm=TRUE)
            AC10 <- quantile(as.numeric(x),probs=0.1,na.rm=TRUE)
            Md <- fivenum(as.numeric(x))[3]
            res <- (AC90+AC10-2*Md)/(2*Md)
          }
          res
        }
        
        K2 <- function(x){
          if(is.factor(x)){ res <- NA} else{
            QU <- quantile(as.numeric(x),na.rm=TRUE) 
            AC90 <- quantile(as.numeric(x),probs=0.9,na.rm=TRUE)
            AC10 <- quantile(as.numeric(x),probs=0.1,na.rm=TRUE)
            Q1 <- QU[2]
            Q3 <- QU[4]
            res <- (AC90-AC10)/(1.9*(Q3-Q1))
          }
          res
        }
        
        K3 <- function(x){
          if(is.factor(x)){ res <- NA} else{
            FN <- fivenum(as.numeric(x))
            Einf <- quantile(as.numeric(x),probs=0.125,na.rm=TRUE)
            Esup <- quantile(as.numeric(x),probs=0.875,na.rm=TRUE)
            Finf <- FN[2]
            Fsup <- FN[4]
            res <- (Esup-Einf)/(1.7*(Fsup-Finf))   
          }
          res
        }
        
        if ( options$mean )
          stats[['mean']] <- ifelse(is.numeric(column),mean(column,na.rm=TRUE),
                                    NA)
        
        if ( options$median )
          stats[['median']] <- ifelse(is.numeric(column),median(column,
                                                                na.rm=TRUE),NA)
        
        if ( options$mode )
          stats[['mode']] <- 
          if(is.numeric(column)) {
            names(table(column)[ table(column) == max(table(column)) ])
          } else NA
        
        if ( options$geommean )
          stats[['geommean']] <- mediageom(column)          
        
        if ( options$trimean )
          stats[['trimean']] <- trimedia(column)
        
        if ( options$avquartile )
          stats[['avquartile']] <- promcuar(column)
        
        if ( options$midrange )
          stats[['midrange']] <- midR(column)
        
        if(options$trimmean){
          Trim <- accessors$trim()
          nTrim <- sum(!is.na(Trim))
          ifelse(is.numeric(column) && nTrim>0,
                 if( nTrim > 0 ){
                   res <- mean(column, trim = Trim, na.rm = TRUE)
                   stats[[paste0('trimmean')]] <- res
                 },NA)
        }
        
        if(options$winsmean){
          Win <- accessors$win()
          nWin <- sum(!is.na(Win))
          ifelse(is.numeric(column) && nWin>0,
                 if( nWin > 0 ){
                   res <- WRS2::winmean(column, tr = Win, na.rm = FALSE)
                   stats[[paste0('winsmean')]] <- res
                 },NA)
        }        
        
        if ( options$variance )
          stats[['variance']] <- ifelse(is.numeric(column),var(column,na.rm=TRUE),
                                        NA)   
        
        if ( options$stddev )
          stats[['stddev']] <- ifelse(is.numeric(column),sd(column,na.rm=TRUE),
                                      NA)
        
        if ( options$cv )
          stats[['cv']] <- CV(column)
        
        if ( options$geomsd )
          stats[['geomsd']] <- sdgeom(column)  
        
        if ( options$meandev )
          stats[['meandev']] <- desvmed(column)  
        
        if ( options$range )
          stats[['range']] <- rango(column)
        
        if ( options$iqr )
          stats[['iqr']] <- IQR(column)
        
        if (options$qd)
          stats[['qd']] <- QD(column)
        
        if (options$mad)
          stats[['mad']] <- MAD(column)
        
        if (options$rcv)
          stats[['rcv']] <- RCV(column)
        
        if(options$irange){
          NPerc <- accessors$inner()
          nNPerc <- sum(!is.na(NPerc))
          ifelse(is.numeric(column) && nNPerc>0,
                 if( nNPerc > 0 ){
                   .ACinf <- quantile(column,probs=(1-NPerc)/2,na.rm=TRUE)
                   .ACsup <- quantile(column,probs=1-(1-NPerc)/2,na.rm=TRUE)
                   res <- .ACsup-.ACinf
                   stats[[paste0('ir')]] <- res
                 },NA)
        }        
        
        if(options$min)
          stats[['min']] <- ifelse(is.numeric(column),min(column, na.rm=TRUE),
                                   NA)
        
        if(options$max)
          stats[['max']] <- ifelse(is.numeric(column),max(column, na.rm=TRUE),
                                   NA)
        
        if (options$q1)
          stats[['q1']] <- ifelse(is.numeric(column),quantile(column,
                                                              na.rm=TRUE)[2],NA)
        
        if (options$q2)
          stats[['q2']] <- ifelse(is.numeric(column),quantile(column,
                                                              na.rm=TRUE)[3],NA)
        
        if (options$q3)
          stats[['q3']] <- ifelse(is.numeric(column),quantile(column,
                                                              na.rm=TRUE)[4],NA)
        
        if(options$pc){
          pcValues <- accessors$percentiles()
          npcValues <- length(pcValues)
          
          if( npcValues > 0 ){
            if(is.factor(column)){ 
              for(i in 1:npcValues)
                stats[[paste0('perc', i)]] <- NA } else{
                  quants <- quantile(column, pcValues)
                  for(i in 1:npcValues)
                    stats[[paste0('perc', i)]] <- quants[i]
                }
          }
        }
        
        if (options$beta1)
          stats[['beta1']] <- beta1(column)
        
        if (options$gamma1)
          stats[['gamma1']] <- gamma1(column)
        
        if (options$h1)
          stats[['h1']] <- H1(column)
        
        if (options$h3)
          stats[['h3']] <- H3(column)        
        
        if (options$beta2)
          stats[['beta2']] <- beta2(column)
        
        if (options$gamma2)
          stats[['gamma2']] <- gamma2(column)        
        
        if (options$k2)
          stats[['k2']] <- K2(column)
        
        if (options$k3)
          stats[['k3']] <- K3(column)
        
      }
      else if (n==0) {
        stats <- list(
          n=0, missing=total, mean=NaN, median=NaN, mode=NaN, geommean=NaN, 
          trimean=NaN, avquartile=NaN, midrange=NaN, trimmean=NaN, winsmean= NaN,
          variance=NaN,stddev=NaN,geomsd=NaN,meandev=NaN,cv=NaN,
          range=NaN, iqr=NaN, qd=NaN, mad=NaN, rcv=NaN, ir= NaN, min=NaN, 
          max=NaN, q1=NaN, q2=NaN, q3=NaN,beta1=NaN,gamma1=NaN,
          h1=NaN, h3=NaN,beta2=NaN,gamma2=NaN, k2=NaN, k3=NaN
        )
      }
      return(stats)  
    }
