discvarsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "discvarsClass",
    inherit = discvarsBase,
     private = list(
       .init=function() {
         private$.initppmf()
         private$.initpcdf()         
         private$.initpsurv()                  
         private$.initpinterv()
         private$.initpicdf()         
        values <- private$.getProbValues()
        if (length(values) > 1L)
          for (k in seq.int(2L, length(values))) self$results$probstab$addRow(rowKey=k)
        values <- private$.getQuantValues()
        if (length(values) > 1L)
          for (k in seq.int(2L, length(values))) self$results$quantstab$addRow(rowKey=k)
      },       
        .run = function() {
        convention <- switch(self$options$distros,
          geom=.("X counts failures before the first success (0, 1, 2, ...)."),
          negbinom=.("X counts failures before the requested number of successes (0, 1, 2, ...)."),NULL)
        self$results$InfoTab$setNote("counting",convention)

        .saraDiscreteUpper(self$options)
          distros <- self$options$distros
          InfoTab <- self$results$InfoTab
          probstab <- self$results$probstab
          quantstab <- self$results$quantstab
          
          if (distros=='binom'){
            distroslabel <- 'Binomial'
            param1 <- paste('n:',self$options$binomn)
            param2 <- paste('p:',self$options$binomp)  
            paramlabel <- paste(param1,param2,sep='; ')
          } else if (distros=='poiss'){
            distroslabel <- 'Poisson'
            param1 <- paste('\u3bb:',self$options$lambda)
            paramlabel <- param1
          } else if (distros=='negbinom'){
            distroslabel <- 'Negative Binomial'
            param1 <- paste('r:',self$options$negbinomr)
            param2 <- paste('p:',self$options$negbinomp)  
            paramlabel <- paste(param1,param2,sep='; ')
          } else if (distros=='geom'){
            distroslabel <- 'Geometric'
            param1 <- paste('p:',self$options$geomp)
            paramlabel <- param1
          } else if (distros=='hypergeom'){
            distroslabel <- 'Hypergeometric'
            param1 <- paste('m:',self$options$hyperm)
            param2 <- paste('n:',self$options$hypern)
            param3 <- paste('k:',self$options$hyperk)
            paramlabel <- paste(param1,param2,param3,sep='; ')            
          }
          Info <- matrix(NA,nrow=1,ncol=2)
          
          Info[1,1] <- distroslabel
          Info[1,2] <- paramlabel
          
          InfoTab$setRow(rowNo=1, values=list(
            DistributionColumn=Info[1,1],
            ParametersColumn=Info[1,2]))
          
          varValues <- private$.getProbValues()
          
          if(length(varValues)>0){
            
            ProbRowNo <- 1
            
            for(i in 1:length(varValues)){
              varname <- paste0('x = ',varValues[i])
              pmfval <- if(distros=='binom'){
                dbinom(varValues[i],self$options$binomn,self$options$binomp)
              } else if(distros=='poiss'){
                dpois(varValues[i],self$options$lambda)
              } else if(distros=='negbinom'){
                dnbinom(varValues[i],self$options$negbinomr,self$options$negbinomp)                
              } else if(distros=='geom'){
                dgeom(varValues[i],self$options$geomp)
              } else if(distros=='hypergeom'){
                dhyper(varValues[i],self$options$hyperm,self$options$hypern,self$options$hyperk)
              }
              cdfval <- if(distros=='binom'){
                pbinom(varValues[i],self$options$binomn,self$options$binomp)
              } else if(distros=='poiss'){
                ppois(varValues[i],self$options$lambda)
              } else if(distros=='negbinom'){
                pnbinom(varValues[i],self$options$negbinomr,self$options$negbinomp)                
              } else if(distros=='geom'){
                pgeom(varValues[i],self$options$geomp)
              } else if(distros=='hypergeom'){
                phyper(varValues[i],self$options$hyperm,self$options$hypern,self$options$hyperk)
              }
              survval <- if(distros=='binom'){
                pbinom(varValues[i],self$options$binomn,self$options$binomp,lower.tail=FALSE)
              } else if(distros=='poiss'){
                ppois(varValues[i],self$options$lambda,lower.tail=FALSE)
              } else if(distros=='negbinom'){
                pnbinom(varValues[i],self$options$negbinomr,self$options$negbinomp,
                        lower.tail=FALSE)                
              } else if(distros=='geom'){
                pgeom(varValues[i],self$options$geomp,lower.tail=FALSE)
              } else if(distros=='hypergeom'){
                phyper(varValues[i],self$options$hyperm,self$options$hypern,
                       self$options$hyperk,lower.tail=FALSE)
              }
              
              probstab$setRow(rowNo=ProbRowNo, values=list(`varValues`=varname,
                                                           `pmf`=pmfval,`cdf`=cdfval,`surv`=survval))
              ProbRowNo <- ProbRowNo + 1
            }
          }
          
          quantValues <- private$.getQuantValues()
          
          if(length(quantValues)>0){
            
            ProbRowNo <- 1
            
            for(i in 1:length(quantValues)){
              varname <- paste0('Prob = ',quantValues[i])
              lTail <- if(distros=='binom'){
                qbinom(quantValues[i],self$options$binomn,self$options$binomp)
              } else if(distros=='poiss'){
                qpois(quantValues[i],self$options$lambda)
              } else if(distros=='negbinom'){
                qnbinom(quantValues[i],self$options$negbinomr,self$options$negbinomp)                
              } else if(distros=='geom'){
                qgeom(quantValues[i],self$options$geomp)
              } else if(distros=='hypergeom'){
                qhyper(quantValues[i],self$options$hyperm,self$options$hypern,self$options$hyperk)
              }
              rTail <- if(distros=='binom'){
                qbinom(quantValues[i],self$options$binomn,self$options$binomp,lower.tail=FALSE)
              } else if(distros=='poiss'){
                qpois(quantValues[i],self$options$lambda,lower.tail=FALSE)
              } else if(distros=='negbinom'){
                qnbinom(quantValues[i],self$options$negbinomr,self$options$negbinomp,lower.tail=FALSE)                
              } else if(distros=='geom'){
                qgeom(quantValues[i],self$options$geomp,lower.tail=FALSE)
              } else if(distros=='hypergeom'){
                qhyper(quantValues[i],self$options$hyperm,self$options$hypern,self$options$hyperk,lower.tail=FALSE) 
              }
              
              quantstab$setRow(rowNo=ProbRowNo, values=list(`quantValues`=varname,
                                                           `lTail`=lTail,`rTail`=rTail))
              ProbRowNo <- ProbRowNo + 1
            }
          }
        },
      #### Plot functions ---- Adapted from distrACTION
      .initppmf = function() {
        image <- self$results$get('pmfplot')
        
        width <- 450
        height <- 400
        
        image$setSize(width * 2, height)
      }, 
      .initpcdf = function() {
        image <- self$results$get('cdfplot')
        
        width <- 450
        height <- 400
        
        image$setSize(width * 2, height)
      },
      .initpsurv = function() {
        image <- self$results$get('survplot')
        
        width <- 450
        height <- 400
        
        image$setSize(width * 2, height)
      },
      .initpinterv = function() {
        image <- self$results$get('intervplot')
        
        width <- 450
        height <- 400
        
        image$setSize(width * 2, height)
      },
      .initpicdf = function() {
        image <- self$results$get('icdfplot')
        
        width <- 450
        height <- 400
        
        image$setSize(width * 2, height)
      },      
      .ppmf = function(image, ggtheme, theme, ...) {
        if (!self$options$ppmf) return(FALSE)
        .saraDistributionPlot(self$options,"density",TRUE,ggtheme,theme,translate=function(message) .saraMessage(message,self))
      },
      .pcdf = function(image, ggtheme, theme, ...) {
        if (!self$options$pcdf) return(FALSE)
        .saraDistributionPlot(self$options,"cdf",TRUE,ggtheme,theme,translate=function(message) .saraMessage(message,self))
      },
      .psurv = function(image, ggtheme, theme, ...) {
        if (!self$options$psurv) return(FALSE)
        .saraDistributionPlot(self$options,"survival",TRUE,ggtheme,theme,translate=function(message) .saraMessage(message,self))
      },
      .pinterv = function(image, ggtheme, theme, ...) {
        if (!self$options$pinterv) return(FALSE)
        .saraDistributionPlot(self$options,"interval",TRUE,ggtheme,theme,translate=function(message) .saraMessage(message,self))
      },
      .picdf = function(image, ggtheme, theme, ...) {
        if (!self$options$picdf) return(FALSE)
        .saraDistributionPlot(self$options,"quantile",TRUE,ggtheme,theme,translate=function(message) .saraMessage(message,self))
      },
    #### Helper functions ----
      .getProbValues = function(){
        probValues <- self$options$valuesfunc
        if (!is.na(probValues) && is.character(probValues))
          probValues <- as.numeric(unlist(strsplit(probValues, ",")))
        if(any(!is.finite(as.numeric(probValues)))){
          jmvcore::reject(paste0('Random variable values must be non-negative integers (',
                                 paste0(unlist(strsplit(self$options$valuesfunc, ","))[is.na(as.numeric(unlist(strsplit(self$options$valuesfunc, ","))))],collapse=','),').'))          
        }        
        if(any(!(probValues%%1==0 & sign(probValues)>=0))){
          jmvcore::reject(paste0("Random variable values must be non-negative integers (",
                                 paste0(probValues[!(probValues%%1==0 & sign(probValues)>=0)],
                                        collapse=','),")."))
        }
        probValues <- sort(unique(probValues[!is.na(probValues)]))
        return(probValues)
      },
    .getQuantValues = function(){
      quantValues <- self$options$qvalues
      if (!is.na(quantValues) && is.character(quantValues))
        quantValues <- as.numeric(unlist(strsplit(quantValues, ",")))
      if(any(is.na(as.numeric(quantValues)))){
        jmvcore::reject(paste0('Random variable values must be a real number between 0 and 1(',
                               paste0(unlist(strsplit(self$options$qvalues, ","))[is.na(as.numeric(unlist(strsplit(self$options$qvalues, ","))))],collapse=','),').'))          
      }
      if(any(quantValues<0 | quantValues>1)){
        jmvcore::reject(paste0("Quantile values must be real numbers between 0 and 1 (",
                               paste0(quantValues[quantValues<0 | quantValues>1],
                                      collapse=','),")."))
      }      
      quantValues <- sort(unique(quantValues[!is.na(quantValues)]))
      return(quantValues)
    },
    .getppvalue = function(){
      ppval <- self$options$ppvalue
      if (!is.na(ppval) && is.character(ppval))
        ppval <- as.numeric(unlist(strsplit(ppval, ",")))
      ppval <- unique(ppval[!is.na(ppval)])[1]
      return(ppval)
    },    
    .getX1value = function(){
      X1val <- self$options$x1value
      if (!is.na(X1val) && is.character(X1val))
        X1val <- as.numeric(unlist(strsplit(X1val, ",")))
      X1val <- unique(X1val[!is.na(X1val)])[1]
      return(X1val)
    },
    .getX2value = function(){
      X2val <- self$options$x2value
      if (!is.na(X2val) && is.character(X2val))
        X2val <- as.numeric(unlist(strsplit(X2val, ",")))
      X2val <- unique(X2val[!is.na(X2val)])[1]
      return(X2val)
    },
    .getpqvalue = function(){
      pqvalue <- self$options$pqvalue
      if (!is.na(pqvalue) && is.character(pqvalue))
        pqvalue <- as.numeric(unlist(strsplit(pqvalue, ",")))
      pqvalue <- unique(pqvalue[!is.na(pqvalue)])
      return(pqvalue)
    }  
      )
)
