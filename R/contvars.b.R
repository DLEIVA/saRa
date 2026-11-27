
# This file is a generated template, your changes will not be overwritten

contvarsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "contvarsClass",
    inherit = contvarsBase,
    private = list(
      .init=function() {
        private$.initcontppdf()
        private$.initcontpcdf()         
        private$.initcontpsurv()                  
        private$.initcontpinterv()
        private$.initcontpicdf()         
        values <- private$.getcontProbValues()
        if (length(values) > 1L)
          for (k in seq.int(2L, length(values))) self$results$Contprobstab$addRow(rowKey=k)
        values <- private$.getcontQuantValues()
        if (length(values) > 1L)
          for (k in seq.int(2L, length(values))) self$results$Contquantstab$addRow(rowKey=k)
      },
      .run = function() {
        cdistros <- self$options$cdistros
        parameterNames <- switch(cdistros, norm="sigma", tdist="tnu", chisqdist="chinu",
                                 fdist=c("f1nu", "f2nu"), exp="rate", character())
        # Distribution names are validated independently of UI limits.
        if (cdistros %in% c("chisq", "chi")) parameterNames <- "chinu"
        if (cdistros %in% c("expon", "expdist")) parameterNames <- "rate"
        for (parameter in parameterNames)
          if (!is.finite(self$options[[parameter]]) || self$options[[parameter]] <= 0)
            jmvcore::reject("Scale, rate and degrees of freedom must be positive", code="invalid_distribution_parameters")
        if (cdistros %in% c("unif", "uniform") &&
            (!is.finite(self$options$unifmin) || !is.finite(self$options$unifmax) ||
             self$options$unifmin >= self$options$unifmax))
          jmvcore::reject("Uniform minimum must be smaller than maximum", code="invalid_distribution_parameters")
        ContInfoTab <- self$results$ContInfoTab
        Contprobstab <- self$results$Contprobstab
        Contquantstab <- self$results$Contquantstab
        
        if (cdistros=='norm'){
          cdistroslabel <- 'Normal'
          param1 <- paste('μ:',self$options$mu)
          param2 <- paste('σ:',self$options$sigma)  
          paramlabel <- paste(param1,param2,sep='; ')
        } else if (cdistros=='tdist'){
          cdistroslabel <- 't'
          param1 <- paste('\u03BD:',self$options$tnu)
          paramlabel <- param1
        } else if (cdistros=='chisqdist'){
          cdistroslabel <- '\u03c7\u00B2'
          param1 <- paste('\u03BD:',self$options$chinu)
          paramlabel <- param1
        } else if (cdistros=='fdist'){
          cdistroslabel <- 'F'
          param1 <- paste('\u03BD\u2081:',self$options$f1nu)
          param2 <- paste('\u03BD\u2082:',self$options$f2nu)          
          paramlabel <- paste(param1,param2,sep='; ')
        } else if (cdistros=='exp'){
          cdistroslabel <- 'Exponential'
          param1 <- paste('\u03bb:',self$options$rate)
          paramlabel <- param1            
        } else if (cdistros=='unif'){
          cdistroslabel <- 'Uniform'
          param1 <- paste('\u03b1:',self$options$unifmin)
          param2 <- paste('\u03b2:',self$options$unifmax)
          paramlabel <- paste(param1,param2,sep='; ')            
        }
        Info <- matrix(NA,nrow=1,ncol=2)
        
        Info[1,1] <- cdistroslabel
        Info[1,2] <- paramlabel
        
        ContInfoTab$setRow(rowNo=1, values=list(
          DistributionColumn=Info[1,1],
          ParametersColumn=Info[1,2]))
        
        ContvarValues <- private$.getcontProbValues()
        
        if(length(ContvarValues)>0){        
        
        ProbRowNo <- 1
        
        for(i in 1:length(ContvarValues)){
          varname <- paste0('x = ',ContvarValues[i])
          pdfval <- if(cdistros=='norm'){
            dnorm(ContvarValues[i],self$options$mu,self$options$sigma)
          } else if(cdistros=='tdist'){
            dt(ContvarValues[i],self$options$tnu)
          } else if(cdistros=='chisqdist'){
            dchisq(ContvarValues[i],self$options$chinu)                
          } else if(cdistros=='fdist'){
            df(ContvarValues[i],self$options$f1nu,self$options$f2nu)
          } else if(cdistros=='exp'){
            dexp(ContvarValues[i],self$options$rate)
          } else if(cdistros=='unif'){
            dunif(ContvarValues[i],self$options$unifmin,self$options$unifmax)
          }
          cdfval <- if(cdistros=='norm'){
            pnorm(ContvarValues[i],self$options$mu,self$options$sigma)
          } else if(cdistros=='tdist'){
            pt(ContvarValues[i],self$options$tnu)
          } else if(cdistros=='chisqdist'){
            pchisq(ContvarValues[i],self$options$chinu)                
          } else if(cdistros=='fdist'){
            pf(ContvarValues[i],self$options$f1nu,self$options$f2nu)
          } else if(cdistros=='exp'){
            pexp(ContvarValues[i],self$options$rate)
          } else if(cdistros=='unif'){
            punif(ContvarValues[i],self$options$unifmin,self$options$unifmax)
          }
          survval <- if(cdistros=='norm'){
            pnorm(ContvarValues[i],self$options$mu,self$options$sigma,lower.tail=FALSE)
          } else if(cdistros=='tdist'){
            pt(ContvarValues[i],self$options$tnu,lower.tail=FALSE)
          } else if(cdistros=='chisqdist'){
            pchisq(ContvarValues[i],self$options$chinu,lower.tail=FALSE)                
          } else if(cdistros=='fdist'){
            pf(ContvarValues[i],self$options$f1nu,self$options$f2nu,lower.tail=FALSE)
          } else if(cdistros=='exp'){
            pexp(ContvarValues[i],self$options$rate,lower.tail=FALSE)
          } else if(cdistros=='unif'){
            punif(ContvarValues[i],self$options$unifmin,self$options$unifmax,lower.tail=FALSE)
          }
          
          Contprobstab$setRow(rowNo=ProbRowNo, values=list(`contvarValues`=varname,
                                                           `contpdf`=pdfval,`contcdf`=cdfval,`contsurv`=survval))
          ProbRowNo <- ProbRowNo + 1
        }
        }
        ContquantValues <- private$.getcontQuantValues()
        
        if(length(ContquantValues)>0){
          
          ProbRowNo <- 1
          
          for(i in 1:length(ContquantValues)){
            varname <- paste0('Prob = ',ContquantValues[i])
            lTail <- if(cdistros=='norm'){
              qnorm(ContquantValues[i],self$options$mu,self$options$sigma)
            } else if(cdistros=='tdist'){
              qt(ContquantValues[i],self$options$tnu)
            } else if(cdistros=='chisqdist'){
              qchisq(ContquantValues[i],self$options$chinu)                
            } else if(cdistros=='fdist'){
              qf(ContquantValues[i],self$options$f1nu,self$options$f2nu)
            } else if(cdistros=='exp'){
              qexp(ContquantValues[i],self$options$rate)
            } else if(cdistros=='unif'){
              qunif(ContquantValues[i],self$options$unifmin,self$options$unifmax)
            }
            rTail <- if(cdistros=='norm'){
              qnorm(ContquantValues[i],self$options$mu,self$options$sigma,lower.tail=FALSE)
            } else if(cdistros=='tdist'){
              qt(ContquantValues[i],self$options$tnu,lower.tail=FALSE)
            } else if(cdistros=='chisqdist'){
              qchisq(ContquantValues[i],self$options$chinu,lower.tail=FALSE)                
            } else if(cdistros=='fdist'){
              qf(ContquantValues[i],self$options$f1nu,self$options$f2nu,lower.tail=FALSE)
            } else if(cdistros=='exp'){
              qexp(ContquantValues[i],self$options$rate,lower.tail=FALSE)
            } else if(cdistros=='unif'){
              qunif(ContquantValues[i],self$options$unifmin,self$options$unifmax,lower.tail=FALSE)
            }
            
            Contquantstab$setRow(rowNo=ProbRowNo, values=list(`contquantValues`=varname,
                                                              `contlTail`=lTail,`contrTail`=rTail))
            ProbRowNo <- ProbRowNo + 1
          }
        }
      },
      #### Plot functions ---- Adapted from distrACTION
      .initcontppdf = function() {
        image <- self$results$get('contpdfplot')
        
        width <- 450
        height <- 400
        
        image$setSize(width * 2, height)
      }, 
      .initcontpcdf = function() {
        image <- self$results$get('contcdfplot')
        
        width <- 450
        height <- 400
        
        image$setSize(width * 2, height)
      },
      .initcontpsurv = function() {
        image <- self$results$get('contsurvplot')
        
        width <- 450
        height <- 400
        
        image$setSize(width * 2, height)
      },
      .initcontpinterv = function() {
        image <- self$results$get('contintervplot')
        
        width <- 450
        height <- 400
        
        image$setSize(width * 2, height)
      },
      .initcontpicdf = function() {
        image <- self$results$get('conticdfplot')
        
        width <- 450
        height <- 400
        
        image$setSize(width * 2, height)
      },
      .contppdf = function(image, ggtheme, theme, ...) {
        if (!self$options$contppdf) return(FALSE)
        .saraDistributionPlot(self$options,"density",FALSE,ggtheme,theme,translate=function(message) .saraMessage(message,self))
      },
      .contpcdf = function(image, ggtheme, theme, ...) {
        if (!self$options$contpcdf) return(FALSE)
        .saraDistributionPlot(self$options,"cdf",FALSE,ggtheme,theme,translate=function(message) .saraMessage(message,self))
      },
      .contpsurv = function(image, ggtheme, theme, ...) {
        if (!self$options$contpsurv) return(FALSE)
        .saraDistributionPlot(self$options,"survival",FALSE,ggtheme,theme,translate=function(message) .saraMessage(message,self))
      },
      .contpinterv = function(image, ggtheme, theme, ...) {
        if (!self$options$contpinterv) return(FALSE)
        .saraDistributionPlot(self$options,"interval",FALSE,ggtheme,theme,translate=function(message) .saraMessage(message,self))
      },
      .contpicdf = function(image, ggtheme, theme, ...) {
        if (!self$options$contpicdf) return(FALSE)
        .saraDistributionPlot(self$options,"quantile",FALSE,ggtheme,theme,translate=function(message) .saraMessage(message,self))
      },
      #### Helper functions ----
      .getcontProbValues = function(){
        cdistros <- self$options$cdistros
        probValues <- self$options$contvaluesfunc
        if (!is.na(probValues) && is.character(probValues))
          probValues <- as.numeric(unlist(strsplit(probValues, ",")))
        if(any(is.na(as.numeric(probValues)))){
          jmvcore::reject(paste0('Random variable values must be a real number (',
                                 paste0(unlist(strsplit(self$options$contvaluesfunc, ","))[is.na(as.numeric(unlist(strsplit(self$options$contvaluesfunc, ","))))],collapse=','),').'))          
        }
        if(cdistros%in%c('chisqdist','fdist','exp')){
          if(any(probValues < 0)) jmvcore::reject(paste0('Random variable values must be non-negative (',
                                                  paste0(probValues[probValues < 0],collapse=','),').')) 
        }
        probValues <- sort(unique(probValues[!is.na(probValues)]))
        return(probValues)
      },
      .getcontQuantValues = function(){
        quantValues <- self$options$contqvalues
        if (!is.na(quantValues) && is.character(quantValues))
          quantValues <- as.numeric(unlist(strsplit(quantValues, ",")))
        if(any(is.na(as.numeric(quantValues)))){
          jmvcore::reject(paste0('Random variable values must be a real number between 0 and 1(',
                                 paste0(unlist(strsplit(self$options$contqvalues, ","))[is.na(as.numeric(unlist(strsplit(self$options$contqvalues, ","))))],collapse=','),').'))          
        }
        if(any(quantValues<0 | quantValues>1)){
          jmvcore::reject(paste0('Quantile values must be between 0 and 1 (',
                                 paste0(quantValues[quantValues<0 | quantValues>1],collapse=','),').'))
        }
        quantValues <- sort(unique(quantValues[!is.na(quantValues)]))
        return(quantValues)
      },
      .getcontppvalue = function(){
        cdistros <- self$options$cdistros
        ppval <- self$options$contppvalue
        if (!is.na(ppval) && is.character(ppval))
          ppval <- as.numeric(unlist(strsplit(ppval, ",")))
        if(cdistros%in%c('chisqdist','fdist','exp')){
          if(ppval<0) jmvcore::reject(paste0('Random variable value must be a non-negative number (',ppval,').')) 
        }
        ppval <- unique(ppval[!is.na(ppval)])
        return(ppval)
      },
      .getcontX1value = function(){
        cdistros <- self$options$cdistros
        X1val <- self$options$contx1value
        if (!is.na(X1val) && is.character(X1val))
          X1val <- as.numeric(unlist(strsplit(X1val, ",")))
        if(cdistros%in%c('chisqdist','fdist','exp')){
          if(X1val<0) jmvcore::reject(paste0('X1 must be a non-negative number (',X1val,').'))
        }
        X1val <- unique(X1val[!is.na(X1val)])[1]
        return(X1val)
      },
      .getcontX2value = function(){
        cdistros <- self$options$cdistros
        X2val <- self$options$contx2value
        if (!is.na(X2val) && is.character(X2val))
          X2val <- as.numeric(unlist(strsplit(X2val, ",")))
        if(cdistros%in%c('chisqdist','fdist','exp')){
          if(X2val<0) jmvcore::reject(paste0('X2 must be a non-negative number (',X2val,').'))
        }
        X2val <- unique(X2val[!is.na(X2val)])[1]
        return(X2val)
      },
      .getcontpqvalue = function(){
        pqvalue <- self$options$contpqvalue
        if (!is.na(pqvalue) && is.character(pqvalue))
          pqvalue <- as.numeric(unlist(strsplit(pqvalue, ",")))
        pqvalue <- unique(pqvalue[!is.na(pqvalue)])
        return(pqvalue)
      }      
    )
)