
# This file is a generated template, your changes will not be overwritten

contvarsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "contvarsClass",
    inherit = contvarsBase,
    private = list(
      .init=function() {
        #private$.initcontppdf()
        #private$.initcontpcdf()         
        #private$.initcontpsurv()                  
        #private$.initcontpinterv()
        #private$.initcontpicdf()         
      },
      .run = function() {
        cdistros <- self$options$cdistros
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
          if(length(ContvarValues) > 1){
            for(k in 2:length(ContvarValues))
              Contprobstab$addRow(rowKey=k,values=list(`x`='',
                                                       `contpdf`='',`contcdf`='',`contsurv`=''))
          }        
        
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
          if(length(ContquantValues) > 1){
            for(k in 2:length(ContquantValues))
              Contquantstab$addRow(rowKey=k,values=list(`Prob`='',
                                                        `lTail`='',`rTail`=''))
          }
          
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
      
      #### Helper functions ----
      .getcontProbValues = function(){
        cdistros <- self$options$cdistros
        probValues <- self$options$contvaluesfunc
        if (!is.na(probValues) && is.character(probValues))
          probValues <- as.numeric(unlist(strsplit(probValues, ",")))
        if(cdistros%in%c('chisqdist','fdist','exp')){
          probValues[probValues < 0] <- NA 
        }
        if(cdistros=='unif'){
          probValues[probValues < self$options$unifmin |probValues > self$options$unifmax] <- NA           
        }
        probValues <- unique(probValues[!is.na(probValues)])
        return(probValues)
      },
      .getcontQuantValues = function(){
        quantValues <- self$options$contqvalues
        if (!is.na(quantValues) && is.character(quantValues))
          quantValues <- as.numeric(unlist(strsplit(quantValues, ",")))
        quantValues[quantValues < 0 | quantValues > 1] <- NA
        quantValues <- unique(quantValues[!is.na(quantValues)])
        return(quantValues)
      }    
    )
)