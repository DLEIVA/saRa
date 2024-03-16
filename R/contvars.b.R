
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
      }
      
    )
)