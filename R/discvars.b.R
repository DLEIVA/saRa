discvarsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "discvarsClass",
    inherit = discvarsBase,
     private = list(
       .init=function() {
      },       
        .run = function() {
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
          }
          Info <- matrix(NA,nrow=1,ncol=2)
          
          Info[1,1] <- distroslabel
          Info[1,2] <- paramlabel
          
          InfoTab$setRow(rowNo=1, values=list(
            DistributionColumn=Info[1,1],
            ParametersColumn=Info[1,2]))
          
          varValues <- private$.getProbValues()
          
          if(length(varValues)>0){
            if(length(varValues) > 1){
              for(k in 2:length(varValues))
                probstab$addRow(rowKey=k,values=list(`x`='',
                                                     `pmf`='',`cdf`='',`surv`=''))
            }
            
            ProbRowNo <- 1
            
            for(i in 1:length(varValues)){
              varname <- paste0('x = ',varValues[i])
              pmfval <- if(distros=='binom'){
                dbinom(varValues[i],self$options$binomn,self$options$binomp)
              } else if(distros=='poiss'){
                dpois(varValues[i],self$options$lambda)
              }
              cdfval <- if(distros=='binom'){
                pbinom(varValues[i],self$options$binomn,self$options$binomp)
              } else if(distros=='poiss'){
                ppois(varValues[i],self$options$lambda)
              }
              survval <- if(distros=='binom'){
                pbinom(varValues[i],self$options$binomn,self$options$binomp,lower.tail=FALSE)
              } else if(distros=='poiss'){
                ppois(varValues[i],self$options$lambda,lower.tail=FALSE)
              }
              
              probstab$setRow(rowNo=ProbRowNo, values=list(`varValues`=varname,
                                                           `pmf`=pmfval,`cdf`=cdfval,`surv`=survval))
              ProbRowNo <- ProbRowNo + 1
            }
          }
          
          quantValues <- private$.getQuantValues()
          
          if(length(quantValues)>0){
            if(length(quantValues) > 1){
              for(k in 2:length(quantValues))
                quantstab$addRow(rowKey=k,values=list(`Prob`='',
                                                     `lTail`='',`rTail`=''))
            }
            
            ProbRowNo <- 1
            
            for(i in 1:length(quantValues)){
              varname <- paste0('Prob = ',quantValues[i])
              lTail <- if(distros=='binom'){
                qbinom(quantValues[i],self$options$binomn,self$options$binomp)
              } else if(distros=='poiss'){
                qpois(quantValues[i],self$options$lambda)
              }
              rTail <- if(distros=='binom'){
                qbinom(quantValues[i],self$options$binomn,self$options$binomp,lower.tail=FALSE)
              } else if(distros=='poiss'){
                qpois(quantValues[i],self$options$lambda,lower.tail=FALSE)
              }
              
              quantstab$setRow(rowNo=ProbRowNo, values=list(`quantValues`=varname,
                                                           `lTail`=lTail,`rTail`=rTail))
              ProbRowNo <- ProbRowNo + 1
            }
          }          
          
        },
      #### Init plots ----

    #### Helper functions ----
      .getProbValues = function(){
        probValues <- self$options$valuesfunc
        if (!is.na(probValues) && is.character(probValues))
          probValues <- as.numeric(unlist(strsplit(probValues, ",")))
        probValues[probValues < 0 ] <- NA
        probValues <- unique(probValues[!is.na(probValues)])
        return(probValues)
      },
    .getQuantValues = function(){
      quantValues <- self$options$qvalues
      if (!is.na(quantValues) && is.character(quantValues))
        quantValues <- as.numeric(unlist(strsplit(quantValues, ",")))
      quantValues[quantValues < 0 | quantValues > 1] <- NA
      quantValues <- unique(quantValues[!is.na(quantValues)])
      return(quantValues)
    }    
      )
)
