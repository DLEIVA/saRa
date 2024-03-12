discvarsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "discvarsClass",
    inherit = discvarsBase,
    private = list(
      .init=function() {
        #private$.initProbsTable()
      },       
        .run = function() {
          distros <- self$options$distros
          InfoTab <- self$results$InfoTab
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
          
          #if(self$options$pmf){
          #  probValues <- private$.getProbValues()
          #  nprobValues <- length(probValues)
          #}
    
        },
      #### Init tables ----
      .initProbsTable = function(){
        probstab <- self$results$probstab
        probstab$addRow(rowKey=1, values=list())
        if((self$options$pmf) == FALSE){
          probstab <- self$results$probstab
          probstab$addColumn(
            name=' ',
            title=' ',
            type='text')
        }      
      },
      #### Additional functions ----
      .getProbValues = function(){
        probValues <- self$options$values
        if (is.character(probValues))
          probValues <- as.numeric(unlist(strsplit(probValues, ",")))
        probValues[probValues < 0 | probValues > 1] <- NA
        probValues <- probValues[!is.na(probValues)]
        
        return(probValues)
      }
      )
)
