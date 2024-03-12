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
            distros <- 'Binomial'
            param1 <- paste('n:',self$options$binomn)
            param2 <- paste('p:',self$options$binomp)  
          } else if (distros=='poiss'){
            distros <- 'Poisson'
            param1 <- paste('\u3bb:',self$options$lambda)
            param2 <- NA
          }
          Info <- matrix(NA,nrow=2,ncol=2)
          
          Info[1,1] <- distros
          Info[2,1] <- NA
          Info[1,2] <- param1
          Info[2,2] <- param2
          
          InfoTab$setRow(rowNo=1, values=list(
            DistributionColumn=Info[1,1],
            ParametersColumn=Info[1,2]))
          InfoTab$setRow(rowNo=2, values=list(
            DistributionColumn=Info[2,1],
            ParametersColumn=Info[2,2]))
          
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
