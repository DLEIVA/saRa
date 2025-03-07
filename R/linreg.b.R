linRegClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "linRegClass",
  inherit = linRegBase,
  public = list(
    initialize=function(...) {
      super$initialize(...)
      require('car')
      require('lmtest')
    }
  ),
  private = list(
    .init = function() {
      
      if (! self$parent$options$norm)
        return(){
          termsAll <- private$.getModelTerms()
          for (i in seq_along(termsAll)) {
            
            treset <- jmvcore::Table$new(
              options=self$options,
              name='reset',
              title='Specification Test',
              rows=1,
              clearWith=list(
                "dep",
                "blocks"),
              columns=list(
                list(
                  name='t[reset]',
                  title='',
                  type='text',
                  content="Ramsey's RESET test"),
                list(
                  name='s[reset]',
                  title='Statistic'),
                list(
                  name='p[reset]',
                  title='p',
                  format='zto,pvalue')
              )
            )
            
            self$parent$results$models$get(key=i)$assump$insert(1, treset)
            treset$addRow(1, values=list(
              `t[reset]`="RESET"))
            
            treset$setNote('saRa', 'Additional results provided by <em>saRa</em>')
          }
          
          if (! self$parent$options$durbin)
            return(){
              image <- self$results$get('acfplot')
              
              width <- 450
              height <- 400
              
              image$setSize(width * 2, height)
      }
    },
    .run = function() {
      
      if (! self$parent$options$norm)
        return(){
          groups <- self$parent$results$models
          termsAll <- private$.getModelTerms()
          .residuals <- private$.computeResiduals()
          
          for (i in seq_along(termsAll)) {
            residuals <- .residuals[[i]]
            residuals <- scale(residuals)
            
            
            # Specification Tests
            treset <- groups$get(key=i)$assump$get('reset')
            model <- self$parent$models[[i]]
            
            # Ramsey Test   
            res <- try(lmtest::resettest(model), silent=TRUE) 
            if (jmvcore::isError(res)) {
              values <- list(`s[reset]`=NaN, `p[reset]`='')
            } else {
              values <- list(`s[reset]`=res$statistic, `p[reset]`=res$p.value)
            }
            
            treset$setRow(rowNo=1, values)
          }
          
          if (! self$parent$options$durbin)
            return(){ groups <- self$parent$results$models
            termsAll <- private$.getModelTerms()
            .residuals <- private$.computeResiduals()
            
            for (i in seq_along(termsAll)) {
              residuals <- .residuals[[i]]
              residuals <- scale(residuals)
              acf(residuals,main='ACF plot')
            }
        }
      
    },
    .modelTerms = NULL,
    .getModelTerms = function() {
      if (is.null(private$.modelTerms)) {
        blocks <- self$parent$options$blocks
        
        terms <- list()
        if (is.null(blocks)) {
          terms[[1]] <- c(self$parent$options$covs, self$parent$options$factors)
        } else {
          for (i in seq_along(blocks)) {
            terms[[i]] <- unlist(blocks[1:i], recursive = FALSE)
          }
        }
        private$.modelTerms <- terms
      }
      
      return(private$.modelTerms)
    },
    .computeResiduals = function() {
      res <- list()
      for (i in seq_along(self$parent$models))
        res[[i]] <- self$parent$models[[i]]$residuals
      
      return(res)
    }
  )
)