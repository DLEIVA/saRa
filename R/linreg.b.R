# Extend the built-in regression using its existing assumption checkboxes.
linRegClass <- if (requireNamespace('jmvcore',quietly=TRUE)) R6::R6Class(
  "linRegClass",inherit=linRegBase,
  private=list(
    .enabled=function() any(vapply(c("norm","durbin","resPlots"),
      function(name) isTRUE(self$parent$options[[name]]),logical(1))),
    .isReady=function() {
      if (is.null(self$parent$options$dep) || !length(self$parent$options$dep)) return(FALSE)
      terms <- private$.getModelTerms()
      length(terms)>0L && length(terms[[1]])>0L
    },
    .table=function(name,title,labels,option,note,columns=NULL,refs=character()) {
      if (is.null(columns)) columns <- list(
        list(name="test",title=.saraAddonMessage("Test",self),type="text"),
        list(name="stat",title=.saraAddonMessage("Statistic",self),type="number"),
        list(name="df1",title=.saraAddonMessage("df1",self),type="integer"),
        list(name="df2",title=.saraAddonMessage("df2",self),type="integer"),
        list(name="p",title="p",type="number",format="zto,pvalue"))
      t <- jmvcore::Table$new(options=self$parent$options,name=name,title=title,
        rows=length(labels),columns=columns,refs=refs,visible=paste0("(",option,")"),
        clearWith=list("dep","covs","factors","blocks","weights",option))
      for (i in seq_along(labels)) t$addRow(as.character(i),list(test=labels[i]))
      t$setNote("saRa",paste(.saraAddonMessage("Table obtained with the saRa module.",self),note))
      t
    },
    .image=function(i,kind,title,option) {
      self$.setResourcesPathSource(self$parent$.__enclos_env__$private$.resourcesPathSource)
      .saraAddonImage$new(owner=self,options=self$parent$options,
        name=paste0("sara",kind,i),width=700,height=480,renderFun=".diagnosticPlot",
        title=jmvcore::format(.saraAddonMessage("{title} - Model {model}",self),title=title,model=i),visible=paste0("(",option,")"),refs=if(kind=="influence") "car" else character(),
        clearWith=list("dep","covs","factors","blocks","weights",option))
    },
    .fillTests=function(table,results) {
      if (is.null(table)) return()
      for (i in seq_along(results)) {
        r <- results[[i]]
        table$setRow(rowNo=i,list(stat=r$stat,df1=r$df1,df2=r$df2,p=r$p))
        if (!is.null(r$error)) table$addFootnote(rowNo=i,"stat",.saraAddonMessage(r$error,self))
      }
    },
    .init=function() {
      if (!private$.enabled() || !private$.isReady()) return()
      for (i in seq_along(private$.getModelTerms())) {
        group <- self$parent$results$models$get(key=i)
        if (is.null(group)) next
        a <- group$assump
        if (isTRUE(self$parent$options$norm)) {
          reset <- jmvcore::Table$new(options=self$parent$options,name="reset",
            title=.saraAddonMessage("Specification Test",self),rows=1,clearWith=list("dep","covs","factors","blocks","weights","norm"),
            columns=list(list(name="t[reset]",title=.saraAddonMessage("Test",self),type="text"),
              list(name="s[reset]",title="F",type="number"),
              list(name="p[reset]",title="p",type="number",format="zto,pvalue")))
          reset$addRow("1",list(`t[reset]`="Ramsey RESET"))
          reset$setNote("saRa",.saraAddonMessage("Table obtained with the saRa module. Tests squared and cubed fitted values for functional-form misspecification.",self))
          a$add(reset)
          a$add(private$.table("saraNormal",.saraAddonMessage("Normality Tests",self),c("Shapiro-Wilk",
            .saraAddonMessage("Kolmogorov-Smirnov (Lilliefors)",self),"Anderson-Darling"),"norm",
            .saraAddonMessage("Kolmogorov-Smirnov uses the Lilliefors correction for estimated mean and variance. Residual normality diagnostics.",self),
            columns=list(list(name="test",title=.saraAddonMessage("Test",self),type="text"),
              list(name="stat",title=.saraAddonMessage("Statistic",self),type="number"),
              list(name="df1",title=.saraAddonMessage("df1",self),type="integer",visible=FALSE),
              list(name="df2",title=.saraAddonMessage("df2",self),type="integer",visible=FALSE),
              list(name="p",title="p",type="number",format="zto,pvalue"))))
          a$add(private$.table("saraBP",.saraAddonMessage("Heteroskedasticity Tests",self),c("Breusch-Pagan",
            .saraAddonMessage("Koenker (studentized Breusch-Pagan)",self),"Goldfeld-Quandt","Harrison-McCabe"),"norm",
            .saraAddonMessage("Null hypothesis: constant error variance. GQ: two-sided, ordered by fitted values, split at 50%, no central omission. HMC: ordered by fitted values, split at 50%; 5,000 simulations, seed 123456. BP statistic is chi-square, GQ is F; HMC is a residual sum-of-squares fraction.",self)))
          norm <- a$get("norm")
          if (!is.null(norm)) norm$setVisible(FALSE)
        }
        if (isTRUE(self$parent$options$durbin)) {
          a$add(private$.table("saraDW",.saraAddonMessage("Durbin-Watson Tests by Lag",self),character(),"durbin",
            .saraAddonMessage("Lags 1-10, capped at N-1. Two-sided p-values from residual bootstrapping with 5,000 resamples; seed = 123456. Complete-case row order; p-values are not adjusted across lags.",self),
            columns=list(list(name="lag",title=.saraAddonMessage("Lag",self),type="integer"),
              list(name="acf",title=.saraAddonMessage("Autocorrelation",self),type="number"),
              list(name="dw",title="DW",type="number"),
              list(name="p",title="p",type="number",format="zto,pvalue")),refs="car"))
          a$add(private$.image(i,"acf",.saraAddonMessage("Residual ACF",self),"durbin"))
        }
        if (isTRUE(self$parent$options$resPlots)) {
          titles <- c(residual=.saraAddonMessage("Residuals vs Fitted",self),scale=.saraAddonMessage("Scale-Location",self),
                      qq=.saraAddonMessage("Normal Q-Q with Residual Histogram",self),influence=.saraAddonMessage("Influence Plot",self))
          for (kind in names(titles)) a$add(private$.image(i,kind,titles[[kind]],"resPlots"))
          a$add(private$.table("saraInfluence",.saraAddonMessage("Influence Indices (Noteworthy Cases)",self),character(),"resPlots",
            .saraAddonMessage("Same noteworthy rule as car::influencePlot(): union of the two largest absolute studentized residuals, leverage values and Cook's distances. Case labels identify original fitted rows; selection is diagnostic, not an exclusion rule.",self),
            columns=list(list(name="case",title=.saraAddonMessage("Case",self),type="text"),
              list(name="StudRes",title="StudRes",type="number"),
              list(name="Hat",title="Hat",type="number"),
              list(name="CookD",title="CookD",type="number")),refs="car"))
        }
      }
    },
    .run=function() {
      if (!private$.enabled() || !private$.isReady()) return()
      models <- self$parent$models
      for (i in seq_along(models)) {
        model <- models[[i]]
        if (is.null(model)) next
        group <- self$parent$results$models$get(key=i)
        if (is.null(group)) next
        a <- group$assump
        if (isTRUE(self$parent$options$norm)) {
          results <- .saraRegressionTests(model)
          t <- a$get("reset")
          if (!is.null(t)) {
            r <- results$reset
            t$setRow(rowNo=1,list(`s[reset]`=r$stat,`p[reset]`=r$p))
            if (!is.null(r$error)) t$addFootnote(rowNo=1,"s[reset]",.saraAddonMessage(r$error,self))
          }
          private$.fillTests(a$get("saraBP"),results[c("bp","koenker","gq","hmc")])
          private$.fillTests(a$get("saraNormal"),.saraRegressionNormality(model))
          t <- a$get("norm")
          if (!is.null(t)) t$setVisible(FALSE)
        }
        if (isTRUE(self$parent$options$durbin)) {
          r <- .saraRegressionDW(model)
          t <- a$get("saraDW")
          if (!is.null(t)) {
            t$deleteRows()
            if (!is.null(r$error)) {
              t$addRow("error",list(lag=NA_integer_,acf=NaN,dw=NaN,p=NaN))
              t$addFootnote(rowNo=1,"dw",.saraAddonMessage(r$error,self))
            } else for (j in seq_len(nrow(r$data)))
              t$addRow(as.character(j),as.list(r$data[j,,drop=FALSE]))
          }
          image <- a$get(paste0("saraacf",i))
          if (!is.null(image)) image$setState(list(kind="acf",values=.saraResidualACF(model,i)))
        }
        if (isTRUE(self$parent$options$resPlots)) {
          state <- .saraRegressionFrame(model,i)
          for (kind in c("residual","scale","qq","influence")) {
            image <- a$get(paste0("sara",kind,i))
            if (!is.null(image)) image$setState(list(kind=kind,values=state))
          }
          t <- a$get("saraInfluence")
          if (!is.null(t)) {
            t$deleteRows()
            if (!is.null(state$error)) {
              t$addRow("error",list(case=.saraAddonMessage("Unavailable",self),StudRes=NaN,Hat=NaN,CookD=NaN))
              t$addFootnote(rowNo=1,"StudRes",.saraAddonMessage(state$error,self))
            } else for (j in state$selected) {
              row <- state$data[j,,drop=FALSE]
              t$addRow(as.character(j),list(case=row$case,StudRes=row$studentized,Hat=row$hat,CookD=row$cook))
            }
          }
        }
      }
    },
    .getModelTerms=function() {
      blocks <- self$parent$options$blocks
      if (is.null(blocks) || !length(blocks))
        return(list(c(self$parent$options$covs,self$parent$options$factors)))
      lapply(seq_along(blocks),function(i) unlist(blocks[seq_len(i)],recursive=FALSE))
    },
    .diagnosticPlot=function(image,ggtheme=NULL,...) {
      .saraDrawRegressionPlot(image$state,ggtheme,self=.saraAddonTranslationOwner(self))
    }
  )
)
