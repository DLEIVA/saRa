
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
      .contppdf = function(image, ...) {
        
        if (! self$options$contppdf)
          return()
        
        Color <- c("#e0bc6b","#7b9ee6")
        
        cdistros <- self$options$cdistros
        
        x <- if(cdistros=='norm'){
          data.frame(xlim=c(self$options$mu-4*self$options$sigma,self$options$mu+4*self$options$sigma))
          } else if(cdistros=='tdist'){
          data.frame(xlim=c(-4,4))
          } else if(cdistros=='chisqdist'){
            data.frame(xlim=c(0,self$options$chinu+3*sqrt(2*self$options$chinu)))
          } else if(cdistros=='fdist'){data.frame(xlim=c(0,5))} else if(cdistros=='exp'){
            data.frame(xlim=c(0,5))} else if(cdistros=='unif'){
              data.frame(xlim=c(self$options$unifmin,self$options$unifmax))}
        
        k <- private$.getcontppvalue()
        dval <- if(cdistros=='norm'){dnorm(k,self$options$mu,self$options$sigma)
        } else if(cdistros=='tdist'){dt(k,self$options$tnu)
        } else if(cdistros=='chisqdist'){dchisq(k,self$options$chinu)
        } else if(cdistros=='fdist'){df(k,self$options$f1nu,self$options$f2nu)
        } else if(cdistros=='exp'){dexp(k,self$options$rate)
        } else if(cdistros=='unif'){dunif(k,self$options$unifmin,self$options$unifmax)}
        
        cdistroslabel <- if (cdistros=='norm'){'Normal: '} else if(cdistros=='tdist'){
          't: '} else if(cdistros=='chisqdist'){'\u03c7\u00B2: '} else if(cdistros=='fdist'){
            'F: '} else if(cdistros=='exp'){'Exponential: '} else if(cdistros=='unif'){'Uniform: '}
        
        p <- ggplot(x,aes(x=xlim)) +
          stat_function(fun = if(cdistros=='norm'){dnorm} else if(cdistros=='tdist'){
            dt} else if(cdistros=='chisqdist'){dchisq} else if(cdistros=='fdist'){
              df} else if(cdistros=='exp'){dexp} else if(cdistros=='unif'){dunif},
            args= if(cdistros=='norm'){list(mean=self$options$mu,sd=self$options$sigma)
              } else if(cdistros=='tdist'){list(df=self$options$tnu)} else if(cdistros=='chisqdist'){
                list(df=self$options$chinu)} else if(cdistros=='fdist'){
                list(df1=self$options$f1nu,df2=self$options$f2nu)} else if(cdistros=='exp'){
                  list(rate=self$options$rate)} else if(cdistros=='unif'){
                    list(min=self$options$unifmin,max=self$options$unifmax)},color=Color[2],lwd=1.1) +
          ggtitle(paste0(cdistroslabel,'f(X = ',k,') = ',round(dval,2))) + 
          geom_segment(aes(x=k,y=0,xend=k,yend=dval),color=Color[1],lwd=1.1) +
          geom_point(aes(x=k,y=dval),col=Color[1]) +
          ylab('') + xlab('') + guides(fill=FALSE) + theme_classic() +
          theme(axis.text.x=element_text(size=13),
                axis.text.y=element_text(size=13),
                axis.title.x = element_text(size=14),
                axis.title.y = element_text(size=14))        
        
        return(p)                
      },       
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
      },
      .getcontppvalue = function(){
        cdistros <- self$options$cdistros
        ppval <- self$options$contppvalue
        if (!is.na(ppval) && is.character(ppval))
          ppval <- as.numeric(unlist(strsplit(ppval, ",")))
        if(cdistros%in%c('chisqdist','fdist','exp')){
          ppval[ppval < 0] <- NA 
        }
        if(cdistros=='unif'){
          ppval[ppval < self$options$unifmin |ppval > self$options$unifmax] <- NA           
        }
        ppval <- unique(ppval[!is.na(ppval)])
        return(ppval)
      }      
    )
)