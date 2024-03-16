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
            distroslabel <- 'Geometric'
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
      .ppmf = function(image, ...) {
        
        if (! self$options$ppmf)
          return()
        
        Color <- c("#e0bc6b", "#9f9f9f")
        
        distros <- self$options$distros
        
        n <- if(distros=='binom'){self$options$binomn} else if(distros=='poiss'){
          round(self$options$lambda+4*sqrt(self$options$lambda))} else if(distros=='negbinom'){
            round((self$options$negbinomr*(1-self$options$negbinomp))/self$options$negbinomp+
                    3*sqrt(self$options$negbinomr*(1-self$options$negbinomp))/self$options$negbinomp^2)  
          } else if(distros=='geom'){
            round((1-self$options$geomp)/self$options$geomp+3*sqrt((1-self$options$geomp)/self$options$geomp^2))
          } else if(distros=='hypergeom'){
            self$options$hyperk*self$options$hyperm/(self$options$hyperm+self$options$hypern)+
              4*sqrt(self$options$hyperk*self$options$hyperm/(self$options$hyperm+self$options$hypern)*
                       (1-self$options$hyperm/(self$options$hyperm+self$options$hypern))*
                       (self$options$hyperm+self$options$hypern+self$options$hyperk)/
                       (self$options$hyperm+self$options$hypern)-1)}
        p <- if(distros=='binom'){self$options$binomp} else if(distros=='poiss'){NA} else if(distros=='negbinom'){
          self$options$negbinomp}  else if(distros=='geom'){self$options$geomp} else if(distros=='hypergeom'){NA}
        lambda <- if(distros=='binom'){NA} else if(distros=='poiss'){
          self$options$lambda} else if(distros=='negbinom'){NA} else if(distros=='geom'){
            NA} else if(distros=='hypergeom'){NA} 
        k <- private$.getppvalue()
        
        distroslabel <- if (distros=='binom'){'Binomial: '} else if(distros=='poiss'){
          'Poisson: '} else if(distros=='negbinom'){'Negative Binomial: '} else if(distros=='geom'){
            'Geometric: '}  else if(distros=='hypergeom'){'Hypergeometric: '}     
        
        plotData <- data.frame(x=0:n,pmf=if(distros=='binom'){
          dbinom(0:n,n,p)} else if(distros=='poiss'){dpois(0:n,lambda)
            } else if(distros=='negbinom'){dnbinom(0:n,self$options$negbinomr,p)} else if(distros=='geom'){
              dgeom(0:n,p) 
            } else if(distros=='hypergeom'){dhyper(0:n,self$options$hyperm,self$options$hypern,self$options$hyperk)},
          cdf=if(distros=='binom'){pbinom(0:n,n,p)} else if(distros=='poiss'){
            ppois(0:n,lambda)} else if(distros=='negbinom'){pnbinom(0:n,self$options$negbinomr,p)
              }  else if(distros=='geom'){pgeom(0:n,p)} else if(distros=='hypergeom'){
                phyper(0:n,self$options$hyperm,self$options$hypern,self$options$hyperk)},
          surv=if(distros=='binom'){pbinom(0:n,n,p,lower.tail=FALSE)
          } else if(distros=='poiss'){ppois(0:n,lambda,lower.tail=FALSE)
            } else if(distros=='negbinom'){pnbinom(0:n,self$options$negbinomr,p,lower.tail=FALSE)
              } else if(distros=='geom'){pgeom(0:n,p,lower.tail=FALSE)} else if(distros=='hypergeom'){
                phyper(0:n,self$options$hyperm,self$options$hypern,self$options$hyperk,lower.tail=FALSE)})
        
        p <- ggplot(plotData,aes(x=x,y=pmf,fill= (x==k))) +
          geom_col() + 
          scale_fill_manual(values = setNames(c(Color[1],Color[2]),c(T,F))) +
          scale_x_continuous('', 0:n, 0:n, c(0,n)) +  
          ggtitle(paste0(distroslabel,'Pr(X = ',k,') = ',round(plotData$pmf[plotData$x==k],2))) +
          ylab('') + xlab('') + guides(fill=FALSE) + theme_classic() +
          theme(axis.text.x=element_text(size=13),
                axis.text.y=element_text(size=13),
                axis.title.x = element_text(size=14),
                axis.title.y = element_text(size=14))
        return(p)                
      }, 
      .pcdf = function(image, ...) {
        
        if (! self$options$pcdf)
          return()
        
        Color <- c("#e0bc6b","#9f9f9f")
        
        distros <- self$options$distros
        
        n <- if(distros=='binom'){self$options$binomn} else if(distros=='poiss'){
          round(self$options$lambda+4*sqrt(self$options$lambda))} else if(distros=='negbinom'){
            round((self$options$negbinomr*(1-self$options$negbinomp))/self$options$negbinomp+
                    3*sqrt(self$options$negbinomr*(1-self$options$negbinomp))/self$options$negbinomp^2)  
          } else if(distros=='geom'){
            round((1-self$options$geomp)/self$options$geomp+3*sqrt((1-self$options$geomp)/self$options$geomp^2))
          } else if(distros=='hypergeom'){
            self$options$hyperk*self$options$hyperm/(self$options$hyperm+self$options$hypern)+
              4*sqrt(self$options$hyperk*self$options$hyperm/(self$options$hyperm+self$options$hypern)*
                       (1-self$options$hyperm/(self$options$hyperm+self$options$hypern))*
                       (self$options$hyperm+self$options$hypern+self$options$hyperk)/
                       (self$options$hyperm+self$options$hypern)-1)
          }
        p <- if(distros=='binom'){self$options$binomp} else if(distros=='poiss'){NA} else if(distros=='negbinom'){
          self$options$negbinomp}  else if(distros=='geom'){self$options$geomp} else if(distros=='hypergeom'){NA}
        lambda <- if(distros=='binom'){NA} else if(distros=='poiss'){
          self$options$lambda} else if(distros=='negbinom'){NA} else if(distros=='geom'){
            NA} else if(distros=='hypergeom'){NA} 
        k <- private$.getppvalue()
        
        distroslabel <- if (distros=='binom'){'Binomial: '} else if(distros=='poiss'){
          'Poisson: '} else if(distros=='negbinom'){'Negative Binomial: '} else if(distros=='geom'){
            'Geometric: '}  else if(distros=='hypergeom'){'Hypergeometric: '}      
        
        plotData <- data.frame(x=0:n,pmf=if(distros=='binom'){
          dbinom(0:n,n,p)} else if(distros=='poiss'){dpois(0:n,lambda)
          } else if(distros=='negbinom'){dnbinom(0:n,self$options$negbinomr,p)} else if(distros=='geom'){
            dgeom(0:n,p) 
          } else if(distros=='hypergeom'){dhyper(0:n,self$options$hyperm,self$options$hypern,self$options$hyperk)},
          cdf=if(distros=='binom'){pbinom(0:n,n,p)} else if(distros=='poiss'){
            ppois(0:n,lambda)} else if(distros=='negbinom'){pnbinom(0:n,self$options$negbinomr,p)
            }  else if(distros=='geom'){pgeom(0:n,p)} else if(distros=='hypergeom'){
              phyper(0:n,self$options$hyperm,self$options$hypern,self$options$hyperk)},
          surv=if(distros=='binom'){pbinom(0:n,n,p,lower.tail=FALSE)
          } else if(distros=='poiss'){ppois(0:n,lambda,lower.tail=FALSE)
          } else if(distros=='negbinom'){pnbinom(0:n,self$options$negbinomr,p,lower.tail=FALSE)
          } else if(distros=='geom'){pgeom(0:n,p,lower.tail=FALSE)} else if(distros=='hypergeom'){
            phyper(0:n,self$options$hyperm,self$options$hypern,self$options$hyperk,lower.tail=FALSE)})        

        p <- ggplot(plotData,aes(x=x,y=pmf,fill= (x<=k))) +
          geom_col() + 
          scale_fill_manual(values = setNames(c(Color[1],Color[2]),c(T,F))) +
          scale_x_continuous('', 0:n, 0:n, c(0,n)) +
          ggtitle(paste0(distroslabel,'Pr(X \u2264 ',k,') = ',round(plotData$cdf[plotData$x==k],2))) +
          ylab('') + xlab('') + guides(fill=FALSE) + theme_classic() +
          theme(axis.text.x=element_text(size=13),
                axis.text.y=element_text(size=13),
                axis.title.x = element_text(size=14),
                axis.title.y = element_text(size=14))
        return(p)                
      },
      .psurv = function(image, ...) {
        
        if (! self$options$psurv)
          return()
        
        Color <- c("#e0bc6b","#9f9f9f")
        
        distros <- self$options$distros
        
        n <- if(distros=='binom'){self$options$binomn} else if(distros=='poiss'){
          round(self$options$lambda+4*sqrt(self$options$lambda))} else if(distros=='negbinom'){
            round((self$options$negbinomr*(1-self$options$negbinomp))/self$options$negbinomp+
                    3*sqrt(self$options$negbinomr*(1-self$options$negbinomp))/self$options$negbinomp^2)  
          } else if(distros=='geom'){
            round((1-self$options$geomp)/self$options$geomp+3*sqrt((1-self$options$geomp)/self$options$geomp^2))
          } else if(distros=='hypergeom'){
            self$options$hyperk*self$options$hyperm/(self$options$hyperm+self$options$hypern)+
              4*sqrt(self$options$hyperk*self$options$hyperm/(self$options$hyperm+self$options$hypern)*
                       (1-self$options$hyperm/(self$options$hyperm+self$options$hypern))*
                       (self$options$hyperm+self$options$hypern+self$options$hyperk)/
                       (self$options$hyperm+self$options$hypern)-1)
          }
        p <- if(distros=='binom'){self$options$binomp} else if(distros=='poiss'){NA} else if(distros=='negbinom'){
          self$options$negbinomp}  else if(distros=='geom'){self$options$geomp} else if(distros=='hypergeom'){NA}
        lambda <- if(distros=='binom'){NA} else if(distros=='poiss'){
          self$options$lambda} else if(distros=='negbinom'){NA} else if(distros=='geom'){
            NA} else if(distros=='hypergeom'){NA} 
        k <- private$.getppvalue()
        
        distroslabel <- if (distros=='binom'){'Binomial: '} else if(distros=='poiss'){
          'Poisson: '} else if(distros=='negbinom'){'Negative Binomial: '} else if(distros=='geom'){
            'Geometric: '}  else if(distros=='hypergeom'){'Hypergeometric: '}       
        
        plotData <- data.frame(x=0:n,pmf=if(distros=='binom'){
          dbinom(0:n,n,p)} else if(distros=='poiss'){dpois(0:n,lambda)
          } else if(distros=='negbinom'){dnbinom(0:n,self$options$negbinomr,p)} else if(distros=='geom'){
            dgeom(0:n,p) 
          } else if(distros=='hypergeom'){dhyper(0:n,self$options$hyperm,self$options$hypern,self$options$hyperk)},
          cdf=if(distros=='binom'){pbinom(0:n,n,p)} else if(distros=='poiss'){
            ppois(0:n,lambda)} else if(distros=='negbinom'){pnbinom(0:n,self$options$negbinomr,p)
            }  else if(distros=='geom'){pgeom(0:n,p)} else if(distros=='hypergeom'){
              phyper(0:n,self$options$hyperm,self$options$hypern,self$options$hyperk)},
          surv=if(distros=='binom'){pbinom(0:n,n,p,lower.tail=FALSE)
          } else if(distros=='poiss'){ppois(0:n,lambda,lower.tail=FALSE)
          } else if(distros=='negbinom'){pnbinom(0:n,self$options$negbinomr,p,lower.tail=FALSE)
          } else if(distros=='geom'){pgeom(0:n,p,lower.tail=FALSE)} else if(distros=='hypergeom'){
            phyper(0:n,self$options$hyperm,self$options$hypern,self$options$hyperk,lower.tail=FALSE)})
        
        p <- ggplot(plotData,aes(x=x,y=pmf,fill= (x>k))) +
          geom_col() + 
          scale_fill_manual(values = setNames(c(Color[1],Color[2]),c(T,F))) +
          scale_x_continuous('', 0:n, 0:n, c(0,n)) +  
          ggtitle(paste0(distroslabel,'Pr(X > ',k,') = ',round(plotData$surv[plotData$x==k],2))) +
          ylab('') + xlab('') + guides(fill=FALSE) + theme_classic() +
        theme(axis.text.x=element_text(size=13),
              axis.text.y=element_text(size=13),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))
        return(p)                
      },
      .pinterv = function(image, ...) {
        
        if (! self$options$pinterv)
          return()
        
        Color <- c("#e0bc6b","#9f9f9f")
        
        distros <- self$options$distros
        
        n <- if(distros=='binom'){self$options$binomn} else if(distros=='poiss'){
          round(self$options$lambda+4*sqrt(self$options$lambda))} else if(distros=='negbinom'){
            round((self$options$negbinomr*(1-self$options$negbinomp))/self$options$negbinomp+
                    3*sqrt(self$options$negbinomr*(1-self$options$negbinomp))/self$options$negbinomp^2)  
          } else if(distros=='geom'){
            round((1-self$options$geomp)/self$options$geomp+3*sqrt((1-self$options$geomp)/self$options$geomp^2))
          } else if(distros=='hypergeom'){
            self$options$hyperk*self$options$hyperm/(self$options$hyperm+self$options$hypern)+
              4*sqrt(self$options$hyperk*self$options$hyperm/(self$options$hyperm+self$options$hypern)*
                       (1-self$options$hyperm/(self$options$hyperm+self$options$hypern))*
                       (self$options$hyperm+self$options$hypern+self$options$hyperk)/
                       (self$options$hyperm+self$options$hypern)-1)
          }
        p <- if(distros=='binom'){self$options$binomp} else if(distros=='poiss'){NA} else if(distros=='negbinom'){
          self$options$negbinomp}  else if(distros=='geom'){self$options$geomp} else if(distros=='hypergeom'){NA}
        lambda <- if(distros=='binom'){NA} else if(distros=='poiss'){
          self$options$lambda} else if(distros=='negbinom'){NA} else if(distros=='geom'){
            NA} else if(distros=='hypergeom'){NA} 
        
        distroslabel <- if (distros=='binom'){'Binomial: '} else if(distros=='poiss'){
          'Poisson: '} else if(distros=='negbinom'){'Negative Binomial: '} else if(distros=='geom'){
            'Geometric: '}  else if(distros=='hypergeom'){'Hypergeometric: '}  
        
        k1 <- private$.getX1value() 
        k2 <- private$.getX2value()
        
        plotData <- data.frame(x=0:n,pmf=if(distros=='binom'){
          dbinom(0:n,n,p)} else if(distros=='poiss'){dpois(0:n,lambda)
          } else if(distros=='negbinom'){dnbinom(0:n,self$options$negbinomr,p)} else if(distros=='geom'){
            dgeom(0:n,p) 
          } else if(distros=='hypergeom'){dhyper(0:n,self$options$hyperm,self$options$hypern,self$options$hyperk)},
          cdf=if(distros=='binom'){pbinom(0:n,n,p)} else if(distros=='poiss'){
            ppois(0:n,lambda)} else if(distros=='negbinom'){pnbinom(0:n,self$options$negbinomr,p)
            }  else if(distros=='geom'){pgeom(0:n,p)} else if(distros=='hypergeom'){
              phyper(0:n,self$options$hyperm,self$options$hypern,self$options$hyperk)},
          surv=if(distros=='binom'){pbinom(0:n,n,p,lower.tail=FALSE)
          } else if(distros=='poiss'){ppois(0:n,lambda,lower.tail=FALSE)
          } else if(distros=='negbinom'){pnbinom(0:n,self$options$negbinomr,p,lower.tail=FALSE)
          } else if(distros=='geom'){pgeom(0:n,p,lower.tail=FALSE)} else if(distros=='hypergeom'){
            phyper(0:n,self$options$hyperm,self$options$hypern,self$options$hyperk,lower.tail=FALSE)})
        
        p <- ggplot(plotData,aes(x=x,y=pmf,fill= (x>=k1 & x<=k2))) +
          geom_col() + 
          scale_fill_manual(values = setNames(c(Color[1],Color[2]),c(T,F))) +
          scale_x_continuous('', 0:n, 0:n, c(0,n)) +
          ggtitle(paste0(distroslabel,'Pr(',k1,' \u2264 X \u2264 ',k2,') =',
                         round(sum(plotData$pmf[plotData$x>=k1 & plotData$x<=k2]),2))) +
          ylab('') + xlab('') + guides(fill=FALSE) + theme_classic() +
          theme(axis.text.x=element_text(size=13),
                axis.text.y=element_text(size=13),
                axis.title.x = element_text(size=14),
                axis.title.y = element_text(size=14))
        return(p)                
      },  
      .picdf = function(image, ...) {
        
        if (! self$options$picdf)
          return()
        
        Color <- c("#e0bc6b","#9f9f9f")
        
        distros <- self$options$distros
        
        tail <- self$options$tail
        
        n <- if(distros=='binom'){self$options$binomn} else if(distros=='poiss'){
          round(self$options$lambda+4*sqrt(self$options$lambda))} else if(distros=='negbinom'){
            round((self$options$negbinomr*(1-self$options$negbinomp))/self$options$negbinomp+
                    3*sqrt(self$options$negbinomr*(1-self$options$negbinomp))/self$options$negbinomp^2)  
          } else if(distros=='geom'){
            round((1-self$options$geomp)/self$options$geomp+3*sqrt((1-self$options$geomp)/self$options$geomp^2))
          } else if(distros=='hypergeom'){
            self$options$hyperk*self$options$hyperm/(self$options$hyperm+self$options$hypern)+
              4*sqrt(self$options$hyperk*self$options$hyperm/(self$options$hyperm+self$options$hypern)*
                       (1-self$options$hyperm/(self$options$hyperm+self$options$hypern))*
                       (self$options$hyperm+self$options$hypern+self$options$hyperk)/
                       (self$options$hyperm+self$options$hypern)-1)
          }
        p <- if(distros=='binom'){self$options$binomp} else if(distros=='poiss'){NA} else if(distros=='negbinom'){
          self$options$negbinomp}  else if(distros=='geom'){self$options$geomp} else if(distros=='hypergeom'){NA}
        lambda <- if(distros=='binom'){NA} else if(distros=='poiss'){
          self$options$lambda} else if(distros=='negbinom'){NA} else if(distros=='geom'){
            NA} else if(distros=='hypergeom'){NA} 
        
        distroslabel <- if (distros=='binom'){'Binomial: '} else if(distros=='poiss'){
          'Poisson: '} else if(distros=='negbinom'){'Negative Binomial: '} else if(distros=='geom'){
            'Geometric: '}  else if(distros=='hypergeom'){'Hypergeometric: '}  
        
        q <- private$.getpqvalue()
        
        plotData <- data.frame(x=0:n,pmf=if(distros=='binom'){
          dbinom(0:n,n,p)} else if(distros=='poiss'){dpois(0:n,lambda)
          } else if(distros=='negbinom'){dnbinom(0:n,self$options$negbinomr,p)} else if(distros=='geom'){
            dgeom(0:n,p) 
          } else if(distros=='hypergeom'){dhyper(0:n,self$options$hyperm,self$options$hypern,self$options$hyperk)},
          cdf=if(distros=='binom'){pbinom(0:n,n,p)} else if(distros=='poiss'){
            ppois(0:n,lambda)} else if(distros=='negbinom'){pnbinom(0:n,self$options$negbinomr,p)
            }  else if(distros=='geom'){pgeom(0:n,p)} else if(distros=='hypergeom'){
              phyper(0:n,self$options$hyperm,self$options$hypern,self$options$hyperk)},
          surv=if(distros=='binom'){pbinom(0:n,n,p,lower.tail=FALSE)
          } else if(distros=='poiss'){ppois(0:n,lambda,lower.tail=FALSE)
          } else if(distros=='negbinom'){pnbinom(0:n,self$options$negbinomr,p,lower.tail=FALSE)
          } else if(distros=='geom'){pgeom(0:n,p,lower.tail=FALSE)} else if(distros=='hypergeom'){
            phyper(0:n,self$options$hyperm,self$options$hypern,self$options$hyperk,lower.tail=FALSE)})        

        if(tail=='left'){
          quant1 <- q
          qs <- if(distros=='binom'){qbinom(quant1,n,p)} else if(distros=='poiss'){
              qpois(quant1,lambda)} else if(distros=='negbinom'){
                qnbinom(quant1,self$options$negbinomr,p)
              } else if(distros=='geom'){qgeom(quant1,p)} else if(distros=='hypergeom'){
                qhyper(quant1,self$options$hyperm,self$options$hypern,self$options$hyperk)}
          q1 <- qs
          p <- ggplot(plotData,aes(x=x,y=pmf,fill= (x==q1))) +
            geom_col() + 
            scale_fill_manual(values = setNames(c(Color[1],Color[2]),c(T,F))) +
            scale_x_continuous('', 0:n, 0:n, c(0,n)) +
            ggtitle(TeX(paste0(distroslabel,' $Q_{',quant1,'} = ',q1,'$')))          
        }  else if(tail=='right'){
          quant1 <- 1-q
          qs <- if(distros=='binom'){
            qbinom(quant1,n,p)} else if(distros=='poiss'){
              qpois(quant1,lambda)} else if(distros=='negbinom'){
                qnbinom(quant1,self$options$negbinomr,p)
              } else if(distros=='geom'){qgeom(quant1,p)} else if(distros=='hypergeom'){
                qhyper(quant1,self$options$hyperm,self$options$hypern,self$options$hyperk)}
          q1 <- qs
          p <- ggplot(plotData,aes(x=x,y=pmf,fill= (x==q1))) +
            geom_col() + 
            scale_fill_manual(values = setNames(c(Color[1],Color[2]),c(T,F))) +
            scale_x_continuous('', 0:n, 0:n, c(0,n)) +
            ggtitle(TeX(paste0(distroslabel,' $Q_{',quant1,'} = ',q1,'$')))          
        } else if(tail=='central'){
          quant1 <- (1-q)/2
          quant2 <- 1-(1-q)/2
          qs <- if(distros=='binom'){
            qbinom(c(quant1,quant2),n,p)} else if(distros=='poiss'){
              qpois(c(quant1,quant2),lambda)} else if(distros=='negbinom'){
                qnbinom(c(quant1,quant2),self$options$negbinomr,p)
              } else if(distros=='geom'){qgeom(c(quant1,quant2),p)
              } else if(distros=='hypergeom'){
                qhyper(c(quant1,quant2),self$options$hyperm,self$options$hypern,self$options$hyperk)}
          q1 <- qs[1]
          q2 <- qs[2]
          p <- ggplot(plotData,aes(x=x,y=pmf,fill= (x==q1 | x==q2))) +
            geom_col() + 
            scale_fill_manual(values = setNames(c(Color[1],Color[2]),c(T,F))) +
            scale_x_continuous('', 0:n, 0:n, c(0,n)) +
            ggtitle(TeX(paste0(distroslabel,' $Q_{',quant1,'} = ',q1,
                               ' \\phantom{xx} ','Q_{',quant2,'} = ',q2,'$')))        
        }        
        
        p <- p + ylab('') + xlab('') + guides(fill=FALSE) + theme_classic() +
          theme(axis.text.x=element_text(size=13),
                axis.text.y=element_text(size=13),
                axis.title.x = element_text(size=14),
                axis.title.y = element_text(size=14))
        return(p)                
      },
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
    },
    .getppvalue = function(){
      ppval <- self$options$ppvalue
      if (!is.na(ppval) && is.character(ppval))
        ppval <- as.numeric(unlist(strsplit(ppval, ",")))
      ppval[ppval < 0] <- NA
      ppval <- unique(ppval[!is.na(ppval)])[1]
      return(ppval)
    },    
    .getX1value = function(){
      X1val <- self$options$x1value
      if (!is.na(X1val) && is.character(X1val))
        X1val <- as.numeric(unlist(strsplit(X1val, ",")))
      X1val[X1val < 0] <- NA
      X1val <- unique(X1val[!is.na(X1val)])[1]
      return(X1val)
    },
    .getX2value = function(){
      X2val <- self$options$x2value
      if (!is.na(X2val) && is.character(X2val))
        X2val <- as.numeric(unlist(strsplit(X2val, ",")))
      X2val[X2val < 0] <- NA
      X2val <- unique(X2val[!is.na(X2val)])[1]
      return(X2val)
    },
    .getpqvalue = function(){
      pqvalue <- self$options$pqvalue
      if (!is.na(pqvalue) && is.character(pqvalue))
        pqvalue <- as.numeric(unlist(strsplit(pqvalue, ",")))
      pqvalue[pqvalue < 0 | pqvalue > 1] <- NA
      pqvalue <- unique(pqvalue[!is.na(pqvalue)])
      return(pqvalue)
    }  
      )
)
