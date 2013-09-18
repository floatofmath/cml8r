setClass("simulationSequence",
         representation(scenarios = 'list',
                        params = 'list',
                        simResults = 'list'))

setMethod("initialize",
          signature = "simulationSequence",
          definition = function(.Object,scenarios,sequence,reassessment.factor,B,v=1/2,...){
            if(all(c(missing(scenarios),missing(sequence),missing(reassessment.factor),missing(B)))){
              return(.Object)
            }
            .Object@scenarios <- scenarios
            .Object@params <- sequence
            first.stages <- lapply(.Object@params,function(p) firstStageData(reassess(p,2*v),B))
            second.stages <- lapply(.Object@params,function(p) secondStageData(reassess(p,2*(1-v)),B))
            if(reassessment.factor == 1) {
              second.stages.ssr <- lapply(.Object@params,function(p) secondStageData(reassess(p,2*(1-v)),B))
            } else { 
              second.stages.ssr <- lapply(.Object@params,function(p) secondStageData(reassess(p,reassessment.factor*2*(1-v)),B))
            }
            .Object@simResults <- lapply(scenarios,function(scenario){
              lapply(1:length(sequence),function(i) simulationResult(scenario,first.stages[[i]],second.stages[[i]],second.stages.ssr[[i]]))
            })
            return(.Object)
          }
          )

simulationSequence <- function(scenarios,sequence,B,reassessment.factor=1,...){
  new('simulationSequence',scenarios,sequence,reassessment.factor,B,...)
}

setMethod("show",
          signature = "simulationSequence",
          definition = function(object) {
            cat("simulationResults for Scenarios:\n")
            cat(paste(1:length(object@scenarios),lapply(object@scenarios,names),sep='.) ',collapse='\n'),'\n')
            cat("and Parameters:\n")
            pseq <- list()
            for(sn in slotNames(object@params[[1]])){
              running <- lapply(object@params,slot,name=sn)
              if(!allEqual(running)){
                cat(sn,':\n',sep='')
                cat(paste(1:length(running),lapply(running,round,2),sep='.) ',collapse='\n'),'\n')
              }
            }
            cat("======================================\n")
            lapply(1:length(object@scenarios),function(i){
              cat("Individual powers for Scenario:",names(object@scenarios[[i]]),"\n")
              print(sapply(object@simResults[[i]],function(run) rowMeans(run@rejections)))
            })
          }
          )

setMethod(f = "combine",
          signature = "simulationSequence",
          definition = function(a,b){
            warning("EXPERIMENTAL: Combining simulation sequences may produce unwanted side-effects")
            out <- new('simulationSequence')
            out@scenarios <- a@scenarios
            out@params <- a@params
            out@simResults <- lapply(1:length(a@simResults),function(s) lapply(1:length(a@simResults[[s]]), function(r) combine(a@simResults[[s]][[r]],b@simResults[[s]][[r]])))
            out
          })
          
setMethod(f = "plotSim",
         signature = "simulationSequence",
          definition = function(object,fun=individual,index = 1:length(object@params),reference = NULL,...){
            ## decompose data
            p <- cc(lapply(object@simResults,function(scenario) as.numeric(t(sapply(scenario,power,f=fun)))),c)
            pcon <- names(power(object@simResults[[1]][[1]],fun))
            df <- data.frame(Index = rep(index,length(pcon)),
                             Power = p,
                             Name = rep(pcon,each=length(object@params),length(object@scenarios)),
                             Scenario = rep(sapply(object@scenarios,names),each=length(pcon)*length(object@params)))
                               ##paste('S',1:length(object@scenarios),sep='')
            if(!is.null(reference)){
              sid <- lapply(sapply(object@scenarios,names),function(s) which(df$Scenario == s))
              df$Power <- df$Power/pmax(round(rep(df$Power[sid[[reference]]],length(object@scenarios)),6),10^-9)
              df <- df[-sid[[reference]],]
              df$Scenario <- paste(df$Scenario,'/',names(object@scenarios[[reference]]),sep='')
              plot <- qplot(Index,y=Power,facets=~Name,data=df,colour=Scenario,geom='line',...)
            } else {
              plot <- qplot(Index,y=Power,facets=~Name,data=df,colour=Scenario,geom='line',...)
            }
            m <- getMean(object)[,-1]
            m2m1 <- apply(m,2,function(x) c(x[2]/x[1]))
            e2e1 <- apply(m,2,function(x) c(x[3]/x[1]))
            if(!all(e2e1==e2e1[1])|!all(m2m1==m2m1[1])){
              warning("Not all treatment or endpoint mean proportions equal")
            }
            new('simPlot',plot=plot,caption=paste("Power simulations for scenarios: T2/T1 =",m2m1[1],", E2/E1 =",e2e1[1]))
        })

      

setMethod(f = "getMean",
          signature = "simulationSequence",
          definition = function(object){
            sapply(object@params,function(x) x@mean)
          })

setMethod(f = "printTable",
          signature = "simulationSequence",
          definition = function(object,powers,errors){
            cnames <- sapply(1:length(object@params),function(i) {
              m <- object@params[[i]]@mean
              paste('$m=',m[1],"$",collapse=' ')
            })
            rnames <- sapply(object@scenarios,names)
            if(missing(errors)){
              out <- cc(lapply(1:length(object@scenarios),function(i){              
                                        #cat("Individual powers for Scenario:",names(object@scenarios[[i]]),"\n")
                t <- simplify2array(lapply(object@simResults[[i]],power,f=powers))
                paste(apply(round(t,3),2,paste,collapse=' {\\em '),'}')
              }),rbind)
            } else {
              out <- cc(lapply(1:length(object@scenarios),function(i){              
                                        #cat("Individual powers for Scenario:",names(object@scenarios[[i]]),"\n")
                t <- simplify2array(mclapply(object@simResults[[i]],power,f=powers))
                t[,1] <- power(object@simResults[[i]][[1]],f=errors)
                paste(apply(round(t,3),2,paste,collapse=' {\\em '),'}')
              }),rbind)
            }
            colnames(out) <- cnames
            rownames(out) <- rnames
            out
          })
          
setMethod(f = "power",
          signature = "simulationSequence",
          definition = function(object,f,e){
            cnames <- sapply(1:length(object@params),function(i) {
              m <- object@params[[i]]@mean
              paste('$m=',m[1],"$",collapse=' ')
            })
            rnames <- sapply(object@scenarios,names)
            if(missing(e)){
              out <- cc(lapply(1:length(object@scenarios),function(i){              
                                        #cat("Individual powers for Scenario:",names(object@scenarios[[i]]),"\n")
                t <- simplify2array(mclapply(object@simResults[[i]],power,f=f))
                t
              }),rbind)
            } else {
              out <- cc(lapply(1:length(object@scenarios),function(i){              
                                        #cat("Individual powers for Scenario:",names(object@scenarios[[i]]),"\n")
                t <- simplify2array(mclapply(object@simResults[[i]],power,f=f))
                t[,1] <- power(object@simResults[[i]][[1]],f=e)
                t
              }),rbind)
            }
            colnames(out) <- cnames
            rownames(out) <- paste(rep(rnames,each=nrow(out)/length(rnames)),rownames(out))
            out
          })


  

