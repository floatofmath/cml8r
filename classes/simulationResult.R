setClass("simulationResult",
         representation(first.stage.data = 'firstStageData',
                        second.stage.test = 'graphTest',
                        rejections = 'data.frame',
                        selections = 'character'))

setMethod("initialize",
          signature = "simulationResult",
          definition = function(.Object,scenario,first.stage,second.stage,second.stage.ssr,alpha,...){
            if(all(c(missing(scenario),missing(first.stage),missing(second.stage),missing(alpha)))){
              return(.Object)
            } else if(class(scenario) == 'selectOneTest'){
              .Object <- selectOne(first.stage,second.stage,second.stage.ssr,"conditional",scenario,alpha=alpha,...)
            } else if(class(scenario) == 'secondStageTest'){
              .Object <- reweightingTest(first.stage,second.stage,second.stage.ssr,scenario,alpha=alpha)
#              adapted.tests <- new('adaptedTests',scenario,first.stage)
#              .Object <- perform(adapted.tests,second.stage,second.stage.ssr,...)
            } else if(class(scenario) == "graphMCP"){
              .Object@second.stage.test <- scenario              
              .Object@first.stage.data <- first.stage
              .Object@rejections <- as.data.frame(apply((first.stage+second.stage)@data,1,function(z) gMCP(scenario,pnorm(-z),alpha=alpha,useC=T)@rejected))
              .Object@selections <- rep("Both",length(first.stage))
            }
            return(.Object)
          })

setMethod("show",
          signature = "simulationResult",
            definition = function(object){
              cat("*** Simulation Results *** \n")
              cat("Simulation Parameters:\n")
              print(object@first.stage.data@params)
              if(class(object@second.stage.test)=="secondStageTest"){
                cat("Scenario",object@second.stage.test@name,'\n')
              } else if(class(object@second.stage.test)=="graphMCP"){
                cat("Scenario pre-planned procedure")
              }
              cat("Overall number of runs:\n")
              print(B2 <- ncol(object@rejections))
              cat("First stage simulations:",B1 <- length(object@selections),"\n")
              cat("times second stage simulations:\n")
              print(B2/B1)
              cat("Individual powers:\n")
              print(rowMeans(object@rejections))
            }
          )


simulationResult <- function(scenario,first.stage,second.stage,second.stage.ssr = second.stage,alpha=.025){
  new('simulationResult',scenario,first.stage,second.stage,second.stage.ssr,alpha)
}

setMethod(f = "[",
          signature="simulationResult",
          definition = function(x,i,j,drop){
            unlist(x@rejections[i,j])
          })

setMethod(f = 'power',
          signature = 'simulationResult',
          definition = function(object,f,...){
            rowMeans(apply(object@rejections,2,f))
          })

primsec <- function(x,prim=c(1,2),sec=c(3,4)){
  c('Primary' = any(x[prim]),'Secondary'=any(x[sec]))
}
individual <- function(x,names = paste('H',1:length(x),sep='')) {
  names(x) <- names
  x
}

setMethod(f = 'plotSim',
          signature = 'simulationResult',
          definition = function(object,f = individual){
            result <- power(object,f)
            p <- new('simPlot')
            p@plot <- qplot(names(result),result,geom='bar',stat='identity',xlab=NULL,ylab='Power')
            p@caption = object@second.stage.test@name
            p
          })
          
          
setMethod(f = 'combine',
          signature = 'simulationResult',
          definition = function(a,b){
            out <- new('simulationResult')
            out@first.stage.data <- combine(a@first.stage.data,b@first.stage.data)
            out@second.stage.test <- combine(a@second.stage.test,b@second.stage.test)
            out@rejections <- cbind(a@rejections,b@rejections)
            selections <- c(a@selections,b@selections)
            out
          })
          
          
