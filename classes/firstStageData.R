setClass("firstStageData",
         contains = c("stageData"))

setMethod(f = "initialize",
          signature = "firstStageData",
          definition = function(.Object,params,B,...){
            .Object@params <- params
            .Object@data <- params@data.generator(B,...)
            return(.Object)
          })

firstStageData <- function(params,B,...){
  new("firstStageData",params,B,...)
}
