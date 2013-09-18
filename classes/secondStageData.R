setClass("secondStageData",
         contains = c("stageData"))

setMethod(f = "initialize",
          signature = "secondStageData",
          definition = function(.Object,params,B,...){
            if(class(params)=='firstStageData'){
              .Object@params <- params@params
            } else if(class(params) == 'simParams') {
              .Object@params <- params
            }
            .Object@data <- .Object@params@data.generator(B,...)
            return(.Object)
          })

secondStageData <- function(params,B,...){
  new("secondStageData",params,B,...)
}
