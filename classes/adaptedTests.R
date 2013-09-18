setClass("adaptedTests",
         representation(second.stage.test = "secondStageTest",
                        first.stage.data = "firstStageData",
                        conditional.tests = "list"))

setMethod(f = "initialize",
          signature = 'adaptedTests',
          definition = function(.Object,second.stage.test,first.stage.data,wipe.data.matrix=T){
            .Object@second.stage.test <- second.stage.test
            .Object@first.stage.data <- first.stage.data
            if(wipe.data.matrix){
              .Object@first.stage.data@data <- matrix(nr=0,nc=0)
              .Object@second.stage.test@hint <- matrix(nr=0,nc=0)
            }
            .Object@conditional.tests <- simplify2array(lapply(1:nrow(first.stage.data@data),function(i) new('conditionalTest',second.stage.test,first.stage.data@data[i,])))
            return(.Object)
          }
          )

adaptedTests <- function(second.stage.test,first.stage.data,wipe.data.matrix=T){
  new('adaptedTests',second.stage.test,first.stage.data,wipe.data.matrix=T)
}

setMethod(f = "perform",
          signature = "adaptedTests",
          definition = function(test,data,data.ssr,quiet = FALSE,...){
            result <- new('simulationResult')
            if(!all(length(test@conditional.tests) == c(length(data),length(data.ssr)))){
              stop("First and second stage data need to have same number of simulations")
            }
            ctests <- test@conditional.tests
            result@second.stage.test <- test@second.stage.test
            result@first.stage.data <- test@first.stage.data
            result@rejections <- as.data.frame(lapply(1:nrow(data@data), function(i) {
              if(ctests[[i]]@SSR){
                ctests[[i]]@conditional.test(data.ssr[i,])
              } else {
                ctests[[i]]@conditional.test(data[i,])
              }}))
            result@selections <- sapply(test@conditional.tests,function(ctest) ctest@select)
            if(!quiet) cat("Simulation run titled:",test@second.stage.test@name,"with",length(data),"repetitions finished\n")
            return(result)
          })
