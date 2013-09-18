setClass("stageData",
         representation(
           params = "simParams",
           data = "matrix")
         )

sim.stage <- function(stage,B,...){
  if(missing(B)){
    stop("Number of simulation runs, 'B', missing")
  }
  if(isNull(stage@params)){
    stage@data <- matrix(nr=0,nc=0)
  } else {
    stage@data <- sim.normal(stage@params@mean,stage@params@corMat,B,...)
  }
  return(stage)
}

setMethod(f = "length",
          signature = "stageData",
          definition = function(x){
            nrow(x@data)
          })

setMethod(f = "npatients",
          signature = "stageData",
          definition = function(object){
            object@params@n
          })

setMethod(f = "nhyp",
          signature = "stageData",
          definition = function(object){
            ncol(object@data)
          })

setMethod(f = "[",
          signature="stageData",
          definition = function(x,i,j,drop){
            x@data[i,j]
          })

setMethod(f = "[<-",
          signature="stageData",
          definition = function(x,i,j,value){
            x@params@mutation <- "Object has been mutated"
            x@data[i,j] <- value
            x
          })

setMethod(f = "+",
          signature=c("stageData","stageData"),
          definition = function(e1,e2){
            n1 <- e1@params@n
            n2 <- e2@params@n
            e2@params@mutation <- "Object has been mutated"
            e2@data <- sqrt(n1/(n1+n2))*e1@data + sqrt(n2/(n1+n2))*e2@data
            e2@params@n <- n1+n2
            e2
          })

setMethod(f = "*",
          signature=c("numeric","stageData"),
          definition = function(e1,e2){
            e2@params@mutation <- "Object has been mutated"
            e2@data <- e1 * e2@data
            e2
          })
          
setMethod(f = "means",
          signature="stageData",
          definition = function(object,...){
            colMeans(object@data,...)
          })

setMethod(f = "sds",
          signature="stageData",
          definition = function(object,...){
            n <- nrow(object@data)
            k <- ncol(object@data)
            m <- colMeans(object@data)
            sqrt(colMeans((object@data - matrix(rep(m,n),nc=k,byrow=T))^2))
          })

setMethod(f = "show",
          signature="stageData",
          definition = function(object){
            print(object@params)
            cat("Simulated data object with ",nrow(object@data)," replications\n",sep='')
            cat("Estimated means:\n")
            print(round(means(object),2))
            cat("Estimated standard deviations:\n")
            print(round(sds(object),2))
            cat("Estimated correlation matrix:\n")
            print(round(cor(object@data),2))
            if(nchar(object@params@mutation)>0){
              cat("Warning:",object@params@mutation,"|||\n")
            }
          })

setMethod(f = "combine",
          signature = "stageData",
          definition = function(a,b){
            out <- a
            out@params <- combine(a@params,b@params)
            out@data <- tryCatch(rbind(a@data,b@data),error = function(...) matrix(ncol=0,nrow=0))
            out
          })
              
            
