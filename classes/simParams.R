setClass("simParams",
         representation(
           mean = "numeric",
           corMat = "matrix",
           n = "numeric",
           data.generator = "function",
           mutation = "character"),
         validity = function(object){
           if(any(dim(object@corMat) != length(object@mean))){
             stop("Dimensions of mean vector and correlation matrix not compatibel")
           } else {
             return(TRUE)
           }
         })


setMethod("initialize",
          signature = "simParams",
          definition = function(.Object,mean,corMat,n){
            if(!all(c(missing(mean),missing(corMat)))){
              .Object@mean <- mean
              .Object@corMat <- corMat
              .Object@n <- n
              .Object@data.generator <- function(B,...) {
                ans <- rmvnorm(B,.Object@mean*sqrt(.Object@n/2),.Object@corMat,...)
                ans[sample(1:nrow(ans),B),]
              }
              .Object@mutation <- ""
              validObject(.Object)
            }
            return(.Object)
          })

simParams <- function(mean,corMat,n){
  new('simParams',mean,corMat,n)
}

setMethod(f = "isNull",
          signature = "simParams",
          definition = function(object){
            any(sapply(names(getSlots("simParams")),function(s) eval(substitute(length(object@slot)==0,list(slot = as.name(s))))))
          }
          )

setMethod('reassess',
          signature = "simParams",
          def = function(object,factor){
            object <- simParams(object@mean,object@corMat,object@n*factor)
            return(object)
          })

setMethod(f = "setParams",
          signature = "simParams",
          definition = function(object,...){
            arglist <- list(...)
            ## evil (:-=|
            lapply(names(arglist),function(name) slot(object,name) <<- arglist[[name]])
            return(object)
          }
          )

setMethod(f = "print",
          signature = "simParams",
          definition = function(x,...){
            cat('Mean vector:\n')
            print(x@mean)
            cat('Correlation matrix:\n')
            print(x@corMat)
            cat("Number of patients per group:\n")
            print(x@n)
          })

setMethod(f = "==",
          signature = c("simParams","simParams"),
          definition = function(e1,e2){
            all(c(e1@mean == e2@mean,
                  e1@corMat == e2@corMat))
          })
            
setMethod(f = 'combine',
          signature = c("simParams","simParams"),
          definition = function(a,b){
            if(!a == b){
              stop("Only observations with identical distributions may be joined")
            } else {
              simParams(a@mean,a@corMat,n = a@n+b@n)
            }
          })
