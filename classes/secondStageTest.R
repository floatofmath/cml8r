setClass("secondStageTest",
         representation(name = "character",
                        test = "function",
                        hint = "matrix",
                        v = "numeric")
         )

## setMethod(f = "initialize",
##           signature = "secondStageTest",
##           definition = function(.Object,name,test,hint){
##             .Object@name <- name
##             .Object@test <- test
##             .Object@hint <- hint
##             return(.Object)
##           })

adaptTest <- function(name,test,hint=matrix(nr=0,nc=0),v=1/2){
  new('secondStageTest',name=name,test=test,hint=hint,v=v)
}

setMethod(f = "show",
          signature = "secondStageTest",
          definition = function(object){
            print(names(object))
          })
          
setMethod(f = "perform",
          signature = c(test="secondStageTest",data="firstStageData"),
          definition = function(test,data,...){
            result <- new('adaptedTest')
            result@second.stage.test <- test
            ix <- 1:nrow(data@data)
            result@conditional.tests <- lapply(ix,function(i) test@test(data[i,]))
            return(result)
          }
          )


setMethod(f = "reassess",
          signature = "secondStageTest",
          ## attention this is a bit dirty but we may use this later to produce conditional reassessment rules SSR <- function etc
          definition = function(object,factor){
            if(missing(factor)) factor <- TRUE
            slot(object,'SSR') <- factor
            return(object)
          })


setMethod(f = "names",
          signature = "secondStageTest",
          definition = function(x){
            return(x@name)
          })

setReplaceMethod(f = "names",
                 signature = "secondStageTest",
                 definition = function(x,value){
                   x@name <- value
                   return(x)
                 })

setMethod(f = "==",
          signature = c("secondStageTest","secondStageTest"),
          definition = function(e1,e2){
            all(c(e1@name == e1@name,
                  all(e1@hint == e2@hint)))
          })

setMethod(f = "combine",
          signature = c("secondStageTest","secondStageTest"),
          definition = function(a,b){
            if(!a==b){
              stop("Second Stage Tests not compatible")
            } else {
              warning("Nothing combined return first argument")
              a
            }
          })
