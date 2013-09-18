setClass("conditionalTest",
         representation(name = "character",
                        conditional.test = "function",
                        SSR = "logical",
                        select = "character")
         )

setMethod("initialize",
          signature = "conditionalTest",
          definition = function(.Object,second.stage.test,first.stage.datum){
            .Object@name <- second.stage.test@name
            res <- second.stage.test@test(first.stage.datum)
            .Object@conditional.test <- res$test
            .Object@select <- res$select
            .Object@SSR <- second.stage.test@SSR
            return(.Object)
          })
