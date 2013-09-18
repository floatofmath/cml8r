setGeneric("isNull",
           def = function(object){
             standardGeneric("isNull")
           }
           )

setGeneric('reassess',
           def = function(object,factor,...){
             standardGeneric('reassess')
           })

setGeneric("setParams",
           def = function(object,...){
             standardGeneric("setParams")
           })

setGeneric("simStage",
           def=function(stage,adaptation,...){
             standardGeneric("simStage")
           })

setGeneric("perform",
           def=function(test,data,...){
             standardGeneric("perform")
           })

setGeneric("power",
           def=function(object,f,...){
             standardGeneric("power")
           })

setGeneric("plotSim",
           def=function(object,...){
             standardGeneric("plotSim")
           })

setGeneric("means",
           def=function(object,...){
             standardGeneric("means")
           })

setGeneric("npatients",
           def=function(object,...){
             standardGeneric("npatients")
           })
setGeneric("power",
           def=function(object,...){
             standardGeneric("power")
           })

setGeneric("sds",
           def=function(object,...){
             standardGeneric("sds")
           })

setGeneric("combine",
           def=function(a,b,...){
             standardGeneric("combine")
           })

setGeneric("nhyp",
           def=function(object){
             standardGeneric("nhyp")
           })

setGeneric("getMean",
           def=function(object){
             standardGeneric("getMean")
           })

setGeneric("printTable",
           def=function(object,powers,errors,...){
             standardGeneric("printTable")
           })
