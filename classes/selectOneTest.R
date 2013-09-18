setClass("selectOneTest",
         representation(selector = "function",
                        SSR = "logical",
                        v = "numeric"),
         contains = 'secondStageTest')

selectOneTest <- function(name,selector,SSR,hint=matrix(nr=0,nc=0),v=1/2,...){
  new('selectOneTest',name=name,
      test=function(...) NULL,
      selector=selector,
      SSR = SSR,
      hint=hint,
      v=v)
}


          
