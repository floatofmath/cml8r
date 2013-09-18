setClassUnion("graphTest",
              members = c("graphMCP","secondStageTest","selectOneTest"))

setMethod("names",
          signature = "graphMCP",
          function(x) {
            "graphMCP"
          })
          
setMethod("combine",
          signature = "graphMCP",
          function(a,b){
            warning("Nothing combined returning first argument")
            a
          })

setMethod("combine",
          signature = "graphTest",
          function(a,b){
            warning("Nothing combined returning first argument")
            a
          })

