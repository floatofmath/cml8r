setClass('gg')
setClass('ggplot')
setClass('simPlot',
         representation(plot = 'gg',
                        caption = 'character'))

setMethod('show',
          signature = 'simPlot',
          definition = function(object){
            p <- object@plot + opts(title = object@caption)
            print(p)
          })
