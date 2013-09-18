## let's start with a graph for a two-treatment/two-endpoint scenario
G <- simpleSuccessiveI()

## and add two more that would be appropriate if only one treatment would be selected
sG1 <- matrix2graph(matrix(c(0,0,1,0,
                             0,0,0,0,
                             0,0,0,0,
                             0,0,0,0),nr=4,byrow=T),c(1,0,0,0))
sG2 <- matrix2graph(matrix(c(0,0,0,0,
                             0,0,0,1,
                             0,0,0,0,
                             0,0,0,0),nr=4,byrow=T),c(0,1,0,0))

## originial weights
worg <- generateWeights(G@m,G@weights)[,5:8]
## select T1
sw2s1 <- generateWeights(sG1@m,sG1@weights)[,5:8]
## select T2
sw2s2 <- generateWeights(sG2@m,sG2@weights)[,5:8]


## Scenario 1a, drop a fixed treatment no alpha resampling
scenario1.a <- matrix2graph(sG1@m,c(1/2,0,0,0))

## Scenario 1b, drop a fixed treatment with alpha resampling
scenario1.b <- selectOneTest(name = 'Method 2a',
                             test = function(...) { NULL },
                             selector = function(...) { c(TRUE,FALSE,TRUE,FALSE) },
                             hint = generateWeights(G@m,G@weights),
                             SSR = F)

## Scenario 1c, drop a fixed treatment with alpha resampling and sample size reallocation
scenario1.c <- selectOneTest(name = 'Method 2b',
                             test = function(...) { NULL },
                             selector = function(...) { c(TRUE,FALSE,TRUE,FALSE) },
                             hint = generateWeights(G@m,G@weights),
                             SSR = T)

## Scenario 2a, always select more promising  treatment no alpha resampling

## Scenario 2b, always select more promising treatment with alpha resampling
scenario2.b <- selectOneTest(name = 'Rule 2b',
                            test = function(...){
                              NULL
                            },
                            selector = function(z){
                              if(z[1] > z[2]){
                                c(TRUE,FALSE,TRUE,FALSE)
                              } else {
                                c(FALSE,TRUE,FALSE,TRUE)
                              }
                            },
                            hint = generateWeights(G@m,G@weights),
                            SSR = FALSE
                            )

## Scenario 2c, always select more promising treatment with alpha resampling and sample size reallocation
scenario2.c <- selectOneTest(name = 'Rule 2c',
                            test = function(...){
                              NULL
                            },
                            selector = function(z){
                              if(z[1] > z[2]){
                                c(TRUE,FALSE,TRUE,FALSE)
                              } else {
                                c(FALSE,TRUE,FALSE,TRUE)
                              }
                            },
                            hint = generateWeights(G@m,G@weights),
                            SSR = TRUE
                            )

## Scenario 3a, sometimes select more promising teratment, sometimes drop more promising treatment, no alpha resampling, sometimes continue with both treatments

## Scenario 3b, sometimes select more promising teratment, sometimes drop more promising treatment, with alpha resampling, sometimes continue with both treatments

scenario3.b <- adaptTest(
                name = 'Rule 3b',
                test = function(z,w1=sw2s1,w2=sw2s2,pboth=1/2,pbest=1/2){
                  if(runif(1) < pboth){
                    return(c(0,as.numeric(worg)))
                  } else {
                    if(z[1]>=z[2]){
                      if(runif(1) > pbest) w1 <- w2
                    } else {
                      if(runif(1) < pbest) w1 <- w2
                    }
                  }
                  return(c(0,as.numeric(w1)))
                },
                hint = generateWeights(G@m,G@weights))

                  
## Scenario 3c, sometimes select more promising teratment, sometimes drop more promising treatment, with alpha resampling and sample size reallocation, sometimes continue with both treatments

scenario3.c <- adaptTest(
                name = 'Rule 3c',
                test = function(z,w1=sw2s1,w2=sw2s2,pboth=1/2,pbest=1/2){
                  if(runif(1) < pboth){
                    return(c(0,as.numeric(worg)))
                  } else {
                    if(z[1]>=z[2]){
                      if(runif(1) > pbest) w1 <- w2
                    } else {
                      if(runif(1) < pbest) w1 <- w2
                    }
                  }
                  return(c(1,as.numeric(w1)))
                },
                hint = generateWeights(G@m,G@weights))


## many-to-one correlation

exp1.R <- matrix(c(1,1/2,NA,NA,
                   1/2,1,NA,NA,
                   NA,NA,1,1/2,
                   NA,NA,1/2,1),byrow=T,nr=4)

