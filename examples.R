require(gMCP)
require(mvtnorm)
require(parallel)
source('functions.R')
source('classes.R')

### a simple example

## first we need to define the scenario

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

## fixed sequence test  
fixed <- matrix2graph(sG1@m,c(1/2,1/2,0,0))

## adaptive test: select one treatment 1/2 of 
adaptive <- adaptTest(
                name = 'Rule 3c',
                test = function(z,w1=sw2s1,w2=sw2s2,pboth=1/2,pbest=3/2){
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

## Next set up the distributions

## global null
m1 <- c(0,0,0,0)
## treatment 1 effective
m2 <- c(1,0,1,0)
## both treatments effective
m3 <- c(1,1,1,1)

## many-to-one correlation
R <- matrix(c(1,1/2,NA,NA,
              1/2,1,NA,NA,
              NA,NA,1,1/2,
              NA,NA,1/2,1),byrow=T,nr=4)


## We assume T1 and T2 to be equally effective with means 1/4,
## sd = 4, cor(E1,E2) = .3 -> 220 patients per group and stage
## gives primary power of .8 for the preplanned design
n <- 220
evalPower(G,alpha=.025,mean = m3*sqrt(n/2)/4, sigma = sM(.3,R),f=list(Primary = function(x) {x[1] | x[2]}))


## lets set up the random number generators

null <- simParams(m1/4,sM(.3,R),n/2)
one <- simParams(m2/4,sM(.3,R),n/2)
both <- simParams(m3/4,sM(.3,R),n/2)

debug(simulationResult)
simulation <- simulationSequence(scenarios=list(fixed,adaptive),sequence=list(null,one,both),B=100)

