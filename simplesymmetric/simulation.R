require(mvtnorm)
require(gMCP)
require(parallel)
options(mc.cores=7)
source('../functions.R')
source('functions.R')





## Scenario (A)
##         d, c, e, s1,s2,  r, g,   D,gamma,  v,cpup,cpdn,sftt, B, alpha
A <- simulation(.0,.0,.0,.0,.0,  .5,.5,  .3,0,    .5,1,0,-10,   10^5, .025)

## Scenario (B)
##         d, c, e, s1,s2,  r, g,   D,gamma,  v,cpup,cpdn,sftt, B, alpha
B1 <- simulation( .3,.0, 1,.0,.0,  .5,.5,  .3,0,    .5,1,0,qnorm(.5),   10^5, .025)
B2 <- simulation( .3,.0, 1,.5,.0,  .5,.5,  .3,0,    .5,1,0,qnorm(.5),   10^5, .025)
B3 <- simulation( .3,.0, 1, 1,.0,  .5,.5,  .3,0,    .5,1,0,qnorm(.5),   10^5, .025)

## Scenario (C)
##         d, c, e, s1,s2,  r, g,   D,gamma,  v,cpup,cpdn,sftt, B, alpha
C1 <- simulation( .3,.5, 1,.0,.0,  .5,.5,  .3,0,    .5,1,0,qnorm(.5),   10^5, .025)
C2 <- simulation( .3,.5, 1,.5,.0,  .5,.5,  .3,0,    .5,1,0,qnorm(.5),   10^5, .025)
C3 <- simulation( .3,.5, 1, 1,.0,  .5,.5,  .3,0,    .5,1,0,qnorm(.5),   10^5, .025)

## Scenario (D)
##         d, c, e, s1,s2,  r, g,   D,gamma,  v,cpup,cpdn,sftt, B, alpha
D1 <- simulation( .3, 1, 1,.0,.0,  .5,.5,  .3,0,    .5,1,0,qnorm(.5),   10^5, .025)
D2 <- simulation( .3, 1, 1,.5,.0,  .5,.5,  .3,0,    .5,1,0,qnorm(.5),   10^5, .025)
D3 <- simulation( .3, 1, 1, 1,.0,  .5,.5,  .3,0,    .5,1,0,qnorm(.5),   10^5, .025)


or <- .3
g <- .5
R <- diag(1,8)
R[2,7] <- R[7,2] <- R[3,8] <- R[8,3] <- g
R[1,4] <- R[4,1] <- R[5,2] <- R[2,5] <- R[6,3] <- R[3,6] <- r
R[5,7] <- R[7,5] <- R[6,8] <- R[8,6] <- r*g


C <- matrix(c(-1,1,0,0,0,0,0,0,
              -1,0,1,0,0,0,0,0,
              0,0,0,-1,1,0,0,0,
              0,0,0,-1,0,1,0,0,
              0,0,0,0,0,0,1,0,
              0,0,0,0,0,0,0,1),ncol=8,byrow=T)

sigma <- C%*%R%*%t(C)

rho <- diag(1/sqrt(diag(sigma))) %*% sigma %*% diag(1/sqrt(diag(sigma)))

