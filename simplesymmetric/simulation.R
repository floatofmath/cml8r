require(mvtnorm)
require(gMCP)
require(parallel)
options(mc.cores=7)
source('../functions.R')
source('functions.R')


simulation(.3,0,1,1,1,
           .5,0,
           .3,gamma=1/2,
           1/2,
           .7,
           .3,
           0,B=10^5,alpha=.05)


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

