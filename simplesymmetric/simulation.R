require(mvtnorm)
require(gMCP)
require(parallel)
options(mc.cores=7)
source('../functions.R')
source('functions.R')



set.seed(675940)

B <- 10^5

## Scenario (A)
##         d, c, e, s1,s2,  r, g,   D,gamma,  v,cpup,cpdn,sftt, B, alpha
A <- simulation(.0,.0,.0,.0,.0,  .3,.5,  .3,0,    .5,1,0,qnorm(.05),   B, .025, t1=TRUE)

## Scenario (B)
##         d, c, e, s1,s2,  r, g,   D,gamma,  v,cpup,cpdn,sftt, B, alpha
B1 <- simulation( .3,.0, 1,.0,.0,  .3,.5,  .3,0,    .5,1,0,qnorm(.05),   B, .025)
#B2 <- simulation( .3,.0, 1,.5,.0,  .3,.5,  .3,0,    .5,1,0,qnorm(.05),   B, .025)
#B3 <- simulation( .3,.0, 1, 1,.0,  .3,.5,  .3,0,    .5,1,0,qnorm(.05),   B, .025)

## Scenario (C)
##         d, c, e, s1,s2,  r, g,   D,gamma,  v,cpup,cpdn,sftt, B, alpha
C1 <- simulation( .3,.5, 1,.0,.0,  .3,.5,  .3,0,    .5,1,0,qnorm(.05),   B, .025)
#C2 <- simulation( .3,.5, 1,.5,.0,  .3,.5,  .3,0,    .5,0,0,qnorm(.05),   B, .025)
#C3 <- simulation( .3,.5, 1, 1,.0,  .3,.5,  .3,0,    .5,1,0,qnorm(.05),   B, .025)

## Scenario (D)
##         d, c, e, s1,s2,  r, g,   D,gamma,  v,cpup,cpdn,sftt, B, alpha
D1 <- simulation( .3, 1, 1,.0,.0,  .3,.5,  .3,0,    .5,1,0,qnorm(.05),   B, .025)
#D2 <- simulation( .3, 1, 1,.5,.0,  .3,.5,  .3,0,    .5,1,0,qnorm(.05),   B, .025)
#D3 <- simulation( .3, 1, 1, 1,.0,  .3,.5,  .3,0,    .5,1,0,qnorm(.05),   B, .025)

## Scenario (E)
##         d, c, e, s1,s2,   r,   g,   D,gamma,  v,cpup,cpdn,sftt, B, alpha
E1 <- simulation( .4, .5, 1, 1/4,.0,  .3,.5,  .3,0,    .5,1,0,qnorm(.05),   B, .025)
E2 <- simulation( .4, .5, 1, 2/4,.0,  .3,.5,  .3,0,    .5,1,0,qnorm(.05),   B, .025)
E3 <- simulation( .4, .5, 1, 1,  .0,  .3,.5,  .3,0,    .5,1,0,qnorm(.05),   B, .025)
fname <- paste('simulations_',format(Sys.time(),"%y%m%d"),'.Rd',sep='')
save(A,B1,C1,D1,E1,E2,E3,file=fname)


make.cor <- function(r,g){
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


k <- matrix(c(1,0,0,
              sqrt(r),sqrt(1-r),0,
              sqrt(s),0,sqrt(1-s)),nr=3)

rmvnorm(ccc,diag(1,3))    

    
C <- matrix(c(1,0,0,0,0,0,0,0,
              0,1,0,0,0,0,0,0,
              sqrt(r),0,sqrt(1-r),0,0,0,0,0,
              0,sqrt(r),0,sqrt(1-r),0,0,0,0,
              0,0,0,0,1
rnorm(8) 
    
B <- 10^3
d <- round(seq(0,.5,.1),2)
c <- round(seq(0,1,.5),2)
e <- round(seq(0,1,.5),2)
s1 <- round(seq(0,.6,.3),2)
s2 <- 0
r <- .3
g <- c(0,.3,.9)
D <- .3
gamma <- 0
v <- .5
cpup <- 1
cpdn <- 0
sftt <- qnorm(.05)
alpha <- .025

dim(G <- expand.grid(d=d,c=c,e=e,s1=s1,s2=s2,r=r,g=g,D=D,gamma=gamma,v=v,cpup=cpup,cpdn=cpdn,sftt=sftt,B=B,alpha=alpha))
G$t1 <- G$d==0

out <- list()

for(i in 1:nrow(G)){
    p <- as.list(G[i,],names=varnames)
    out[[i]] <- cbind(as.data.frame(matrix(rep(p,each=20),nr=20)),do.call(simulation,p))
}

gridsim <- do.call(rbind,apply(G,1,function(p) {
}))
