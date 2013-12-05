##
B <- 10000000
r <- .3
g <- -.9


cP1 <- rnorm(B)
cP2 <- {sqrt(1-r^2)*rnorm(B)+r*cP1}
scP1 <- {sqrt(1-g^2)*rnorm(B)+g*cP1}

t1P1 <- rnorm(B)
t1P2 <- {sqrt(1-r^2)*rnorm(B)+r*t1P1}
s1P1 <- {sqrt(1-g^2)*rnorm(B)+g*t1P1}

t2P1 <- rnorm(B)
t2P2 <- {sqrt(1-r^2)*rnorm(B)+r*t2P1}
s2P1 <- {sqrt(1-g^2)*rnorm(B)+g*t2P1}

e1P1 <- t1P1-cP1
e1P2 <- t1P2-cP2
e2P1 <- t2P1-cP1
e2P2 <- t2P2-cP2
se2P1 <- s2P1-scP1
se1P1 <- s1P1-scP1

cor(t1P1,t2P1)
cor(cP1,t1P1)

cor(e1P1,e1P2)
round(cor(cbind(e1P1,e2P1,e1P2,e2P2,s1P1,s2P1)),2)
make.correlation(r,-g) - round(cor(cbind(e1P1,e2P1,e1P2,e2P2,se1P1,se2P1)),3)

round(cor(cbind(cP1,cP2,t1P1,t1P2,t2P1,t2P2,s1P1,s2P1)),2)

round(cor(cbind(cP1,cP2,t1P1,t1P2,t2P1,t2P2,se1P1,se2P1)),2)

cormat <- function(x,y,e){
    s1 <- matrix(c(1,x,0,0,0,0,0,0,
                   x,1,0,0,0,0,0,0,
                   0,0,1,x,0,0,e,0,
                   0,0,x,1,0,0,0,0,
                   0,0,0,0,1,x,0,e,
                   0,0,0,0,x,1,0,y,
                   0,0,e,y,0,0,1,0,
                   0,0,0,0,e,y,0,1),byrow=T,nc=8)
    B <- matrix(c(-1,0,1,0,0,0,0,0,
                  0,-1,0,1,0,0,0,0,
                  -1,0,0,0,1,0,0,0,
                  0,-1,0,0,0,1,0,0,
                  0,0,0,0,0,0,1,0,
                  0,0,0,0,0,0,0,1),byrow=T,nc=8)
    s2 = B%*%s1%*%t(B)
    r = diag(diag(s2^{-1/2}),6,6)%*%s2%*%diag(diag(s2^{-1/2}),6,6)
    return(list(s1,B,s2,r))
}

det(cormat(.3,.99,.3*.9)[[4]])

cor(t1P1,t1P2)

sd(t1P2)
