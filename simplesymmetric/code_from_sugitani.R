library(mvtnorm)
library(gMCP)

#- Setting parameters -#
weight1 <- 0.5; weight2 <- 0.5;
alpha <- 0.025; seed <- 4989;
e <- 0.0000000000001
c <- alpha/2

#- Start of function "sim" -#
sim <- function(n,nsim,mean,gamma1,gamma2,p,q,rho){

#--- Step1: Data generation ---#
sigma <- diag(4)
sigma[1,2]<-sigma[2,1]<-sigma[3,4]<-sigma[4,3]<-0.5
sigma[1,3]<-sigma[3,1]<-sigma[2,4]<-sigma[4,2]<-rho
sigma[1,4]<-sigma[4,1]<-sigma[2,3]<-sigma[3,2]<-rho/2
set.seed(seed)
x <- rmvnorm(nsim, mean=sqrt(n)*mean, sigma=sigma)
pval <- 1-pnorm(x)

#--- Step2: Treatment selection rule ---#

select <- rbinom(n=nsim,size=1,p=p)
select[select==0] <- rbinom(n=length(select[select==0]),size=1,p=q)+2
pval <- cbind(pval,select)


#--- Step3: Hypothesis testing ---#

#- select=1: Continue with both treatments -#

#- Definition of the Bonferroni-based graph

m <- rbind(H1=c(0, gamma1, 1-gamma1, 0),
           H2=c(gamma2, 0, 0, 1-gamma2),
           H3=c(0, 1, 0, 0),
           H4=c(1, 0, 0, 0))
weights <- c(weight1, weight2, 0, 0)
graph <- new("graphMCP", m=m, weights=weights)

if (length(select[select==1])!=0){
power.sub1 <- matrix(0,length(select[select==1]),4)
pval.sub1 <- pval[select==1,-5]
for (i in 1:length(select[select==1])){
result <-gMCP(graph, as.vector(pval.sub1[i,]), test="Bonferroni", alpha=0.025)
power.sub1[i,]<-matrix(ifelse(attributes(result)$rejected=="TRUE",1,0),1,4)
}
}
else {power.sub1 <- matrix(0,0,4)}

#- select=2: Continue with a randomly selected treatment -#
if (length(select[select==2])!=0){
power.sub2 <- matrix(0,length(select[select==2]),4)
pval.sub2 <- pval[select==2,-5]
for (i in 1:length(select[select==2])){
if (runif(1)<0.5){
power.sub2[i,1]<- ifelse(pval.sub2[i,1]<c,1,0)
power.sub2[i,3]<- ifelse(pval.sub2[i,1]<c&pval.sub2[i,3]<(1-gamma1)*c,1,0)
}
else{
power.sub2[i,2]<- ifelse(pval.sub2[i,2]<c,1,0)
power.sub2[i,4]<- ifelse(pval.sub2[i,2]<c&pval.sub2[i,4]<(1-gamma2)*c,1,0)
}
}
}
else {power.sub2 <- matrix(0,0,4)}

#- select=3: Continue with the treatment with the larger interim mean -#
power.sub3 <- matrix(0,length(select[select==3]),4)
pval.sub3 <- pval[select==3,-5]
for (i in 1:length(select[select==3])){
if (pval.sub3[i,1]<pval.sub3[i,2]){
power.sub3[i,1]<- ifelse(pval.sub3[i,1]<c,1,0)
power.sub3[i,3]<- ifelse(pval.sub3[i,1]<c&pval.sub3[i,3]<(1-gamma1)*c,1,0)
}
else{
power.sub3[i,2]<- ifelse(pval.sub3[i,2]<c,1,0)
power.sub3[i,4]<- ifelse(pval.sub3[i,2]<c&pval.sub3[i,4]<(1-gamma2)*c,1,0)
}
}

#- Combine the results from select1 to 3 -#
power <- rbind(power.sub1,power.sub2,power.sub3)
success <- ifelse(apply(power[,c(1,2)],1,sum)>=1,1,0)
colnames(power) <- c("H1","H2","H3","H4")
final <- cbind(success,power)
signif(apply(final,2,mean),digits=3)

} 
#- End of function "sim" -#

#- Example -#
#- Case (A) -#
sim(n=106,nsim=1000000,mean=c(0,0,0,0),gamma1=0.5,gamma2=0.5,p=0.5,q=0.5,rho=0.5)
sim(n=106,nsim=1000000,mean=c(0,0,0,0),gamma1=0,gamma2=0,p=0.5,q=0.5,rho=0.5)
sim(n=106,nsim=1000000,mean=c(0,0,0,0),gamma1=1-e,gamma2=1-e,p=0.5,q=0.5,rho=0.5)
sim(n=106,nsim=1000000,mean=c(0,0,0,0),gamma1=0.5,gamma2=0.5,p=0  ,q=1,rho=0.5)
sim(n=106,nsim=1000000,mean=c(0,0,0,0),gamma1=0,gamma2=0,p=0  ,q=1,rho=0.5)


#- Case (B) -#
sim(n=106,nsim=100000,mean=c(0.3,0,0.3,0),gamma1=0.5,gamma2=0.5,p=0.5,q=0.5,rho=0.5)
sim(n=106,nsim=100000,mean=c(0.3,0,0.3,0),gamma1=0,gamma2=0,p=0.5,q=0.5,rho=0.5)
sim(n=106,nsim=100000,mean=c(0.3,0,0.3,0),gamma1=1-e,gamma2=1-e,p=0.5,q=0.5,rho=0.5)
sim(n=106,nsim=100000,mean=c(0.3,0,0.3,0),gamma1=0.5,gamma2=0.5,p=0,q=1,rho=0.5)
sim(n=106,nsim=100000,mean=c(0.3,0,0.3,0),gamma1=0,gamma2=0,p=0,q=1,rho=0.5)

#- Case (C) -#
sim(n=106,nsim=100000,mean=c(0.3,0.15,0.3,0.15),gamma1=0.5,gamma2=0.5,p=0.5,q=0.5,rho=0.5)
sim(n=106,nsim=100000,mean=c(0.3,0.15,0.3,0.15),gamma1=0,gamma2=0,p=0.5,q=0.5,rho=0.5)
sim(n=106,nsim=100000,mean=c(0.3,0.15,0.3,0.15),gamma1=1-e,gamma2=1-e,p=0.5,q=0.5,rho=0.5)
sim(n=106,nsim=100000,mean=c(0.3,0.15,0.3,0.15),gamma1=0.5,gamma2=0.5,p=0,q=1,rho=0.5)
sim(n=106,nsim=100000,mean=c(0.3,0.15,0.3,0.15),gamma1=0,gamma2=0,p=0,q=1,rho=0.5)

#- Case (D) -#
sim(n=106,nsim=100000,mean=c(0.3,0.3,0.3,0.3),gamma1=0.5,gamma2=0.5,p=0.5,q=0.5,rho=0.5)
sim(n=106,nsim=100000,mean=c(0.3,0.3,0.3,0.3),gamma1=0,gamma2=0,p=0.5,q=0.5,rho=0.5)
sim(n=106,nsim=100000,mean=c(0.3,0.3,0.3,0.3),gamma1=1-e,gamma2=1-e,p=0.5,q=0.5,rho=0.5)
sim(n=106,nsim=100000,mean=c(0.3,0.3,0.3,0.3),gamma1=0.5,gamma2=0.5,p=0,q=1,rho=0.5)
sim(n=106,nsim=100000,mean=c(0.3,0.3,0.3,0.3),gamma1=0,gamma2=0,p=0,q=1,rho=0.5)