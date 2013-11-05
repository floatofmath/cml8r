data <- function(B,n,m,s,R){
  h <- ncol(R)
  S <- R
  m <- m*sqrt(n/2)/s
  rmvnorm(B,m,S)
}


make.correlation <- function(r,g){
  ## 1:4: Efficacy T1E1,T2E1,T1E2,T2E2, 5:6 Safety S1,S2
  R <- kronecker(diag(c(1,1,0),3,3),matrix(1/2,2,2)+diag(1/2,2,2))
  ## Correlation between efficacy and safety
  R[1,5] <- R[5,1] <- -g
  R[2,6] <- R[6,2] <- -g
  R[3,5] <- R[5,3] <- -g*r
  R[4,6] <- R[6,4] <- -g*r
  ## Correlation between primary and secondary endpoints
  diag(R) <- 1
  R <- sM(r,R)
  return(R)
}

mean.vector <- function(d,c,e,s1,s2){
  c(d,d*c,d*e,d*c*e,d*(-s1),d*c*(-s2))
}

simulation <- function(d,c,e,s1,s2,#efficacy/safety profile
                       r,g,#correlation structure
                       D,gamma,#design effect size, between treatment exchange
                       v,#interim timing
                       cpup,cpdn,sftt,#conditional power upper limit, conditional power lower limit, safety threshhold
                       B=10^4,
                       alpha=.025,
                       t1=FALSE # should type 1 errors be counted?
                       ){
  ## correlation structure                     
  R <- make.correlation(r,g)
  ## number of simulation runs
  #B <- 10^4
  ## bonferroni sample size formula (we may replace that with sth. gmcp'ish)
  n2 <- n1 <- 110#ceiling(((qnorm(alpha/2,lower=F)+qnorm(.8))^2)/D^2)
  ## drop one treatment
  n2ssr <- n2*1.5
  ## efficacy/safety profile
  m <- mean.vector(d,c,e,s1,s2)
  ## standard deviations
  s <- rep(1,6)
  ## stage 1 data
  stage1 <- data(B,n1,m,s,R)
  ## pre-planned graph
  gamma <- 0
  G <- simpleSuccessiveI()
  ## G <- generalSuccessive(weights = c(1/2,1/2))
  ## G <- replaceVariables(G,list('gamma'=gamma,'delta'=gamma))
  
  gw <- generateWeights(G@m,G@weights)
  inthyp <- gw[,1:4]
  weights <- gw[,5:8]

  ## indeces for which intersection contain a particular elemntary hypothesis
  clo.idx <- mclapply(1:4,function(i) which(inthyp[,i]>0))
  sel1.idx <- list(which(inthyp[,1]>0),which(inthyp[,3]>0 & inthyp[,1]<1),which(inthyp[,1]>0 & inthyp[,3]>0))
  sel2.idx <- list(which(inthyp[,2]>0),which(inthyp[,4]>0 & inthyp[,2]<1),which(inthyp[,2]>0 & inthyp[,4]>0))

  ## For each 1st stage random sample compute the 60 weighted pCE rates
  Aj <- mclapply(1:B,function(i) matrix(pCEfast(weights,stage1[i,1:4],rep(v,4),alpha),ncol=4))


  stage2 <- data(B,n2,m,s,R)
  stage2ssr <- data(B,n2ssr,m,s,R)
  combinedp <- pnorm(sqrt(v)*stage1 + sqrt(1-v)*stage2,lower=F)
  combinedpr <- pnorm(sqrt(v)*stage1 + sqrt(1-v)*stage2ssr,lower=F)
                                        #combinedp <- pnorm(data(B,n1+n2,m,s,R),lower=F)
  p2 <- pnorm(stage2,lower=F)
  p2r <- pnorm(stage2ssr,lower=F)
  p1 <- pnorm(stage1,lower=F)


  ## For each 2nd stage random sample compute second stage p-values
  ## and local test decisions
  ltd <- mclapply(1:B,function(i) rep(p2[i,1:4],each=15) <= Aj[[i]])

  Bj <- mclapply(Aj,rowSums)
  early <- sum(sapply(Bj,function(b) any(b>=1)))
  
  ihd <- mclapply(1:B,function(i) apply(ltd[[i]],1,any) | Bj[[i]] >= 1)
  ned <- mclapply(1:B,function(i) apply(ltd[[i]],1,any))

  ## Sequentially improved bonferroni test
  ctd <- simplify2array(mclapply(1:B,function(i) sapply(clo.idx,function(h) all(ihd[[i]][h]))))
  ## pre-planned test procedure
  ctdne <- simplify2array(mclapply(1:B,function(i) sapply(clo.idx,function(h) all(ned[[i]][h]))))

  ## AGbMTP select treatment 1
  ctds1 <- matrix(NA,ncol=B,nrow=4)
  ctds1[c(1,3),] <- simplify2array(mclapply(1:B,function(i) c(all(p2[i,1] <= Bj[[i]][sel1.idx[[1]]]),
                                                              all(p2[i,1] <= Bj[[i]][sel1.idx[[3]]]) & all(p2[i,3] <= Bj[[i]][sel1.idx[[2]]]))))

  ## AGbMTP select treatment 2
  ctds2 <- matrix(NA,ncol=B,nrow=4)
  ctds2[c(2,4),] <- simplify2array(mclapply(1:B,function(i) c(all(p2[i,2] <= Bj[[i]][sel2.idx[[1]]]),
                                                              all(p2[i,2] <= Bj[[i]][sel2.idx[[3]]]) & all(p2[i,4] <= Bj[[i]][sel2.idx[[2]]]))))

  ## AGbMTP select treatment 1 SSR
  ctds1r <- matrix(NA,ncol=B,nrow=4)
  ctds1r[c(1,3),] <- simplify2array(mclapply(1:B,function(i) c(all(p2r[i,1] <= Bj[[i]][sel1.idx[[1]]]),
                                                             all(p2r[i,1] <= Bj[[i]][sel1.idx[[3]]]) & all(p2r[i,3] <= Bj[[i]][sel1.idx[[2]]]))))
  
  ## AGbMTP select treatment 2 SSR
  ctds2r <- matrix(NA,ncol=B,nrow=4)
  ctds2r[c(2,4),] <- simplify2array(mclapply(1:B,function(i) c(all(p2r[i,2] <= Bj[[i]][sel2.idx[[1]]]),
                                                             all(p2r[i,2] <= Bj[[i]][sel2.idx[[3]]]) & all(p2r[i,4] <= Bj[[i]][sel2.idx[[2]]]))))
  
  ## sCTP select treatment 1
  stps1 <- matrix(NA,ncol=B,nrow=4)
  stps1[c(1,3),] <- simplify2array(mclapply(1:B,function(i) c(combinedp[i,1] <= alpha/2,combinedp[i,3] <= (1-gamma)*alpha/2)))
  stps1[3,] <- stps1[1,] & stps1[3,]

  ## sCTP select treatment 2
  stps2 <- matrix(NA,ncol=B,nrow=4)
  stps2[c(2,4),] <- simplify2array(mclapply(1:B,function(i) c(combinedp[i,2] <= alpha/2,combinedp[i,4] <= (1-gamma)*alpha/2)))
  stps2[4,] <- stps2[2,] & stps2[4,]

  ## sCTP select treatment 1 SSR
  stps1r <- matrix(NA,ncol=B,nrow=4)
  stps1r[c(1,3),] <- simplify2array(mclapply(1:B,function(i) c(combinedpr[i,1] <= alpha/2,combinedpr[i,3] <= (1-gamma)*alpha/2)))
  stps1r[3,] <- stps1r[1,] & stps1r[3,]

  ## sCTP select treatment 2 SSR
  stps2r <- matrix(NA,ncol=B,nrow=4)
  stps2r[c(2,4),] <- simplify2array(mclapply(1:B,function(i) c(combinedpr[i,2] <= alpha/2,combinedpr[i,4] <= (1-gamma)*alpha/2)))
  stps2r[4,] <- stps2r[2,] & stps2r[4,]

  ## interim metrics

  larger1 <- which(stage1[,1]>stage1[,2])
  larger2 <- which(stage1[,1]<stage1[,2])
  unsave1 <- which(stage1[,5]<sftt)
  unsave2 <- which(stage1[,6]<sftt)
  cpower1 <- pnorm((qnorm(alpha/2,lower=F) - sqrt(v)*stage1[,1])/sqrt(1-v)-d*sqrt(n2/2)/s[1],lower=F)
  cpower1r <- pnorm((qnorm(alpha/2,lower=F) - sqrt(v)*stage1[,1])/sqrt(1-v)-d*sqrt(n2ssr/2)/s[1],lower=F)
  cpower2 <- pnorm((qnorm(alpha/2,lower=F) - sqrt(v)*stage1[,2])/sqrt(1-v)-d*sqrt(n2/2)/s[1],lower=F)
  cpower2r <- pnorm((qnorm(alpha/2,lower=F) - sqrt(v)*stage1[,2])/sqrt(1-v)-d*sqrt(n2ssr/2)/s[1],lower=F)


  ## select larger treatment
  ctdl <- ctds2
  ctdl[,larger1] <- ctds1[,larger1]
  
  ctdlr <- ctds2r
  ctdlr[,larger1] <- ctds1r[,larger1]
  
  stpl <- stps1
  stpl[,larger2] <- stps2[,larger2]

  stplr <- stps1r
  stplr[,larger2] <- stps2r[,larger2]


  ## sugitani in 50% of cases select 1, select the better in 50% of cases
  ## correction: in select the better in 75% of cases

  select <- sample(1:B,B/2)
  goon <- setdiff(1:B,select)
  sellar <- sample(select,3*B/8)
  selsma <- setdiff(select,sellar)
  selone <- c(intersect(larger1,sellar),intersect(larger2,selsma))
  seltwo <- c(intersect(larger2,sellar),intersect(larger1,selsma))
  ## print(B - length(unique(c(selsma,selone,seltwo))))
  ## print(sum(duplicated(c(goon,selone,seltwo))))

  ctdsu <- ctd
  ctdsu[,selone] <- ctds1[,selone]
  ctdsu[,seltwo] <- ctds2[,seltwo]

  ctdsur <- ctd
  ctdsur[,selone] <- ctds1r[,selone]
  ctdsur[,seltwo] <- ctds2r[,seltwo]

  stpsu <- ctdne
  stpsu[,selone] <- stps1[,selone]
  stpsu[,seltwo] <- stps2[,seltwo]

  stpsur <- ctdne
  stpsur[,selone] <- stps1r[,selone]
  stpsur[,seltwo] <- stps2r[,seltwo]

  
  
  ## select larger treatment if cpel >.8 and cpes <.5, drop unsafe treatments
  
  ctdls <- ctd
  ctdls[,(cpower1>cpup) & (cpower2 <cpdn)] <- ctds1r[,(cpower1>cpup) & (cpower2 <cpdn)]
  ctdls[,(cpower2>cpup) & (cpower1 <cpdn)] <- ctds2r[,(cpower2>cpup) & (cpower1 <cpdn)]
  ctdls[,unsave1] <- ctds2r[,unsave1]
  ctdls[,unsave2] <- ctds1r[,unsave2]
  ctdls[,intersect(unsave1,unsave2)] <- NA

  stpls <- ctdne
  stpls[,(cpower1>cpup) & (cpower2 <cpdn)] <- stps1r[,(cpower1>cpup) & (cpower2 <cpdn)]
  stpls[,(cpower2>cpup) & (cpower1 <cpdn)] <- stps2r[,(cpower2>cpup) & (cpower1 <cpdn)]
  stpls[,unsave1] <- stps2r[,unsave1]
  stpls[,unsave2] <- stps1r[,unsave2]
  stpls[,intersect(unsave1,unsave2)] <- NA

  alt <- rep(1,4)
  if(!t1){ alt[which(m[1:4] == 0)] <- 0 }
  ## hypothetical
  preplanned <- alt*rowSums(ctd)/B
  preplanned <- c(preplanned,sum(apply(ctd,2,any,na.rm=T))/B,rowSums(is.na(ctd)[c(1,2),])/B)
  improved <- alt*rowSums(ctdne)/B
  improved <- c(improved,sum(apply(ctdne,2,any,na.rm=T))/B,rowSums(is.na(ctdne)[c(1,2),])/B)
  pctds1 <- alt*rowSums(ctds1)/B
  pctds1 <- c(pctds1,sum(apply(ctds1,2,any,na.rm=T))/B,rowSums(is.na(ctds1)[c(1,2),])/B)
  pctds2 <- alt*rowSums(ctds2)/B
  pctds2 <- c(pctds2,sum(apply(ctds2,2,any,na.rm=T))/B,rowSums(is.na(ctds2)[c(1,2),])/B)
  pctds1r <- alt*rowSums(ctds1r)/B
  pctds1r <- c(pctds1r,sum(apply(ctds1r,2,any,na.rm=T))/B,rowSums(is.na(ctds1r)[c(1,2),])/B)
  pctds2r <- alt*rowSums(ctds2r)/B
  pctds2r <- c(pctds2r,sum(apply(ctds2r,2,any,na.rm=T))/B,rowSums(is.na(ctds2r)[c(1,2),])/B)
  pstps1 <- alt*rowSums(stps1)/B
  pstps1 <- c(pstps1,sum(apply(stps1,2,any,na.rm=T))/B,rowSums(is.na(stps1)[c(1,2),])/B)
  pstps2 <- alt*rowSums(stps2)/B
  pstps2 <- c(pstps2,sum(apply(stps2,2,any,na.rm=T))/B,rowSums(is.na(stps2)[c(1,2),])/B)
  pstps1r <- alt*rowSums(stps1r)/B
  pstps1r <- c(pstps1r,sum(apply(stps1r,2,any,na.rm=T))/B,rowSums(is.na(stps1r)[c(1,2),])/B)
  pstps2r <- alt*rowSums(stps2r)/B
  pstps2r <- c(pstps2r,sum(apply(stps2r,2,any,na.rm=T))/B,rowSums(is.na(stps2r)[c(1,2),])/B)

  ## simple
  pctdl <- alt*rowSums(ctdl,na.rm=T)/B
  pctdl <- c(pctdl,sum(apply(ctdl,2,any,na.rm=T))/B,rowSums(is.na(ctdl)[c(1,2),])/B)
  pstpl <- alt*rowSums(stpl,na.rm=T)/B
  pstpl <- c(pstpl,sum(apply(stpl,2,any,na.rm=T))/B,rowSums(is.na(stpl)[c(1,2),])/B)
  pctdlr <- alt*rowSums(ctdlr,na.rm=T)/B
  pctdlr <- c(pctdlr,sum(apply(ctdlr,2,any,na.rm=T))/B,rowSums(is.na(ctdlr)[c(1,2),])/B)
  pstplr <- alt*rowSums(stplr,na.rm=T)/B
  pstplr <- c(pstplr,sum(apply(stplr,2,any,na.rm=T))/B,rowSums(is.na(stplr)[c(1,2),])/B)


  ## Sugitani
  pctdsu <- alt*rowSums(ctdsu,na.rm=T)/B
  pctdsu <- c(pctdsu,sum(apply(ctdsu,2,any,na.rm=T))/B,rowSums(is.na(ctdsu)[c(1,2),])/B)
  pstpsu <- alt*rowSums(stpsu,na.rm=T)/B
  pstpsu <- c(pstpsu,sum(apply(stpsu,2,any,na.rm=T))/B,rowSums(is.na(stpsu)[c(1,2),])/B)
  pctdsur <- alt*rowSums(ctdsur,na.rm=T)/B
  pctdsur <- c(pctdsur,sum(apply(ctdsur,2,any,na.rm=T))/B,rowSums(is.na(ctdsur)[c(1,2),])/B)
  pstpsur <- alt*rowSums(stpsur,na.rm=T)/B
  pstpsur <- c(pstpsur,sum(apply(stpsur,2,any,na.rm=T))/B,rowSums(is.na(stpsur)[c(1,2),])/B)


 
  
  ## sophisticated
  pctdls <- alt*rowSums(ctdls,na.rm=T)/B
  pctdls <- c(pctdls,sum(apply(ctdls,2,any,na.rm=T))/B,rowSums(is.na(ctdls)[c(1,2),])/B)
  pstpls <- alt*rowSums(stpls,na.rm=T)/B
  pstpls <- c(pstpls,sum(apply(stpls,2,any,na.rm=T))/B,rowSums(is.na(stpls)[c(1,2),])/B)

  out <- rbind(
    preplanned,
    improved,
    pctds1,
    pctds2,
    pctds1r,
    pctds2r,
    pstps1,
    pstps2,
    pstps1r,
    pstps2r,
    pctdl,
    pstpl,
    pctdlr,
    pstplr,
    pctdsu,
    pstpsu,
    pctdsur,
    pstpsur,
    pctdls,
    pstpls)
  row.names(out) <- c('Pre-planned','Improved','agMTP Select 1','agMTP Select 2',
                      'agMTP  Select 1 SSR','agMTP Select 2 SSR',
                      'sCTP Select 1','sCTP Select 2',
                      'sCTP Select 1 SSR','sCTP Select 2 SSR',
                      'agMTP Select better','sCTP Select better',
                      'agMTP Select better SSR','sCTP Select better SSR',
                      'agMTP Rule I','sCTP Rule I',
                      'agMTP Rule I SSR','sCTP Rule I SSR',
                      'agMTP Complex rule','sCTP Complex rule')
  return(out)
                      
}
