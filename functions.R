
  

## concatenate list elements
cc <- function(list,fun=c){
  if(fun == c)
  do.call(fun,list)
  ## n <- length(list)
  ## start <- list[[1]]
  ## lapply(tail(list,n-1),function(e) start <<- fun(start,e))
  ## start
}

## this is useful when trying to rbind matrices with 0 rows or columns (as happens in loops)
mbind <- function(m1,m2,margin=2){
  bind <- function(...) {
    if(margin==1){
      rbind(...)
    } else if(margin==2){
      cbind(...)
    } else {
      stop("Unsupported margin argument")
    }
  }
  if(is.null(m1)){
    m2
  } else {
    bind(m1,m2)
  }
}

## identity function
id <- function(x) return(x)

## are all elements of a list equal to each other
allEqual <- function(list){
  ## dirty
  if(class(list[[1]])=='function'){
    return(TRUE)
  }
  all(sapply(list,function(element) all(element == list[[1]])))
}


## so I don't have to set the proper core number by hand every time
setCores <- function(c=NULL,...){
  require(parallel)
  host <- system('hostname',intern=T)
  if(host=='msi-251'){    
    options("mc.cores"=2)
  } else if( host=='workstation') {
    options("mc.cores"=3)
  } else if( !is.null(c)){
    options("mc.cores"=c)
  }
}


## proportional efficacy, returns a vector of proportionally paramterezid effect sizes, effect*treatment*endpoint
m.prop <- function(d,c,g=1){
  c(d,d*c,d*g,d*g*c)
}

## set between endpoint correlation in treatment to control correlation matrix
sM <- function(r,R){
  R[1,3] <- R[3,1] <- R[2,4] <- R[4,2] <- r
  R[1,4] <- R[4,1] <- R[2,3] <- R[3,2] <- r*R[1,2]
  R
}

## rqmvnorm produces only 2^n number of samples we reduce it to exactly B by random selection
sim.normal <- function(m,R,B,...){
  data <- rqmvnorm(B,m,R,...)
  select <- sample(1:nrow(data),B)
  return(data[1:B,])
}


######################################################

## rename that function
evalPower <- function(Gp,...){
  calcPower(G=Gp@m,weights=Gp@weights,...)
}

## rename that function
firstStage <- function(graph,pvalues,...){
  doInterim(graph,qnorm(1-pvalues),...)
}

## renames that function
secondStage <- function(interim,select=rep(1,length(interim@z1)),...){
  ## f <- function(pvalues,...){
  ##   z2 <- qnorm(1-pvalues)
  ##   (secondStageTest(...))(z2)
  ## }
  f <- secondStageTest(interim,select,...)
  f
}

## special purpose function if only one treatment is selected
## then we do not need to recompute the second stage weights in each scenario
selectOne <- function(first,second,second.ssr,select,scenario,alpha=.025,wipe.data=T){
  if(length(scenario@hint)==0){
    stop("selectOne, no precomputed weights found, please set the hint slot of your scenario!")
  }
  ## number of hypotheses
  n <- nhyp(first)
  ## weight matrix
  mat <- scenario@hint[,(n+1):(2*n)]
  res <- new("simulationResult")
  res@first.stage.data <- first
  v <- scenario@v
  if(wipe.data){
    ## probably we don't want to save all first stage data
    res@first.stage.data@data <- matrix(nr=0,nc=0)
  }
  res@second.stage.test <- scenario

  if(select[1] == "conditional"){
    ## make a selection based on the first stage data
    selections <- apply(first@data,1,scenario@selector)
  } else if(class(select) == 'logical' && length(select) == n){
    ## or use external selection vector
    selections <- matrix(rep(select,length(first)),nrow=n)
  } else {
    stop('Invalid select argument!')
  }
  #print(selections[,1])
  if(v == 1){
    pooled <- first + second
    ## droped treatments get their statistics set to a very small value
    pooled[,!selections[,1]] <- -10
    ## weights are set to zero
    mat[,!selections[,1]] <- 0
    #print('bohoo ;-)')
    ## since we are at the end apply fixed sample test
    res@rejections <- as.data.frame(apply(pooled@data,1,(gMCP:::decideTest),bounds <- mat*alpha))
  } else {
    ## partial conditional errors
    pces <- apply(first@data,1,function(z1) pCEfast(z1=z1,w=mat,v=v,alpha=alpha))
    ssT <- sapply(1:ncol(pces),function(i) selectOneFast(matrix(pces[,i],ncol=n),selections[,i]))
    res@selections <- apply(selections,2,function(select) paste("H",which(select),sep='',collapse=', '))
    if(scenario@SSR){
      res@rejections <- as.data.frame(lapply(1:length(ssT),function(i) ssT[[i]](second.ssr@data[i,])))
      #print('Ghostbusters!')
    } else {
      res@rejections <- as.data.frame(lapply(1:length(ssT),function(i) ssT[[i]](second@data[i,])))
      #print('who you gonna call?')
    }
  }
  res
}
  
  
reweightingTest <- function(first,second,second.ssr,scenario,alpha=.025,wipe.data=T){
  n <- nhyp(first)
  mat <- scenario@hint[,(n+1):(2*n)]
  res <- new("simulationResult")
  res@first.stage.data <- first
  if(wipe.data==T){
    res@first.stage.data@data <- matrix(nrow=0,ncol=0)
  }
  ## we need to set this somewhere scenario specific!!!
  v <- scenario@v
  #v <- npatients(first)/(npatients(first)+npatients(second))
  res@second.stage.test <- scenario
  Bj <- apply(first@data,1,function(z1) rowSums(matrix(pCEfast(z1=z1,w=mat,v=v,alpha=alpha),ncol=n)))
  sst <- apply(first@data,1,function(z1) scenario@test(z1))
  second@data[sst[1,]==1,] <- second.ssr@data[sst[1,]==1,]
  print(table(sst[1,]))
  res@rejections <- as.data.frame(ssTfast(Bj,sst[-1,],first@data,second@data,second.ssr@data,v,alpha))
  res
}

selectOneFast <- function(cem,select){
  ## fast version of secondStageTest if only one Treatment is selected hierarchical endpoints are supported 
  n <- nrow(cem)
  w2s <- cem>0
  w2s[,!select] <- 0
  Cs <- w2s*rowSums(cem)
  ## we don't need this here because we give the pce to one hyp only anyways
  #Cs[rowSums(cem)>=1,] <- 1
  function(z) {(gMCP:::decideTest)(z,Cs)}
}



## compute partial conditional errors
pCEfast <- function(w,z1,v,alpha){
  n <- nrow(w)
  zv <- rep(z1*sqrt(v),each=n)
  sv <- rep(sqrt(1-v),each=n)
  wv <- as.numeric(w)
  pnorm({qnorm(wv*alpha,lower.tail=FALSE)-zv}/sv,lower.tail=FALSE)
}


## set weights such that they are proportional to first stage evidence
mymatch <- function (w2, B, z1, v, alpha, enhanced = T,eps = 10^-5) 
{
    if (all(w2 == 0)) {
        return(w2)
    }
    if (B > 1) {
        return(rep(1, length(w2)))
    }
    d <- function(alpha, w2, z1, v, B) {
        sum(gMCP:::partialCE(w2, z1, v, alpha)) - B
    }
    r <- uniroot(d, c(0+eps, 1-eps), w2 = w2, z1 = z1, v = v, B = B)$root
    gMCP:::partialCE(w2, z1, v, r)
}


## functions for formating the output into multicolumn latex tables
reps <- function(m,idx=F){
  ix <- c(0,cumsum(head(m,-1) != tail(m,-1)))
  if(idx){
    return(ix)
  }
  out <- m[unique(1+ix * length(ix)/max(ix+1))]
  attr(out,'reps') <- length(ix)/length(unique(ix))
  out
}

multictab <- function(t,margins,...){
  c <- ncol(t)
  pmargins <- function(margin,s1=T){
    head <- reps(margin)
    if(attr(head,"reps")==1){
      paste(paste(head,collapse=" & "),"\\\\ \\hline")
    }
    if(s1){
      paste(" & ",paste("\\multicolumn{",attr(head,'reps'),"}{|c|}{",head,"}",collapse=" & "),"\\\\ \\hline")
    } else {      
      paste(paste("\\multicolumn{",attr(head,'reps'),"}{|c|}{",head,"}",collapse=" & "),"\\\\ \\hline")
    }
  }
  extraheaders <- paste(sapply(margins,pmargins),collapse=' \n ')
  print(xtable(t),include.colnames=FALSE,add.to.row = list(pos = list(0,3),command = c(extraheaders,'')),...)
}








            
          
          

