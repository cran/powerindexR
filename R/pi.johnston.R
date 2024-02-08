pi.johnston <-
function(quota,weights,quasiminimal=FALSE){
  
  if (sum(weights)<quota){
    stop("The quota is greater than the sum of the weights. Please, introduce valid parameters.")
  }
  
  if (round(quota,5)==0){
    stop("The quota is zero. Please, introduce valid parameters.")
  }
  
  n<-length(weights)
  nrow.coa <- 2^n - 1
  coa_sums<-list()
  createGH<-list()
  kk<-0
  kkk<-0
  for (i in as.numeric(0:nrow.coa)){
    xx<-coalition(binary.digit(i, n), n)
    Splayers<-(1:n)[xx==1]
    sxx<-sum(weights[Splayers])
    aux<-0
    if (sum(xx)!=0) aux<-max(weights[Splayers])
    suppressWarnings(dxx<-quota+aux)
    if(sxx>=quota & sxx<dxx){
      kk<-kk+1
      coa_sums[[kk]]<-c(i,sxx)
      for (j in Splayers){
        if(sxx-weights[j]<quota){
          kkk<-kkk+1
          createGH[[kkk]]<-c(i,j)	
        }
      }
    }
  }
  createGH<-do.call(rbind,createGH)
  coa_sums<-do.call(rbind,coa_sums)
  Q<-nrow(coa_sums) # <-kk
  whichQ<-t(sapply(1:Q,function(t) coalition(binary.digit(coa_sums[t,1],n),n)))
  
  createdegree<-rep(0,nrow(createGH)) 
  
  for (k in 1:nrow(createGH)){
    createdegree[k]<-sum(createGH[,1]==createGH[k,1])
  }
  
  c_isuperd<-matrix(0,nrow=n,ncol=n)
  J<-rep(0,n)
  for (i in 1:n){
    for (d in 1:n){c_isuperd[i,d]<-length(which(createdegree[which(createGH[,2]==i)]==d))}
    J[i]<-1/Q*sum(c_isuperd[i,]/(1:n))
  }
  
  result<-list()
  names.result<-c()
  result[[1]]<-J
  names.result<-c(names.result,"Johnston")
  if (quasiminimal==TRUE){
    result[[2]]<-Q
    names.result<-c(names.result,"Number of Quasi-Minimal Winning Coalitions")
    result[[3]]<-whichQ
    names.result<-c(names.result,"Quasi-Minimal Winning Coalitions")
  }
  names(result)<-names.result
  
  return(result)
}

