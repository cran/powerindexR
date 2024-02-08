MWC <-
function(quota,weights){
  n<-length(weights)
  nrow.coa <- 2^n - 1
  coa_sums<-list()
  kk<-1
  for (i in as.numeric(0:nrow.coa)){
    coa.i<-coalition(binary.digit(i, n), n)
    scoa.i<-sum(weights[coa.i==1])
    aux<-0
    if (sum(coa.i)!=0) aux<-min(weights[coa.i==1])
    dcoa.i<-quota+aux
    if(scoa.i>=quota & scoa.i<dcoa.i){
      coa_sums[[kk]]<-c(i,scoa.i)
      kk<-kk+1
    }
  }
  
  coa_sums<-do.call(rbind,coa_sums)
  M<-nrow(coa_sums)
  whichM<-t(sapply(as.numeric(1:M),function(t) coalition(binary.digit(coa_sums[t,1],n),n)))
  
  result<-list()
  names.result<-c()
  result[[1]]<-M
  names.result<-c(names.result,"Number of Minimal Winning Coalitions")
  result[[2]]<-whichM
  names.result<-c(names.result,"Minimal Winning Coalitions")
  names(result)<-names.result
  
  return(result)}
