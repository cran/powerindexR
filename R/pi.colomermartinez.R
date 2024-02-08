pi.colomermartinez <-
  function(quota,weights,minimal=FALSE){
    
    if (sum(weights)<quota){
      stop("The quota is greater than the sum of the weights. Please, introduce valid parameters.")
    }

    if (round(quota,5)==0){
      stop("The quota is zero. Please, introduce valid parameters.")
    }    
        
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
    
    indexdeegre<-as.numeric((quota):sum(weights))
    
    g<-coa_sums[which(coa_sums[,2]>=quota & coa_sums[,2]<=sum(weights)),1]

    CM<-rep(0,n)
    CM<-sapply(1:n,function(i,whichM,indexdeegre,g){
      ii<-which(whichM[,i]==1)
      if (sum(ii)>0){
      g.aux<-matrix(0,ncol=length(indexdeegre),nrow=length(ii))
      for (t in 1:length(ii)){
        g.aux[t,coa_sums[which(coa_sums[,1]==g[ii[t]]),2]-(quota-1)]<-1
      }
      sum(apply(g.aux,2,sum)/indexdeegre)*weights[i]/M
      } else {
        0
      }
    }, whichM=whichM,indexdeegre=indexdeegre,g=g)
    
    
    result<-list()
    names.result<-c()
    result[[1]]<-CM
    names.result<-c(names.result,"Colomer-Martinez")
    if (minimal==TRUE){
      result[[2]]<-M
      names.result<-c(names.result,"Number of Minimal Winning Coalitions")
      result[[3]]<-whichM
      names.result<-c(names.result,"Minimal Winning Coalitions")
    }
    names(result)<-names.result
    
    return(result)}