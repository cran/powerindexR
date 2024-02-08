pi.johnstoncolomermartinez <-
function(quota,weights){
  
  if (sum(weights)<quota){
    stop("The quota is greater than the sum of the weights. Please, introduce valid parameters.")
  }

  if (round(quota,5)==0){
    stop("The quota is zero. Please, introduce valid parameters.")
  }

  n<-length(weights)
  nrow.coa <- 2^n - 1
  coa_sums<-list()
  createJH<-list()
  kkk<-0
  for (i in as.numeric(0:nrow.coa)){
    xx<-coalition(binary.digit(i, n), n)
    Splayers<-(1:n)[xx==1]
    sxx<-sum(weights[Splayers])
    coa_sums[[i+1]]<-c(i,sxx)
    Snoplayers<-(1:n)[xx!=1]
    for (j in Snoplayers){
      if(sxx>=(quota-weights[j])&sxx<=(quota-1)){
        if (xx[j]!=1){
          xx0<-xx
          xx0[j]<-1
          id0<-id.coalition(xx0,n)
          kkk<-kkk+1
          createJH[[kkk]]<-c(id0,j)
         }
      }
    }
  }

  createJH<-do.call(rbind,createJH)
  coa_sums<-do.call(rbind,coa_sums)
  id.Q<-as.numeric(names(table(createJH[,1])))-1
  Q<-length(id.Q)
  
  createdegree<- sapply(1:nrow(createJH),function(k,createJH,weights){
    sum(weights[createJH[which(createJH[,1]==createJH[k,1]),2]])
  }, createJH=createJH,weights=weights)
  
  
  JCM<-sapply(1:n,function(i,createJH,weights,createdegree){
    jcm_isuperd<-rep(0,sum(weights))
    
    aux<-which(createJH[,2]==i)
    tdegree.aux<-createdegree[aux]
    for (d in 1:sum(weights)){
      jcm_isuperd[d]<-length(which(tdegree.aux==d))
    }
    weights[i]/Q*sum(jcm_isuperd/(1:sum(weights)))
  }, createJH=createJH,weights=weights,createdegree=createdegree)
  
  
  result<-list()
  names.result<-c()
  result[[1]]<-JCM
  names.result<-c(names.result,"Jonhston-Colomer-Martinez")
  names(result)<-names.result
  
  return(result)}
