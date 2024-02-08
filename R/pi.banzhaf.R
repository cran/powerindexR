pi.banzhaf <-
  function(quota,weights,partition=NULL,normalized=FALSE,swing=FALSE){
    
    if (sum(weights)<quota){
      stop("The quota is greater than the sum of the weights. Please, introduce valid parameters.")
    }

    if (round(quota,5)==0){
      stop("The quota is zero. Please, introduce valid parameters.")
    }
    
    partition0<-partition
    n<-length(weights)
    if (missing(partition)){partition<-1:n}
    np<-max(partition)
    tam.union<- sapply(1:n,function(i,partition){
      length(which(partition==i))
    }, partition=partition)
    
    weight.union<- sapply(1:n,function(i,partition,weights){
      sum(weights[which(partition==i)])
    }, partition=partition,weights=weights)
    
    num.results<- sapply(1:length(weights),function(jug,n,weights,partition,tam.union,weight.union){
      i.union<-partition[jug]
      vector.weight.iunion<-weights[which(partition==i.union)]
      vector.weight.iunion<-vector.weight.iunion[-which(which(partition==i.union)==jug)]
      
      vector.weight.union<-weight.union[-i.union]
      
      emax<-max(vector.weight.iunion,vector.weight.union)
      coef0<-matrix(0,nrow=length(vector.weight.iunion)+length(vector.weight.union),ncol=(emax+1))
      
      il<-length(vector.weight.iunion)
      for(i in 1:il){ 
        ip1<-round(vector.weight.iunion[i])
        if(il>0){
          coef0[i,(ip1+1)]<-1
          coef0[i,1]<-coef0[i,1]+1
        }
      }
    
      
      
      ik<-length(vector.weight.union)
      if (ik>0){
      for(j in 1:ik){
        i0<-round(vector.weight.union[j])
        coef0[(il+j),(i0+1)]<-1
        coef0[(il+j),1]<-coef0[(il+j),1]+1
      }
      }
      
      p1<-coef0[1,]
      if ((il+ik)>1){
        for(j in 2:(il+ik)){
          p1<-multiply2(p1,coef0[j,])
        }
      }
      
      if(weights[jug]!=0){
        xmin<-max(0,quota-weights[jug]+1)
        xmax<-min(quota,length(p1))
        v.jug<-sum(p1[xmin:xmax])
      } else {
        v.jug<-0
      }
      if (is.na(v.jug)) v.jug<-0
      swings<-v.jug
      bzo<-v.jug/2^(il+ik)
      c(swings,bzo)
      
    }, n=n,weights=weights,partition=partition,tam.union=tam.union,weight.union=weight.union)
    
    swings<-num.results[1,]
    bzo<-num.results[2,]
    
    result<-list()
    names.result<-c()
    result[[1]]<-bzo
    if (is.null(partition0)){
      names.result[1]<-"Banzhaf value"
    } else {
      names.result[1]<-"Banzhaf-Owen value"
    }
    if (normalized==TRUE){
      result[[2]]<-bzo/sum(bzo)
      if (sum(bzo==0)==n) result[[2]]<-rep(0,n)
      if (is.null(partition0)){
        names.result<-c(names.result,"Normalized Banzhaf value")
      } else {
        names.result<-c(names.result,"Normalized Banzhaf-Owen value")
      }
    }
    if (swing==TRUE){
      if (normalized==TRUE){
        result[[3]]<-swings
      } else {
        result[[2]]<-swings
      }
      names.result<-c(names.result,"Swings")
    }
    names(result)<-names.result
    
    return(result)}
