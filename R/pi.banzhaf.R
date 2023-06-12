pi.banzhaf <-
  function(quota,weights,partition=NULL,normalized=FALSE,swing=FALSE){
    
    partition0<-partition
    n<-length(weights)
    if (missing(partition)){partition<-1:n}
    
    np<-max(partition)
    tam.union<-c()
    weight.union<-c()
    for(i in 1:np){
      tam.union[i]<-length(which(partition==i))
      weight.union[i]<-sum(weights[which(partition==i)])
    }
    
    
    jug<-1
    swings<-c()
    bzo<-c()
    for(jug in 1:length(weights)){
      
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
          coef0[i,1]<-1
          coef0[i,(ip1+1)]<-1
        }
      }
      
      ik<-length(vector.weight.union)
      
      for(j in 1:ik){
        i0<-round(vector.weight.union[j])
        coef0[(il+j),(i0+1)]<-1
        coef0[(il+j),1]<-1
      }
      
      
      
      p1<-coef0[1,]
      if ((il+ik)>1){
        for(j in 2:(il+ik)){
          p1<-multiply2(p1,coef0[j,])
        }
      }
      
      (v.jug<-sum(p1[(max(0,quota-weights[jug]+1)):(quota)]))
      swings[jug]<-v.jug
      bzo[jug]<-v.jug/2^(il+ik)
      
    }
    
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
