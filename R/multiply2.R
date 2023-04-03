multiply2<-function(coeforig,pol){
  
  coeforig.d<-which(coeforig>0)-1
  pol.d<-which(pol>0)-1
  
  coef.result1<-rep(0,max(coeforig.d)+max(pol.d)+1)
  coef.result2<-coef.result1
  coef.result1[coeforig.d+1]<-coeforig[coeforig.d+1]
  coef.result2[coeforig.d+pol.d[2]+1]<-coeforig[coeforig.d+1]
  coef.result<-coef.result1+coef.result2
  
  return(coef.result)}