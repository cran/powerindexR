multiply1<-function(pol1,pol2){
  res<-rep(0,0,0,0)
  for (j in 1:nrow(pol2)){
    pol2j<-pol2[j,]
    pol2jaux<-matrix(rep(pol2j,nrow(pol1)),nrow=nrow(pol1),byrow=TRUE)
    if (nrow(pol1)>1){
      aux<-cbind(pol1[,1:3]+pol2jaux[,1:3],pol1[,4]*pol2jaux[,4])
    } else {
      aux<-t(as.matrix(c(pol1[,1:3]+pol2jaux[,1:3],pol1[,4]*pol2jaux[,4])))
    }
    res<-rbind(res,aux)
  }
  return(res)} 