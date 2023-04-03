numero.coalition<-function(coal,n){
  numero<-0
  for(i in 1:n){numero<-numero+2^(i-1)*coal[i]}
  numero
}