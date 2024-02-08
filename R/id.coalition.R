id.coalition<-function(coal,n){
  number<-0
  for(i in as.numeric(1:n)){number<-number+2^(i-1)*coal[i]}
  number
}
