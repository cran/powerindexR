powerindex <-
function(quota,weights,index=c("S","B","J","CM","JCM"),partition=NULL,quasiminimal=FALSE,minimal=FALSE,normalized=FALSE,swing=FALSE){

  if (sum(weights)<quota){
    stop("The quota is greater than the sum of the weights. Please, introduce valid parameters.")
  }  
  
  if (round(quota,5)==0){
    stop("The quota is zero. Please, introduce valid parameters.")
  }  
 
if (missing(quota)&missing(weights)){
message("Please, introduce a quota and a distribution of weights.")
} else {
if (missing(index)&((quasiminimal==TRUE)|(minimal==TRUE))){
if (quasiminimal==TRUE){
res<-QMWC(quota,weights)
}
if (minimal==TRUE){
res<-MWC(quota,weights)
}
} else {
if (index=="B"){
if (missing(partition)){
if (normalized==FALSE){
if (swing==FALSE){
res<-pi.banzhaf(quota,weights)
} else {
res<-pi.banzhaf(quota,weights,swing=TRUE)
}
} else {
if (swing==FALSE){
res<-pi.banzhaf(quota,weights,normalized=TRUE)
} else {
res<-pi.banzhaf(quota,weights,normalized=TRUE,swing=TRUE)
}
}
}else{
if (normalized==FALSE){
if (swing==FALSE){
res<-pi.banzhaf(quota,weights,partition)
} else {
res<-pi.banzhaf(quota,weights,partition,swing=TRUE)
}
} else{
if (swing==FALSE){
res<-pi.banzhaf(quota,weights,partition,normalized=TRUE)
} else {
res<-pi.banzhaf(quota,weights,partition,normalized=TRUE,swing=TRUE)
}}
}
}
if (index=="S"){
if (missing(partition)){
res<-pi.shapley(quota,weights)
}else{
res<-pi.shapley(quota,weights,partition)
}
}

if (index=="J"){res<-pi.johnston(quota,weights,quasiminimal)}
if (index=="CM"){res<-pi.colomermartinez(quota,weights,minimal)}
if (index=="JCM"){res<-pi.johnstoncolomermartinez(quota,weights)}
if ((index!="S")&(index!="B")&(index!="J")&(index!="CM")&(index!="JCM")){
message("Please, revise your options. Such power index is not yet considered.")
}
} 
if (missing(index)&(quasiminimal==FALSE)&(minimal==FALSE)){
message("Please, revise your options.")
}




}



return(res)}
