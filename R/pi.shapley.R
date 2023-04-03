pi.shapley <-
function(quota,weights,partition=NULL){

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
sh<-c()
for(jug in 1:length(weights)){
  i.union<-partition[jug]
vector.weight.iunion<-weights[which(partition==i.union)]        
weighti<-vector.weight.iunion[which(which(partition==i.union)==jug)]
vector.weight.iunion<-vector.weight.iunion[-which(which(partition==i.union)==jug)] 
  
vector.weight.union<-weight.union[-i.union]
vector.weight<-c(vector.weight.iunion,vector.weight.union)


iz<-length(vector.weight.iunion)
it<-length(vector.weight.union)

pi<-iz+1
m<-max(partition)

p1<-as.matrix(t(c(0,0,0,1)))

if(iz>0){
for(i in 1:iz){
ip1<-round(vector.weight.iunion[i])
coefz<-rbind(c(0,0,0,1),c(ip1,0,1,1))
p1<-multiply1(p1,coefz)
}}

if(it>0){
for(i in 1:it){
ip1<-round(vector.weight.union[i])
coeft<-rbind(c(0,0,0,1),c(ip1,1,0,1))
p1<-multiply1(p1,coeft)
}}


gx<-p1[,1]
ii<-which(gx>=(quota-weighti)&(gx<=(quota-1)))
p1arrayii<-p1[ii,]
if (is.matrix(p1arrayii)==FALSE){p1arrayii<-t(as.matrix(p1arrayii))}


pr<-(factorial(p1arrayii[,2])*factorial(m-p1arrayii[,2]-1)/factorial(m))
pl<-(factorial(p1arrayii[,3])*factorial(pi-p1arrayii[,3]-1)/factorial(pi))
shi<-sum(p1arrayii[,4]*pr*pl)


sh[jug]<-shi

}

result<-list()
names.result<-c()
result[[1]]<-sh
if (is.null(partition0)){
names.result[1]<-"Shapley value"
} else {
names.result[1]<-"Owen value"
}
names(result)<-names.result

return(result)}
