pi.johnston <-
function(quota,weights,quasiminimal=FALSE){

n<-length(weights)

coa<-createBitMatrix(n)[,1:n]
id.coa<-c()
for (l in 1:nrow(coa)){id.coa[l]<-numero.coalition(coa[l,],n)}

coefL<-list()
for (i in 1:n){
coefM<-matrix(0,ncol=sum(weights),nrow=nrow(coa))
for (l in 1:nrow(coa)){
if (coa[l,i]!=1){
coa.l<-which(coa[l,]==1)
coefM[l,sum(weights[coa.l])]<-1
}
}
coefL[[i]]<-coefM
}


coegL<-list()
for (i in 1:n){
weights.i<-weights[-i]
coegM<-matrix(0,ncol=1,nrow=nrow(coa))
for (j in 1:n-1){
for (l in (quota-weights[i]):(quota-1)){
for (k in 1:nrow(coa)){
if (coa[k,i]!=1){
if (sum(coa[k,-i])==j){
if (sum(weights.i[which(coa[k,-i]==1)])==l){
coegM[k,1]<-1
}
}
}
}
}
}
coegL[[i]]<-coegM
}


createG<-matrix(0,ncol=n,nrow=nrow(coa))
for (i in 1:n){createG[,i]<-coegL[[i]]}

createGH<-matrix(0,ncol=n,nrow=nrow(coa))
for (i in 1:n){
for (k in 1:nrow(coa)){
if (createG[k,i]!=0){
coa.aux<-coa[k,]
coa.aux[i]<-1
id0<-numero.coalition(coa.aux,n)
createGH[which(id.coa==id0),i]<-1
}
}
}

Q<-length(which(apply(createGH,1,sum)>0))
whichQ<-coa[(which(apply(createGH,1,sum)>0)),] 

createdegree<-matrix(0,ncol=n,nrow=nrow(coa))

for (k in 1:nrow(createGH)){
createGH.k<-sum(createGH[k,])
createdegree[k,which(createGH[k,]==1)]<-createGH.k
}

c_isuperd<-matrix(0,nrow=n,ncol=n)
J<-rep(0,n)
for (i in 1:n){
for (d in 1:n){c_isuperd[i,d]<-length(which(createdegree[,i]==d))}
J[i]<-1/Q*sum(c_isuperd[i,]/(1:n))
}

result<-list()
names.result<-c()
result[[1]]<-J
names.result<-c(names.result,"Johnston")
if (quasiminimal==TRUE){
result[[2]]<-Q
names.result<-c(names.result,"Number of Quasi-Minimal Winning Coalitions")
result[[3]]<-whichQ
names.result<-c(names.result,"Quasi-Minimal Winning Coalitions")
}
names(result)<-names.result

return(result)}
