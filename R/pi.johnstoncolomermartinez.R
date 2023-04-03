pi.johnstoncolomermartinez <-
function(quota,weights){
n<-length(weights)

coa<-createBitMatrix(n)[,1:n]
id.coa<-c()
for (l in 1:nrow(coa)){
id.coa[l]<-numero.coalition(coa[l,],n)
}

# crea f^i
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

coejL<-list()
for (i in 1:n){
weights.i<-weights[-i]
coejM<-matrix(0,ncol=1,nrow=nrow(coa))

for (j in 1:n-1){
for (l in (quota-weights[i]):(quota-1)){
for (k in 1:nrow(coa)){
if (coa[k,i]!=1){
if (sum(coa[k,-i])==j){
if (sum(weights.i[which(coa[k,-i]==1)])==l){
coejM[k,1]<-1
}
}
}
}
}
}
coejL[[i]]<-coejM
}


createJ<-matrix(0,ncol=n,nrow=nrow(coa))
for (i in 1:n){createJ[,i]<-coejL[[i]]}

createJH<-matrix(0,ncol=n,nrow=nrow(coa))
for (i in 1:n){
for (k in 1:nrow(coa)){
if (createJ[k,i]!=0){
coa.aux<-coa[k,]
coa.aux[i]<-1
id0<-numero.coalition(coa.aux,n)
createJH[which(id.coa==id0),i]<-1
}
}
}

Q<-length(which(apply(createJH,1,sum)>0))
createdegree<-matrix(0,ncol=n,nrow=nrow(coa))

for (k in 1:nrow(createJH)){
createdegree[k,which(createJH[k,]==1)]<-weights[which(createJH[k,]==1)]
}

tdegree<-apply(createdegree,1,sum)

jcm_isuperd<-matrix(0,nrow=n,ncol=sum(weights))
JCM<-rep(0,n)
for (i in 1:n){
      tdegree.aux<-tdegree[which(createdegree[,i]>0)]
for (d in 1:sum(weights)){
jcm_isuperd[i,d]<-length(which(tdegree.aux==d))
}
JCM[i]<-weights[i]/Q*sum(jcm_isuperd[i,]/(1:sum(weights)))
}


result<-list()
names.result<-c()
result[[1]]<-JCM
names.result<-c(names.result,"Jonhston-Colomer-Martinez")
names(result)<-names.result

return(result)}
