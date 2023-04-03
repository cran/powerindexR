pi.colomermartinez <-
function(quota,weights,minimal=FALSE){
n<-length(weights)

coa<-createBitMatrix(n)[,1:n]
id.coa<-c()
for (l in 1:nrow(coa)){id.coa[l]<-numero.coalition(coa[l,],n)}


coefM<-matrix(0,ncol=sum(weights),nrow=nrow(coa)) 

for (l in 1:nrow(coa)){
coa.l<-which(coa[l,]==1)
coefM[l,sum(weights[coa.l])]<-1
}

indexdeegre<-(quota):ncol(coefM)
g<-coefM[,indexdeegre]

indexg<-which(apply(g,1,sum)>0)
coag<-coa[indexg,]
coefMg<-g[indexg,]

coag.aux<-coag
if ((nrow(coag)-1)>1){
for (m in 1:(nrow(coag)-1)){
for (k in (m+1):(nrow(coag))){
if (check.subset(coag[m,],coag[k,])){
coag.aux[k,]<-rep(0,n)
}
}
}
}

M<-length(which(apply(coag.aux,1,sum)>0))
whichM<-coa[(which(apply(coag.aux,1,sum)>0)),]

indexgM<-indexg[which(apply(coag.aux,1,sum)>0)]
coagM<-coag[which(apply(coag.aux,1,sum)>0),] 
g<-coefMg[which(apply(coag.aux,1,sum)>0),]


CM<-rep(0,n)

for (i in 1:n){
ii<-which(coagM[,i]==1)
if (length(ii)>1){
CM[i]<-sum(apply(g[ii,],2,sum)/indexdeegre)*weights[i]/M
}else{
CM[i]<-sum(g[ii,]/indexdeegre)*weights[i]/M
}
}

result<-list()
names.result<-c()
result[[1]]<-CM
names.result<-c(names.result,"Colomer-Martinez")
if (minimal==TRUE){
result[[2]]<-nrow(coagM)
names.result<-c(names.result,"Number of Minimal Winning Coalitions")
result[[3]]<-coagM
names.result<-c(names.result,"Minimal Winning Coalitions")
}
names(result)<-names.result

return(result)}
