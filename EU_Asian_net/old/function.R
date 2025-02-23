# install.packages('network')
# install.packages('intergraph')
library(network)
library(intergraph)
library(igraph)


#function to compute likelihood given parameter v1 and v2, for data X
Likv<-function(v1,v2,X){
p=dim(X)[2]
n=dim(X)[1]
likv=0
for(i in 2:n){
a1=X[i,,]*(1-X[i-1,,])*log(v1+.0001)+(1-X[i,,])*(1-X[i-1,,])*log(1-v1+.0001)
+(1-X[i,,])*X[i-1,,]*log(v2+.0001)+X[i,,]*X[i-1,,]*log(1-v2+.0001)
a1[lower.tri(a1)]=0
likv=likv+sum(a1)}
return(likv)
}

#estimate parameter  alpha(v1) and beta(v2) matrices given X and q
v1v2<-function(X,q){
nuhat=nu(X,q)
p=dim(X)[2]
n=dim(X)[1]
q=length(unique(nuhat))
thetahat=matrix(rep(0,q^2),nrow=q)
etahat=thetahat
for(j in 1:q){
for(k in j:q){
thetahat[j,k]=sum(X[2:n,nuhat==j,nuhat==k]*(1-X[1:(n-1),nuhat==j,nuhat==k]))/(sum(1-X[1:(n-1),nuhat==j,nuhat==k])+.0001)
etahat[j,k]=sum((1-X[2:n,nuhat==j,nuhat==k])*X[1:(n-1),nuhat==j,nuhat==k])/(sum(X[1:(n-1),nuhat==j,nuhat==k])+.0001)
}}
thetahat=thetahat+t(thetahat)-diag(diag(thetahat))
etahat=etahat+t(etahat)-diag(diag(etahat))
ahat=matrix(rep(0,p^2),nrow=p)
alphahat=ahat
betahat=ahat
for(j in 1:p){
for(k in j:p){
alphahat[j,k]=thetahat[nuhat[j],nuhat[k]]
betahat[j,k]=etahat[nuhat[j],nuhat[k]]
}}
alphahat=alphahat+t(alphahat)-diag(diag(alphahat))
betahat=betahat+t(betahat)-diag(diag(betahat))
v1v2=array(rep(0,p^2*2),dim=c(p,p,2))
v1v2[,,1]=alphahat
v1v2[,,2]=betahat
return(v1v2)
}

#compute  theta and eta (half matrices) and standard errors from X, k=0 is old method and k=1 is new method
u1u2<-function(X,q,k){
if(k==0) {nuhat=nu0(X,q)}
else {nuhat=nu(X,q)}
p=dim(X)[2]
n=dim(X)[1]
q=length(unique(nuhat))
thetahat=matrix(rep(0,q^2),nrow=q)
etahat=thetahat
vara=thetahat
varb=thetahat
pn=rep(0,q)
for(j in 1:q)
{
pn[j]=sum(nuhat==j)
}
for(j in 1:q){
for(k in j:q){
thetahat[j,k]=sum(X[2:n,nuhat==j,nuhat==k]*(1-X[1:(n-1),nuhat==j,nuhat==k]))/(sum(1-X[1:(n-1),nuhat==j,nuhat==k])+.0001)
etahat[j,k]=sum((1-X[2:n,nuhat==j,nuhat==k])*X[1:(n-1),nuhat==j,nuhat==k])/(sum(X[1:(n-1),nuhat==j,nuhat==k])+.0001)
vara[j,k]=thetahat[j,k]*(thetahat[j,k]+etahat[j,k])*(1-thetahat[j,k])/(etahat[j,k]+.0001)/(n*(pn[k]*(k>j)+(pn[j]-1)*.5*(k==j))*pn[j])
varb[j,k]=etahat[j,k]*(thetahat[j,k]+etahat[j,k])*(1-etahat[j,k])/(thetahat[j,k]+.0001)/(n*(pn[k]*(k>j)+(pn[j]-1)*.5*(k==j))*pn[j])
}}
return(cbind(thetahat,sqrt(vara),etahat,sqrt(varb)))
}

#cluster for X
nu<-function(X,q){
n=dim(X)[1]
p=dim(X)[2]
ahat=matrix(rep(0,p^2),nrow=p)
bhat=ahat
for(j in 1:p){
for(k in j:p){
ahat[j,k]=sum(X[2:n,j,k]*(1-X[1:(n-1),j,k]))/(sum(1-X[1:(n-1),j,k])+.0001)
bhat[j,k]=sum((1-X[2:n,j,k])*X[1:(n-1),j,k])/(sum(X[1:(n-1),j,k])+.0001)
}}
ahat=(ahat+t(ahat))-diag(diag(ahat))
bhat=(bhat+t(bhat))-diag(diag(bhat))
W1=ahat
W2=1-bhat
W1=W1-diag(diag(W1))
W2=W2-diag(diag(W2))
D1=apply(W1,1,sum)
D2=apply(W2,1,sum)
L1=diag(1/sqrt(D1+.0001))%*%W1%*%diag(1/sqrt(D1+.0001))
L2=diag(1/sqrt(D2+.0001))%*%W2%*%diag(1/sqrt(D2+.0001))
ev=eigen(L1+L2)
ind=order(abs(ev$value),decreasing=T)
gamma=ev$vector[,ind[1:q]]
km<-kmeans(gamma,centers = q,nstart=20)
nuhat=km$cluster
return(nuhat)
}

#cluster without model
nu0<-function(X,q){
n=dim(X)[1]
p=dim(X)[2]
W=apply(X,2:3,sum)
W=W-diag(diag(W))
D=apply(W,1,sum)
L=diag(1/sqrt(D+.0001))%*%W%*%diag(1/sqrt(D+.0001))
ev=eigen(L)
ind=order(abs(ev$value),decreasing=T)
gamma=ev$vector[,ind[1:q]]
km<-kmeans(gamma,centers = q,nstart=20)
nuhat3=km$cluster
return(nuhat3)
}


library(aricode)


