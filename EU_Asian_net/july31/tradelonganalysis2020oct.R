dyadic = read.csv('COW_Trade_4.0/COW_Trade_4.0/Dyadic_COW_4.0.csv', header=TRUE, sep=',')
nation = read.csv('COW_Trade_4.0/COW_Trade_4.0/National_COW_4.0.csv', header=TRUE, sep=',')
ccode = sort(unique(nation$ccode)) # 205
year = sort(unique(dyadic$year)) # 145
country_id1 = c(1:205)[match(dyadic$ccode1, ccode)]
country_id2 = c(1:205)[match(dyadic$ccode2, ccode)]
year_id = c(1:145)[match(dyadic$year, year)]
part1_df = data.frame(V1=country_id1, V2=country_id2, year=year_id, weight=dyadic$flow1)
part2_df = data.frame(V1=country_id2, V2=country_id1, year=year_id, weight=dyadic$flow2)
trade_df = rbind(part1_df, part2_df)
# here, I delete all rows which contains missing trade data and whose trading amounts is 0
trade_df<-trade_df[!(trade_df$weight==-9 | trade_df$weight==0),]

graphs = array(0, dim=c(length(year), length(ccode), length(ccode)))
for (i in 1 : dim(trade_df)[1]){
  record = trade_df[i,]
  graphs[record[[3]], record[[1]], record[[2]]] = 1
}

# after adding following code, you can draw vertex with country name. I have checked
country = unique(nation$statename)
dimnames(graphs)[[1]] = year
dimnames(graphs)[[2]] = country
dimnames(graphs)[[3]] = country

edget=apply(graphs,1,sum)

csid=c(1:48,51:57,61:67,69:75,77:168,170:205)

X=graphs[81:145,csid,csid]
n=dim(X)[1]
p=dim(X)[2]
time=year[81:145]

library(sand)
g1 <- graph_from_adjacency_matrix(X[1,,], mode='undirected')
hist(degree(g1),col='lightblue')

dn=rep(0,n*p)
dnn=matrix(dn,nrow=n)
for(i in 1:n)
{
  g1 <- graph_from_adjacency_matrix(X[i,,], mode='undirected')
dnn[i,]=(degree(g1))
}

plot(year[80+1:n],dnn[,1],type='l')
for(i in 2:p)
{
  lines(year[80+1:n],dnn[,i])
}

table(round(a1,2))
coun[a1<0]

a1=rep(0,p)
for(i in 1:p)
{
  if(sum(dnn[,i])==0){a1[i]=-1}
  else
  {
fm=ar(dnn[,i],order.max=1,methods='ols')
a1[i]=fm$ar}
}

A <- as_adjacency_matrix(g1)
v.attrs <- as_data_frame(g1, what="vertices")

library(ergm)
g1.s <- network::as.network(as.matrix(A),
                                directed=FALSE)

my.fm=formula(g1.s ~ edges)
my.ergm <- formula(g1.s ~ edges + kstar(2) + triangle)
summary(my.ergm)
fm0=ergm(my.ergm)

#change point
source('C:/work/network/function.R')
q=2
lik3=rep(0,n-8)
for(i in 5:(n-4))
{
n1hat=i
X1=X[1:n1hat,,]
X2=X[(n1hat+1):n,,]
ch1=v1v2(X1,q)
ch2=v1v2(X2,q)
lik3[i-4]=Likv(ch1[,,1]+.0001,ch1[,,2]+.0001,X1)+Likv(ch2[,,1]+.0001,ch2[,,2]+.0001,X2)
}

cid=which.max(lik3)
cid=38

year[80+cid+4]
plot(year[index],lik3,type='l')
lines(year[index],lik3,lty=2)


n1hat=cid+4
X1=X[1:n1hat,,]
X2=X[(n1hat+1):n,,]

nu1=nu(X1,q)
nu2=nu(X2,q)

k=1
pa1=u1u2(X1,q,k)
pa2=u1u2(X2,q,k)

library(aricode)

coun=country[csid]
coun[nu1==1 & nu2==1]

#estimtion
ahat=matrix(rep(0,p^2),nrow=p)
bhat=ahat
Xlead=X[2:n,,]
for(j in 1:p){
for(k in j:p){
ahat[j,k]=sum(Xlead[,j,k]*(1-X[1:(n-1),j,k]))/(sum(1-X[1:(n-1),j,k])+.0001)
bhat[j,k]=sum((1-Xlead[,j,k])*X[1:(n-1),j,k])/(sum(X[1:(n-1),j,k])+.0001)
}}
ahat=(ahat+t(ahat))-diag(diag(ahat))
bhat=(bhat+t(bhat))-diag(diag(bhat))

q=2
#cluster
W1=ahat
W2=1-bhat
W1=W1-diag(diag(W1))
W2=W2-diag(diag(W2))
D1=apply(W1,1,sum)
D2=apply(W2,1,sum)
L1=diag(1/sqrt(D1+.0001))%*%W1%*%diag(1/sqrt(D1+.0001))
L2=diag(1/sqrt(D2+.0001))%*%W2%*%diag(1/sqrt(D2+.0001))
ev=eigen(L1+L2)
gamma=ev$vector[,1:q]
km<-kmeans(gamma,centers = q,nstart=20)
nuhat=km$cluster

i=31
i=50

png("tr2050.png", width=1024, height=768,bg='transparent')
i=50
net <- graph_from_adjacency_matrix(X[i,,], mode='undirected')
V(net)$cluster = nu2
colrs <- c("tomato","gold")
V(net)$color <- colrs[V(net)$cluster]
layout = gamma[,1:2]
plot(net, layout=layout, vertex.size=5, vertex.label.cex=.6, edge.color='black')
dev.off()

#V(net)$cluster = as.numeric(as.character(attributes$status))
#  colrs <- c("gold","tomato")
 colrs <- c("tomato","gold")
 V(net)$color <- colrs[V(net)$cluster]
 #V(net)$color=as.numeric(V(net)$cluster)+1
 # layout = gamma
layout=layout_with_dh
#layout=layout_as_tree
  plot(net, layout=layout, vertex.size=5, vertex.label.cex=.6, edge.color='gray')

#stationary test
#estimtion
ahat=matrix(rep(0,p^2),nrow=p)
bhat=ahat
T=ahat
Xlead=X[2:n,,]
for(j in 1:p){
for(k in j:p){
ahat[j,k]=sum(Xlead[,j,k]*(1-X[1:(n-1),j,k]))/(sum(1-X[1:(n-1),j,k])+.0001)
bhat[j,k]=sum((1-Xlead[,j,k])*X[1:(n-1),j,k])/(sum(X[1:(n-1),j,k])+.0001)
}}
ahat=(ahat+t(ahat))-diag(diag(ahat))
bhat=(bhat+t(bhat))-diag(diag(bhat))

epr=array(rep(0,p^2*(n-1)),dim=c(p,p,n-1))
for(j in 1:p){
for(k in j:p){
epr[j,k,]=(Xlead[,j,k])*(X[1:(n-1),j,k])*rbinom(1,1,min(max(ahat[j,k]/(1-bhat[j,k]+.00001),0),1))-
(1-Xlead[,j,k])*(1-X[1:(n-1),j,k])*rbinom(1,1,min(max(bhat[j,k]/(1-ahat[j,k]+.00001),0),1))-
(1-Xlead[,j,k])*(X[1:(n-1),j,k])+(Xlead[,j,k])*(1-X[1:(n-1),j,k])

#epr[j,k,]=ep[j,k,1:(n-1)]

n211=sum((epr[j,k,2:(n-1)]==1)*(epr[j,k,1:(n-2)]==1))
n210=sum((epr[j,k,2:(n-1)]==1)*(epr[j,k,1:(n-2)]==0))
n201=sum((epr[j,k,2:(n-1)]==0)*(epr[j,k,1:(n-2)]==1))
n200=sum((epr[j,k,2:(n-1)]==0)*(epr[j,k,1:(n-2)]==0))
n220=sum((epr[j,k,2:(n-1)]==-1)*(epr[j,k,1:(n-2)]==0))
n202=sum((epr[j,k,2:(n-1)]==0)*(epr[j,k,1:(n-2)]==-1))
n222=sum((epr[j,k,2:(n-1)]==-1)*(epr[j,k,1:(n-2)]==-1))
n221=sum((epr[j,k,2:(n-1)]==-1)*(epr[j,k,1:(n-2)]==1))
n212=sum((epr[j,k,2:(n-1)]==1)*(epr[j,k,1:(n-2)]==-1))
n11=sum((epr[j,k,]==1))
n10=sum((epr[j,k,]==0))
n12=sum((epr[j,k,]==-1))

T[j,k]=(n211-n11^2/n+.0001)^2/(n11^2/n+.00001)+(n210-n11*n10/n+.0001)^2/(n11*n10/n+.00001)
+(n201-n11*n10/n+.0001)^2/(n11*n10/n+.00001)+(n200-(n10)^2/n+.0001)^2/((n10)^2/n+.00001)
+(n220-n12*n10/n+.0001)^2/(n12*n10/n+.00001)+(n202-n12*n10/n+.0001)^2/(n12*n10/n+.00001)
+(n222-(n12)^2/n+.0001)^2/((n12)^2/n+.00001)+(n221-n12*n11/n+.0001)^2/(n12*n11/n+.00001)+(n212-n12*n11/n+.0001)^2/(n12*n11/n+.00001)
}}

sT=sum(T)

B=500
Tb=array(rep(0,p^2*B),dim=c(p,p,B))

for(b in 1:B){
pb=sample(1:(n-1))
eprb=epr[,,pb]
for(j in 1:p){
for(k in j:p){
n211=sum((eprb[j,k,2:(n-1)]==1)*(eprb[j,k,1:(n-2)]==1))
n210=sum((eprb[j,k,2:(n-1)]==1)*(eprb[j,k,1:(n-2)]==0))
n201=sum((eprb[j,k,2:(n-1)]==0)*(eprb[j,k,1:(n-2)]==1))
n200=sum((eprb[j,k,2:(n-1)]==0)*(eprb[j,k,1:(n-2)]==0))
n220=sum((eprb[j,k,2:(n-1)]==-1)*(eprb[j,k,1:(n-2)]==0))
n202=sum((eprb[j,k,2:(n-1)]==0)*(eprb[j,k,1:(n-2)]==-1))
n222=sum((eprb[j,k,2:(n-1)]==-1)*(eprb[j,k,1:(n-2)]==-1))
n221=sum((eprb[j,k,2:(n-1)]==-1)*(eprb[j,k,1:(n-2)]==1))
n212=sum((eprb[j,k,2:(n-1)]==1)*(eprb[j,k,1:(n-2)]==-1))
n11=sum((eprb[j,k,]==1))
n10=sum((eprb[j,k,]==0))
n12=sum((eprb[j,k,]==-1))

Tb[j,k,b]=(n211-n11^2/n+.0001)^2/(n11^2/n+.00001)+(n210-n11*n10/n+.0001)^2/(n11*n10/n+.00001)
+(n201-n11*n10/n+.0001)^2/(n11*n10/n+.00001)+(n200-(n10)^2/n+.0001)^2/((n10)^2/n+.00001)
+(n220-n12*n10/n+.0001)^2/(n12*n10/n+.00001)+(n202-n12*n10/n+.0001)^2/(n12*n10/n+.00001)
+(n222-(n12)^2/n+.0001)^2/((n12)^2/n+.00001)+(n221-n12*n11/n+.0001)^2/(n12*n11/n+.00001)+(n212-n12*n11/n+.0001)^2/(n12*n11/n+.00001)
}}
}

sTb=apply(Tb,3,sum)

Test=sum(sT<sTb)/B
