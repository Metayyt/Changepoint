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


#change point
source('C:/work/network/function.R')

cid=38

n1hat=cid+4
X1=X[1:n1hat,,]
X2=X[(n1hat+1):n,,]

#stationary test
#estimtion
X=X2
n=dim(X)[1]
p=dim(X)[2]

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
