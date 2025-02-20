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


X=graphs[126:145,,]
n=dim(X)[1]
p=dim(X)[2]
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

i=1
  net <- graph_from_adjacency_matrix(X[i,,], mode='undirected')
  V(net)$cluster = nuhat
#V(net)$cluster = as.numeric(as.character(attributes$status))
  colrs <- c("tomato", "gold")
  V(net)$color <- colrs[V(net)$cluster]
 #V(net)$color=as.numeric(V(net)$cluster)+1
  layout = gamma
#layout=layout_with_lgl
#layout=layout_on_grid
  plot(net, layout=layout, vertex.size=5, vertex.label.cex=1, edge.color='gray')
