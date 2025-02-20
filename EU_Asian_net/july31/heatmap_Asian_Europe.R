#dyadic = read.csv('COW_Trade_4.0/COW_Trade_4.0/Dyadic_COW_4.0.csv', header=TRUE, sep=',')
#nation = read.csv('COW_Trade_4.0/COW_Trade_4.0/National_COW_4.0.csv', header=TRUE, sep=',')
Asia_dyadic = read.csv('july31/COW_Trade_4.0/COW_Trade_4.0//Dyadic_Asia_result.csv', header=TRUE, sep=',')
Asia_nation = read.csv('july31/COW_Trade_4.0/COW_Trade_4.0//National_Asia_result.csv', header=TRUE, sep=',')

ccode = sort(unique(Asia_nation$ccode)) # 50
year = sort(unique(Asia_dyadic$year)) # 145
country_id1 = c(1:length(ccode))[match(Asia_dyadic$ccode1, ccode)]
country_id2 = c(1:length(year))[match(Asia_dyadic$ccode2, ccode)]
year_id = c(1:145)[match(Asia_dyadic$year, year)]
part1_df = data.frame(V1=country_id1, V2=country_id2, year=year_id, weight=Asia_dyadic$flow1)
part2_df = data.frame(V1=country_id2, V2=country_id1, year=year_id, weight=Asia_dyadic$flow2)
trade_df = rbind(part1_df, part2_df)
# here, I delete all rows which contains missing trade data and whose trading amounts is 0
trade_df<-trade_df[!(trade_df$weight==-9 | trade_df$weight==0),]

graphs = array(0, dim=c(length(year), length(ccode), length(ccode)))
for (i in 1 : dim(trade_df)[1]){
  record = trade_df[i,]
  graphs[record[[3]], record[[1]], record[[2]]] = 1
}

# after adding following code, you can draw vertex with country name. I have checked
country = unique(Asia_nation$statename)
dimnames(graphs)[[1]] = year
dimnames(graphs)[[2]] = country
dimnames(graphs)[[3]] = country


source('function.R')
edget=apply(graphs,1,sum)

#csid=c(1:48,51:57,61:67,69:75,77:168,170:205)##delete repeated counties

X=graphs[81:145,,] ##choose the data from 1950-2014
n=dim(X)[1]
p=dim(X)[2]
time=year[81:145]
##find change point
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
cid
#cid=38

year[80+cid+4]
index  = c(85:141)
plot(year[index],lik3,type='l')
lines(year[index],lik3,lty=2)



X=graphs[81:145,,]
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
which(nuhat==1)

length(which(nuhat==1))#a = L1+L2
#a =a[order(nuhat),order(nuhat)]

adj_a = X[1,order(nuhat),order(nuhat)]
for(i in 2:(cid+4)){
  adj_a = adj_a+X[i,order(nuhat),order(nuhat)]
}
adj_aver =adj_a/(cid+4)
colnames(adj_aver)
# 假设你想要重新排序列名，首先定义新的列名顺序
new_order <-  c( "Cyprus","Iran","Turkey",            
                     "Iraq","Syria","Lebanon","Jordan","Israel","Saudi Arabia","Kuwait","China","Japan","Pakistan","Yemen","Bahrain","Qatar","United Arab Emirates", "Oman",
                     "Mongolia","Taiwan","Korea","North Korea","South Korea","India","Bhutan","Bangladesh","Myanmar",
                     "Sri Lanka","Maldives","Nepal","Thailand","Cambodia","Laos","Vietnam" ,"Malaysia","Singapore", "Brunei","Philippines" ,
                     "Indonesia","East Timor","Republic of Vietnam", "Afghanistan", "Turkmenistan","Tajikistan", "Kyrgyzstan", "Uzbekistan", "Kazakhstan","Armenia","Georgia","Azerbaijan" )  # 按照你想要的顺序
 
adj_aver_reordered <- adj_aver[new_order, new_order]

#rownames(a) = colnames(a) = c("","")
###heatmap
##
library(ggplot2)
library(reshape2)

newa =melt(adj_aver)
ggplot(newa, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "gray", high = "blue", mid = "lightblue", midpoint = 0.3, limit = c(0, 1), space = "Lab", name="Links") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "", title = "Heatmap of Adjacency Matrix")

