dyadic = read.csv('july31/COW_Trade_4.0/COW_Trade_4.0/Dyadic_Europe_result.csv', header=TRUE, sep=',')
nation = read.csv('july31/COW_Trade_4.0/COW_Trade_4.0/National_Europe_result.csv', header=TRUE, sep=',')
#dyadic = read.csv('july31/COW_Trade_4.0/COW_Trade_4.0/Dyadic_Asia_result.csv', header=TRUE, sep=',')
#nation = read.csv('july31/COW_Trade_4.0/COW_Trade_4.0/National_Asia_result.csv', header=TRUE, sep=',')
ccode = sort(unique(nation$ccode)) # 50
year = sort(unique(dyadic$year)) # 145
country_id1 = c(1:length(ccode))[match(dyadic$ccode1, ccode)]
country_id2 = c(1:length(year))[match(dyadic$ccode2, ccode)]
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

source('function.R')
edget=apply(graphs,1,sum)


# 找到全为0的列
zero_cols = which(apply(adj_aver, 2, function(col) all(col <= 0.000001)))

# non_zero_cols 是所有非零列
non_zero_cols = setdiff(1:dim(X)[2], zero_cols)

# Non zero columns 对应的 country
non_zero_country = country[non_zero_cols]

# 对非零列进行聚类
 = graphs[,-zero_cols,-zero_cols]
X=graphs[81:145,,] ##choose the data from 1950-2014
n=dim(X)[1]
p=dim(X)[2]
time=year[81:145]
##find change point
q=2 ## cluster个数
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

