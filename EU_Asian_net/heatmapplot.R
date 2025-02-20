library(ggplot2)
library(reshape2)
X=graphs[81:(80+cid +4),,]
X=graphs[(80+cid +5):145,,]
adj_a = X[1,,]
for(i in 2:(dim(X)[1])){
  adj_a = adj_a + X[i,,]
}
adj_aver = adj_a / (dim(X)[1])


zero_cols = which(apply(adj_aver, 2, function(col) all(col == 0)))

non_zero_cols = setdiff(1:dim(X)[2], zero_cols)

non_zero_country = country[non_zero_cols]

X_reduced = X[,-zero_cols,-zero_cols]
n_reduced = dim(X_reduced)[1]
p_reduced = dim(X_reduced)[2]

ahat=matrix(rep(0,p_reduced^2),nrow=p_reduced)
bhat=ahat
Xlead=X_reduced[2:n_reduced,,]
for(j in 1:p_reduced){
  for(k in j:p_reduced){
    ahat[j,k]=sum(Xlead[,j,k]*(1-X_reduced[1:(n_reduced-1),j,k]))/(sum(1-X_reduced[1:(n_reduced-1),j,k])+.0001)
    bhat[j,k]=sum((1-Xlead[,j,k])*X_reduced[1:(n_reduced-1),j,k])/(sum(X_reduced[1:(n_reduced-1),j,k])+.0001)
  }
}
ahat=(ahat+t(ahat))-diag(diag(ahat))
bhat=(bhat+t(bhat))-diag(diag(bhat))

q=1
# Cluster
W1=ahat
W2=1-bhat
W1=W1-diag(diag(W1))
W2=W2-diag(diag(W2))
D1=apply(W1,1,sum)
D2=apply(W2,1,sum)
L1=diag(1/sqrt(D1+.000001))%*%W1%*%diag(1/sqrt(D1+.000001))
L2=diag(1/sqrt(D2+.000001))%*%W2%*%diag(1/sqrt(D2+.000001))
ev = eigen( L1 + L2  )

# Sort indice by eigenvector
gamma=ev$vector[,1:q]
km<-kmeans(gamma,centers = q,nstart=100)
gamma_order = order(abs(gamma), decreasing = TRUE)
nuhat = gamma_order
cluster_1_size = length(non_zero_cols)
 
non_zero_order = non_zero_cols[nuhat]  
final_order = c(non_zero_order, zero_cols)

adj_aver_complete = adj_a / (dim(X)[1])

library(viridis)
library(RColorBrewer)
colors <- brewer.pal(9, "YlOrBr")
newa = melt(adj_aver_complete[final_order, final_order])
heatmap_plot <- ggplot(newa, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  #  scale_fill_gradientn(colors = colors) +
  scale_fill_gradient2(low = "lightblue", high = "red", mid = "orange", midpoint =  0.5, limit = c(0, 1), space = "Lab", name = "Links") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "")


heatmap_plot <- heatmap_plot +
  geom_segment(aes(x = cluster_1_size + 0.5, xend = cluster_1_size + 0.5, y = 0.5, yend = p + 0.5), linetype = "solid", color = "black", size = 1) +
  geom_segment(aes(x = 0.5, xend = p + 0.5, y = cluster_1_size + 0.5, yend = cluster_1_size + 0.5), linetype = "solid", color = "black", size = 1)

heatmap_plot <- heatmap_plot +
  geom_segment(aes(x = cluster_1_size + cluster_2_size + 0.5, xend = cluster_1_size + cluster_2_size + 0.5, y = 0.5, yend = p + 0.5), linetype = "solid", color = "black", size = 1) +
  geom_segment(aes(x = 0.5, xend = p + 0.5, y = cluster_1_size + cluster_2_size + 0.5, yend = cluster_1_size + cluster_2_size + 0.5), linetype = "solid", color = "black", size = 1)

print(heatmap_plot)

