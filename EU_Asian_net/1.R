library(NbClust)

ev = eigen(L1 + L2)
sorted_indices = order(abs(ev$values), decreasing = TRUE)
gamma = ev$vectors[, sorted_indices[1:nrow(L1)]]

W = L1 + L2
ev = eigen(W)
distance_matrix <- 1 / (W + 1e-10)


dist_matrix <- dist(W) 
result <- NbClust(data = dist_matrix, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 5, method = "ward.D2")
library(cluster)
gap_stat <- clusGap(W, FUN = kmeans, K.max = 10, B = 50)
plot(gap_stat, main = "Gap Statistic for Optimal Clusters")


threshold = 1e-5
significant_indices = which(abs(ev$values) > threshold)

significant_values = ev$values[significant_indices]
significant_vectors = ev$vectors[, significant_indices]

W_approx = significant_vectors %*% diag(significant_values) %*% t(significant_vectors)

ev_approx = eigen(W_approx)

q=1
sorted_indices = order(abs(ev_approx$values), decreasing = TRUE)
gamma = ev_approx$vectors[, sorted_indices[1:q]]

nb = NbClust(gamma, distance = "euclidean", min.nc = 2, max.nc = 10, method = "ward.D2")

print(nb$Best.nc)

barplot(table(nb$Best.nc[1,]), xlab = "Number of Clusters", ylab = "Number of Criteria", main = "Best number of clusters based on NbClust")
