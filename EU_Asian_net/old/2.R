library(NbClust)

ev = eigen(L1 + L2)

q=2
sorted_indices = order(abs(ev$values), decreasing = TRUE)
gamma = ev_approx$vectors[, sorted_indices[1:q]]

nb = NbClust(gamma, distance = "euclidean", min.nc = 2, max.nc = 10, method = "average")

print(nb$Best.nc)

barplot(table(nb$Best.nc[1,]), xlab = "Number of Clusters", ylab = "Number of Criteria", main = "Best number of clusters based on NbClust")
