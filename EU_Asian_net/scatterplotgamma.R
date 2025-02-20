# add country as the row name of gamma
rownames(gamma) = non_zero_country
# plot(gamma) and add the row name aside by the dot
data <- as.data.frame(gamma)
par(mar = c(5, 5, 5, 5))  

plot(data[,1], data[,2], xlab = "PC1", ylab = "PC2", xlim = c(-0.6, 0), ylim = c(-0.3, 0.3), pch = 19, col = "red")

text(data[,1], data[,2], labels = rownames(data), pos = 3, offset = 0.5, cex = 0.8, col = "blue")
