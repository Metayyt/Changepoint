three_visit_data <- as.matrix(three_visit_data)  
#xind <- 2:11
#zind <- 2:9
X <- three_visit_data[, xind]  
Z <- three_visit_data[, zind]  
threshold <- full_out[["train.res"]][["threshold"]] 
theta <- full_out[["train.res"]][["theta"]]  

n <- nrow(X)
h <- n^(-1) * log(n)  

l <- 0    

X1 <- cbind(1, X)  #
n <- nrow(X1)
d <- ncol(X1)


XX <- X1 [,which(full_out[["train.res"]][["coefficient"]][["beta"]]!=0)]
for (i in seq_along(threshold)) {

  smooth_features <- t(sapply(1:n, function(j, X1, Z, theta, a, h) {

    X1[j, ] * pnorm(drop(c(-1, Z[j, ]) %*% c(a, theta)) / h)
  }, X1 = X1, Z = Z, theta = theta, a = threshold[i], h = h))
  
  colnames(smooth_features) <- paste0("smooth_", i, "_", colnames(smooth_features))

  XX <- cbind(XX, smooth_features[,which(full_out[["train.res"]][["coefficient"]][["delta"]][i,]!=0)])
}

colnames(XX)


# get y
y <- three_visit_data[, 1]

#features = XX[,c(2:8,12,14,15,17,18,22)]
#features = XX[,c(2:8,12,14,15,17,18,22)]
colnames(XX)

ful_ols_model <- lm(y ~ XX-1)
summary(ful_ols_model)


#cor(features)
