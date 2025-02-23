

X=graphs[81:(80+cid +4),,]
X=graphs[(80+cid +5):145,,]
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

q=3
#cluster
W1=ahat
W2=1-bhat
W1=W1-diag(diag(W1))
W2=W2-diag(diag(W2))
D1=apply(W1,1,sum)
D2=apply(W2,1,sum)
L1=diag(1/sqrt(D1+.000001))%*%W1%*%diag(1/sqrt(D1+.000001))
L2=diag(1/sqrt(D2+.000001))%*%W2%*%diag(1/sqrt(D2+.000001))
ev <- eigen(L1 + L2)  

eigenvalues <- abs(ev$values)

sorted_indices = order(eigenvalues, decreasing = TRUE)

plot(eigenvalues[sorted_indices], type = "b", pch = 19, xlab = "Index", ylab = "Absolute Eigenvalue", 
    main = "Scree Plot of Absolute Eigenvalues", col = "blue")

grid()


ev <- eigen(L1 + L2) 

eigenvalues <- ev$values

sorted_indices = order(eigenvalues, decreasing = TRUE)

plot(eigenvalues[sorted_indices], type = "b", pch = 19, xlab = "Index", ylab = "Eigenvalue", 
     main = "After 1991 Euro Eigenvalues", col = "blue")

grid()
