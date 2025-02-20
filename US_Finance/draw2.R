## individual, Xia and Qiu 2016
#### for each curve 1,5,6,16,35, 36
industry = colnames(data_SS)[-50]
par(mfrow=c(2,2))
for (i in c(5,16,35, 36)) {
  plot(data_SS[, i+1],
        xlab = "Dates",
        ylab = "Average Value Weighted Returns",
        main = industry[i+1],
        col = 'gray', #rgb(0.7,0.7,0.7,0.4),
        xaxt="n",
        ylim = c(-6,6))
  ind_x = seq(1, nrow(data_SS), length.out = 10)
  axis(1, at= ind_x, labels = data_SS[ind_x, 1])
  ## indivudual
  lines(y = res$indJump[[i]]$mu, x = Lt[[i]]*nrow(data_SS),
        col = "blue", lty = 1, lwd = 1.5)
  points(x = res$indJump[[i]]$mu_jumptime*nrow(data_SS),
         y = rep(-6.4, length(res$indJump[[i]]$mu_jumptime)),
         pch= rep("*", length(res$indJump[[i]]$mu_jumptime)),
         cex = 2, col = "blue", xpd = TRUE)
  text(x = res$indJump[[i]]$mu_jumptime*nrow(data_SS),
       y = rep(-6.4, length(res$indJump[[i]]$mu_jumptime)),
       labels = data_SS$Date[res$indJump[[i]]$mu_jumptime*nrow(data_SS)],
       xpd = TRUE, pos = 3, cex = 0.5, col = "blue")
  ## our mean
  lines(obsGrid*nrow(data_SS), mu, col = "red", lty = 1, lwd = 1.5)
  abline( v = round(mu_jumptime *nrow(data_SS)), col = "red", lty = "dashed")
}
par(mfrow=c(1,1))
## End(Not run)