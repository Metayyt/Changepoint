plot.fmb <- function(res){
  ## confidence band
  cbandMu <- FPMD:::pwCBFun(res)
  workGrid <- res$workGrid
  muWork <- res$muWork
  rho = res$rho
  tau_est = c(0, res$mu_jumptime, 1)
  mudata = t(rbind(cbandMu, muWork))
  ###
  bandGrid = t(rbind(cbandMu, workGrid))
  lband = lapply(1:(length(tau_est) - 1), function(i)
    as.data.frame(bandGrid[workGrid < (tau_est[i + 1] - rho) &
                             workGrid >= (tau_est[i] + rho),]))
  par(mfrow = c(2, 2))
  ind_x = seq(1, nrow(data_SS), length.out = 18)
  ## 1
  plot(x = NULL, y = NULL, ylim = c(-3, 3), xlim = c(0.03, 0.275),
       xlab = "Dates",
       ylab = "Average Value Weighted Returns",
       main = '',
       col = rgb(0.7,0.7,0.7,0.4), xaxt="n")
  axis(1, at= (ind_x/nrow(data_SS))[1:6], labels=data_SS[ind_x[1:6], 1])
  lapply(lband[1:4], function(x) polygon(c(x$workGrid, rev(x$workGrid)),c(x$lwCI,rev(x$upCI)),
                                         col = rgb(0.7,0.7,0.7,0.4) , border = NA))
  for (i in 1:4) {
    matplot(workGrid[workGrid < (tau_est[i+1]- rho) & workGrid >= (tau_est[i]+ rho) ],
            mudata[workGrid < (tau_est[i+1] - rho) & workGrid >= (tau_est[i]+rho), ],
            type = "l", add = TRUE, lty = c(3, 3, 1), col = c(4,4,2), lwd = 2)
  }
  legend('top', legend = c( 'Estimated mean', 'Pointwise confidence interval'),
         cex = 1, lwd = 2, col = c(2,4), lty = c(1,3), bty = "n")
  points(x = res$mu_jumptime[1:4], y = rep(-3.2, length(res$mu_jumptime[1:4])),
         pch= rep("*", length(res$mu_jumptime[1:4])), cex = 2, col = 4, xpd = TRUE)
  # lines(res$obsGrid, res$mu, col = 4)
  text(x = res$mu_jumptime[1:4], y = rep(-3.2, length(res$mu_jumptime[1:4])),
       labels = data_SS$Date[round(res$mu_jumptime[1:4]*nrow(data_SS))],
       xpd = TRUE, pos = 3, cex = 0.8, col = 4)
  ## 2
  plot(x = NULL, y = NULL, ylim = c(-1.5, 1.5), xlim = c(0.28, 0.53),
       xlab = "Dates",
       ylab = "Average Value Weighted Returns",
       main = '',
       col = rgb(0.7,0.7,0.7,0.4), xaxt="n")
  axis(1, at= (ind_x/nrow(data_SS))[5:9], labels=data_SS[ind_x[5:9], 1])
  ###
  lapply(lband[5:7], function(x) polygon(c(x$workGrid, rev(x$workGrid)),c(x$lwCI,rev(x$upCI)),
                                         col = rgb(0.7,0.7,0.7,0.4) , border = NA))
  for (i in 5:7) {
    matplot(workGrid[workGrid < (tau_est[i+1]- rho) & workGrid >= (tau_est[i]+ rho) ],
            mudata[workGrid < (tau_est[i+1] - rho) & workGrid >= (tau_est[i]+rho), ],
            type = "l", add = TRUE, lty = c(3, 3, 1), col = c(4,4,2), lwd = 2)
  }
  legend('top', legend = c( 'Estimated mean', 'Pointwise confidence interval'),
         cex = 1, lwd = 2, col = c(2,4), lty = c(1,3), bty = "n")
  points(x = res$mu_jumptime[4:7], y = rep(-1.6, length(res$mu_jumptime[4:7])),
         pch= rep("*", length(res$mu_jumptime[4:7])), cex = 2, col = 4, xpd = TRUE)
  # lines(res$obsGrid, res$mu, col = 4)
  text(x = res$mu_jumptime[4:7], y = rep(-1.6, length(res$mu_jumptime[4:7])),
       labels = data_SS$Date[round(res$mu_jumptime[4:7]*nrow(data_SS))],
       xpd = TRUE, pos = 3, cex = 0.8, col = 4)
  ## 3
  plot(x = NULL, y = NULL, ylim = c(-1.5, 1.5), xlim = c(0.535, 0.75),
       xlab = "Dates",
       ylab = "Average Value Weighted Returns",
       main = '',
       col = rgb(0.7,0.7,0.7,0.4), xaxt="n")
  axis(1, at= (ind_x/nrow(data_SS))[9:14], labels=data_SS[ind_x[9:14], 1])
  ###
  ###
  lapply(lband[8:10], function(x) polygon(c(x$workGrid, rev(x$workGrid)),c(x$lwCI,rev(x$upCI)),
                                          col = rgb(0.7,0.7,0.7,0.4) , border = NA))
  for (i in 8:10) {
    matplot(workGrid[workGrid < (tau_est[i+1]- rho) & workGrid >= (tau_est[i]+ rho) ],
            mudata[workGrid < (tau_est[i+1] - rho) & workGrid >= (tau_est[i]+rho), ],
            type = "l", add = TRUE, lty = c(3, 3, 1), col = c(4,4,2), lwd = 2)
  }
  legend('top', legend = c( 'Estimated mean', 'Pointwise confidence interval'),
         cex = 1, lwd = 2, col = c(2,4), lty = c(1,3), bty = "n")
  points(x = res$mu_jumptime[7:10], y = rep(-1.6, length(res$mu_jumptime[7:10])),
         pch= rep("*", length(res$mu_jumptime[7:10])), cex = 2, col = 4, xpd = TRUE)
  # lines(res$obsGrid, res$mu, col = 4)
  text(x = res$mu_jumptime[7:10], y = rep(-1.6, length(res$mu_jumptime[7:10])),
       labels = data_SS$Date[round(res$mu_jumptime[7:10]*nrow(data_SS))],
       xpd = TRUE, pos = 3, cex = 0.7, col = 4)
  ## 4
  plot(x = NULL, y = NULL, ylim = c(-5, 5), xlim = c(0.75, 0.98),
       xlab = "Dates",
       ylab = "Average Value Weighted Returns",
       main = '',
       col = rgb(0.7,0.7,0.7,0.4), xaxt="n")
  axis(1, at= (ind_x/nrow(data_SS))[14:18], labels=data_SS[ind_x[14:18], 1])
  lapply(lband[11:17], function(x) polygon(c(x$workGrid, rev(x$workGrid)),c(x$lwCI,rev(x$upCI)),
                                           col = rgb(0.7,0.7,0.7,0.4) , border = NA))
  for (i in 11:14) {
    matplot(workGrid[workGrid < (tau_est[i+1]- rho) & workGrid >= (tau_est[i]+ rho) ],
            mudata[workGrid < (tau_est[i+1] - rho) & workGrid >= (tau_est[i]+rho), ],
            type = "l", add = TRUE, lty = c(3, 3, 1), col = c(4,4,2), lwd = 2)
  }
  legend('topleft', legend = c( 'Estimated mean', 'Pointwise confidence interval'),
         cex = 1, lwd = 2, col = c(2,4), lty = c(1,3), bty = "n")
  points(x = res$mu_jumptime[10:13], y = rep(-5.3, length(res$mu_jumptime[10:13])),
         pch= rep("*", length(res$mu_jumptime[10:13])), cex = 2, col = 4, xpd = TRUE)
  # lines(res$obsGrid, res$mu, col = 4)
  text(x = res$mu_jumptime[10:13], y = rep(-5.3, length(res$mu_jumptime[10:13])),
       labels = data_SS$Date[round(res$mu_jumptime[10:13]*nrow(data_SS))],
       xpd = TRUE, pos = 3, cex = 0.8, col = 4)
}
plot.fmb(res)