data(avwreturn)
data_SS = avwreturn$data_SS
Ly = avwreturn$Ly
Lt = avwreturn$Lt

## global test
bw.seq = seq(0.01, 0.21, by = 0.01)
reject = rep(NA, length(bw.seq))
## reject H0: no jump points
for (i in 1:length(bw.seq)) {
  reject[i] = MDTest(bw = bw.seq[i], alpha = 0.05, nRegGrid = 201, Lt = Lt, Ly = Ly, Wtype = 'MIX')
  cat('reject:', reject[i], 'for h = ', bw.seq[i], '\n')
}
###
res = FPMD(Ly = Ly, Lt = Lt, wi = NULL, Wtype = "MIX", zeta = 1,
           bw.seq = NULL, NbGrid = 101, kFolds = 5, refined = TRUE,
           individualCurveJump = TRUE, nRegGrid = 301,
           npoly = 1, nder = 0, alpha = 0.05, cutoff = max, M_max = 15)
## with change points
mu_jumptime <- res$mu_jumptime
mu_jumpsize <- res$mu_jumpsize
# mu_jumpsize <- res$mu_jumpsize_h_tau
h_tau <- res$h_tau
h_d <- res$h_d
zeta <- res$zeta
wi <- res$wi
mu <- res$mu
muWork <- res$muWork
obsGrid <- res$obsGrid
workGrid <- res$workGrid
##
par(mfrow=c(1,1))
matplot(data_SS[,-1], type = "p", pch = 1, xlab = "Dates",
        ylab = "Average Value Weighted Returns",
        main = '',
        col = rgb(0.7,0.7,0.7,0.4), xaxt="n")
ind_x = seq(1, nrow(data_SS), length.out = 18)
axis(1, at=ind_x, labels=data_SS[ind_x, 1])
abline( v = round(mu_jumptime *nrow(data_SS)), lty = 2)
lines((obsGrid*nrow(data_SS)), mu, col = 4)
data_SS$Date[round(mu_jumptime*nrow(data_SS))]
