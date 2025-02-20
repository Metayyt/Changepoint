library(fdapace)
library(Rcpp)
library(RcppEigen)
sourceCpp("CPPlwls1d_LR.cpp")

FMbreaks <- function(Ly, Lt, wi = NULL, Wtype = c("OBS", "SUBJ", "MIX", "OPT"), 
                     zeta = NULL, Nzeta = 1, bw.seq = NULL, gamma = 1, 
                     npoly = 1, nder = 0, kFolds = 10, refined = FALSE, NbGrid = 101,
                     useBW1SE = FALSE, individualCurveJump = FALSE){
  
  # Check the data validity for further analysis
  CheckData(Ly,Lt)
  
  inputData <-  fdapace:::HandleNumericsAndNAN(Ly,Lt);
  # inputData <- HandleNumericsAndNAN(Ly,Lt);
  Ly <-  inputData$Ly;
  Lt <-  inputData$Lt;
  
  # Set the options structure members that are still NULL
  optns <- list(methodBwMu = 'CV',
               # methodBwCov = 'GCV',
               # kFoldMuCov = 5,
               nRegGrid = 51, 
               error = TRUE, 
               kernel='epan', 
               verbose=FALSE)
  optns <- SetOptions(Ly, Lt, optns);
  optns$useBW1SE = useBW1SE;
  # optns$dataType = 'Sparse'
  
  
  # Check the options validity for the PCA function. 
  numOfCurves = length(Ly);
  CheckOptions(Lt, optns, numOfCurves)
  
  ##
  obsGrid = sort(unique( c(unlist(Lt))));
  regGrid = seq(min(obsGrid), max(obsGrid),length.out = optns$nRegGrid);
  mi = sapply(Lt, length)
  Wtype = match.arg(Wtype)
  
  
  ## get a grid points if zeta
  if (is.null(zeta)) {
    
    # smcObj = fdapace:::GetSmoothedMeanCurve(Ly, Lt, obsGrid, regGrid, optns)
    # mu <- smcObj$mu
    # bwMu = smcObj$bw_mu
    
    ## or Mean with one-sided smooth
    ## CV bandwidth
    bwMu = CVLwls1D_lr(bw.seq = bw.seq, y = Ly, t = Lt, kernel= optns$kernel, dataType= optns$dataType,
                       npoly = npoly, nder = nder, kFolds = 5, useBW1SE = optns$useBW1SE);
    # print(bwMu)
    xin = unlist(Lt);
    yin = unlist(Ly)[order(xin)];
    xin = sort(xin);
    win = weightFun(h = bwMu, wi = wi, mi = mi, n = numOfCurves, Wtype = Wtype)   # rep(1, length(xin));
    ## two one-side estimate
    mu = CPPlwls1d_LR(bw = bwMu, kernel_type = optns$kernel, win = win$weight,
                      xin = xin, yin = yin, xout = obsGrid, npoly = npoly)[,2]
    
    ## Covariance function and sigma2
    # smooth cov and/or sigma2
    scsObj <- fdapace:::GetSmoothedCovarSurface(Ly, Lt, mu = mu, obsGrid, regGrid, optns)
    # scsObj <- apply(mu, 2, function(x)GetSmoothedCovarSurface(Ly, Lt, mu = x, obsGrid, regGrid, optns))
    # scsObj <- scsObj[[which.min(c(scsObj[[1]][['sigma2']], scsObj[[2]][['sigma2']]))]]
    sigma2 <- scsObj[['sigma2']]
    smoothedCov <- scsObj$smoothCov
    # workGrid: possibly truncated version of the regGrid
    workGrid <- scsObj$outGrid
    # convert mu to truncated workGrid
    muWork <- ConvertSupport(fromGrid = obsGrid, toGrid = workGrid, mu=mu)
    # Get the results for the eigen-analysis
    eigObj <- fdapace:::GetEigenAnalysisResults(smoothCov = scsObj$smoothCov, workGrid, optns, muWork = muWork)
    fittedCov <- eigObj$fittedCov
    
    
    ### epanechnikov kernel
    K_Epa = function(u) 1.5 * (1 - u^2) * (abs(u) <= 1);
    K_Epa_r = function(u, r) 1.5 * (1 - u^2) *u^r * (abs(u) <= 1);
    phi_fun <- function(u) {
      nu0 = integrate(K_Epa_r, 0, 1, r=0)$value
      nu1 = integrate(K_Epa_r, 0, 1, r=1)$value
      nu2 = integrate(K_Epa_r, 0, 1, r=2)$value
      res = K_Epa(u) * (nu2 - nu1*u)/(nu0*nu2 - nu1^2)
      return(res^2)
    }
    ## one-side kernel norm
    normK_star <- integrate(phi_fun, 0, 1)$value
    ## density f_T is uniform 
    df <- density(runif(10000), kernel = "rectangular")
    f.T <- approx(df$x,df$y,xout=regGrid)$y
    
    # if(max(mi) > numOfCurves)
    if (det(smoothedCov) < 1e-8) {
      Omega <- 2*((sum(mi*win$wi^2)/bwMu)*normK_star * (diag(fittedCov) + sigma2)/f.T +
                    sum(mi*(mi-1)*win$wi^2)*diag(fittedCov))
    } else {
      Omega <- 2*((sum(mi*win$wi^2)/bwMu)*normK_star * (diag(smoothedCov) + sigma2)/f.T +
                    sum(mi*(mi-1)*win$wi^2)*diag(smoothedCov))
    }
    
    ## cut threshold:
    alpha = 0.05
    # zeta = seq(max(stats::qnorm(1-alpha / 2) * sqrt(abs(Omega))),
    #            min(stats::qnorm(1-alpha / 2) * sqrt(abs(Omega))), length.out = Nzeta)
    
    zeta = max(stats::qnorm(1-alpha / 2) * sqrt(abs(Omega)))
    
  }
  
  Obj = Threselect(zeta = zeta, bw.seq = bw.seq, gamma = gamma, npoly = npoly, 
                   nder = nder, Ly = Ly, Lt = Lt, wi = wi, Wtype = Wtype, 
                   kFolds = kFolds, refined = refined, NbGrid = NbGrid, optns = optns)
  zeta = Obj$zeta
  weight = Obj$weight
  smcObj = Obj$smcObj
  scsObj = Obj$scsObj
  
  # workGrid: possibly truncated version of the regGrid
  workGrid <- scsObj$outGrid
  rho = smcObj$rho_d
  # cutloc = c(smcObj$h_tau, smcObj$mu_jumptime, 1-smcObj$h_tau)
  # cutloc = c(0, smcObj$mu_jumptime, 1)
  # cutGrid = lapply(1:(length(cutloc) - 1), function(k)
  #   workGrid[workGrid < (cutloc[k + 1] - rho) &
  #              workGrid >= (cutloc[k] + rho)])
  # workGrid = unlist(cutGrid)
  
  ## mu estimate on each smooth part of workGrid
  # xin = unlist(Lt)
  # yin = unlist(Ly)[order(xin)]
  # xin = sort(xin)
  # win = rep(wi, times = mi)
  # xincut = lapply(1:(length(cutloc) - 1), function(k)
  #   xin[xin < (cutloc[k + 1] - rho) & xin >= (cutloc[k] + rho)])
  # yincut = lapply(1:(length(cutloc) - 1), function(k)
  #   yin[xin < (cutloc[k + 1] - rho) & xin >= (cutloc[k] + rho)])
  # wincut = lapply(1:(length(cutloc) - 1), function(k)
  #   win[xin < (cutloc[k + 1] - rho) & xin >= (cutloc[k] + rho)])
  # 
  # muWork = mapply(function(t, x, y, w) {
  #   Lwls1D(bw = smcObj$h_tau, kernel_type = optns$kernel, npoly = npoly,
  #     nder = nder, xin = x, yin = y, xout = t, win = w)
  # }, cutGrid, xincut, yincut, wincut, SIMPLIFY = FALSE)
  # muWork = unlist(muWork)
  ## or convert mu to truncated workGrid
  muWork <- ConvertSupport(obsGrid, toGrid = workGrid, mu=smcObj$mu)
 
  ## cov on the WorkGrid
  # optns$userBwCov = smcObj$h_tau
  # optns$useBinnedCov = FALSE
  scsObj = fdapace:::GetSmoothedCovarSurface(y = Ly, t = Lt, mu = smcObj$mu, obsGrid = obsGrid, 
                                   regGrid = workGrid, optns = optns) 
  # Get the results for the eigen-analysis
  eigObj = fdapace:::GetEigenAnalysisResults(smoothCov = scsObj$smoothCov, workGrid, optns, muWork = muWork)
  fittedCov = eigObj$fittedCov
  
  
  ### individual curve estimate and jump detection
  LX = indJump = list() # vector("list", numOfCurves)
  if(individualCurveJump){
    
    ### predict the individual curve X_i
    Lmu = split(smcObj$mu, rep(1:numOfCurves, mi))
    Leps = mapply(function(y, x) y - x, y = Ly, x = Lmu, SIMPLIFY = FALSE)
    
    LU = mapply(function(t, y){
      bw = regCVBwSelC(x = t, y = y, deg = npoly, interval = c(0, 0.2))
      # print(bw)
      Lwls1D(bw = bw, kernel_type = optns$kernel, npoly = npoly,
             nder = nder, xin = t, yin= y, xout = t,
             win = rep(1, length(t)))
    }, t = Lt, y = Leps, SIMPLIFY = FALSE)
    LX = mapply(function(x,y)x+y, Lmu, LU, SIMPLIFY = FALSE)
    
    names(indJump) = paste('Curve', 1:numOfCurves)
    for (i in 1:numOfCurves) {
      indJump[[i]] = indMeanbreak(y = Ly[[i]], t = Lt[[i]], zeta = NULL, 
                                  gamma = gamma, kernel = optns$kernel, npoly = npoly,
                                  nder = nder)
    }

  }
  
  
  ##
  ret <- list(sigma2 = scsObj$sigma2, 
              obsGrid = obsGrid, 
              workGrid = workGrid, 
              muWork = muWork,
              mu = smcObj$mu, 
              smoothedCov = scsObj$smoothCov, 
              fittedCov = eigObj$fittedCov, 
              optns = optns, 
              h_tau = smcObj$h_tau, 
              h_d = smcObj$h_d, 
              mu_jumptime = smcObj$mu_jumptime, 
              mu_jumpsize = smcObj$mu_jumpsize,
              mu_jumpsize_h_tau = smcObj$mu_jumpsize_h_tau,
              timings = Obj$timings,
              timings_jump = smcObj$timings,
              wi = weight$wi,
              mi = mi,
              zeta = zeta,
              rho = rho,
              LX = LX,
              indJump = indJump)
  
  return(ret)
  
  
}

## different weight
weightFun <- function(h, wi, mi, n, Wtype = c("OBS", "SUBJ", "MIX", "OPT")){
  
  Wtype = match.arg(Wtype)
  if (is.null(wi)) {
    
    if(Wtype == "OBS"){
      wi = rep(1/sum(mi), n)
    } else if(Wtype == "SUBJ"){
      wi = 1/(n*mi)
    } else if(Wtype == "OPT"){
      wi = (1/h + mi - 1)^(-1)/sum(mi/(1/h + mi - 1))
    } else if(Wtype == "MIX") {
      c1 <- ( 1/(h*mean(mi)) + mean(mi^2)/mean(mi)^2 )/n
      c2 <- (mean(1/mi)/h + 1)/n
      alp <- c2/(c1+c2)
      wi <- alp/sum(mi) + (1-alp)/(n*mi)
      
    }
    
  }
  
  weight = rep(wi, times = mi)

  
  
  return(list(weight = weight, wi = wi))
}

## select zeta by BIC 
Threselect <- function(zeta, bw.seq, gamma, npoly, nder, Ly, Lt, wi, Wtype, 
                       kFolds, refined, NbGrid, optns){
  
  firsttsthres <- Sys.time()
  
  kernel = optns$kernel
  ## 
  mi <- sapply(Lt, length)
  n <- length(Lt)
  xin = unlist(Lt);
  yin = unlist(Ly)[order(xin)];
  xin = sort(xin);
  # Generate basic grids:
  # obsGrid:  the unique sorted pooled time points of the sample and the new
  # data
  # regGrid: the grid of time points for which the smoothed covariance
  # surface assumes values
  # jumpRegGrid: the grid of time points for jump detection
  obsGrid = sort(unique( c(unlist(Lt))));
  regGrid = seq(min(obsGrid), max(obsGrid),length.out = optns$nRegGrid);
  
  
  
  smcObj = scsObj = vector("list", length = length(zeta))
  BIC = rep(0, length(zeta))
  
  for (i in 1:length(zeta)) {
    
    cat('zeta = ', zeta[i], '\n')
    
    firsttsCVmu <- Sys.time()
    ## mean function
    if(length(bw.seq) == 1) {
      h_tau = h_d = bw.seq
    } else {
      ## select by CV procedure
      if(kFolds == 1){
        bw = cv.select(bw.seq = bw.seq, zeta = zeta[i], y = Ly, t = Lt, NbGrid = NbGrid,  kernel = kernel,
                       npoly = npoly, nder = nder)
      } else{
        bw = CVbandwidth(bw.seq = bw.seq, Ly = Ly, Lt = Lt, zeta = zeta[i], refined = refined,
                         npoly = npoly, nder = nder, optns = optns, kFolds = kFolds, NbGrid = NbGrid)
      }
      
      h_tau = bw[1]
      h_d = bw[2]
    }
   
    cat('CV procedure selected: ', 'h_tau = ', h_tau, ';', 'h_d = ', h_d, ' for zeta = ', zeta[i],  '\n')
    # Get the mean function using the bandwith estimated above:
    # different type of weight
    win = weightFun(h = h_tau, wi = wi, mi = mi, n = n, Wtype = Wtype)
    # mu: the smoothed mean curve evaluated at times 'jumpGrid'
    # jumpGrid = seq(min(obsGrid), max(obsGrid),length.out = 1000)
    smcObj[[i]] = MeanBreaksFP(xin = xin, yin= yin, weight = win$weight, xout = regGrid,
                               h_tau = h_tau, h_d = h_d, zeta = zeta[i], refined = refined, 
                               npoly = npoly, nder = nder, kernel = kernel, NbGrid = NbGrid)
    mu = smcObj[[i]]$mu
    mu_jumpsize = smcObj[[i]]$mu_jumpsize 
    
    lasttsCVmu <- Sys.time()
    
    firsttsCov <- Sys.time() #First time-stamp for calculation of the covariance
    ## Covariance function and sigma2
    
    scsObj[[i]] = fdapace:::GetSmoothedCovarSurface(y = Ly, t = Lt, mu = mu, obsGrid = obsGrid, 
                                          regGrid = regGrid, optns = optns) 
    sigma2 <- scsObj[[i]][['sigma2']]
    lasttsCov <- Sys.time()
    
    ## BIC
    Pn = h_tau^2 + (log(n)*sum(mi*(mi-1+1/h_tau)*(win$wi)^2))^(1/2)
    # Pn = sqrt(log(n)*h_tau/n) # log(n)*h_tau/sqrt(n)
    
    # BIC[i] = log(sigma2) + ifelse(any(mu_jumpsize == 0), 0, sum(1/abs(mu_jumpsize)^gamma))* Pn
    BIC[i] = log(sigma2) + ifelse(any(mu_jumpsize == 0), 0, length(mu_jumpsize)) * Pn^gamma
    
  }
  zeta = zeta[which.min(BIC)]
  smcObj = smcObj[[which.min(BIC)]]
  scsObj = scsObj[[which.min(BIC)]]
  
  ##
  cat('Final CV procedure selected: ', 'h_tau = ', smcObj$h_tau, ';', 'h_d = ', smcObj$h_d, '\n')
  cat('and selected: ', 'zeta = ', zeta, '\n')
  cat('change point at:', smcObj$mu_jumptime, '\n')
  #
  timestamps = c(lasttsCVmu, lasttsCov, firsttsthres, firsttsCVmu, firsttsCov)
  
  
  if(is.null(timestamps)) {
    timings = NULL;
  } else {
    timestamps = c(Sys.time(), timestamps)
    timings = round(digits=3, timestamps[1:3]-timestamps[4:6]);
    names(timings) <- c('total', 'mu', 'cov')
  }
  
  
  return(list(zeta = zeta, mi = mi, weight = win, smcObj = smcObj, scsObj = scsObj, timings = timings));
}




## Individual Mean function
indMeanbreak <- function(y, t, zeta = NULL, gamma = 1, npoly, nder, kernel) {
  
  
  if (is.null(zeta)) {
    zeta <- seq(0.01, 1, 0.2)
  }
  
  n <- length(y)
  smcObj = list()
  BIC = sigma2 = rep(0, length(zeta))
  
  for (i in 1:length(BIC)) {
    
    cat('zeta = ', zeta[i], '\n')
    
    ## mean function
    
    bw = cv.select(zeta = zeta[i], t = t, y = y, kernel = kernel,
                   npoly = npoly, nder = nder)
    
    # Get the mean function using the bandwith estimated above:
    # mu: the smoothed mean curve evaluated at times 'obsGrid'
    # workGrid = seq(0,1, length.out = 101)
    smcObj[[i]] = MeanBreaksFP(xin = t, yin= y, weight = rep(1/length(t), length(t)), xout = t,
                               h_tau = bw, h_d = bw, zeta = zeta[i],  
                               npoly = npoly, nder = nder, kernel = kernel)
    muout = smcObj[[i]]$muout
    mu_jumpsize = smcObj[[i]]$mu_jumpsize # smcObj[[i]]$mu_jumpsize
    
    sigma2[i] = mean((y - muout)^2)
    
    ## BIC
    pn = ifelse( any(mu_jumpsize == 0), 0,  sum(1/abs(mu_jumpsize)^gamma)*(log(n)*bw)^(1/2)/n )
    BIC[i] = log(sigma2[i]) + pn
    
  }
  zeta = zeta[which.min(BIC)]
  smcObj = smcObj[[which.min(BIC)]]
  
  
  ret <- list(mu = smcObj$mu, 
              h_tau = smcObj$h_tau, 
              h_d = smcObj$h_d, 
              mu_jumptime = smcObj$mu_jumptime, 
              mu_jumpsize_h_tau = smcObj$mu_jumpsize_h_tau,
              mu_jumpsize = smcObj$mu_jumpsize,
              zeta = zeta,
              rho = smcObj$rho_d)
  
  
  return(list(zeta = zeta, smcObj = smcObj));
  
}


## global test for jump points
ncp.test <- function(bw, alpha = 0.05, Lt, Ly, 
                     Wtype = c("OBS", "SUBJ", "MIX", "OPT")){
  
  
  Wtype = match.arg(Wtype)
  optns <- list(methodBwMu = 'CV',
                nRegGrid = 51, 
                error = TRUE, 
                kernel='epan', 
                verbose=FALSE)
  optns <- SetOptions(Ly, Lt, optns);

  ## Mean with one-sided smooth
  mi = sapply(Lt, length)
  numOfCurves = length(Ly);
  ##
  xin = unlist(Lt);
  yin = unlist(Ly)[order(xin)];
  xin = sort(xin);
  win = weightFun(h = bw, wi = NULL, mi = mi, n = numOfCurves, Wtype = Wtype)   # rep(1, length(xin));
  ## two one-side estimate on a cut region
  obsGrid = sort(unique(xin));
  mu_lr = CPPlwls1d_LR(bw = bw, kernel_type = optns$kernel, win = win$weight,
                       xin = xin, yin = yin, xout = obsGrid, npoly = 1)
  ## choose the one with smaller residual mean sequare
  mu = mu_lr[,which.min(apply(mu_lr, 2, function(x) mean((yin - x)^2)))]
  ## Covariance function and sigma2
  # smooth cov and/or sigma2
  regGrid = seq(bw, 1 - bw,length.out = optns$nRegGrid);
  scsObj <- fdapace:::GetSmoothedCovarSurface(Ly, Lt, mu = mu, obsGrid, regGrid, optns)
  sigma2 <- scsObj[['sigma2']]
  smoothedCov <- scsObj$smoothCov
  # workGrid: possibly truncated version of the regGrid
  workGrid <- scsObj$outGrid
  # convert mu to truncated workGrid
  muWork <- ConvertSupport(fromGrid = obsGrid, toGrid = workGrid, mu=mu)
  # muWork = approx(obsGrid, mu, xout= workGrid)$y
  # Get the results for the eigen-analysis
  eigObj <- fdapace:::GetEigenAnalysisResults(smoothCov = scsObj$smoothCov, workGrid, optns, muWork = muWork)
  fittedCov <- eigObj$fittedCov
  
  
  ### epanechnikov kernel
  K_Epa = function(u) 0.75 * (1 - u^2) * (abs(u) <= 1);
  K_Epa_r = function(u, r) 0.75 * (1 - u^2) *u^r * (abs(u) <= 1);
  nu0 = integrate(K_Epa_r, 0, 1, r=0)$value
  nu1 = integrate(K_Epa_r, 0, 1, r=1)$value
  nu2 = integrate(K_Epa_r, 0, 1, r=2)$value
  phi_fun <- function(u) {
    res = K_Epa(u) * (nu2 - nu1*u)/(nu0*nu2 - nu1^2)
    return(res^2)
  }
  ## one-side kernel norm
  normK_star <- integrate(phi_fun, 0, 1)$value
  C_K = (K_Epa(1)^2*(nu2 - nu1)^2 + 2*nu2^2*K_Epa(0)^2)/(nu0*nu2 - nu1^2)
  B_K = (2*log(1/bw))^(1/2) + (2*log(1/bw))^(-1/2)*(log(log(1/bw))/2 + log(C_K/(2*sqrt(pi)*normK_star)))
  ## density f_T is uniform 
  df <- density(runif(10000), kernel = "rectangular")
  f.T <- approx(df$x, df$y, xout=regGrid)$y
  ###
  if (det(smoothedCov) < 1e-10) {
    Omega <- 2*((sum(mi*win$wi^2)/bw)*normK_star * (diag(fittedCov) + sigma2)/f.T +
                  sum(mi*(mi-1)*win$wi^2)*diag(fittedCov))
  } else {
    Omega <- 2*((sum(mi*win$wi^2)/bw)*normK_star * (diag(smoothedCov) + sigma2)/f.T +
                  sum(mi*(mi-1)*win$wi^2)*diag(smoothedCov))
  }
  ## test statistics
  mu_lr = CPPlwls1d_LR(bw = bw, kernel_type = optns$kernel, win = win$weight,
                       xin = xin, yin = yin, xout = workGrid, npoly = 1)
  mu_diff = mu_lr[,2]-mu_lr[,1]
  mu_diff_abs = abs(mu_diff)
  ##
  reject = ifelse(max(mu_diff_abs/sqrt(Omega)) > 
                    B_K - log(log((1-alpha)^(-1/2)))/(2*log(1/bw))^(1/2), 1 , 0)
  
  return(reject)
}
