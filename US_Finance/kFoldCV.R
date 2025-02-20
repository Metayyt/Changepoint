library(RcppEigen)
######################## k-fold cross validation to select h_tau and h_d ########
CVbandwidth <- function(bw.seq = NULL, Ly, Lt, zeta, npoly, nder, optns, NbGrid, refined, kFolds = 5){
  
  dataType = optns$dataType
  # If 'Ly' and 'Lt' are vectors "cheat" and break them in a list of 10 elements
  if ( is.vector(Ly) && is.vector(Lt) && !is.list(Lt) && !is.list(Ly) ){
    if (length(Lt) < 21) {
      stop("You are trying to use a local linear weight smoother in a vector with less than 21 values.\n")
    }
    myPartition =   c(1:10, sample(10, length(t)-10, replace=TRUE));
    Ly = split(Ly, myPartition)
    Lt = split(Lt, myPartition)
    dataType = 'Sparse';
  } 
  
  kernel = optns$kernel
  
  # Make everything into vectors
  ncohort = length(Lt);
  n=length(Lt[1])
  tt  = unlist(Lt);
  yy  = unlist(Ly);
  ind = unlist(lapply( 1:ncohort, function(j) rep(j, times=length(Lt[[j]]))));
  yyn = yy[order(tt)];
  ind = ind[order(tt)];
  ttn = sort(tt);
  
  
  
  if (is.null(bw.seq)) {
    
    # Get minimum reasonable bandwidth
    a0=ttn[1];
    b0=ttn[length(ttn)];
    rang = b0-a0;
    m_max = max(sapply(t, length))
    dstar = fdapace:::Minb(tt, npoly + 2 + m_max/ncohort); #
    ## data driven bandwidth selection, dense case intend to have smaller minimal bandwidth
    if (dataType != 'Dense'){
      h0 = 2.5*dstar;
    } else {
      h0 = dstar;
    }
    if (h0 > rang/4){
      h0 = h0*.75;
      warning(sprintf("Warning: the min bandwith choice is too big, reduce to %f !", (h0)  ))
    }    
    
    # Get the candidate bandwidths
    nbw = 7;
    bw = rep(0,nbw-1);
    for (i in 1:(nbw-1)){
      bw[i]=2.5*rang/n*(n/5)^((i-1)/(nbw-1)); 
    }
    bw.seq = bw-min(bw)+h0;
    
    
  } else {
    
    nbw = length(bw.seq)
  }
  
  
  
  cv = array(Inf, dim = c(length(bw.seq), length(bw.seq), kFolds));
  #count = c();
  theFolds =  fdapace:::CreateFolds(unique(ind), k= kFolds)
  # theFolds =  caret::createFolds(unique(ind), k= kFolds)
  
  
  for (j in 1:(nbw-1)){
    
    # for (k in 1:(nbw-1))
    for (k in (j):(nbw-1)) {
      
      cat('CV procedure: h_tau = ', bw.seq[j], ';', 'h_d = ', bw.seq[k], '\n')
      
      for (i in 1:kFolds){
        
        xout= ttn[ ind %in% theFolds[[i]]];
        obs = yyn[ ind %in% theFolds[[i]]];
        xin = ttn[!ind %in% theFolds[[i]]];
        yin = yyn[!ind %in% theFolds[[i]]];
        
        win=rep(1/length(yin),length(yin));
        print(bw.seq)
        
        muout = tryCatch(
          MeanBreaksFP(xin = xin, yin= yin, weight = win, xout = xout, NbGrid = NbGrid,
                       h_tau = bw.seq[j], h_d = bw.seq[k], zeta = zeta, 
                       npoly=npoly, nder= nder, kernel = kernel, refined = refined)$muout, 
          error=function(err) {
            print(err)
            warning('Invalid bandwidth during CV. Try enlarging the window size.')
            return(Inf)
          })
        print(muout)
        cv[j,k,i] = sum((obs - muout)^2)
        # cv[j,k,i] = trapzRcpp(xout, (obs - muout)^2)
        # print(cv)
        if(is.na(cv[j,k,i])){
          cv[j,k,i] = Inf;
        }
      }
    }
  }
  #cv = cv[(count/ncohort>0.90)];
  #bw = bw[(count/ncohort>0.90)];
  if(min(cv) == Inf){
    stop("All bandwidths resulted in infinite CV costs.")
  }
  
  cvMean = apply(cv, c(1,2), mean)
  cvMeanid = which(cvMean == min(cvMean), arr.ind=TRUE)
  if(optns$useBW1SE){
    cvMeanid = which(cvMean < min(cvMean) + 
                       apply(cv, c(1,2), sd)[cvMeanid]/sqrt(kFolds), arr.ind=TRUE)
    cvMeanid = cvMeanid[which.max(cvMeanid[,1]),]
  }
  bopt = bw.seq[cvMeanid];
  names(bopt) = c('h_tau', 'h_d')
  
  return(bopt)
  
}


## cross validation for one-side kernel mean estimate
CVLwls1D_lr <- function(bw.seq = NULL, y, t, kernel, npoly, nder, dataType, kFolds, useBW1SE = FALSE ){
  

  # If 'y' and 't' are vectors "cheat" and break them in a list of 10 elements
  if ( is.vector(y) && is.vector(t) && !is.list(t) && !is.list(y) ){
    if (length(t) < 21) {
      stop("You are trying to use a local linear weight smoother in a vector with less than 21 values.\n")
    }
    myPartition =   c(1:10, sample(10, length(t)-10, replace=TRUE));
    y = split(y, myPartition)
    t = split(t, myPartition)
    dataType = 'Sparse';
  } 
  
  # Make everything into vectors
  ncohort = length(t);

  n=length(t)
  tt  = unlist(t);
  yy  = unlist(y);
  ind = unlist(lapply( 1:ncohort, function(j) rep(j, times=length(t[[j]]))));
  yyn = yy[order(tt)];
  ind = ind[order(tt)];
  ttn = sort(tt);
  
  
  
  if (is.null(bw.seq)) {
    
    # Get minimum reasonable bandwidth
    a0=ttn[1];
    b0=ttn[length(ttn)];
    rang = b0-a0;
    m_max = max(sapply(t, length))
    dstar = fdapace:::Minb(tt, npoly + m_max/ncohort); # 
    ## data driven bandwidth selection, dense case intend to have smaller minimal bandwidth
    if (dataType != 'Dense'){
      h0 = 2.5*dstar;
    } else {
      h0 = dstar;
    }
    if (h0 > rang/4){
      h0 = h0*.75;
      warning(sprintf("Warning: the min bandwith choice is too big, reduce to %f !", (h0)  ))
    }    
    
    # Get the candidate bandwidths
    nbw = 7;
    # bw = seq(0.075, 0.125, 0.01)
    bw = rep(0,nbw-1);
    # m_max = length(unique(t));
    for (i in 1:(nbw-1)){
      bw[i]=2.5*rang/n*(n/5)^((i-1)/(nbw-1)); # Straight from MATLAB
    }
    bw.seq = bw-min(bw)+h0;
    
    ###
    # a =  max(diff(sort(ttn)))
    # bw.min <- quantile(a, 0.05)
    # bw.max <- diff(range(ttn))*0.25
    # nbw = 10
    # bw.seq <- seq(bw.min, bw.max, length.out= nbw) 
    
  } else {
    
    nbw = length(bw.seq)
  }
  
  cv = matrix(Inf, ncol = length(bw.seq), nrow = kFolds);
  #count = c();
  theFolds =  fdapace:::CreateFolds(unique(ind), k= kFolds)
  # theFolds =  caret::createFolds(unique(ind), k= kFolds)
  
  
  for (j in 1:(nbw-1)){
    # cv[j]=0;
    # count[j]=0;
    #for (i in 1:ncohort){
    for (i in 1:kFolds){
      
      xout= ttn[ ind %in% theFolds[[i]]];
      obs = yyn[ ind %in% theFolds[[i]]];
      xin = ttn[!ind %in% theFolds[[i]]];
      yin = yyn[!ind %in% theFolds[[i]]];
      
      win=rep(1,length(yin));
      if (length(xout) == 0) {
        xout <- seq(min(xin, na.rm = TRUE), max(xin, na.rm = TRUE), length.out = 100)
      }
      
      muLR = tryCatch(
        
        CPPlwls1d_LR(bw = bw.seq[j], kernel_type = kernel, npoly=npoly, nder= nder,
                     xin = xin, yin= yin, xout=xout, win = win),
        error=function(err) {
          warning('Invalid bandwidth during CV. Try enlarging the window size.')
          return(Inf)
        })
      
      # cv[i,j] =  sum((obs-muLR[,2])^2)
      cv[i,j] =  min(sum((obs-muLR[,1])^2), sum((obs-muLR[,2])^2))
      # print(cv)
      if(is.na(cv[i,j])){
        cv[i,j] = Inf;
      }
      
      
    }
  }
  
  if(min(cv) == Inf){
    stop("All bandwidths resulted in infinite CV costs.")
  }
  if( useBW1SE ){
    # This will pick the bandwidth that is the max but it's average cost is at most
    # 1 standard error of the minimum cost /  I use means so it is more straighforward what the SE is.
    bopt = bw.seq[max(which(
      colMeans(cv) < min(colMeans(cv)) + apply(cv,2, sd)[which.min(colMeans(cv))]/sqrt(kFolds)))]
  } else {
    bopt = bw.seq[which.min( colMeans(cv))];
  }
  # bopt = bw[which.min( colMeans(cv))];
  return(bopt)
}



##### leave-one-out cross validation for individual curve ########

cv.score <- function(bw, zeta, t, y, npoly=1, nder = 0, NbGrid, kernel){
  
  n = length(t);
  if(is.vector(y) && is.vector(t) && !is.list(t) && !is.list(y)){
    
    y.hat <- MeanBreaksFP(xin = t, yin= y, weight = rep(1/length(t), length(t)), xout = t,
                          h_tau = bw, h_d = bw, zeta = zeta, NbGrid = NbGrid,
                          npoly = npoly, nder = nder, kernel = kernel)$muout
    mu.hat <- mean(y.hat)
    residuals <- (n/(n-1)) * mu.hat - y.hat / (n-1) - y
    
    
  } else{
    
    # h_tau = bw[1]
    # h_d = bw[2] 
    y.hat = vector("list", length = n)
    for (i in 1:n) {
      
      xin = unlist(t[-i]);
      yin = unlist(y[-i])[order(xin)];
      xin = sort(xin);
      win = rep(1/length(xin), length(xin))
      
      y.hat[[i]] <- MeanBreaksFP(xin = xin, yin= yin, weight = win, xout = t[[i]],
                            h_tau = bw[1], h_d = bw[2], zeta = zeta, NbGrid = NbGrid,
                            npoly = npoly, nder = nder, kernel = kernel)$muout
      
      
    }
    
    residuals = unlist(Map("-", y,  y.hat))
    
  }
  
  
  mean(residuals^2)
 

}

cv.select <- function(bw.seq = NULL, zeta, t, y, npoly=1, nder = 0, NbGrid, kernel) { 		
  
  
  
  if(is.vector(y) && is.vector(t) && !is.list(t) && !is.list(y)){
    
    rangex   <- diff(range(t))
    meshx    <- max(diff(sort(t)))
    interval <- c(ifelse(npoly<2, meshx/2, meshx), rangex/2)
    
    bopt <- optimize(cv.score, interval, t = t, y = y, zeta = zeta, NbGrid = NbGrid,
                     npoly = npoly, nder = nder, kernel = kernel)$minimum 
    
    
  } else {
    
    
    # ini = seq(0.015, 0.05, length.out = 3)
    # re <- sapply(ini, function(h){
    #    res <- optim(par = c(h, 0.05), fn = cv.score, gr =  NULL, method = "L-BFGS-B",
    #                lower = rep(0.01, 2), upper = rep(0.12, 2), t = t, y = y,
    #                zeta = zeta, npoly = npoly, nder = nder, kernel = kernel)
    #    c(res$par, res$value)
    # 
    # })
    # bopt = re[1:2, which.min(re[3,])]
    # names(bopt) = c('h_tau', 'h_d')
    
    if (is.null(bw.seq)) {
  
      tt  = unlist(t);
      yy  = unlist(y);
      yyn = yy[order(tt)];
      ttn = sort(tt);
      
      a =  max(diff(sort(ttn)))
      bw.min <- quantile(a, 0.05)
      bw.max <- diff(range(ttn))*0.25
      nbw = 6
      bw.seq <- seq(bw.min, bw.max, length.out= nbw)
      
    } else {
      nbw = length(bw.seq)
    }
    
    
    cv = matrix(Inf, length(bw.seq), length(bw.seq));
    #count = c();
    
    for (j in 1:(nbw-1)){
      
      # for (k in 1:(nbw-1))
      for (k in j:(nbw-1)) {
        
        cat('CV procedure: h_tau = ', bw.seq[j], ';', 'h_d = ', bw.seq[k], '\n')
        bw = c(bw.seq[j], bw.seq[k])
        cv[j,k] = cv.score(bw = bw, zeta = zeta, t = t, y = y, NbGrid = NbGrid,
                           npoly = npoly, nder = nder, kernel = kernel)
        
      }
    }
    #cv = cv[(count/ncohort>0.90)];
    #bw = bw[(count/ncohort>0.90)];
    if(min(cv) == Inf){
      stop("All bandwidths resulted in infinite CV costs.")
    }
    
    bopt = bw.seq[which(cv == min(cv), arr.ind=TRUE)];
    names(bopt) = c('h_tau', 'h_d')
    
  }
    
  
  return(bopt)
  
}





