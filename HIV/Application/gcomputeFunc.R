
###inter.mod1-prediction model for the first intermediate outcome CD4 counts
###final.mod-prediction model for the final outcome of interest
###baselineVar-baseline covariates
###numCarlo-number of monte carlo simulations
###treatmentSequence denote the potential treatment regimes (00, 01, 10, 11)

gcomputeFunc=function(inter.mod1, final.mod, data, baselineVar, numCarlo=2000, treatSeq=c(0,0) ){
  
  ### generate L0 from empirical distribution of L0 obtained from the data
  baselineSim=NULL
  for(ind in 1:length(baselineVar)){
    draw_g=NULL
    temp_g=rnorm(numCarlo, mean=mean(data[, baselineVar[ind]]), sd=sd(data[,baselineVar[ind]]))
    temp_g[which(temp_g < 0)] = 0
    draw_g=cbind(draw_g, temp_g )
    colnames(draw_g)=baselineVar[ind]
    baselineSim=cbind(baselineSim, draw_g)
  }
  baselineSim=data.frame(baselineSim, A1=rep(treatSeq[1], numCarlo))

  ############# CD4 count after the first treatment   
  LEU3N2_mean=predict(inter.mod1, newdata = baselineSim)
  temp_g=NULL
  temp_g=rnorm(numCarlo, mean=LEU3N2_mean, sd=summary(inter.mod1)$sigma)
  temp_g[which(temp_g < 0)] = 0
  baselineSim=data.frame(baselineSim, LEU3N2=temp_g )
  baselineSim=data.frame(baselineSim, A2=rep(treatSeq[2], numCarlo))
  
  ###### generate the final outcome of interest Y, CD4 count after the second treatment
  Y_mean=predict(final.mod, newdata = baselineSim) 
  Y_g=rnorm(numCarlo, mean=Y_mean, sd=summary(final.mod)$sigma) 
  
  baselineSim=data.frame(baselineSim, LEU3N3=Y_g)
  
  return( baselineSim[, c("A1", "A2", "LEU3N3")] )
  
  
}




