rm(list=ls())
#### THIS produces Figure 11 in the appendix
### weight truncation in AIPTW and IPTW, restricting to overlap regions in PENCOMP

DIREC="codes/Application/Results/"### where results from allMethodRun.R are stored

#####After running allMethodRun.R, run the following script to reproduce Figure 10 in the main text.
###########load the results
tableAIPTW=read.table(paste0(DIREC, "tableAIPTW_truncate.txt"), header = T)
tableIPTW=read.table(paste0(DIREC, "tableIPTW_truncate.txt"), header = T)
tablePENCOMP=read.table(paste0(DIREC, "tablePENCOMP_truncate.txt"), header = T)
tableNaive=read.table(paste0(DIREC, "tableNaive.txt"), header = T)
tableGcompute=read.table(paste0(DIREC, "tableGcompute.txt"), header = T)

################AIPTW ###################################################################################
aiptw=tableAIPTW[which(!is.na(tableAIPTW[,1])), c(1, 2, 5, 6, 9, 10)] ###keep only the estimate and se of delta11 delta10, delta01


################PENCOMP###################################################################################
pencomp=tablePENCOMP[which(!is.na(tablePENCOMP[,1])), c(1, 2, 5, 6, 9, 10)]

#################################################################################
naive=tableNaive[which(!is.na(tableNaive[,1])), c(1, 2, 5, 6, 9, 10)]

################iptw###################################################################################
iptw=tableIPTW[which(!is.na(tableIPTW[,1])), c(1, 2, 5, 6, 9, 10)]


################g computation###################################################################################
gcompute=tableGcompute[which(!is.na(tableGcompute[,1])), c(1, 2, 5, 6, 9, 10)]



pdf(paste(DIREC, "Figure11_plots_truncation.pdf", sep=""))


par(mfrow = c(3,2),
    oma = c(2,2,3,2) + 0.1,
    mar = c(2,0.5,0,0) + 0.1, cex.main = 1.5)

index=1  ###estimate delta11
maxMin=c(-12,2)

time=c(1:15)
plot(time,as.numeric(pencomp[,index]), pch=17, ylim=maxMin, col="red", xaxt='n',yaxt='n',
     main="", adj=0, cex=2, cex.axis=0.85)
points(time, as.numeric(aiptw[,index]), pch=8, col="blue",  cex=2)
points(time, as.numeric(gcompute[,index]), pch=1, col="black", cex=2)
points(time, as.numeric(iptw[,index]), pch=22, col="green4", cex=2)
points(time, as.numeric(naive[,index]), pch=3, col="navy", cex=2)
axis(side = 2, at =c(-10, -8, -6, -4, -2, 0, 2), tck = -0.03, labels = c(-10, -8, -6, -4, -2, 0, 2))
abline(h=0)
#mtext(expression(bold(paste("Estimates"))), line = 1.5, at=2, side=3)
mtext(expression(bold(paste(Delta, "11"))), line = 0, at=8, side=3)


#####################SE plots ###########################################
index=2  ###se of estimate delta11
maxMin=c(0,3)

plot(time,as.numeric(pencomp[,index]), pch=17, ylim=maxMin, col="red", xaxt='n',yaxt='n',
     main="", adj=0, cex=2, cex.axis=0.85)
points(time, as.numeric(aiptw[,index]), pch=8, col="blue",  cex=2)
points(time, as.numeric(gcompute[,index]), pch=1, col="black", cex=2)
points(time, as.numeric(iptw[,index]), pch=22, col="green4", cex=2)
points(time, as.numeric(naive[,index]), pch=3, col="navy", cex=2)
axis(side = 4, at = seq(0,10), tck = -0.03, labels =seq(0, 10))
mtext(expression(bold(paste("SE11"))), line = 0, at=8, side=3)


###########################################################################
index=3  ##estimate delta10
maxMin=c(-15,3)
plot(time,as.numeric(pencomp[,index]), pch=17, ylim=maxMin, col="red", xaxt='n',yaxt='n',
     main="", adj=0, cex=2, cex.axis=0.85)
points(time, as.numeric(aiptw[,index]), pch=8, col="blue",  cex=2)
points(time, as.numeric(gcompute[,index]), pch=1, col="black", cex=2)
points(time, as.numeric(iptw[,index]), pch=22, col="green4", cex=2)
points(time, as.numeric(naive[,index]), pch=3, col="navy", cex=2)
axis(side = 2, at =c(-15, -10, -5, 0, 5), tck = -0.03, labels = c(-15, -10, -5, 0, 5))
abline(h=0)
mtext(expression(bold(paste(Delta, "10"))), line = 0, at=8, side=3)


#####################SE plots ###########################################
index=4  ###se of estimate delta10

maxMin=c(0,4)
plot(time,as.numeric(pencomp[,index]), pch=17, ylim=maxMin, col="red", xaxt='n',yaxt='n',
     main="", adj=0, cex=2, cex.axis=0.85)
points(time, as.numeric(aiptw[,index]), pch=8, col="blue",  cex=2)
points(time, as.numeric(gcompute[,index]), pch=1, col="black", cex=2)
points(time, as.numeric(iptw[,index]), pch=22, col="green4", cex=2)
points(time, as.numeric(naive[,index]), pch=3, col="navy", cex=2)
axis(side = 4, at = seq(0,4), tck = -0.03, labels =seq(0, 4))
mtext(expression(bold(paste("SE10"))), line = 0, at=8, side=3)



###########################################################################
index=5  ###estimate delta01
maxMin=c(-10,3)
plot(time,as.numeric(pencomp[,index]), pch=17, ylim=maxMin, col="red", xaxt='n',yaxt='n',
     main="", adj=0, cex=2, cex.axis=0.85)
points(time, as.numeric(aiptw[,index]), pch=8, col="blue",  cex=2)
points(time, as.numeric(gcompute[,index]), pch=1, col="black", cex=2)
points(time, as.numeric(iptw[,index]), pch=22, col="green4", cex=2)
points(time, as.numeric(naive[,index]), pch=3, col="navy", cex=2)
axis(side = 2, at =c(-10, -5, 0, 5), tck = -0.03, labels = c(-10, -5, 0, 5))
abline(h=0)
mtext(expression(bold(paste(Delta, "01"))), line = 0, at=8, side=3)
axis(side = 1, at = seq(1:16), labels = seq(1:16), tck = -0.01, las=2)




#####################SE plots ###########################################
index=6 ###se of estimate of delta01
maxMin=c(0,5)
plot(time,as.numeric(pencomp[,index]), pch=17, ylim=maxMin, col="red", xaxt='n',yaxt='n',
     main="", adj=0, cex=2, cex.axis=0.85)
points(time, as.numeric(aiptw[,index]), pch=8, col="blue",  cex=2)
points(time, as.numeric(gcompute[,index]), pch=1, col="black", cex=2)
points(time, as.numeric(iptw[,index]), pch=22, col="green4", cex=2)
points(time, as.numeric(naive[,index]), pch=3, col="navy", cex=2)
axis(side = 4, at = seq(0,10), tck = -0.03, labels =seq(0, 10))
mtext(expression(bold(paste("SE01"))), line = 0, at=8, side=3)
axis(side = 1, at = seq(1:16), labels = seq(1:16), tck = -0.01, las=2)



dev.off()
