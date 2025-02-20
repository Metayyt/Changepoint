rm(list=ls())
#### THIS COMBINES THE RESULTS TOGETHER TO PRODUCE FIGURE 10
### NO restricting to overlap regions in PENCOMP and weight truncation in AIPTW and IPTW

DIREC="codes/Application/Results/"### where results from allMethodRun.R are stored

#####After running allMethodRun.R, run the following script to reproduce Figure 10 in the main text.
###########load the results
tableAIPTW=read.table(paste0(DIREC, "tableAIPTW.txt"), header = T)
tableIPTW=read.table(paste0(DIREC, "tableIPTW.txt"), header = T)
tablePENCOMP=read.table(paste0(DIREC, "tablePENCOMP.txt"), header = T)
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


pdf(paste(DIREC, "Figure10_plots_withoutRemovingExtreme.pdf", sep=""))


par(mfrow = c(3,2),
    oma = c(2,2,3,2) + 0.1,
    mar = c(2,0.5,0,0) + 0.1, cex.main = 1.5)

index=1  ###estimate delta11
maxMin=c(-12,17)

time=c(1:15)
plot(time,as.numeric(pencomp[,index]), pch=17, ylim=maxMin, col="red", xaxt='n',yaxt='n',
     main="", adj=0, cex=2, cex.axis=0.85)
points(time, as.numeric(aiptw[,index]), pch=8, col="blue",  cex=2)
points(time, as.numeric(gcompute[,index]), pch=1, col="black", cex=2)
points(time, as.numeric(iptw[,index]), pch=22, col="green4", cex=2)
points(time, as.numeric(naive[,index]), pch=3, col="navy", cex=2)
axis(side = 2, at =c(-10, -5, 0, 5, 10, 15), tck = -0.03, labels = c(-10, -5, 0, 5, 10, 15))
abline(h=0)
#mtext(expression(bold(paste("Estimates"))), line = 1.5, at=2, side=3)
mtext(expression(bold(paste(Delta, "11"))), line = 0, at=8, side=3)


#####################SE plots ###########################################
index=2  ##se of estimate delta11
maxMin=c(0,8)

plot(time,as.numeric(pencomp[,index]), pch=17, ylim=maxMin, col="red", xaxt='n',yaxt='n',
     main="", adj=0, cex=2, cex.axis=0.85)
points(time, as.numeric(aiptw[,index]), pch=8, col="blue",  cex=2)
points(time, as.numeric(gcompute[,index]), pch=1, col="black", cex=2)
points(time, as.numeric(iptw[,index]), pch=22, col="green4", cex=2)
points(time, as.numeric(naive[,index]), pch=3, col="navy", cex=2)
axis(side = 4, at = seq(0,10), tck = -0.03, labels =seq(0, 10))
mtext(expression(bold(paste("SE11"))), line = 0, at=8, side=3)


arrows(10, max(maxMin)-0.5, 10, max(maxMin), length=0.1)
text(11, max(maxMin)-1, paste("AIPTW=",round(as.numeric(aiptw[10,index]),0)), col="blue")

arrows(9, max(maxMin)-0.5, 9, max(maxMin), length=0.1)
text(7.7, max(maxMin)-1, paste("AIPTW=",round(as.numeric(aiptw[9,index]),0)), col="blue")


###########################################################################
index=3  ###estimate of delta10
maxMin=c(-15,10)
plot(time,as.numeric(pencomp[,index]), pch=17, ylim=maxMin, col="red", xaxt='n',yaxt='n',
     main="", adj=0, cex=2, cex.axis=0.85)
points(time, as.numeric(aiptw[,index]), pch=8, col="blue",  cex=2)
points(time, as.numeric(gcompute[,index]), pch=1, col="black", cex=2)
points(time, as.numeric(iptw[,index]), pch=22, col="green4", cex=2)
points(time, as.numeric(naive[,index]), pch=3, col="navy", cex=2)
axis(side = 2, at =c(-15, -10, -5, 0, 5, 10), tck = -0.03, labels = c(-15, -10, -5, 0, 5, 10))
abline(h=0)
mtext(expression(bold(paste(Delta, "10"))), line = 0, at=8, side=3)


#####################SE plots ###########################################
index=4 ###se of estimate of delta10

maxMin=c(0,9)
plot(time,as.numeric(pencomp[,index]), pch=17, ylim=maxMin, col="red", xaxt='n',yaxt='n',
     main="", adj=0, cex=2, cex.axis=0.85)
points(time, as.numeric(aiptw[,index]), pch=8, col="blue",  cex=2)
points(time, as.numeric(gcompute[,index]), pch=1, col="black", cex=2)
points(time, as.numeric(iptw[,index]), pch=22, col="green4", cex=2)
points(time, as.numeric(naive[,index]), pch=3, col="navy", cex=2)
axis(side = 4, at = seq(0,10), tck = -0.03, labels =seq(0, 10))
mtext(expression(bold(paste("SE10"))), line = 0, at=8, side=3)


arrows(10, max(maxMin)-0.5, 10, max(maxMin), length=0.1)
text(11, max(maxMin)-1, paste("AIPTW=",round(as.numeric(aiptw[10,index]),0)), col="blue")

arrows(9, max(maxMin)-0.5, 9, max(maxMin), length=0.1)
text(7.8, max(maxMin)-1, paste("AIPTW=",round(as.numeric(aiptw[9,index]),0)), col="blue")


###########################################################################
index=5  ###estimate of delta01
maxMin=c(-10,11)
plot(time,as.numeric(pencomp[,index]), pch=17, ylim=maxMin, col="red", xaxt='n',yaxt='n',
     main="", adj=0, cex=2, cex.axis=0.85)
points(time, as.numeric(aiptw[,index]), pch=8, col="blue",  cex=2)
points(time, as.numeric(gcompute[,index]), pch=1, col="black", cex=2)
points(time, as.numeric(iptw[,index]), pch=22, col="green4", cex=2)
points(time, as.numeric(naive[,index]), pch=3, col="navy", cex=2)
axis(side = 2, at =c(-10, -5, 0, 5, 10), tck = -0.03, labels = c(-10, -5, 0, 5, 10))
abline(h=0)
mtext(expression(bold(paste(Delta, "01"))), line = 0, at=8, side=3)
axis(side = 1, at = seq(1:15), labels = seq(1:15), tck = -0.01, las=2)




#####################SE plots ###########################################
index=6  ###se of estimate delta01
maxMin=c(0,10)
plot(time,as.numeric(pencomp[,index]), pch=17, ylim=maxMin, col="red", xaxt='n',yaxt='n',
     main="", adj=0, cex=2, cex.axis=0.85)
points(time, as.numeric(aiptw[,index]), pch=8, col="blue",  cex=2)
points(time, as.numeric(gcompute[,index]), pch=1, col="black", cex=2)
points(time, as.numeric(iptw[,index]), pch=22, col="green4", cex=2)
points(time, as.numeric(naive[,index]), pch=3, col="navy", cex=2)
axis(side = 4, at = seq(0,10), tck = -0.03, labels =seq(0, 10))
mtext(expression(bold(paste("SE01"))), line = 0, at=8, side=3)
axis(side = 1, at = seq(1:15), labels = seq(1:15), tck = -0.01, las=2)

arrows(4, max(maxMin)-0.5, 4, max(maxMin), length=0.1)
text(4, max(maxMin)-1.5, paste("AIPTW=",round(as.numeric(aiptw[4,index]),0)), col="blue")

arrows(10, max(maxMin)-0.5, 10, max(maxMin), length=0.1)
text(11, max(maxMin)-1, paste("AIPTW=",round(as.numeric(aiptw[10,index]),0)), col="blue")

arrows(9, max(maxMin)-0.5, 9, max(maxMin), length=0.1)
text(9, max(maxMin)-1.9, paste("AIPTW=",round(as.numeric(aiptw[9,index]),0)), col="blue")

arrows(8, max(maxMin)-0.5, 8, max(maxMin), length=0.1)
text(7, max(maxMin)-1, paste("AIPTW=",round(as.numeric(aiptw[8,index]),0)), col="blue")


dev.off()
