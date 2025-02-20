rm(list=ls())
### this is to produce the legend of Figure 10 and 11. 

DIREC="codes/Application/"  #where image will be stored

png(paste(DIREC, "FiguresandTables/legend.png", sep=""))
plot(1, type="n", axes=FALSE, xlab="", ylab="")

par(fig=c(0,1,0,0.35), new=TRUE)
plot_colors <- c("red","blue", "black", "green4", "navy")
text <- c("PENCOMP", "AIPTW", "G-COMP", 
          "IPTW", "Naive")
legend("center",legend = text, text.width = max(sapply(text, strwidth)),##xpd = TRUE tells OK to plot outside the region
      col=plot_colors, cex=1, horiz = TRUE, pch=c(17,8,1,22, 3), xpd = TRUE, text.font=2, pt.cex=2)



dev.off()










