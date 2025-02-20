####CREATED 6/21/2017
#### MODIFIED 11/20/2017
### this is to produce table 29 in the appendix

rm(list=ls())
DIREC=""  ### where file containing sample sizes is stored

################sample size ###################################################################################
###the results are stored in sampleSizeNum after running allMethodRun.R script

pencomp=sampleSizeSum ###the entries are num11, num10, num01, num00, num11_include, num10_include, num01_include, total
                      ###(the 5-6 entries are the number of subjects included after excluding subject outside overlap region)

pencomp=pencomp[c(7:21),]  

100*pencomp$X6/pencomp$X5
100*pencomp$X7/pencomp$X5
100*pencomp$X8/pencomp$X5


range(100*pencomp$X6/pencomp$X5) ##11
range(100*pencomp$X7/pencomp$X5) ##10
range(100*pencomp$X8/pencomp$X5) ##01


range(c(100*pencomp$X6/pencomp$X5,
100*pencomp$X7/pencomp$X5,
100*pencomp$X8/pencomp$X5))

mean(c(100*pencomp$X6/pencomp$X5,
        100*pencomp$X7/pencomp$X5,
        100*pencomp$X8/pencomp$X5))


sampleSizeAll=pencomp
outPut=sampleSizeAll
outPut=outPut[-c(1),]

outPut=cbind(paste("Window", 1:dim(outPut)[1], sep=""), rep("&", dim(outPut)[1]),
             outPut[,c(1)], rep("&", dim(outPut)[1]), outPut[,c(2)], rep("&", dim(outPut)[1]),
             outPut[,c(3)], rep("&", dim(outPut)[1]), outPut[,c(4)], rep("&", dim(outPut)[1]), rep("&", dim(outPut)[1]),
             outPut[,c(5)], rep("&", dim(outPut)[1]),
             outPut[,c(6)], rep("&", dim(outPut)[1]),
             outPut[,c(7)], rep("&", dim(outPut)[1]),outPut[,c(8)],
             rep("\\\\", dim(outPut)[1]))

###this can be directly copied and pasted into manuscript latex file to produce table 29 in the appendix 
write.table(outPut, paste(DIREC, "sampleSizeTable.txt",sep=""),  quote=F, col.names=F, row.names=F)






