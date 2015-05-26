# Intent: Send data to Fred Euphrat for the TBC3 adaptive management startegy
# Author: MFO
# Date Created: 20150111
# Date Last Edited:20150111

if (Sys.getenv("USER")=='meaganoldfather') setwd("/Users/meaganoldfather/Box Sync/PepperwoodVegPlots/Database/")
 source("Analyses/PWFunctions.R")
#
indv.data<-get.indv.data(2014)
#
envr.data<-get.envr.data(2013)
#
seju.data<-get.seju.data(2013)
  seju.data<-aggregate(cbind(Num.Seedlings, Num.Juveniles,Total.Number)~Species+Plot,   data=seju.data, FUN=sum)
  seju.data<-seju.data[,c(2,1,3,4,5)]
#
plot.data<-get.plot.info()
#
write.csv(indv.data, "Outputs/indv.data.csv", row.names=F)
write.csv(envr.data, "Outputs/envr.data.csv", row.names=F)
write.csv(seju.data, "Outputs/seju.data.csv", row.names=F)
write.csv(plot.data, "Outputs/plot.data.csv", row.names=F)


