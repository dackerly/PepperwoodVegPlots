# 2015 Seedling and Juvenile Surveys 
# Intent: Bring data in and clean
# Author: MFO
# Date created: 20150525
# Date edited: 20150525

# set working directory
if (Sys.getenv("USER")=='meaganoldfather') setwd("/Users/meaganoldfather/Box Sync/PepperwoodVegPlots/Database/")

# source PW Functions
source("Analyses/PWFunctions.R")

SJ15<-get.seju.data(2015)
head(SJ15)

unique(SJ15$Species)

# aggregated by plot& species
SJ15.ag<-plants.by.plot(2015, "SEJU")
head(SJ15.ag)

library(lattice)
barchart(Total.Number~Species,data=SJ15.ag,scales=list(x=list(rot=90,cex=0.8)), col="black", ylim=c(0,700))

# trees for comparison 
TR.SA<-plants.by.plot(2013, "SA.TR")
head(TR.SA)
barchart((SA.Basal.Area+TR.Basal.Area)~Species,data=TR.SA,scales=list(x=list(rot=90,cex=0.8)), col="black")
barchart((SA.Count+TR.Count)~Species,data=TR.SA,scales=list(x=list(rot=90,cex=0.8)), col="black")
