# Intent: Look at DougFir Removal Plots

# clear workspace
rm(list=ls())
#Packages
library("RCurl")
library("data.table")
library("picante")
library("vegan")
library("raster")
library("maptools")
library("rgdal")

# sources in all functions
source_https <- function(url, ...) { 
  # parse and evaluate each .R script
  sapply(c(url, ...), function(u) {
    eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
  })
}
source_https('https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/Analyses/PWfunctions_GitHub.R')

# make a list of plot names
plots<-as.data.frame(get.plot())
colnames(plots)<-"Plot"
plots

# bring in tree indv data
tr.df<-get.indv.data(year=2014)
tr<-tr.df[tr.df$Type=="TR",]
tr<-tr[tr$Species=="PSEMEN",]
tr$DBH<-sqrt(tr$Basal.Area / pi) * 2
tr<-tr[,c(1,4,9)]

tr.big<-aggregate(DBH~Plot, data=tr, FUN=function (x) length(which(x>=25)))
colnames(tr.big)<-c("Plot", "DBH>=25cm")

tr.small<-aggregate(DBH~Plot, data=tr, FUN=function (x) length(which(x<25)))
colnames(tr.small)<-c("Plot", "DBH<25cm")

tr.agg<-cbind(tr.small,tr.big[-1])
tr.agg

# bring in seedling indv data
sj.df<-get.seju.data(year=2015)
sj<-sj.df[sj.df$Species=="PSEMEN",]
sj<-sj[c(1,4,5)]
sj<-aggregate(cbind(Num.Seedlings,Num.Juveniles)~Plot,data=sj,FUN=sum)
head(sj)

# merge data sets together
tr.sj<-merge(tr.agg,sj,by="Plot",all.x=T,all.y=T)
# add in plots that don't have any Doug Fir
tr.sj<-merge(plots,tr.sj,by="Plot",all.x=T)
tr.sj[is.na(tr.sj)]<-0

# bring in plots with Doug Fir Removal
psemen.plots<-c("PPW1320", "PPW1349", "PPW1340", "PPW1341", "PPW1306")
tr.sj$Removal<-F
tr.sj[tr.sj$Plot%in%psemen.plots,"Removal"]<-T
head(tr.sj)
write.csv(tr.sj,"DougfirManagement.csv")

# make plot with doug fir tree basal area vs. doug fir seedling/juvenile with removal plots highlighted
head(tr)
tr$Basal.Area<-((tr$DBH/2)^2)*pi 
tr.plot<-aggregate(Basal.Area~Plot,data=tr,FUN=sum)
tr.plot<-merge(tr.plot,sj,by="Plot",all.x=T,all.y=T)
tr.plot<-merge(plots,tr.plot,by="Plot",all.x=T)
tr.plot[is.na(tr.plot)]<-0
tr.plot$Removal<-1
tr.plot[tr.plot$Plot%in%psemen.plots,"Removal"]<-2
head(tr.plot)

colors=c("black","red")
plot(tr.plot$Basal.Area,(tr.plot$Num.Seedlings+tr.plot$Num.Juveniles), col=colors[tr.plot$Removal], pch=19, xlab="Doug fir Basal Area", ylab=("Seedlings+Juveniles"))

plot()


