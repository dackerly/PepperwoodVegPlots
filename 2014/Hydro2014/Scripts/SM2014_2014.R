# Notes:
# 2014 had a lower mean (see density plot)
# SD for the different years look very similiar

library(classInt)
library(fields)
library(maps)
library(sp)


#source("/Users/meaganoldfather/Dropbox/PepperwoodVegPlots/Database/2013/Woody2013/Scripts/AllDataCreation20131016.R")
#source("/Users/meaganoldfather/Dropbox/PepperwoodVegPlots/Database/2013/Woody2013/Scripts/GISLayersScript20130904.R")
#2013
setwd("../../../")
dir()
hydro.2013<-read.csv("2013/Hydro2013/Data/OriginalCSV/SoilMoisture2013.csv")
head(hydro.2013)
hydro.2013<-hydro.2013[,c(1,4,5)]

colnames(hydro.2013)<- c("PLOT","QUAD", "VWC")
hydro.2013.mean<-aggregate(VWC ~ PLOT, data=hydro.2013, FUN=mean)
colnames(hydro.2013.mean)<- c("PLOT", "MEAN.VWC")

hydro.2013.sd<-aggregate(VWC ~ PLOT,data=hydro.2013, FUN=sd)
colnames(hydro.2013.sd)<- c("PLOT", "SD.VWC")

hist(hydro.2013.mean$MEAN.VWC, xlab= "VWC", col="blue")
hist(hydro.2013.sd$SD.VWC, xlab= "VWC", col="green")

#2014
hydro.2014<-read.csv("2014/Hydro2014/Data/OriginalCSV/SoilMoisture2014.csv")
hydro.2014<-hydro.2014[,c(1,4,5)]
colnames(hydro.2014)<- c("PLOT","QUAD", "VWC")

hydro.2014.mean<-aggregate(VWC ~ PLOT,data=hydro.2014, FUN=mean)
colnames(hydro.2014.mean)<- c("PLOT", "MEAN.VWC")

hydro.2014.sd<-aggregate(VWC ~ PLOT,data=hydro.2014, FUN=sd)
colnames(hydro.2014.sd)<- c("PLOT", "SD.VWC")

hist(hydro.2014.mean$MEAN.VWC, xlab= "VWC", col="red")

hist(hydro.2014.sd$SD.VWC, xlab= "VWC")

#2015
hydro.2015<-read.csv("2015/Hydro2015/Data/OriginalCSV/SoilMoisture2015.csv")
hydro.2015<-hydro.2015[,c(1,4,5)]
colnames(le)<- c("PLOT","QUAD", "VWC")

hydro.2015.mean<-aggregate(VWC ~ PLOT,data=hydro.2015, FUN=mean)
colnames(hydro.2015.mean)<- c("PLOT", "MEAN.VWC")

hydro.2015.sd<-aggregate(VWC ~ PLOT,data=hydro.2015, FUN=sd)
colnames(hydro.2015.sd)<- c("PLOT", "SD.VWC")

hist(hydro.2015.mean$MEAN.VWC, xlab= "VWC", col="red")

hist(hydro.2015.sd$SD.VWC, xlab= "VWC")

# Plot means from years on top of eachother... transparency!

hist(hydro.2014.mean$MEAN.VWC,xlim=c(0,20), col=rgb(0, 1, 0,0.5))
hist(hydro.2013.mean$MEAN.VWC,col= rgb(1, 0, 0,0.5),add=T)
hist(hydro.2015.mean$MEAN.VWC,col= rgb(1, 0, 0,0.5),add=T)

plot(density(hydro.2014.mean$MEAN.VWC), col="red", main= "Volumetric Water Content", xlab="VWC (%)")
lines(density(hydro.2013.mean$MEAN.VWC))
lines(density(hydro.2015.mean$MEAN.VWC), col="orange")
legend("topleft", c("2013","2014", "2015"), lty=c(1,1), lwd=c(2.5,2.5, 2.5),col=c("black","red", "orange"))

all.hydro<- cbind(hydro.2013.mean, hydro.2014.mean, hydro.2015.mean)
all.hydro<-all.hydro[,-3]
colnames(all.hydro)<- c("PLOT", "VWC.2013", "VWC.2014")
head(all.hydro)

# Is the mean different statistically?
with(all.hydro, t.test(VWC.2013,VWC.2014, paired=T))
with(all.hydro, t.test(VWC.2013,VWC.2015, paired=T))

# Using all the measurements #### Not correct, just curious #### 
dim(hydro.2013)
dim(hydro.2014)
hydro.2014<-subset(hydro.2014, hydro.2014$QUAD!="")


h<-cbind(hydro.2013, hydro.2014)
h<-h[,-c(4,5)]
colnames(h)<-c("PLOT", "QUAD", "H2013", "H2014")
head(h)
t.test(h$H2013,h$H2014, paired=T)

# p-value = 1.899e-05 --> different

#############################################################

h2013<-sort.list(all.hydro$VWC.2013)
h2014<-sort.list(all.hydro$VWC.2014)
# Don't match...
(h2013 %in% h2014) # ?


all.hydro$diff<- all.hydro[,2]-all.hydro[,3]
# 2013 was wetter in all plots in 2013 except in PPW1310

plot(all.hydro[,2],all.hydro[,3],type="n",xlab="2013", ylab="2014", xlim=c(0,18), ylim=c(0,18), main="Plot VWC")
text(all.hydro[,2],all.hydro[,3])
     

abline(0,1,lty=5)


mean(all.hydro$diff) # 2.5
mean(all.hydro$VWC.2013) # 9.7
mean(all.hydro$VWC.2014) # 7.2


per.change<-mean(all.hydro$diff)/mean(all.hydro$VWC.2013)
per.change<-per.change *100 
per.change # 25% reduction is soil moisture



# Do certain topography drive is plots being more drier this year? 

head(climate.pts)

topo.clim<-cbind(climate.pts[,-7], all.hydro[,-1]) 
topo.clim<-cbind(topo.clim, plot.info[,c(4,5)])
head(topo.clim)

fit1<-with(topo.clim, lm(VWC.2013~DEM))
fit2<-with(topo.clim, lm(VWC.2014~DEM)) # significant
fit.d<-with(topo.clim, lm(diff~DEM))
summary(fit1)
summary(fit2)
summary(fit.d)

plot(fit.d)
with(topo.clim, plot(DEM, diff, pch=19))


fit3<-with(topo.clim, lm(VWC.2013~MarchRadiation))
fit4<-with(topo.clim, lm(VWC.2014~MarchRadiation))
summary(fit3)
summary(fit4)

fit5<-with(topo.clim, lm(VWC.2013~TMaxJUL))
fit6<-with(topo.clim, lm(VWC.2014~TMaxJUL))
summary(fit5)
summary(fit6)

fit7<-with(topo.clim, lm(VWC.2013~Aspect))
fit8<-with(topo.clim, lm(VWC.2014~Aspect))
summary(fit7)
summary(fit8)

fit9<-with(topo.clim, lm(VWC.2013~Slope))
fit10<-with(topo.clim, lm(VWC.2014~Slope))
summary(fit9)
summary(fit10)

fit11<-with(topo.clim, lm(VWC.2013~tpi500))
fit12<-with(topo.clim, lm(VWC.2014~tpi500))
summary(fit11)
summary(fit12)
with(topo.clim, plot(tpi500, diff))



##### Plot Out In PLot

ploteqc <- function(spobj, z, breaks){
  pal <- tim.colors(length(breaks)-1)
  fb <- classIntervals(z, n = length(pal), 
                       style = "fixed", fixedBreaks = breaks)
  col <- findColours(fb, pal)
  plot(PP)
  points(spobj, col = col, pch = 19)
  image.plot(legend.only = TRUE, zlim = range(breaks), col = pal)
}

hydro.sp<- cbind(plot.info[,c(1:3)], all.hydro[,-1])
hydro.sp

ta.project = '+proj=aea +datum=NAD83 +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000'
utm.project = '+proj=utm +zone=10S +datum=WGS84'
long <- as.numeric(hydro.sp$UTM.E)
lat <- as.numeric(hydro.sp$UTM.N)
plotsSP <- SpatialPoints(data.frame(long,lat), proj4string <- CRS(utm.project))
plotsSP.ta <- spTransform(plotsSP,CRS(ta.project))

hydro.spd.2013<-SpatialPointsDataFrame(plotsSP.ta, data=as.data.frame(hydro.sp$VWC.2013))
hydro.spd.2014<-SpatialPointsDataFrame(plotsSP.ta, data=as.data.frame(hydro.sp$VWC.2014))
hydro.spd.diff<-SpatialPointsDataFrame(plotsSP.ta, data=as.data.frame(hydro.sp$diff))


par(mfrow=c(1,1))
ploteqc(hydro.spd.2013,hydro.sp$VWC.2013, breaks= (0:20))
title("2013 VWC")
ploteqc(hydro.spd.2014,hydro.sp$VWC.2014, breaks= (0:20))
title("2014 VWC")    
ploteqc(hydro.spd.2014,hydro.sp$diff, breaks= (-1:6))
title("Difference between 2013 and 2014")
                                                                     