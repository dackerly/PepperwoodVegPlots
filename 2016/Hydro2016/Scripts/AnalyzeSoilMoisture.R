# Notes:
# 2014 had a lower mean (see density plot)
# SD for the different years look very similiar

library(classInt)
library(fields)
library(maps)
library(sp)

dir()

water.2013<-read.csv("Pepperwood/GitDatabase/2013/Hydro2013/Data/OriginalCSV/SoilMoisture2013.csv")
water.2014<-read.csv("Pepperwood/GitDatabase/2014/Hydro2014/Data/OriginalCSV/SoilMoisture2014.csv")
water.2015<-read.csv("Pepperwood/GitDatabase/2015/Hydro2015/Data/OriginalCSV/SoilMoisture2015.csv")
water.2016<-read.csv("Pepperwood/GitDatabase/2016/Hydro2016/Data/OriginalCSV/SoilMoisture2016.csv")

head(water.2013)
water.2013<-water.2013[,c(1,4,5)]
colnames(water.2013)<- c("PLOT","QUAD", "VWC")

head(water.2014)
water.2014<-water.2014[,c(1,4,5)]
colnames(water.2014)<- c("PLOT","QUAD", "VWC")

head(water.2015)
water.2015<-water.2015[,c(1,2,5)]
colnames(water.2015)<- c("PLOT","QUAD", "VWC")

head(water.2016)
water.2016<-water.2016[,c(1,2,5)]
colnames(water.2016)<- c("PLOT","QUAD", "VWC")

### aggregate data to the plot level 

mean13<-aggregate(VWC ~ PLOT, data=water.2013, FUN=mean)
colnames(mean13)<- c("PLOT", "MEAN.VWC")
sd13<-aggregate(VWC ~ PLOT,data=water.2013, FUN=sd)
colnames(sd13)<- c("PLOT", "SD.VWC")
hist(mean13$MEAN.VWC, xlab= "VWC", col="blue")
hist(sd13$SD.VWC, xlab= "VWC", col="green")

mean14<-aggregate(VWC ~ PLOT, data=water.2014, FUN=mean)
colnames(mean14)<- c("PLOT", "MEAN.VWC")
sd14<-aggregate(VWC ~ PLOT,data=water.2014, FUN=sd)
colnames(sd14)<- c("PLOT", "SD.VWC")
hist(mean14$MEAN.VWC, xlab= "VWC", col="blue")
hist(sd14$SD.VWC, xlab= "VWC", col="green")

mean15<-aggregate(VWC ~ PLOT, data=water.2015, FUN=mean)
colnames(mean15)<- c("PLOT", "MEAN.VWC")
sd15<-aggregate(VWC ~ PLOT,data=water.2015, FUN=sd)
colnames(sd15)<- c("PLOT", "SD.VWC")
hist(mean15$MEAN.VWC, xlab= "VWC", col="blue")
hist(sd15$SD.VWC, xlab= "VWC", col="green")

mean16<-aggregate(VWC ~ PLOT, data=water.2016, FUN=mean)
colnames(mean16)<- c("PLOT", "MEAN.VWC")
sd16<-aggregate(VWC ~ PLOT,data=water.2016, FUN=sd)
colnames(sd16)<- c("PLOT", "SD.VWC")
hist(mean16$MEAN.VWC, xlab= "VWC", col="blue")
hist(sd16$SD.VWC, xlab= "VWC", col="green")


# Plot means across years
plot(density(mean14$MEAN.VWC), main= "Mean Volumetric Water Content", xlab="VWC (%)", xlim=c(0,20))
lines(density(mean13$MEAN.VWC),col="red")
lines(density(mean15$MEAN.VWC), col="orange")
lines(density(mean16$MEAN.VWC), col="blue")
legend("topleft", c("2013","2014", "2015", "2016"), lty=c(1,1), lwd=c(2.5,2.5, 2.5),col=c("red","black", "orange", "blue"))



# plot SD across years
plot(density(sd13$SD.VWC), main= "SD Volumetric Water Content", xlab="VWC (%)", xlim=c(0,20),col="red")
lines(density(sd14$SD.VWC),col="black")
lines(density(sd15$SD.VWC), col="orange")
lines(density(sd16$SD.VWC), col="blue")
legend("topright", c("2013","2014", "2015", "2016"), lty=c(1,1), lwd=c(2.5,2.5, 2.5),col=c("red","black", "orange", "blue"))


#############################################################


mean.sm<- merge(mean13, mean14, by="PLOT")
mean.sm<- merge(mean.sm, mean15, by="PLOT")
mean.sm<- merge(mean.sm, mean16, by="PLOT")
colnames(mean.sm)<- c("PLOT", "2013", "2014", "2015", "2016")
head(mean.sm)

library(ggplot2)
library(reshape2)
library(tidyr)
head(mean.sm)
mean.sm %>% gather(YEAR, VWC, -PLOT)

mean.sm.clean <- mean.sm %>% gather(YEAR, VWC, -PLOT)

ggplot(mean.sm.clean, aes(YEAR,VWC, group=PLOT))+geom_line()+theme_classic()


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
                                                                     