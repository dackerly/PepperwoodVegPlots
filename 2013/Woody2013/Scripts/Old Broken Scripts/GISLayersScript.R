library(sp)
library(maptools)
library(raster)

# projection strings
ta.project = '+proj=aea +datum=NAD83 +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000'
utm.project = '+proj=utm +zone=10S +datum=WGS84' # CHECK GPS

setwd("/Users/meaganoldfather/Dropbox/PepperwoodVegPlots/Database/GIS/")
dir()

dir('DEM')
dem <- raster('DEM/pw_10m_t2.asc')
plot(dem, main="DEM")
#projection(dem)

dir("PW10m_topoclimate")
marrad <- raster('PW10m_topoclimate/marrad.asc')
tmaxJUL<- raster('PW10m_topoclimate/tmxavejul.asc')
tminJAN<- raster ('PW10m_topoclimate/tmnavejan.asc')
plot(marrad, main = "March Radiation")
plot(tmaxJUL, main = "T Max July")
plot(tminJAN, main = " T Min Jan ")
tmaxJUL
dir('PPshapefiles/PPshapefile-teale-albers')
PP <- readShapeSpatial('PPshapefiles/PPshapefile-teale-albers/Pepperwood')
plot(PP,add=T)
#projection(PP)

plot(PP)
PPex <- extent(PP)

tmaxJULpts=getValues(crop(tmaxJUL,PP))
dempts=getValues(crop(dem,PP))
tminJANpts=getValues(crop(tminJAN,PP))
plot(dempts,tmaxJULpts,pch='.')

topoidx <- raster('PW10m_topoclimate/topoidx.asc')
plot(crop(topoidx,PPex))
tpi500 <- raster('PW10m_topoclimate/tpi_500.asc')
plot(crop(tpi500,PPex))

plot(getValues(crop(tpi500,PPex)),tmaxJULpts,pch='.')
plot(getValues(crop(tpi500,PPex)),tminJANpts,pch='.')
plot(dempts,tminJANpts,pch='.')
plot(dempts,getValues(crop(tpi500,PPex)),pch='.')
plot(getValues(crop(marrad,PPex)),tmaxJULpts,pch='.')


source("/Users/meaganoldfather/Dropbox/PepperwoodVegPlots/Database/2013/Woody2013/Scripts/Plot.Info.All.R")
# Do not worry about the plots that the above scripts try to make --> ignore and press enter
# Eventually take out ordination plots from the above script and make a new one specifically for that only
head(Plot.Info.All)
Cover.All<-aggregate(Plot.Info.All$Basal.Area_cm2~Plot.Info.All$Plot.ID+Plot.Info.All$Species, FUN=sum)
head(Cover.All)
Num.All<-aggregate(Plot.Info.All$Number~Plot.Info.All$Plot.ID+Plot.Info.All$Species, FUN=sum) 
head(Num.All)

Total<-merge(Cover.All,Num.All)
head(Total)
names(Total)<-c("Plot.ID", "Species", "Basal.Area_cm2", "Number")
head(Total)

# make lat and long for each plot
long <- as.numeric(plot.info$UTM.E)
lat <- as.numeric(plot.info$UTM.N)
plotsSP <- SpatialPoints(data.frame(long,lat),proj4string <- CRS(utm.project))
plotsSP.ta <- spTransform(plotsSP,CRS(ta.project))

####### DEM MODEL Extract 
#plot(crop(dem,PPex), main= "DEM") # plots dem model on extended area around PW
#plot(PP,add=T) # add outline of PW 
#plot(plotsSP.ta,add=T,pch=19) # adds point at plot location

dem.pts<-extract(dem,plotsSP.ta) # extracts dem values for plots locations
head(dem.pts)
dem.pts<-as.data.frame(cbind(plot.list,dem.pts))
head(dem.pts)
colnames(dem.pts)<- c("Plot.ID","DEM")
head(dem.pts)
dem.pts$DEM<-as.numeric(dem.pts$DEM)
hist(dem.pts$DEM, main="DEM Histogram", xlab="DEM")



Total.Pts<- merge(Total,dem.pts)
head(Total.Pts) 

#### March Radiation Model Extraction
#plot(crop(marrad,PPex), main= "March Radiation") # plots march radiation model on extended area around PW
#plot(PP,add=T)
#plot(plotsSP.ta,add=T,pch=19)

marrad.pts<-extract(marrad,plotsSP.ta) # extracts march radiation values for plots locations
marrad.pts
marrad.pts<-as.data.frame(cbind(plot.list,marrad.pts))
head(marrad.pts)
colnames(marrad.pts)<- c("Plot.ID","MarchRadiation")
head(marrad.pts)
marrad.pts$MarchRadiation<-as.numeric(marrad.pts$MarchRadiation)
hist(marrad.pts$MarchRadiation, main="March Radiation Histogram", xlab="March Radiation")

Total.Pts<- merge(Total.Pts ,marrad.pts)
head(Total.Pts) 

#### Max July Temp Model 
#plot(crop(tmaxJUL,PPex), main ="Max Temp July") # plots max July T on extended area around PW
#plot(PP,add=T)
#plot(plotsSP.ta,add=T,pch=19)

tmaxJUL.pts<-extract(tmaxJUL,plotsSP.ta) # extracts march radiation values for plots locations
tmaxJUL.pts
tmaxJUL.pts<-as.data.frame(cbind(plot.list,tmaxJUL.pts))
head(tmaxJUL.pts)
colnames(tmaxJUL.pts)<- c("Plot.ID","TMaxJUL")
head(tmaxJUL.pts)
tmaxJUL.pts$TMaxJUL<-as.numeric(tmaxJUL.pts$TMaxJUL)

hist(tmaxJUL.pts$TMaxJUL, main ="Histogram of Max Temp July", xlab="Max Temp July C" )

Total.Pts<- merge(Total.Pts,tmaxJUL.pts)
head(Total.Pts) 

#### Min Jan Temp Model Extraction
#plot(crop(tminJAN,PPex), main="Min Temp January") # plots Min Jan Temp on extended area around PW
#plot(PP,add=T)
#plot(plotsSP.ta,add=T,pch=19)

tminJAN.pts<-extract(tminJAN,plotsSP.ta) # extracts min JAN Temp values for plots locations
tminJAN.pts
tminJAN.pts<-as.data.frame(cbind(plot.list,tminJAN.pts))
head(tminJAN.pts)
colnames(tminJAN.pts)<- c("Plot.ID","tminJAN")
head(tminJAN.pts)
tminJAN.pts$tminJAN<-as.numeric(tminJAN.pts$tminJAN)
hist(tminJAN.pts$tminJAN, main="Histogram of Min Temp January", xlab="Min Temp January")


Total.Pts<- merge(Total.Pts,tminJAN.pts)
head(Total.Pts) 

######## TPI5000
plot(crop(tpi500,PPex), main="TPI") # plots TPI on extended area around PW
plot(PP,add=T)
plot(plotsSP.ta,add=T,pch=19)

tpi500.pts<-extract(tpi500,plotsSP.ta) # extracts min JAN Temp values for plots locations
tpi500.pts
tpi500.pts<-as.data.frame(cbind(plot.list,tpi500.pts))
head(tpi500.pts)
colnames(tpi500.pts)<- c("Plot.ID","tpi500")
head(tpi500.pts)
tpi500.pts$tpi500<-as.numeric(tpi500.pts$tpi500)
hist(tpi500.pts$tpi500, main="Histogram of TPI", xlab="Percent Lower Pixels")


Total.Pts<- merge(Total.Pts,tpi500.pts)
head(Total.Pts) 

######### SOIL MOISTURE 
hydro<-read.csv("/Users/meaganoldfather/Dropbox/PepperwoodVegPlots/Database/2013/Hydro2013/Data/OriginalCSV/SoilMoisture2013.csv")
hydro<-hydro[,c(1,5)]
colnames(hydro)<- c("Plot.ID", "WD")
hydro<-aggregate(hydro$WD ~ hydro$Plot.ID, FUN=mean)

colnames(hydro)<- c("Plot.ID", "WD")
head(hydro)

hist(hydro$WD, main= "Histogram of  Field Soil Moisture (Hydrosense)", xlab="Relative Soil Moisture")

Total.Pts<- merge(Total.Pts,hydro)
head(Total.Pts) 

### Adding average size (basal.area / number ) column to Total.Pts data frame

Total.Pts$Ave.Size<-Total.Pts$Basal.Area_cm2/Total.Pts$Number 
head(Total.Pts)
################# Adding Percent Treeness 
source("/Users/meaganoldfather/Dropbox/PepperwoodVegPlots/Database/2013/Woody2013/Scripts/BuildingaRatio.R")
head(final.ratio)
Total.Pts$Percent.TR<-final.ratio$Percent.TR 
head(Total.Pts)

###### Species-specific data frames

#Pts.Species<- unique(Total.Pts$Species)
#Pts.Species

Pts.QUEGAR<- subset(Total.Pts,subset=(Total.Pts$Species == "QUEGAR"))

Pts.QUEGAR<-(Total.Pts[])

Pts.QUEDOU<- subset(Total.Pts,subset=(Total.Pts$Species == "QUEDOU"))
Pts.QUEKEL<- subset(Total.Pts,subset=(Total.Pts$Species == "QUEKEL"))
Pts.QUEAGR<- subset(Total.Pts,subset=(Total.Pts$Species == "QUEAGR"))
Pts.PSEMEN<- subset(Total.Pts,subset=(Total.Pts$Species == "PSEMEN"))
Pts.AMOCAL<- subset(Total.Pts,subset=(Total.Pts$Species == "AMOCAL"))

# Species = QUEGAR, then give basal area, but if not match then give 0


# Histogrmas
hist(Pts.QUEKEL$Basal.Area_cm2) # These probably need zero's too 
hist(Pts.QUEGAR$Basal.Area_cm2)
hist(Pts.QUEDOU$Basal.Area_cm2)
hist(Pts.QUEAGR$Basal.Area_cm2)
hist(Pts.PSEMEN$Basal.Area_cm2)
hist(Pts.AMOCAL$Basal.Area_cm2)

############################################# Basal.Area

### DEM MODEL PLOTS
with(Total.Pts, plot(DEM,Basal.Area_cm2, main="All Species"))
with(Pts.QUEGAR, plot(DEM,Basal.Area_cm2, main="QUEGAR"))
with(Pts.QUEDOU, plot(DEM,Basal.Area_cm2, main = "QUEDOU"))
with(Pts.QUEKEL, plot(DEM,Basal.Area_cm2, main = "QUEKEL"))
with(Pts.QUEAGR, plot(DEM,Basal.Area_cm2, main = "QUEAGR"))
with(Pts.PSEMEN, plot(DEM,Basal.Area_cm2, main = "PSEMEN"))
with(Pts.AMOCAL, plot(DEM,Basal.Area_cm2, main = "AMOCAL"))


### DEM MODEL LM

QUEGAR.DEM.LM<-lm(Basal.Area_cm2~DEM, data=Pts.QUEGAR)
summary(QUEGAR.DEM.LM)

QUEDOU.DEM.LM<-lm(Basal.Area_cm2~DEM, data=Pts.QUEDOU)
summary(QUEDOU.DEM.LM)

QUEAGR.DEM.LM<-lm(Basal.Area_cm2~DEM, data=Pts.QUEAGR)
summary(QUEAGR.DEM.LM)

PSEMEN.DEM.LM<-lm(Basal.Area_cm2~DEM, data=Pts.PSEMEN)
summary(PSEMEN.DEM.LM)

AMOCAL.DEM.LM<-lm(Basal.Area_cm2~DEM, data=Pts.AMOCAL)
summary(AMOCAL.DEM.LM)

####  March Radiation Plots 

with(Total.Pts, plot(MarchRadiation,Basal.Area_cm2))
with(Pts.QUEGAR, plot(MarchRadiation,Basal.Area_cm2))
with(Pts.QUEDOU, plot(MarchRadiation,Basal.Area_cm2))
with(Pts.QUEKEL, plot(MarchRadiation,Basal.Area_cm2))
with(Pts.QUEAGR, plot(MarchRadiation,Basal.Area_cm2, main="QUEAGR"))

yvals2<-predict(QUEAGR.MARR.LM)
lines(Pts.QUEAGR$MarchRadiation,yvals2)
text(600,17000, labels= paste("Pval=", round(summary(QUEAGR.MARR.LM)[[4]][2,4],4)), pos=4)


with(Pts.PSEMEN, plot(MarchRadiation,Basal.Area_cm2))
with(Pts.AMOCAL, plot(MarchRadiation,Basal.Area_cm2))

#### March Radiation LM
QUEGAR.MARR.LM<-lm(Basal.Area_cm2~MarchRadiation, data=Pts.QUEGAR)
summary(QUEGAR.MARR.LM) 

QUEDOU.MARR.LM<-lm(Basal.Area_cm2~MarchRadiation, data=Pts.QUEDOU)
summary(QUEDOU.MARR.LM)  

QUEAGR.MARR.LM<-lm(Basal.Area_cm2~MarchRadiation, data=Pts.QUEAGR)
summary(QUEAGR.MARR.LM)  #--> Significant 

PSEMEN.MARR.LM<-lm(Basal.Area_cm2~MarchRadiation, data=Pts.PSEMEN)
summary(PSEMEN.MARR.LM)  

AMOCAL.MARR.LM<-lm(Basal.Area_cm2~MarchRadiation, data=Pts.AMOCAL)
summary(AMOCAL.MARR.LM) 

### July Max Temp Plots
with(Total.Pts, plot(TMaxJUL,Basal.Area_cm2))
with(Pts.QUEGAR, plot(TMaxJUL,Basal.Area_cm2)) 
with(Pts.QUEDOU, plot(TMaxJUL,Basal.Area_cm2))
with(Pts.QUEKEL, plot(TMaxJUL,Basal.Area_cm2, main = "QUEKEL"))

yvals3<-predict(QUEKEL.TMAXJ.LM)
lines(Pts.QUEKEL$TMaxJUL,yvals3)
text(29.8,6000, labels= paste("Pval=", round(summary(QUEKEL.TMAXJ.LM)[[4]][2,4],4)), pos=4)


with(Pts.QUEAGR, plot(TMaxJUL,Basal.Area_cm2))
with(Pts.PSEMEN, plot(TMaxJUL,Basal.Area_cm2))

#### July Max Temp LM
QUEGAR.TMAXJ.LM<-lm(Basal.Area_cm2~TMaxJUL, data=Pts.QUEGAR)
summary(QUEGAR.TMAXJ.LM)  
plot(QUEGAR.TMAXJ.LM)

QUEDOU.TMAXJ.LM<-lm(Basal.Area_cm2~TMaxJUL, data=Pts.QUEDOU)
summary(QUEDOU.TMAXJ.LM) 

QUEKEL.TMAXJ.LM<-lm(Basal.Area_cm2~TMaxJUL, data=Pts.QUEKEL)
summary(QUEKEL.TMAXJ.LM) # --> significant

QUEAGR.TMAXJ.LM<-lm(Basal.Area_cm2~TMaxJUL, data=Pts.QUEAGR)
summary(QUEAGR.TMAXJ.LM) 

PSEMEN.TMAXJ.LM<-lm(Basal.Area_cm2~TMaxJUL, data=Pts.PSEMEN)
summary(PSEMEN.TMAXJ.LM)

AMOCAL.TMAXJ.LM<-lm(Basal.Area_cm2~TMaxJUL, data=Pts.AMOCAL)
summary(AMOCAL.TMAXJ.LM)

### Min Jan Temp Plots

with(Total.Pts, plot(tminJAN,Basal.Area_cm2))
with(Pts.QUEGAR, plot(tminJAN,Basal.Area_cm2))
with(Pts.QUEDOU, plot(tminJAN,Basal.Area_cm2))
with(Pts.QUEKEL, plot(tminJAN,Basal.Area_cm2, main="QUEKEL"))
with(Pts.QUEAGR, plot(tminJAN,Basal.Area_cm2))
with(Pts.PSEMEN, plot(tminJAN,Basal.Area_cm2))
with(Pts.AMOCAL, plot(tminJAN,Basal.Area_cm2))


### Min Jan Temp LM

QUEGAR.TMinJAN.LM<-lm(Basal.Area_cm2~tminJAN, data=Pts.QUEGAR)
summary(QUEGAR.TMinJAN.LM)  
plot(QUEGAR.TMinJAN.LM)

QUEDOU.TMinJAN.LM<-lm(Basal.Area_cm2~tminJAN, data=Pts.QUEDOU)
summary(QUEDOU.TMinJAN.LM) 
plot(QUEDOU.TMinJAN.LM)

QUEAGR.TMinJAN.LM<-lm(Basal.Area_cm2~tminJAN, data=Pts.QUEAGR)
summary(QUEAGR.TMinJAN.LM)

PSEMEN.TMinJAN.LM<-lm(Basal.Area_cm2~tminJAN, data=Pts.PSEMEN)
summary(PSEMEN.TMinJAN.LM) 

QUEKEL.TMinJAN.LM<-lm(Basal.Area_cm2~tminJAN, data=Pts.QUEKEL)
summary(QUEKEL.TMinJAN.LM)
plot((QUEKEL.TMinJAN.LM))

AMOCAL.TMinJAN.LM<-lm(Basal.Area_cm2~tminJAN, data=Pts.AMOCAL)
summary(AMOCAL.TMinJAN.LM)
plot((AMOCAL.TMinJAN.LM))

###### WD Plots

with(Total.Pts, plot(WD,Basal.Area_cm2, main="All Species"))
with(Pts.QUEGAR, plot(WD,Basal.Area_cm2, main = "QUEGAR"))
with(Pts.QUEDOU, plot(WD,Basal.Area_cm2, main="QUEDOU"))
with(Pts.QUEKEL, plot(WD,Basal.Area_cm2, main="QUEKEL"))
with(Pts.QUEAGR, plot(WD,Basal.Area_cm2, main= "QUEAGR"))
with(Pts.PSEMEN, plot(WD,Basal.Area_cm2, main= "PSEMEN"))
with(Pts.AMOCAL, plot(WD,Basal.Area_cm2, main= "AMOCAL"))

###### WD LM
QUEGAR.WD.LM<-lm(Basal.Area_cm2~WD, data=Pts.QUEGAR)
summary(QUEGAR.WD.LM)  
plot(QUEGAR.WD.LM)

QUEDOU.WD.LM<-lm(Basal.Area_cm2~WD, data=Pts.QUEDOU)
summary(QUEDOU.WD.LM) 
plot(QUEDOU.WD.LM)

QUEAGR.WD.LM<-lm(Basal.Area_cm2~WD, data=Pts.QUEAGR)
summary(QUEAGR.WD.LM)

PSEMEN.WD.LM<-lm(Basal.Area_cm2~WD, data=Pts.PSEMEN)
summary(PSEMEN.WD.LM) 

QUEKEL.WD.LM<-lm(Basal.Area_cm2~WD, data=Pts.QUEKEL)
summary(QUEKEL.WD.LM)
plot((QUEKEL.WD.LM))


AMOCAL.WD.LM<-lm(Basal.Area_cm2~WD, data=Pts.AMOCAL)
summary(AMOCAL.WD.LM)

####### Total Biomass with Many Models --> Nothing significant
Sum.Pts<-with(Total.Pts, aggregate(Basal.Area_cm2~Plot.ID, FUN=sum))
head(Sum.Pts)

# DEM
Sum.DEM.Pts<- merge(Sum.Pts, dem.pts)
head(Sum.DEM.Pts)
with(Sum.DEM.Pts, plot(DEM,Basal.Area_cm2))

Sum.DEM.lm<-lm(Basal.Area_cm2~DEM, data=Sum.DEM.Pts) 
summary(Sum.DEM.lm)

# March Radiation
Sum.Marrad.Pts<- merge(Sum.Pts, marrad.pts)
head(Sum.Marrad.Pts)
with(Sum.Marrad.Pts, plot(MarchRadiation,Basal.Area_cm2))

# Max July Temp 
Sum.TMax.Pts<- merge(Sum.Pts,tmaxJUL.pts)
head(Sum.TMax.Pts)
with(Sum.TMax.Pts, plot(TMaxJUL,Basal.Area_cm2))

#TPI
Sum.TMax.Pts<- merge(Sum.Pts,tpi500.pts)
head(Sum.TMax.Pts)
with(Sum.TMax.Pts, plot(,Basal.Area_cm2))


### tpi500 MODEL PLOTS
with(Total.Pts, plot(tpi500,Basal.Area_cm2, main="All Species"))
with(Pts.QUEGAR, plot(tpi500,Basal.Area_cm2, main="QUEGAR"))
with(Pts.QUEDOU, plot(tpi500,Basal.Area_cm2, main = "QUEDOU"))
with(Pts.QUEKEL, plot(tpi500,Basal.Area_cm2, main = "QUEKEL"))

yvals3<-predict(QUEKEL.tpi500.LM)
lines(Pts.QUEKEL$tpi500,yvals3)
text(0,8000, labels= paste("Pval=", round(summary(QUEKEL.tpi500.LM)[[4]][2,4],4)), pos=4)

with(Pts.QUEAGR, plot(tpi500,Basal.Area_cm2, main = "QUEAGR"))
with(Pts.PSEMEN, plot(tpi500,Basal.Area_cm2, main = "PSEMEN"))
with(Pts.AMOCAL, plot(tpi500,Basal.Area_cm2, main = "AMOCAL"))


### tpi500 MODEL LM

QUEGAR.tpi500.LM<-lm(Basal.Area_cm2~tpi500, data=Pts.QUEGAR)
summary(QUEGAR.tpi500.LM)

QUEDOU.tpi500.LM<-lm(Basal.Area_cm2~tpi500, data=Pts.QUEDOU)
summary(QUEDOU.tpi500.LM)

QUEAGR.tpi500.LM<-lm(Basal.Area_cm2~tpi500, data=Pts.QUEAGR)
summary(QUEAGR.tpi500.LM)


QUEKEL.tpi500.LM<-lm(Basal.Area_cm2~tpi500, data=Pts.QUEKEL)
summary(QUEKEL.tpi500.LM) # THIS IS SIG
with(Pts.QUEKEL, plot(tpi500,Basal.Area_cm2, main = "QUEKEL"))

PSEMEN.tpi500.LM<-lm(Basal.Area_cm2~tpi500, data=Pts.PSEMEN)
summary(PSEMEN.tpi500.LM) # this is significant
with(Pts.PSEMEN, plot(tpi500,Basal.Area_cm2, main="PSEMEN"))

AMOCAL.tpi500.LM<-lm(Basal.Area_cm2~tpi500, data=Pts.AMOCAL)
summary(AMOCAL.tpi500.LM)

#######  CORRELATIONS??? 
plot(Total.Pts$tminJAN, Total.Pts$TMaxJUL)
plot(Total.Pts$DEM, Total.Pts$MarchRadiation)







######## Multiple Regressions ---> Nothing significant

QUEDOU.Multi<-lm(Basal.Area_cm2 ~ tminJAN * TMaxJUL * WD, data=Pts.QUEDOU)
summary(QUEDOU.Multi)

QUEGAR.Multi<-lm(Basal.Area_cm2 ~ tminJAN * TMaxJUL *WD, data=Pts.QUEGAR)
summary(QUEGAR.Multi)

QUEKEL.Multi<-lm(Basal.Area_cm2 ~ tminJAN * TMaxJUL * WD, data=Pts.QUEKEL)
summary(QUEKEL.Multi)

QUEAGR.Multi<-lm(Basal.Area_cm2 ~ tminJAN * TMaxJUL * WD, data=Pts.QUEAGR)
summary(QUEAGR.Multi)

PSEMEN.Multi<-lm(Basal.Area_cm2 ~ tminJAN * TMaxJUL * WD, data=Pts.PSEMEN)
summary(PSEMEN.Multi)

AMOCAL.Multi<-lm(Basal.Area_cm2 ~ tminJAN * TMaxJUL * WD, data=Pts.AMOCAL)
summary(AMOCAL.Multi)

############################################
QUEDOU.Multi<-lm(Basal.Area_cm2 ~ DEM * MarchRadiation * WD, data=Pts.QUEDOU)
summary(QUEDOU.Multi)

QUEGAR.Multi<-lm(Basal.Area_cm2 ~ DEM * MarchRadiation * WD, data=Pts.QUEGAR)
summary(QUEGAR.Multi)

QUEAGR.Multi<-lm(Basal.Area_cm2 ~ DEM * MarchRadiation * WD, data=Pts.QUEAGR)
summary(QUEAGR.Multi)


PSEMEN.Multi<-lm(Basal.Area_cm2 ~ DEM * MarchRadiation * WD, data=Pts.PSEMEN)
summary(PSEMEN.Multi)

AMOCAL.Multi<-lm(Basal.Area_cm2 ~ DEM * MarchRadiation * WD, data=Pts.AMOCAL)
summary(AMOCAL.Multi)
