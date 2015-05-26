# Intent: Creates a data frame ("climate.pts") that contains all the climate values for each PW woody veg plot for the following layers:
#DEM, MarchRadiation, TMaxJUL, TminJAN, Tpi500, Soil.Moisture
# Also creates a dataframe ("bioclim") that merges plot climate data basal.area/number of each species  
# Author: M.F. Oldfather
# Date Created: 20130901 
# Date Last Edited: 20141020

# necessary packages
library(sp)
library(maptools)
library(raster)

# projection strings
ta.project = '+proj=aea +datum=NAD83 +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000'
utm.project = '+proj=utm +zone=10S +datum=WGS84

# use the below script for multiple users, enter your own pathway for setting the working directory in the open brackets after "else"
if (Sys.getenv("USER")=='meaganoldfather') setwd("/Users/meaganoldfather/Box Sync/PepperwoodVegPlots/Database/GIS") else setwd("")

# PW DEM raster
dem <- raster('DEM/pw_10m_t2.asc')
# PW March Radiation raster
marrad <- raster('PW10m_topoclimate/marrad.asc')
# PW Max July Temp raster
tmaxJUL<- raster('PW10m_topoclimate/tmxavejul.asc')
# PW Min July Temp raster
tminJAN<- raster ('PW10m_topoclimate/tmnavejan.asc')
# PW tpi 
topoidx <- raster('PW10m_topoclimate/topoidx.asc')
# PW tpi at 500 meters
tpi500 <- raster('PW10m_topoclimate/tpi_500.asc')


# Pepperwood Preserve outline, used for cropping
PP <- readShapeSpatial('PPshapefiles/PPshapefile-teale-albers/Pepperwood')
PPex <- extent(PP)

# Get values for all Pepperwood pts
Dempts<-getValues(crop(dem,PP))
MarRadpts<-getValues(crop(marrad,PP))
TmaxJULpts<-getValues(crop(tmaxJUL,PP))
TminJANpts<-getValues(crop(tminJAN,PP))
Topopts<-getValues(crop(topoidx,PP))
TPIpts<-getValues(crop(tpi500,PP))

# some exploratory plots about how the  vaibles correlate across the preserve
plot(Dempts,TminJANpts,pch='.')
plot(Dempts,TmaxJULpts,pch='.')
plot(TmaxJULpts,TminJANpts,pch='.')
plot(TPIpts, TminJANpts,pch='.')


# source 
source("/Users/meaganoldfather/Box Sync/PepperwoodVegPlots/Database/Analyses/MasterData.R")

# Get the coordinates for each plot, transform from UTM
long <- as.numeric(plot.info$UTM.E)
lat <- as.numeric(plot.info$UTM.N)
plotsSP <- SpatialPoints(data.frame(long,lat),proj4string <- CRS(utm.project))
plotsSP.ta <- spTransform(plotsSP,CRS(ta.project))
# Spatial Points for all of the plots
plotsSP.ta



# this needs to done with a function...

####### DEM MODEL Extract 
#plot(crop(dem,PPex), main= "DEM") # plots dem model on extended area around PW
#plot(PP,add=T) # add outline of PW 
#plot(plotsSP.ta,add=T,pch=19) # adds point at plot location

dem.pts<-extract(dem,plotsSP.ta) # extracts dem values for plots locations
head(dem.pts)
dem.pts<-as.data.frame(cbind(plot.list,dem.pts))
head(dem.pts)
colnames(dem.pts)<- c("Plot","DEM")
head(dem.pts)
dem.pts$DEM<-as.numeric(dem.pts$DEM)
hist(dem.pts$DEM, main="DEM Histogram", xlab="DEM")

climate.pts<- dem.pts
head(climate.pts) 

#### March Radiation Model Extraction
#plot(crop(marrad,PPex), main= "March Radiation") # plots march radiation model on extended area around PW
#plot(PP,add=T)
#plot(plotsSP.ta,add=T,pch=19)

marrad.pts<-extract(marrad,plotsSP.ta) # extracts march radiation values for plots locations
marrad.pts
marrad.pts<-as.data.frame(cbind(plot.list,marrad.pts))
head(marrad.pts)
colnames(marrad.pts)<- c("Plot","MarchRadiation")
head(marrad.pts)
marrad.pts$MarchRadiation<-as.numeric(marrad.pts$MarchRadiation)
hist(marrad.pts$MarchRadiation, main="March Radiation Histogram", xlab="March Radiation")

climate.pts<- merge(climate.pts,marrad.pts)  
head(climate.pts) 

#### Max July Temp Model 
#plot(crop(tmaxJUL,PPex), main ="Max Temp July") # plots max July T on extended area around PW
#plot(PP,add=T)
#plot(plotsSP.ta,add=T,pch=19)

tmaxJUL.pts<-extract(tmaxJUL,plotsSP.ta) # extracts march radiation values for plots locations
tmaxJUL.pts
tmaxJUL.pts<-as.data.frame(cbind(plot.list,tmaxJUL.pts))
head(tmaxJUL.pts)
colnames(tmaxJUL.pts)<- c("Plot","TMaxJUL")
head(tmaxJUL.pts)
tmaxJUL.pts$TMaxJUL<-as.numeric(tmaxJUL.pts$TMaxJUL)

hist(tmaxJUL.pts$TMaxJUL, main ="Histogram of Max Temp July", xlab="Max Temp July C" )

climate.pts<- merge(climate.pts,tmaxJUL.pts)  
head(climate.pts)  

#### Min Jan Temp Model Extraction
#plot(crop(tminJAN,PPex), main="Min Temp January") # plots Min Jan Temp on extended area around PW
#plot(PP,add=T)
#plot(plotsSP.ta,add=T,pch=19)

tminJAN.pts<-extract(tminJAN,plotsSP.ta) # extracts min JAN Temp values for plots locations
tminJAN.pts
tminJAN.pts<-as.data.frame(cbind(plot.list,tminJAN.pts))
head(tminJAN.pts)
colnames(tminJAN.pts)<- c("Plot","tminJAN")
head(tminJAN.pts)
tminJAN.pts$tminJAN<-as.numeric(tminJAN.pts$tminJAN)
hist(tminJAN.pts$tminJAN, main="Histogram of Min Temp January", xlab="Min Temp January")

climate.pts<- merge(climate.pts,tminJAN.pts)  
head(climate.pts) 

######## TPI5000
#plot(crop(tpi500,PPex), main="TPI") # plots TPI on extended area around PW
#plot(PP,add=T)
#plot(plotsSP.ta,add=T,pch=19)

tpi500.pts<-extract(tpi500,plotsSP.ta) # extracts min JAN Temp values for plots locations
tpi500.pts
tpi500.pts<-as.data.frame(cbind(plot.list,tpi500.pts))
head(tpi500.pts)
colnames(tpi500.pts)<- c("Plot","tpi500")
head(tpi500.pts)
tpi500.pts$tpi500<-as.numeric(tpi500.pts$tpi500)
hist(tpi500.pts$tpi500, main="Histogram of TPI", xlab="Percent Lower Pixels")

climate.pts<- merge(climate.pts,tpi500.pts)  
head(climate.pts) 

######### SOIL MOISTURE 
# use the below script for multiple users, enter your own pathway for setting the working directory in the open brackets after "else"
if (Sys.getenv("USER")=='meaganoldfather') setwd("/Users/meaganoldfather/Box Sync/PepperwoodVegPlots/Database/2013/Hydro2013/Data/OriginalCSV/") else setwd("")

hydro<-read.csv("SoilMoisture2013.csv")
hydro<-hydro[,c(1,5)]
colnames(hydro)<- c("Plot.ID", "SM")
hydro<-aggregate(hydro$SM ~ hydro$Plot.ID, FUN=mean)
colnames(hydro)<- c("Plot", "Soil.Moisture")
head(hydro)

hist(hydro$Soil.Moisture, main= "Histogram of  Field Soil Moisture (Hydrosense)", xlab="Relative Soil Moisture")

climate.pts<- merge(climate.pts,hydro)  
head(climate.pts) 

bioclim<-merge(climate.pts , plot.SA.TR, by="Plot")
head(bioclim)


