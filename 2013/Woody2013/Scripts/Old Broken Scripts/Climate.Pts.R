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
# plot(marrad, main = "March Radiation")
# plot(tmaxJUL, main = "T Max July")
# plot(tminJAN, main = " T Min Jan ")
# tmaxJUL
#dir('PPshapefiles/PPshapefile-teale-albers')
PP <- readShapeSpatial('PPshapefiles/PPshapefile-teale-albers/Pepperwood')
#projection(PP)
PPex <- extent(PP)

# tmaxJULpts=getValues(crop(tmaxJUL,PP))
# dempts=getValues(crop(dem,PP))
# tminJANpts=getValues(crop(tminJAN,PP))
# plot(dempts,tmaxJULpts,pch='.')

topoidx <- raster('PW10m_topoclimate/topoidx.asc')
#plot(crop(topoidx,PPex))
tpi500 <- raster('PW10m_topoclimate/tpi_500.asc')
#plot(crop(tpi500,PPex))

# plot(getValues(crop(tpi500,PPex)),tmaxJULpts,pch='.')
# plot(getValues(crop(tpi500,PPex)),tminJANpts,pch='.')
# plot(dempts,tminJANpts,pch='.')
# plot(dempts,getValues(crop(tpi500,PPex)),pch='.')
# plot(getValues(crop(marrad,PPex)),tmaxJULpts,pch='.')


# source("/Users/meaganoldfather/Dropbox/PepperwoodVegPlots/Database/2013/Woody2013/Scripts/Plot.Info.All.R")
# # Do not worry about the plots that the above scripts try to make --> ignore and press enter
# # Eventually take out ordination plots from the above script and make a new one specifically for that only
# head(Plot.Info.All)
# Cover.All<-aggregate(Plot.Info.All$Basal.Area_cm2~Plot.Info.All$Plot.ID+Plot.Info.All$Species, FUN=sum)
# head(Cover.All)
# Num.All<-aggregate(Plot.Info.All$Number~Plot.Info.All$Plot.ID+Plot.Info.All$Species, FUN=sum) 
# head(Num.All)
# 
# Total<-merge(Cover.All,Num.All)
# head(Total)
# names(Total)<-c("Plot.ID", "Species", "Basal.Area_cm2", "Number")
# head(Total)

###### NEED TO USE plot.TR, plot.SA, plot.SA.TR ... start with combined data


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
hydro<-read.csv("/Users/meaganoldfather/Dropbox/PepperwoodVegPlots/Database/2013/Hydro2013/Data/OriginalCSV/SoilMoisture2013.csv")
hydro<-hydro[,c(1,5)]
colnames(hydro)<- c("Plot.ID", "WD")
hydro<-aggregate(hydro$WD ~ hydro$Plot.ID, FUN=mean)

colnames(hydro)<- c("Plot", "Soil.Moisture")
head(hydro)

hist(hydro$Soil.Moisture, main= "Histogram of  Field Soil Moisture (Hydrosense)", xlab="Relative Soil Moisture")

climate.pts<- merge(climate.pts,hydro)  
head(climate.pts) 
