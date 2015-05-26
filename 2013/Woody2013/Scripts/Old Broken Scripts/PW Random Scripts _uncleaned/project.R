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
plot(dem)
projection(dem)

dir("PW10m_topoclimate")
marrad <- raster('PW10m_topoclimate/marrad.asc')
plot(marrad)

dir('PPshapefiles/PPshapefile-teale-albers')
PP <- readShapeSpatial('PPshapefiles/PPshapefile-teale-albers/Pepperwood')
plot(PP,add=T)
projection(PP)

plot(PP)
PPex <- extent(PP)

plot.data<-lapply(file.list, read.csv, skip=5, nrows=5, header=F)
head(plot.data) 

str(plot.info)
long <- as.numeric(plot.info$UTM.E)
lat <- as.numeric(plot.info$UTM.N)
plotsSP <- SpatialPoints(data.frame(long,lat),proj4string <- CRS(utm.project))
plotsSP.ta <- spTransform(plotsSP,CRS(ta.project))

plot(crop(dem,PPex))
plot(PP,add=T)
plot(plotsSP.ta,add=T,pch=19)

plot(crop(marrad,PPex))
plot(PP,add=T)
plot(plotsSP.ta,add=T,pch=19)

extract(dem,plotsSP.ta)