# Lichens!
# MFO
# Created: 20150820
# Last Edited: 20151023

# clear workspace
rm(list=ls())

#Packages
library("RCurl")
library("data.table")
library("picante")
library(colortools)
library(plotrix)

# sources in all functions
source_https <- function(url, ...) { 
  # parse and evaluate each .R script
  sapply(c(url, ...), function(u) {
    eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
  })
}
source_https('https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/Analyses/PWfunctions_GitHub.R')

# get plot environmental data
clim<-get.clim.pts()
head(clim)

# read in lichen data
lichen<- read.csv("Pepperwood/GitDatabase/2013/Herb2013/Data/OriginalCSV/LICHENDATA.csv")

lichen<- cbind(clim,lichen[,-1])
#write.csv(lichen, "Desktop/TBC3vegplotLichen.csv")
with(lichen, plot(DEM, RAMALINA))
with(lichen, plot(DEM, USNEA))
with(lichen, plot(DEM, CRUSTOSE.FOLIOSE)) 

# make maps for presence/absence of all lichen types on DEM raster
library(raster)
library(maptools)
ta.project = '+proj=aea +datum=NAD83 +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000'
utm.project = '+proj=utm +zone=10S +datum=WGS84' # CHECK GPS

dem <- raster('Pepperwood/GISRasters/DEM/pw_10m_t2.asc')
PP <- readShapeSpatial('Pepperwood/GISRasters/PPshapefiles/PPshapefile-teale-albers/Pepperwood')
PPex <- extent(PP)

demPW<-crop(dem,PP)
plot(demPW)
plot(PP,add=T)

# tranform plot points 
long <- as.numeric(lichen$UTM.E)
lat <- as.numeric(lichen$UTM.N)
plotsSP <- SpatialPoints(data.frame(long,lat),proj4string <- CRS(utm.project))
plotsSP.ta <- spTransform(plotsSP,CRS(ta.project))

# make color scale
col.scale<-c("white","black")
# absence = black , presence = white 


pdf("LichenPlots.pdf")

# RAMALINA
plot(demPW, main="RAMALINA")
plot(PP,add=T)
points(plotsSP.ta, col=col.scale[lichen$RAMALINA+1],pch=19)
legend("topright", legend=c("Absent", "Present"), fill=c(col.scale)  )

# USNEA
plot(demPW, main="USNEA")
plot(PP,add=T)
points(plotsSP.ta, col=col.scale[lichen$USNEA+1],pch=19)
legend("topright", legend=c("Absent", "Present"), fill=c(col.scale)  )

# CRUSTOSE.FOLIOSE
plot(demPW, main= "CRUSTOSE.FOLIOSE")
plot(PP,add=T)
points(plotsSP.ta, col=col.scale[lichen$CRUSTOSE.FOLIOSE+1],pch=19)
legend("topright", legend=c("Absent", "Present"), fill=c(col.scale)  )

dev.off()

# simple models
summary(glm(RAMALINA~DEM, data=lichen, family="binomial"))
summary(glm(USNEA~DEM, data=lichen, family="binomial"))
