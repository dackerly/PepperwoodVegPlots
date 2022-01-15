## make polygons for veg plots

# We had three different sources for plot corners
# original points from Trimble at 50 veg plots, at all four corners
# Prahlad redid the superplots, and a few hectares
# Melina got all 4 corners of 4 new 2018 plots

# First check is whether the edges are in fact 20 m long, based on Trimble. Pretty much variation! Prahlad's are closer. But recall the vegplots were put in using magnetic north. So I used Prahlad's to get an average across the superplots of the delta-x and delta-y aligned with magnetic north, and then adjusted these a bit to get exactly 20 m edges

# these adjusted values were then added to the ORIGINAL SW corners of all 50 plots (i.e. not using Prahlad's updated values, even though they seem more accurate). The reason is that the hectares were put in at -(40,40) from the original locations, not the values PP captured later.

# Someday we should go in with high resolution Trimble and get better corners!

rm(list=ls())
library(sf)
library(sp)
library(rgdal)
library(maptools)
library(raster)

distance=function(vec) {
  x1 <- vec[1]
  x2 <- vec[3]
  y1 <- vec[2]
  y2 <- vec[4]
  sqrt((x1-x2)^2+(y1-y2)^2)
}

## look at mag north offsets from original 50 plot data
v50 <- read.csv('PWD_veg_plots_geospatial/data/Plot_four_corners_Papper.csv')
head(v50)

v50[which.min(v50$NW_N),]

v50$SW2NW <- apply(v50[,2:5],1,distance)
v50$NW2NE <- apply(v50[,4:7],1,distance)
v50$NE2SE <- apply(v50[,6:9],1,distance)
v50$SE2SW <- apply(v50[,c(2,3,8,9)],1,distance)
head(v50)
hist(v50$SW2NW)
hist(v50$NW2NE)
hist(v50$NE2SE)
hist(v50$SE2SW)

# big outlier
which.max(v50$NW2NE)
## now look at the superplots - were these done more carefully?
vS <- read.csv('PWD_veg_plots_geospatial/data/vegplot_corners_corrected.csv')
head(vS)

names(vS)
vS$Plot
vS$SW2NW <- apply(vS[,c(3,4,7,8)],1,distance)
vS$NW2NE <- apply(vS[,c(7,8,9,10)],1,distance)
vS$NE2SE <- apply(vS[,c(5,6,9,10)],1,distance)
vS$SE2SW <- apply(vS[,c(3,4,5,6)],1,distance)
head(vS)

sup <- grep('SUPER',vS$Plot)
veg <- which(!1:23 %in% sup)
hist(vS$SW2NW[sup])
hist(vS$NW2NE[sup])
hist(vS$NE2SE[sup])
hist(vS$SE2SW[sup])

hist(vS$SW2NW[veg])
hist(vS$NW2NE[veg])
hist(vS$NE2SE[veg])
hist(vS$SE2SW[veg])

# SO, Prahlad's corrected superplots are more accurate. Use these to derived a standardized offset for a 20 m line, corresponding to magnetic north, and then construct a synthetic set of points that are exactly 20x20

vS$NWn.SWn <- vS$NW.UTM.N-vS$SW.UTM.N
vS$NWe.SWe <- vS$NW.UTM.E-vS$SW.UTM.E
vS$NEn.NWn <- vS$NW.UTM.N-vS$NE.UTM.N
vS$NEe.NWe <- vS$NE.UTM.E-vS$NW.UTM.E

vS$NEn.SEn <- vS$NE.UTM.N-vS$SE.UTM.N
vS$NEe.SEe <- vS$NE.UTM.E-vS$SE.UTM.E
vS$SEn.SWn <- vS$SW.UTM.N-vS$SE.UTM.N
vS$SEe.SWe <- vS$SE.UTM.E-vS$SW.UTM.E

hist(c(vS$NWn.SWn[veg],vS$NEn.SEn[veg],vS$NEe.NWe[veg],vS$SEe.SWe[veg]))
hist(c(vS$NWe.SWe[veg],vS$NEe.SEe[veg],vS$NEn.NWn[veg],vS$SEn.SWn[veg]))
(nD1 <- mean(c(vS$NWn.SWn[veg],vS$NEn.SEn[veg],vS$NEe.NWe[veg],vS$SEe.SWe[veg])))
(eD1 <- mean(c(vS$NWe.SWe[veg],vS$NEe.SEe[veg],vS$NEn.NWn[veg],vS$SEn.SWn[veg])))

#adjust to get 20m
fac <- 1.036
sqrt((nD1*fac)^2+(eD1*fac)^2)

# best estimate of linear offsets to calculate veg plot corners, following magnetic north
(nDadj <- nD1*fac)
(eDadj <- eD1*fac)

## now swap in Prahlad's coordinates for superplots
head(vS)
head(v50)

v50a <- v50

# If desired, use Prahlad's coordinates where available
# sr <- match(vS$Plot,v50$Plot)
# sr <- sr[-which(is.na(sr))]
# all(v50a$Plot[sr]==vS$Plot[veg])

#v50a$SW_E[sr] <- vS$SW.UTM.E[veg]
#v50a$SW_N[sr] <- vS$SW.UTM.N[veg]

# Now add SW corners for new plots
new4 <- read.csv('PWD_veg_plots_geospatial/data/PWD_New site locations_2018.csv')
names(new4)[2:3] <- c('E','N')
newplots <- paste('PPW',1851:1854,sep='')

v50a <- rbind(v50a,v50a[1:4,])
v50a$Plot[51:54] <- newplots
v50a$SW_E[51] <- new4$E[1]
v50a$SW_N[51] <- new4$N[1]
v50a$SW_E[52] <- new4$E[5]
v50a$SW_N[52] <- new4$N[5]
v50a$SW_E[53] <- new4$E[9]
v50a$SW_N[53] <- new4$N[9]
v50a$SW_E[54] <- new4$E[13]
v50a$SW_N[54] <- new4$N[13]

#now added calculated offsets to create exactly 20 x 20 plots
v50a$NW_E <- v50a$SW_E + eDadj
v50a$NW_N <- v50a$SW_N + nDadj
v50a$NE_E <- v50a$NW_E + nDadj
v50a$NE_N <- v50a$NW_N - eDadj
v50a$SE_E <- v50a$NE_E - eDadj
v50a$SE_N <- v50a$NE_N - nDadj

# check adjustments result in 20 m edges
v50a$SW2NW <- apply(v50a[,2:5],1,distance)
v50a$NW2NE <- apply(v50a[,4:7],1,distance)
v50a$NE2SE <- apply(v50a[,6:9],1,distance)
v50a$SE2SW <- apply(v50a[,c(2,3,8,9)],1,distance)
v50a
# all good!

head(v50a)
write.csv(v50a[,1:9],'plot.info/Plot_four_corners_updated.csv')

# now make shapefiles for 20x20 and 5x5
pcrds <- matrix(nrow = 5,ncol=2,data = NA)
names(v50a)
xxs <- list()
i=1
for (i in 1:nrow(v50a)) {
  #hs[i,]
  pcrds[1,1:2] <- as.numeric(v50a[i,c('SW_E','SW_N')])
  pcrds[2,1:2] <- as.numeric(v50a[i,c('NW_E','NW_N')])
  pcrds[3,1:2] <- as.numeric(v50a[i,c('NE_E','NE_N')])
  pcrds[4,1:2] <- as.numeric(v50a[i,c('SE_E','SE_N')])
  pcrds[5,1:2] <- pcrds[1,1:2]
  
  xx <- Polygon(coords = pcrds)
  xxs[[i]] <- Polygons(list(xx),ID = paste(v50a$Plot[i]))
}

class(xxs)
xxsp <- SpatialPolygons(xxs,proj4string=CRS('+proj=utm +zone=10'))
rownames(v50a) <- paste(v50a$Plot)
xxspd <- SpatialPolygonsDataFrame(xxsp,data = v50a)
plot(xxsp)
proj4string(xxspd)

xxspg <- spTransform(xxspd,CRS('+proj=longlat +datum=WGS84'))
xxspa <- spTransform(xxspd,CRS('+proj=aea +datum=NAD83 +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000'))

rgdal::writeOGR(xxspd,'plot.info/VegPlots-20m-utm/',driver='ESRI Shapefile',layer='Veg54-20')
rgdal::writeOGR(xxspa,'plot.info/VegPlots-20m-aea/',driver='ESRI Shapefile',layer='Veg54-20')
rgdal::writeOGR(xxspg,'plot.info/VegPlots-20m-geo/',driver='ESRI Shapefile',layer='Veg54-20')


# now make quad level shapefiles
# shorten the 20m adjustment factors to 5m vector
(ladj <- nDadj/4)
(sadj <- eDadj/4)

sc <- data.frame(col=rep(c('A','B','C','D'),each=4),row=rep(1:4,4),ndelta=rep(c(0,1,2,3),4)*ladj,edelta=rep(c(0,1,2,3)*ladj,each=4))
sc$edelta[1:4] <- 0:3*(1)*sadj
sc$ndelta[5:8] <- sc$ndelta[1:4] - sadj
sc$ndelta[9:12] <- sc$ndelta[5:8] - sadj
sc$ndelta[13:16] <- sc$ndelta[9:12] - sadj

sc$edelta[5:8] <- sc$edelta[1:4] + ladj
sc$edelta[9:12] <- sc$edelta[5:8] + ladj
sc$edelta[13:16] <- sc$edelta[9:12] + ladj

head(sc)
tail(sc)

vs <- data.frame(h=rep(1:54,each=16),name=rep(v50a$Plot,each=16),col=rep(sc$col,54),row=rep(sc$row,54),quad=NA,SWlon=NA,SWlat=NA,NWlon=NA,NWlat=NA,NElon=NA,NElat=NA,SElon=NA,SElat=NA)
vs$quad <- paste(vs$col,vs$row,sep='')
head(vs)

i=1
for (i in 1:54) {
  plot <- v50a$Plot[i]
  xx <- v50a[v50a$Plot==plot,]
  
  vs$SWlon[vs$name==plot] <- xx$SW_E + sc$edelta
  vs$SWlat[vs$name==plot] <- xx$SW_N + sc$ndelta
  
  
}

vs$NWlon <- vs$SWlon + sadj
vs$NWlat <- vs$SWlat + ladj
vs$NElon <- vs$NWlon + ladj
vs$NElat <- vs$NWlat - sadj
vs$SElon <- vs$NElon - sadj
vs$SElat <- vs$NElat - ladj
head(vs)
vs[1,]
str(vs)

plot(vs[1:16,c('SWlon','SWlat')],asp=1)
points(vs[1:16,c('NWlon','NWlat')],pch=19)
points(vs[1:16,c('NElon','NElat')],pch=19,col='red')
points(vs[1:16,c('SElon','SElat')],pch=19,col='green')

head(vs)

#now make each row into a polygon with the hectare and quad name!
pcrds <- matrix(nrow = 5,ncol=2,data = NA)

xxs <- list()
for (i in 1:nrow(vs)) {
  #hs[i,]
  pcrds[1,1:2] <- as.numeric(vs[i,c('SWlon','SWlat')])
  pcrds[2,1:2] <- as.numeric(vs[i,c('NWlon','NWlat')])
  pcrds[3,1:2] <- as.numeric(vs[i,c('NElon','NElat')])
  pcrds[4,1:2] <- as.numeric(vs[i,c('SElon','SElat')])
  pcrds[5,1:2] <- as.numeric(vs[i,c('SWlon','SWlat')])
  xx <- Polygon(coords = pcrds)
  xxs[[i]] <- Polygons(list(xx),ID = paste(vs$name[i],vs$quad[i]))
}

class(xxs)
xxsp <- SpatialPolygons(xxs,proj4string=CRS('+proj=utm +zone=10'))
rownames(vs) <- paste(vs$name,vs$quad)
xxspd <- SpatialPolygonsDataFrame(xxsp,data = vs)
plot(xxsp)
proj4string(xxspd)

xxspg <- spTransform(xxspd,CRS('+proj=longlat +datum=WGS84'))
xxspa <- spTransform(xxspd,CRS('+proj=aea +datum=NAD83 +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000'))

rgdal::writeOGR(xxspd,'plot.info/Vegplot54-5m-utm/',driver='ESRI Shapefile',layer='Vegplot54-5m')
rgdal::writeOGR(xxspa,'plot.info/Vegplot54-5m-aea/',driver='ESRI Shapefile',layer='Vegplot54-5m')
rgdal::writeOGR(xxspg,'plot.info/Vegplot54-5m-geo/',driver='ESRI Shapefile',layer='Vegplot54-5m')


# # examine superplot corners to see orientation
# s <- sf::read_sf('PWD_veg_plots_geospatial/data/Superplot corners (corrected).kml')
# ss <- as_Spatial(st_zm(s))
# plot(ss)
# length(ss)
# proj4string(ss)
# 
# ssAEA <- spTransform(ss,CRS('+proj=aea +datum=NAD83 +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000'))
# plot(ssAEA)
# head(coordinates(ssAEA))
# tail(coordinates(ssAEA))
# 
# plot(coordinates(ssAEA)[1:4,],asp=1,type='n')
# text(coordinates(ssAEA)[1:4,],label=as.character(1:4))
# 
# plot(coordinates(ssAEA)[5:8,],asp=1)
# 
# plot(coordinates(ssAEA)[9:16,],asp=1,type='n')
# text(coordinates(ssAEA)[9:16,],label=as.character(9:16))
# 
# 
# plot(coordinates(ssAEA)[17:20,],asp=1)
# plot(coordinates(ssAEA)[21:28,],asp=1)
# plot(coordinates(ssAEA)[29:36,],asp=1)
# plot(coordinates(ssAEA)[37:39,],asp=1)
# plot(coordinates(ssAEA)[40:43,],asp=1)
# plot(coordinates(ssAEA)[44:47,],asp=1)
# plot(coordinates(ssAEA)[48:51,],asp=1)
# plot(coordinates(ssAEA)[52:59,],asp=1)
# plot(coordinates(ssAEA)[60:63,],asp=1)
# plot(coordinates(ssAEA)[64:67,],asp=1)
# plot(coordinates(ssAEA)[68:71,],asp=1)
# plot(coordinates(ssAEA)[72:75,],asp=1)
# plot(coordinates(ssAEA)[76:79,],asp=1)
# plot(coordinates(ssAEA)[80:83,],asp=1)
# plot(coordinates(ssAEA)[84:91,],asp=1)
# 
# 
# sqrt((234780.7-234760.9)^2+(66132.72-66130.75)^2)
# 
# 
# 
# ######
# s <- sf::read_sf('data/Superplot corners (corrected).kml')
# plot(s)
# ss <- as_Spatial(st_zm(s))
# plot(ss)
# 
# ####
# v <- sf::read_sf('data/VegPlots.kml')
# vs <- as_Spatial(st_zm(v))
# plot(vs)
# vs$Name
