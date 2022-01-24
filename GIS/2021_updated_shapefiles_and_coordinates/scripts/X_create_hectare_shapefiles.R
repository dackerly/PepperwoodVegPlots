## make polygons for veg plots
rm(list=ls())
library(sf)
library(sp)
library(rgdal)
library(maptools)
library(raster)


# now the hectares
h <- sf::read_sf('PWD_veg_plots_geospatial/data/Hectare plots.kml')
hsg <- as_Spatial(st_zm(h))
plot(hsg)
proj4string(hsg)
hsg$Name
hsg$data

hsa <- spTransform(hsg,CRS('+proj=aea +datum=NAD83 +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000'))
plot(hsa)

hsu <- spTransform(hsg,CRS('+proj=utm +zone=10'))
plot(hsu)

hsa@polygons[[i]]@Polygons[[1]]@area

hsp <- data.frame(h=rep(1:17,each=4),name=rep(hsu$Name,each=4),c=rep(c('SW','NW','NE','SE'),17),lon=NA,lat=NA)
hsc <- data.frame(h=1:17,name=NA,area=NA,lon=NA,lat=NA)

i=2
for (i in 1:17) {
  rows <- (4*(i-1)+1):(4*(i-1)+4)
  hsp[rows,c('lon','lat')] <- hsu@polygons[[i]]@Polygons[[1]]@coords[1:4,]
  hsc$name[i] <- hsu$Name[i]
  hsc$area[i] <- hsu@polygons[[i]]@Polygons[[1]]@area
  hsc[i,c('lon','lat')] <- hsu@polygons[[i]]@Polygons[[1]]@coords[1,]
}
head(hsp)
tail(hsp)
head(hsc)
tail(hsc)

plot(hsp[1:4,c('lon','lat')],asp=1)

rgdal::writeOGR(hsu,'plot.info/Hectares-17-100m-utm/',driver='ESRI Shapefile',layer='hectares17')
rgdal::writeOGR(hsa,'plot.info/Hectares-17-100m-aea/',driver='ESRI Shapefile',layer='hectares17')
rgdal::writeOGR(hsg,'plot.info/Hectares-17-100m-geo/',driver='ESRI Shapefile',layer='hectares17')


### now create shapefiles with 20m subplots
sc <- data.frame(col=rep(c('V','W','X','Y','Z'),each=5),row=rep(1:5,5),ndelta=rep(c(0,20,40,60,80),5),edelta=rep(c(0,20,40,60,80),each=5))
head(sc)
tail(sc)

hs <- data.frame(h=rep(1:17,each=25),name=rep(hsu$Name,each=25),col=rep(sc$col,17),row=rep(sc$row,17),quad=NA,SWlon=NA,SWlat=NA,NWlon=NA,NWlat=NA,NElon=NA,NElat=NA,SElon=NA,SElat=NA)
hs$quad <- paste(hs$col,hs$row,sep='')
head(hs)

i=1
for (i in 1:17) {
  plot <- hsc$name[i]
  xx <- hsc[hsc$name==plot,]
  
  hs$SWlon[hs$name==plot] <- xx$lon + sc$edelta
  hs$SWlat[hs$name==plot] <- xx$lat + sc$ndelta
}
hs$NWlon <- hs$SWlon
hs$NWlat <- hs$SWlat + 20
hs$NElon <- hs$NWlon + 20
hs$NElat <- hs$NWlat
hs$SElon <- hs$NElon
hs$SElat <- hs$NElat-20
head(hs)
plot(hs[1:25,c('SWlon','SWlat')],asp=1)
points(hs[1:25,c('NWlon','NWlat')],pch=19)
points(hs[1:25,c('NElon','NElat')],pch=19,col='red')
points(hs[1:25,c('SElon','SElat')],pch=19,col='green')

head(hs)

#now make each row into a polygon with the hectare and quad name!
pcrds <- matrix(nrow = 5,ncol=2,data = NA)

xxs <- list()
for (i in 1:nrow(hs)) {
  #hs[i,]
  pcrds[1,1:2] <- as.numeric(hs[i,c('SWlon','SWlat')])
  pcrds[2,1:2] <- as.numeric(hs[i,c('NWlon','NWlat')])
  pcrds[3,1:2] <- as.numeric(hs[i,c('NElon','NElat')])
  pcrds[4,1:2] <- as.numeric(hs[i,c('SElon','SElat')])
  pcrds[5,1:2] <- as.numeric(hs[i,c('SWlon','SWlat')])
  xx <- Polygon(coords = pcrds)
  xxs[[i]] <- Polygons(list(xx),ID = paste(hs$name[i],hs$quad[i]))
}

class(xxs)
xxsp <- SpatialPolygons(xxs,proj4string=CRS('+proj=utm +zone=10'))
rownames(hs) <- paste(hs$name,hs$quad)
xxspd <- SpatialPolygonsDataFrame(xxsp,data = hs)
plot(xxsp)
proj4string(xxspd)

xxspg <- spTransform(xxspd,CRS('+proj=longlat +datum=WGS84'))
xxspa <- spTransform(xxspd,CRS('+proj=aea +datum=NAD83 +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000'))

rgdal::writeOGR(xxspd,'plot.info/Hectares-17-20m-utm/',driver='ESRI Shapefile',layer='hectares17-20')
rgdal::writeOGR(xxspa,'plot.info/Hectares-17-20m-aea/',driver='ESRI Shapefile',layer='hectares17-20')
rgdal::writeOGR(xxspg,'plot.info/Hectares-17-20m-geo/',driver='ESRI Shapefile',layer='hectares17-20')
rgdal::writeOGR(xxspg,'plot.info/Hectares-17-20m-kml',driver='KML',layer='hectares17-20')
