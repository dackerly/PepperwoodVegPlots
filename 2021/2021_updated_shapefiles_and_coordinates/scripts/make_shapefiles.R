## make polygons from four corner data
rm(list=ls())
library(sf)
library(sp)
library(rgdal)
library(maptools)
library(raster)

getwd()
x <- read.csv('2021/2021_updated_shapefiles_and_coordinates/adjusted_four_corners.updated_2021-06-01.csv')
head(x)

x$Plot
x$type <- 'veg20'
x$type[55:72] <- 'hectare'
x$nquad <- 16
x$nquad[55:72] <- 25

names(x)
rownames(x) <- x$Plot

plot_type <- 'hectare' #'veg20' #

pcrds <- matrix(nrow = 5,ncol=2,data = NA)
xxs <- list()
rowsel <- which(x$type == plot_type)
i=rowsel[1]
j <- 0
for (i in rowsel) {
  j <- j+1
  pcrds[1,] <- as.numeric(x[i,2:3])
  pcrds[2,] <- as.numeric(x[i,4:5])
  pcrds[3,] <- as.numeric(x[i,6:7])
  pcrds[4,] <- as.numeric(x[i,8:9])
  pcrds[5,] <- as.numeric(x[i,2:3])
  
  xx <- Polygon(coords = pcrds)
  xxs[[j]] <- Polygons(list(xx),ID = x$Plot[i])
}

class(xxs)
xxsp <- SpatialPolygons(xxs,proj4string=CRS('+proj=utm +zone=10'))
pu <- SpatialPolygonsDataFrame(xxsp,data = x[rowsel,])
plot(pu)
proj4string(pu)

pa <- spTransform(pu,CRS('+proj=aea +datum=NAD83 +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000'))
pg <- spTransform(pu,CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

if (plot_type=='veg20') filename <- 'vegplots-54-20m' else filename <- 'hectares-18-100m'

rgdal::writeOGR(pu,paste('plot.info/2021 updated shapefiles and coordinates/shapefiles/',filename,'-utm/',sep=''),driver='ESRI Shapefile',layer=filename)
rgdal::writeOGR(pa,paste('plot.info/2021 updated shapefiles and coordinates/shapefiles/',filename,'-aea/',sep=''),driver='ESRI Shapefile',layer=filename)
rgdal::writeOGR(pg,paste('plot.info/2021 updated shapefiles and coordinates/shapefiles/',filename,'-geo/',sep=''),driver='ESRI Shapefile',layer=filename)


# now make quad level shapefiles
# shorten the 20m adjustment factors to 5m vector
# given two points in x,y space, and a number of intervals to divide up the line between them, calculate the x,y coordinates of the points along the line
interpoints <- function(x1,y1,x2,y2,nintval) {
  dx <- x2-x1
  dy <- y2-y1
  xp <- x1 + dx*(0:nintval)/nintval
  yp <- y1 + dy*(0:nintval)/nintval
  return(matrix(c(xp,yp),ncol=2))
}
#interpoints(0,2,8,8,4)

plot_type <- 'hectare' #'veg20' #'hectare'
if (plot_type == 'veg20') {
  np <- 54
  nq <- 16 
  sc <- data.frame(col=rep(c('A','B','C','D'),each=4),row=rep(1:4,4),SWE=NA,SWN=NA,NWE=NA,NWN=NA,NEE=NA,NEN=NA,SEE=NA,SEN=NA)
  z <- 4
  r1 <- 1:4
  r2 <- 13:16
  r3 <- c(1,5,9,13)
} else {
  np <- 18
  nq <- 25
  sc <- data.frame(col=rep(c('A','B','C','D','E'),each=5),row=rep(1:5,5),SWE=NA,SWN=NA,NWE=NA,NWN=NA,NEE=NA,NEN=NA,SEE=NA,SEN=NA)
  z <- 5
  r1 <- 1:5
  r2 <- 21:25
  r3 <- c(1,6,11,16,21)
}
rs <- which(x$type == plot_type)

vs <- data.frame(v=rep(1:np,each=nq),name=rep(x$Plot[rs],each=nq),col=rep(sc$col,np),row=rep(sc$row,np),quad=NA,SWlon=NA,SWlat=NA,NWlon=NA,NWlat=NA,NElon=NA,NElat=NA,SElon=NA,SElat=NA)
vs$quad <- paste(vs$col,vs$row,sep='')
head(vs)
tail(vs)

(i <- rs[1])
j <- 0
for (i in rs) {
  j <- j+1
  vr <- ((j-1)*(nq)+1):((j)*(nq))
  
  #### THIS ONLY WORKS FOR 16QUAD VEGPLOTS; ROWS ARE HARDWIRED IN ALL THESE SC STATEMENTS######
  sc[r1,c('SWE','SWN')] <- 
    interpoints(x$SW.UTM.E[i],x$SW.UTM.N[i],
                x$NW.UTM.E[i],x$NW.UTM.N[i],z)[-(z+1),]
  
  sc[r1,c('NWE','NWN')] <- 
    interpoints(x$SW.UTM.E[i],x$SW.UTM.N[i],
                x$NW.UTM.E[i],x$NW.UTM.N[i],z)[-1,]
  
  sc[r2,c('SEE','SEN')] <- 
    interpoints(x$SE.UTM.E[i],x$SE.UTM.N[i],
                x$NE.UTM.E[i],x$NE.UTM.N[i],z)[-(z+1),]
  
  sc[r2,c('NEE','NEN')] <- 
    interpoints(x$SE.UTM.E[i],x$SE.UTM.N[i],
                x$NE.UTM.E[i],x$NE.UTM.N[i],z)[-1,]
  
  sc[r3,c('SWE','SWN')] <- 
    interpoints(sc[1,'SWE'],sc[1,'SWN'],
                sc[r2[1],'SEE'],sc[r2[1],'SEN'],z)[-(z+1),]
  
  sc[r3+1,c('SWE','SWN')] <- 
    interpoints(sc[2,'SWE'],sc[2,'SWN'],
                sc[r2[2],'SEE'],sc[r2[2],'SEN'],z)[-(z+1),]
  
  sc[r3+2,c('SWE','SWN')] <- 
    interpoints(sc[3,'SWE'],sc[3,'SWN'],
                sc[r2[3],'SEE'],sc[r2[3],'SEN'],z)[-(z+1),]
  
  sc[r3+3,c('SWE','SWN')] <- 
    interpoints(sc[4,'SWE'],sc[4,'SWN'],
                sc[r2[4],'SEE'],sc[r2[4],'SEN'],z)[-(z+1),]
  
  if (plot_type == 'hectare') sc[r3+4,c('SWE','SWN')] <- 
    interpoints(sc[5,'SWE'],sc[5,'SWN'],
                sc[r2[5],'SEE'],sc[r2[5],'SEN'],z)[-(z+1),]
    
  sc[r3,c('SEE','SEN')] <- 
    interpoints(sc[1,'SWE'],sc[1,'SWN'],
                sc[r2[1],'SEE'],sc[r2[1],'SEN'],z)[-1,]
  
  sc[r3+1,c('SEE','SEN')] <- 
    interpoints(sc[2,'SWE'],sc[2,'SWN'],
                sc[r2[2],'SEE'],sc[r2[2],'SEN'],z)[-1,]
  
  sc[r3+2,c('SEE','SEN')] <- 
    interpoints(sc[3,'SWE'],sc[3,'SWN'],
                sc[r2[3],'SEE'],sc[r2[3],'SEN'],z)[-1,]
  
  sc[r3+3,c('SEE','SEN')] <- 
    interpoints(sc[4,'SWE'],sc[4,'SWN'],
                sc[r2[4],'SEE'],sc[r2[4],'SEN'],z)[-1,]
  
  if (plot_type == 'hectare') sc[r3+4,c('SEE','SEN')] <- 
    interpoints(sc[5,'SWE'],sc[5,'SWN'],
                sc[r2[5],'SEE'],sc[r2[5],'SEN'],z)[-1,]
  
  sc[r3,c('NWE','NWN')] <- 
    interpoints(sc[1,'NWE'],sc[1,'NWN'],
                sc[r2[1],'NEE'],sc[r2[1],'NEN'],z)[-(z+1),]
  
  sc[r3+1,c('NWE','NWN')] <- 
    interpoints(sc[2,'NWE'],sc[2,'NWN'],
                sc[r2[2],'NEE'],sc[r2[2],'NEN'],z)[-(z+1),]
  
  sc[r3+2,c('NWE','NWN')] <- 
    interpoints(sc[3,'NWE'],sc[3,'NWN'],
                sc[r2[3],'NEE'],sc[r2[3],'NEN'],z)[-(z+1),]
  
  sc[r3+3,c('NWE','NWN')] <- 
    interpoints(sc[4,'NWE'],sc[4,'NWN'],
                sc[r2[4],'NEE'],sc[r2[4],'NEN'],z)[-(z+1),]
  
  if (plot_type == 'hectare') sc[r3+4,c('NWE','NWN')] <- 
    interpoints(sc[5,'NWE'],sc[5,'NWN'],
                sc[r2[5],'NEE'],sc[r2[5],'NEN'],z)[-(z+1),]

  sc[r3,c('NEE','NEN')] <- 
    interpoints(sc[1,'NWE'],sc[1,'NWN'],
                sc[r2[1],'NEE'],sc[r2[1],'NEN'],z)[-1,]
  
  sc[r3+1,c('NEE','NEN')] <- 
    interpoints(sc[2,'NWE'],sc[2,'NWN'],
                sc[r2[2],'NEE'],sc[r2[2],'NEN'],z)[-1,]
  
  sc[r3+2,c('NEE','NEN')] <- 
    interpoints(sc[3,'NWE'],sc[3,'NWN'],
                sc[r2[3],'NEE'],sc[r2[3],'NEN'],z)[-1,]
  
  sc[r3+3,c('NEE','NEN')] <- 
    interpoints(sc[4,'NWE'],sc[4,'NWN'],
                sc[r2[4],'NEE'],sc[r2[4],'NEN'],z)[-1,]
  
  if (plot_type == 'hectare') sc[r3+4,c('NEE','NEN')] <- 
    interpoints(sc[5,'NWE'],sc[5,'NWN'],
                sc[r2[5],'NEE'],sc[r2[5],'NEN'],z)[-1,]
  
  sc
  
  vs[vr,6:13] <- sc[1:nq,3:10]
}
head(vs)
tail(vs)
plot(vs[,6:7])

#now make each row into a polygon with the hectare and quad name!
pcrds <- matrix(nrow = 5,ncol=2,data = NA)

xxs <- list()
j <- 0
for (i in 1:nrow(vs)) {
  j <- j+1
  #hs[i,]
  pcrds[1,1:2] <- as.numeric(vs[i,c('SWlon','SWlat')])
  pcrds[2,1:2] <- as.numeric(vs[i,c('NWlon','NWlat')])
  pcrds[3,1:2] <- as.numeric(vs[i,c('NElon','NElat')])
  pcrds[4,1:2] <- as.numeric(vs[i,c('SElon','SElat')])
  pcrds[5,1:2] <- as.numeric(vs[i,c('SWlon','SWlat')])
  xx <- Polygon(coords = pcrds)
  xxs[[j]] <- Polygons(list(xx),ID = paste(vs$name[i],vs$quad[i]))
}

class(xxs)
xxsp <- SpatialPolygons(xxs,proj4string=CRS('+proj=utm +zone=10'))
rownames(vs) <- paste(vs$name,vs$quad)
xxspd <- SpatialPolygonsDataFrame(xxsp,data = vs)
plot(xxsp)
proj4string(xxspd)

xxspg <- spTransform(xxspd,CRS('+proj=longlat +datum=WGS84'))
xxspa <- spTransform(xxspd,CRS('+proj=aea +datum=NAD83 +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000'))

if (plot_type=='veg20') filename <- 'vegplots-54-5m' else filename <- 'hectares-18-20m'

rgdal::writeOGR(xxspd,paste('plot.info/2021 updated shapefiles and coordinates/shapefiles/',filename,'-utm/',sep=''),driver='ESRI Shapefile',layer=filename)
rgdal::writeOGR(xxspa,paste('plot.info/2021 updated shapefiles and coordinates/shapefiles/',filename,'-aea/',sep=''),driver='ESRI Shapefile',layer=filename)
rgdal::writeOGR(xxspg,paste('plot.info/2021 updated shapefiles and coordinates/shapefiles/',filename,'-geo/',sep=''),driver='ESRI Shapefile',layer=filename)
