## make polygons from four corner data
# and output file with corner and center coordinates for all plots and subplots
rm(list=ls())
library(sf)
library(sp)
library(rgdal)
library(maptools)
library(raster)

plot_shapefiles <- TRUE

getwd()
x <- read.csv('2021/2021_updated_shapefiles_and_coordinates/adjusted_four_corners.updated_2021-06-01.csv')

# 72 rows - 54 veg20 plots and 18 ha plots
dim(x)
head(x)
tail(x)

names(x)
rownames(x) <- x$Plot

# given two points in x,y space, and a number of intervals to divide up the line between them, calculate the x,y coordinates of the points along the line
interpoints <- function(x1,y1,x2,y2,nintval) {
  dx <- x2-x1
  dy <- y2-y1
  xp <- x1 + dx*(0:nintval)/nintval
  yp <- y1 + dy*(0:nintval)/nintval
  return(matrix(c(xp,yp),ncol=2))
}
#interpoints(0,2,8,8,4)

# given four points of a quadrangle, calculate center point
# points 1-4 in order around quadrangel, so 1 and 3 are opposite, and 2 and 4 are opposite
centerpoint <- function(x1=0,y1=0,x2=1,y2=2,x3=2,y3=4,x4=3,y4=2) {
  fit1 <- lm(c(y1,y3)~c(x1,x3))
  a1 <- -coefficients(fit1)[2];b1 <- 1;c1 <- -coefficients(fit1)[1]
  fit2 <- lm(c(y2,y4)~c(x2,x4))
  a2 <- -coefficients(fit2)[2];b2 <- 1;c2 <- -coefficients(fit2)[1]
  xc <- (b1*c2-b2*c1)/(a1*b2-a2*b1)
  yc <- (a2*c1-a1*c2)/(a1*b2-a2*b1)
  names(xc) <- NULL
  names(yc) <- NULL
  return(c(xc,yc))
}
centerpoint()

## SET TO RUN FOR BOTH plot_type; override here and run only within loop if only one desired
plot_types <- c('veg20','hectare')
plot_type <- plot_types[1]

p=2
for (p in 1:2) 
{
  plot_type <- plot_types[p]
  
  pcrds <- matrix(nrow = 5,ncol=2,data = NA)
  xxs <- list()
  xxc <- data.frame(Plot=NA,xcenter=NA,ycenter=NA)
  rowsel <- which(x$type == plot_type)
  xxc <- data.frame(Plot=x$Plot[rowsel],Center.x=NA,Center.y=NA)
  i=rowsel[1]
  j <- 0
  for (i in rowsel) {
    j <- j+1
    pcrds[1,] <- as.numeric(x[i,c('SW.UTM.E','SW.UTM.N')])
    pcrds[2,] <- as.numeric(x[i,c('NW.UTM.E','NW.UTM.N')])
    pcrds[3,] <- as.numeric(x[i,c('NE.UTM.E','NE.UTM.N')])
    pcrds[4,] <- as.numeric(x[i,c('SE.UTM.E','SE.UTM.N')])
    pcrds[5,] <- as.numeric(x[i,c('SW.UTM.E','SW.UTM.N')])
    xx <- Polygon(coords = pcrds)
    xxs[[j]] <- Polygons(list(xx),ID = x$Plot[i])
    cp <- centerpoint(pcrds[1,1],pcrds[1,2],pcrds[2,1],pcrds[2,2],pcrds[3,1],pcrds[3,2],pcrds[4,1],pcrds[4,2])
    xxc[j,'Center.x'] <- cp[1]
    xxc[j,'Center.y'] <- cp[2]
  }
  
  class(xxs)
  xxsp <- SpatialPolygons(xxs,proj4string=CRS('+proj=utm +zone=10'))
  pu <- SpatialPolygonsDataFrame(xxsp,data = x[rowsel,])
  if (plot_shapefiles) plot(pu)
  proj4string(pu)
  
  pa <- spTransform(pu,CRS('+proj=aea +datum=NAD83 +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000'))
  pg <- spTransform(pu,CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
  
  if (plot_type=='veg20') filename <- 'vegplots-54-20m-pgon' else filename <- 'hectares-18-100m-pgon'
  
  rgdal::writeOGR(pu,paste('2021/2021_updated_shapefiles_and_coordinates/shapefiles/',filename,'-utm/',sep=''),driver='ESRI Shapefile',layer=filename,check_exists=T,overwrite_layer=T)
  rgdal::writeOGR(pa,paste('2021/2021_updated_shapefiles_and_coordinates/shapefiles/',filename,'-aea/',sep=''),driver='ESRI Shapefile',layer=filename,check_exists=T,overwrite_layer=T)
  rgdal::writeOGR(pg,paste('2021/2021_updated_shapefiles_and_coordinates/shapefiles/',filename,'-geo/',sep=''),driver='ESRI Shapefile',layer=filename,check_exists=T,overwrite_layer=T)
  
  #create and output spatial points file
  xxc.sp <- SpatialPoints(xxc[,2:3],proj4string=CRS('+proj=utm +zone=10'))
  pu <- SpatialPointsDataFrame(xxc.sp,data = xxc)
  if (plot_shapefiles) plot(pu)
  proj4string(pu)
  
  pa <- spTransform(pu,CRS('+proj=aea +datum=NAD83 +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000'))
  pg <- spTransform(pu,CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
  
  if (plot_type=='veg20') filename <- 'vegplots-54-20m-cpt' else filename <- 'hectares-18-100m-cpt'
  
  rgdal::writeOGR(pu,paste('2021/2021_updated_shapefiles_and_coordinates/shapefiles/',filename,'-utm/',sep=''),driver='ESRI Shapefile',layer=filename,check_exists=T,overwrite_layer=T)
  rgdal::writeOGR(pa,paste('2021/2021_updated_shapefiles_and_coordinates/shapefiles/',filename,'-aea/',sep=''),driver='ESRI Shapefile',layer=filename,check_exists=T,overwrite_layer=T)
  rgdal::writeOGR(pg,paste('2021/2021_updated_shapefiles_and_coordinates/shapefiles/',filename,'-geo/',sep=''),driver='ESRI Shapefile',layer=filename,check_exists=T,overwrite_layer=T)

  utmc <- coordinates(pu)
  aeac <- coordinates(pa)
  geoc <- coordinates(pg)
  cpdf <- data.frame(Plot=xxc[,1])
  cpdf$UTM.x <- utmc[,1]
  cpdf$UTM.y <- utmc[,2]
  cpdf$AEA.x <- aeac[,1]
  cpdf$AEA.y <- aeac[,2]
  cpdf$GEO.x <- geoc[,1]
  cpdf$GEO.y <- geoc[,2]
  write.csv(cpdf,paste('2021/2021_updated_shapefiles_and_coordinates/shapefiles/',filename,'.csv',sep=''))
  
  ######################
  # now make quad level shapefiles
  # shorten the 20m adjustment factors to 5m vector
  
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
  
  vs <- data.frame(v=rep(1:np,each=nq),name=rep(x$Plot[rs],each=nq),col=rep(sc$col,np),row=rep(sc$row,np),quad=NA,SWlon=NA,SWlat=NA,NWlon=NA,NWlat=NA,NElon=NA,NElat=NA,SElon=NA,SElat=NA,CENTlon=NA,CENTlat=NA)
  vs$quad <- paste(vs$col,vs$row,sep='')
  head(vs)
  tail(vs)
  
  (i <- rs[1])
  j <- 0
  for (i in rs) {
    j <- j+1
    vr <- ((j-1)*(nq)+1):((j)*(nq))
    
    #### THIS ONLY WORKS FOR 16QUAD VEGPLOTS; ROWS ARE HARDWIRED IN ALL THESE SC STATEMENTS######
    ## SEEMS TO BE FIXED? With the if(type==hectare) commands? output looks right!
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
    s=1
    for (s in 1:nrow(sc)) {
      cp <- centerpoint(sc[s,3],sc[s,4],sc[s,5],sc[s,6],sc[s,7],sc[s,8],sc[s,9],sc[s,10])
      vs$CENTlon[vr[s]] <- cp[1]
      vs$CENTlat[vr[s]] <- cp[2]
    }
    vs[vr,6:13] <- sc[1:nq,3:10]
  }
  head(vs)
  tail(vs)
  #if (plot_shapefiles) plot(vs[,c('CENTlon','CENTlat')])
  
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
  if (plot_shapefiles) plot(xxsp)
  proj4string(xxspd)
  
  xxspg <- spTransform(xxspd,CRS('+proj=longlat +datum=WGS84'))
  xxspa <- spTransform(xxspd,CRS('+proj=aea +datum=NAD83 +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000'))
  
  if (plot_type=='veg20') filename <- 'vegplots-54-5m-pgon' else filename <- 'hectares-18-20m-pgon'
  
  rgdal::writeOGR(xxspd,paste('2021/2021_updated_shapefiles_and_coordinates/shapefiles/',filename,'-utm/',sep=''),driver='ESRI Shapefile',layer=filename,check_exists=T,overwrite_layer=T)
  rgdal::writeOGR(xxspa,paste('2021/2021_updated_shapefiles_and_coordinates/shapefiles/',filename,'-aea/',sep=''),driver='ESRI Shapefile',layer=filename,check_exists=T,overwrite_layer=T)
  rgdal::writeOGR(xxspg,paste('2021/2021_updated_shapefiles_and_coordinates/shapefiles/',filename,'-geo/',sep=''),driver='ESRI Shapefile',layer=filename,check_exists=T,overwrite_layer=T)
  
  #create and output spatial points file
  xxc.sp <- SpatialPoints(vs[,c('CENTlon','CENTlat')],proj4string=CRS('+proj=utm +zone=10'))
  pu <- SpatialPointsDataFrame(xxc.sp,data = vs)
  if (plot_shapefiles) plot(pu)
  proj4string(pu)
  
  pa <- spTransform(pu,CRS('+proj=aea +datum=NAD83 +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000'))
  pg <- spTransform(pu,CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
  
  if (plot_type=='veg20') filename <- 'vegplots-54-5m-cpt' else filename <- 'hectares-18-20m-cpt'
  
  rgdal::writeOGR(pu,paste('2021/2021_updated_shapefiles_and_coordinates/shapefiles/',filename,'-utm/',sep=''),driver='ESRI Shapefile',layer=filename,check_exists=T,overwrite_layer=T)
  rgdal::writeOGR(pa,paste('2021/2021_updated_shapefiles_and_coordinates/shapefiles/',filename,'-aea/',sep=''),driver='ESRI Shapefile',layer=filename,check_exists=T,overwrite_layer=T)
  rgdal::writeOGR(pg,paste('2021/2021_updated_shapefiles_and_coordinates/shapefiles/',filename,'-geo/',sep=''),driver='ESRI Shapefile',layer=filename,check_exists=T,overwrite_layer=T)
  
  utmc <- coordinates(pu)
  aeac <- coordinates(pa)
  geoc <- coordinates(pg)
  cpdf <- data.frame(Plot=vs$name,Quad=vs$quad)
  cpdf$UTM.x <- utmc[,1]
  cpdf$UTM.y <- utmc[,2]
  cpdf$AEA.x <- aeac[,1]
  cpdf$AEA.y <- aeac[,2]
  cpdf$GEO.x <- geoc[,1]
  cpdf$GEO.y <- geoc[,2]
  write.csv(cpdf,paste('2021/2021_updated_shapefiles_and_coordinates/shapefiles/',filename,'.csv',sep=''))
}
