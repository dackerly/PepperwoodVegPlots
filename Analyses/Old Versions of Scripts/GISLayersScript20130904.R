library(sp)
library(maptools)
library(raster)

# projection strings
ta.project = '+proj=aea +datum=NAD83 +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000'
utm.project = '+proj=utm +zone=10S +datum=WGS84' # CHECK GPS

setwd("/Users/meaganoldfather/Dropbox/PepperwoodVegPlots/Database/GIS/")
#dir()

dir('DEM')
dem <- raster('DEM/pw_10m_t2.asc') # NOT AT THE SCALE WE SEE COMMUNITY DIFFERENCES
plot(dem, main="DEM")
#projection(dem)

#dir("PW10m_topoclimate")
marrad <- raster('PW10m_topoclimate/marrad.asc')
tmaxJUL<- raster('PW10m_topoclimate/tmxavejul.asc')
tminJAN<- raster ('PW10m_topoclimate/tmnavejan.asc')
# plot(marrad, main = "March Radiation")
# plot(tmaxJUL, main = "T Max July")
# plot(tminJAN, main = " T Min Jan ")
# tmaxJUL
#dir('PPshapefiles/PPshapefile-teale-albers')
PP <- readShapeSpatial('PPshapefiles/PPshapefile-teale-albers/Pepperwood')
projection(PP)
PPex <- extent(PP)
plot(PPex)

# tmaxJULpts=getValues(crop(tmaxJUL,PP))
# dempts=getValues(crop(dem,PP))
# tminJANpts=getValues(crop(tminJAN,PP))
# plot(dempts,tmaxJULpts,pch='.')

topoidx <- raster('PW10m_topoclimate/topoidx.asc')
plot(crop(topoidx,PPex))
tpi500 <- raster('PW10m_topoclimate/tpi_500.asc')
plot(crop(tpi500,PPex))



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
plot(crop(tpi500,PPex), main="TPI") # plots TPI on extended area around PW
plot(PP,add=T)
plot(plotsSP.ta,add=T,pch=19)

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

bioclim<-merge(climate.pts, plot.TR)
head(bioclim)

bioclim<-merge(climate.pts , plot.SA.TR, by="Plot")

# ### Adding average size (basal.area / number ) column to plot.SA.TR data frame
# 
# #plot.SA.TR$Ave.Size<-plot.SA.TR$Basal.Area_cm2/plot.SA.TR$Number 
# #head(plot.SA.TR)
# ################# Adding Percent Treeness 
# # source("/Users/meaganoldfather/Dropbox/PepperwoodVegPlots/Database/2013/Woody2013/Scripts/BuildingaRatio.R")
# # head(final.ratio)
# # plot.SA.TR$Percent.TR<-final.ratio$Percent.TR 
# # head(plot.SA.TR)
# 
# ###### Species-specific data frames
# 
# Pts.QUEGAR<- subset(bioclim,subset=(bioclim$Species == "QUEGAR"))
# Pts.QUEDOU<- subset(bioclim,subset=(bioclim$Species == "QUEDOU"))
# Pts.QUEKEL<- subset(bioclim,subset=(bioclim$Species == "QUEKEL"))
# Pts.QUEAGR<- subset(bioclim,subset=(bioclim$Species == "QUEAGR"))
# Pts.PSEMEN<- subset(bioclim,subset=(bioclim$Species == "PSEMEN"))
# Pts.AMOCAL<- subset(bioclim,subset=(bioclim$Species == "AMOCAL"))
# Pts.UMBCAL<- subset(bioclim,subset=(bioclim$Species == "UMBCAL"))
# 
# 
# # Histogrmas
# hist(Pts.QUEKEL$Basal.Area) 
# hist(Pts.QUEGAR$Basal.Area)
# hist(Pts.QUEDOU$Basal.Area)
# hist(Pts.QUEAGR$Basal.Area)
# hist(Pts.PSEMEN$Basal.Area)
# hist(Pts.AMOCAL$Basal.Area)
# hist(Pts.UMBCAL$Basal.Area)
# hist(Pts.QUEKEL$Number) 
# hist(Pts.QUEGAR$Number)
# hist(Pts.QUEDOU$Number)
# hist(Pts.QUEAGR$Number)
# hist(Pts.PSEMEN$Number)
# hist(Pts.AMOCAL$Number)
# 
# 
# ############################################# Basal.Area
# 
# ### DEM MODEL PLOTS
# with(bioclim, plot(DEM,Basal.Area, main="All Species"))
# with(Pts.QUEGAR, plot(DEM,Basal.Area, main="QUEGAR"))
# with(Pts.QUEDOU, plot(DEM,Basal.Area, main = "QUEDOU"))
# with(Pts.QUEKEL, plot(DEM,Basal.Area, main = "QUEKEL"))
# with(Pts.QUEAGR, plot(DEM,Basal.Area, main = "QUEAGR"))
# with(Pts.PSEMEN, plot(DEM,Basal.Area, main = "PSEMEN"))
# with(Pts.AMOCAL, plot(DEM,Basal.Area, main = "AMOCAL"))
# 
# 
# ### DEM MODEL LM
# 
# QUEGAR.DEM.LM<-lm(Basal.Area~DEM, data=Pts.QUEGAR)
# summary(QUEGAR.DEM.LM)
# 
# QUEDOU.DEM.LM<-lm(Basal.Area~DEM, data=Pts.QUEDOU)
# summary(QUEDOU.DEM.LM)
# 
# QUEAGR.DEM.LM<-lm(Basal.Area~DEM, data=Pts.QUEAGR)
# summary(QUEAGR.DEM.LM)
# 
# PSEMEN.DEM.LM<-lm(Basal.Area~DEM, data=Pts.PSEMEN)
# summary(PSEMEN.DEM.LM)
# 
# AMOCAL.DEM.LM<-lm(Basal.Area~DEM, data=Pts.AMOCAL)
# summary(AMOCAL.DEM.LM)
# 
# UMBCAL.DEM.LM<-lm(~DEM, data=Pts.UMBCAL)
# summary(UMBCAL.DEM.LM)
# 
# ####  March Radiation Plots 
# 
# with(bioclim, plot(MarchRadiation,Basal.Area))
# with(Pts.QUEGAR, plot(MarchRadiation,Basal.Area))
# with(Pts.QUEDOU, plot(MarchRadiation,Basal.Area))
# with(Pts.QUEKEL, plot(MarchRadiation,Basal.Area))
# with(Pts.QUEAGR, plot(MarchRadiation,Basal.Area, main="QUEAGR"))
# 
# yvals2<-predict(QUEAGR.MARR.LM)
# lines(Pts.QUEAGR$MarchRadiation,yvals2)
# text(600,17000, labels= paste("Pval=", round(summary(QUEAGR.MARR.LM)[[4]][2,4],4)), pos=4)
# 
# 
# with(Pts.PSEMEN, plot(MarchRadiation,Basal.Area))
# with(Pts.AMOCAL, plot(MarchRadiation,Basal.Area))
# 
# #### March Radiation LM
# QUEGAR.MARR.LM<-lm(Basal.Area~MarchRadiation, data=Pts.QUEGAR)
# summary(QUEGAR.MARR.LM) 
# 
# QUEDOU.MARR.LM<-lm(Basal.Area~MarchRadiation, data=Pts.QUEDOU)
# summary(QUEDOU.MARR.LM)  
# 
# QUEAGR.MARR.LM<-lm(Basal.Area~MarchRadiation, data=Pts.QUEAGR)
# summary(QUEAGR.MARR.LM)  #--> Significant
# with(Pts.QUEAGR, plot(MarchRadiation,Basal.Area, main = "Quercus agrifolia", ylab="Basal Area (cm2)"
#                       , xlab= "March Radiation", pch=19))
# yvals3<-predict(QUEAGR.MARR.LM)
# lines(Pts.QUEAGR$MarchRadiation,yvals3)
# text(450,25000, labels= paste("Pval=", round(summary(QUEAGR.MARR.LM)[[4]][2,4],4)), pos=4)
# 
# 
# PSEMEN.MARR.LM<-lm(Basal.Area~MarchRadiation, data=Pts.PSEMEN)
# summary(PSEMEN.MARR.LM)  
# 
# AMOCAL.MARR.LM<-lm(Basal.Area~MarchRadiation, data=Pts.AMOCAL)
# summary(AMOCAL.MARR.LM) 
# 
# UMBCAL.MARR.LM<-lm(~MarchRadiation, data=Pts.UMBCAL)
# summary(UMBCAL.MARR.LM) 
# 
# 
# ### July Max Temp Plots
# with(plot.SA.TR, plot(TMaxJUL,Basal.Area))
# with(Pts.QUEGAR, plot(TMaxJUL,Basal.Area)) 
# with(Pts.QUEDOU, plot(TMaxJUL,Basal.Area))
# with(Pts.QUEKEL, plot(TMaxJUL,Basal.Area, main = "QUEKEL"))
# 
# yvals3<-predict(QUEKEL.TMAXJ.LM)
# lines(Pts.QUEKEL$TMaxJUL,yvals3)
# text(29.8,6000, labels= paste("Pval=", round(summary(QUEKEL.TMAXJ.LM)[[4]][2,4],4)), pos=4)
# 
# 
# with(Pts.QUEAGR, plot(TMaxJUL,Basal.Area))
# with(Pts.PSEMEN, plot(TMaxJUL,Basal.Area))
# 
# #### July Max Temp LM
# QUEGAR.TMAXJ.LM<-lm(Basal.Area~TMaxJUL, data=Pts.QUEGAR)
# summary(QUEGAR.TMAXJ.LM)  
# plot(QUEGAR.TMAXJ.LM)
# 
# QUEDOU.TMAXJ.LM<-lm(Basal.Area~TMaxJUL, data=Pts.QUEDOU)
# summary(QUEDOU.TMAXJ.LM) 
# 
# QUEKEL.TMAXJ.LM<-lm(Basal.Area~TMaxJUL, data=Pts.QUEKEL)
# summary(QUEKEL.TMAXJ.LM) # --> significant
# 
# QUEAGR.TMAXJ.LM<-lm(Basal.Area~TMaxJUL, data=Pts.QUEAGR)
# summary(QUEAGR.TMAXJ.LM) 
# 
# PSEMEN.TMAXJ.LM<-lm(Basal.Area~TMaxJUL, data=Pts.PSEMEN)
# summary(PSEMEN.TMAXJ.LM)
# 
# AMOCAL.TMAXJ.LM<-lm(Basal.Area~TMaxJUL, data=Pts.AMOCAL)
# summary(AMOCAL.TMAXJ.LM)
# 
# UMBCAL.TMAXJ.LM<-lm(~TMaxJUL, data=Pts.UMBCAL)
# summary(UMBCAL.TMAXJ.LM)
# 
# ### Min Jan Temp Plots
# 
# with(bioclim, plot(tminJAN,Basal.Area))
# with(Pts.QUEGAR, plot(tminJAN,Basal.Area))
# with(Pts.QUEDOU, plot(tminJAN,Basal.Area))
# with(Pts.QUEKEL, plot(tminJAN,Basal.Area, main="QUEKEL"))
# with(Pts.QUEAGR, plot(tminJAN,Basal.Area))
# with(Pts.PSEMEN, plot(tminJAN,Basal.Area))
# with(Pts.AMOCAL, plot(tminJAN,Basal.Area))
# 
# 
# ### Min Jan Temp LM
# 
# QUEGAR.TMinJAN.LM<-lm(Basal.Area~tminJAN, data=Pts.QUEGAR)
# summary(QUEGAR.TMinJAN.LM)  
# plot(QUEGAR.TMinJAN.LM)
# 
# QUEDOU.TMinJAN.LM<-lm(Basal.Area~tminJAN, data=Pts.QUEDOU)
# summary(QUEDOU.TMinJAN.LM) 
# plot(QUEDOU.TMinJAN.LM)
# 
# QUEAGR.TMinJAN.LM<-lm(Basal.Area~tminJAN, data=Pts.QUEAGR)
# summary(QUEAGR.TMinJAN.LM)
# 
# PSEMEN.TMinJAN.LM<-lm(Basal.Area~tminJAN, data=Pts.PSEMEN)
# summary(PSEMEN.TMinJAN.LM) 
# 
# QUEKEL.TMinJAN.LM<-lm(Basal.Area~tminJAN, data=Pts.QUEKEL)
# summary(QUEKEL.TMinJAN.LM)
# plot((QUEKEL.TMinJAN.LM))
# 
# AMOCAL.TMinJAN.LM<-lm(Basal.Area~tminJAN, data=Pts.AMOCAL)
# summary(AMOCAL.TMinJAN.LM)
# plot((AMOCAL.TMinJAN.LM))
# 
# UMBCAL.TMinJAN.LM<-lm(~tminJAN, data=Pts.UMBCAL)
# summary(UMBCAL.TMinJAN.LM)
# plot((UMBCAL.TMinJAN.LM))
# 
# 
# ###### WD Plots
# 
# with(bioclim, plot(Soil.Moisture,Basal.Area, main="All Species"))
# with(Pts.QUEGAR, plot(Soil.Moisture,Basal.Area, main = "QUEGAR"))
# with(Pts.QUEDOU, plot(Soil.Moisture,Basal.Area, main="QUEDOU"))
# with(Pts.QUEKEL, plot(Soil.Moisture,Basal.Area, main="QUEKEL"))
# with(Pts.QUEAGR, plot(Soil.Moisture,Basal.Area, main= "QUEAGR"))
# with(Pts.PSEMEN, plot(Soil.Moisture,Basal.Area, main= "PSEMEN"))
# with(Pts.AMOCAL, plot(Soil.Moisture,Basal.Area, main= "AMOCAL"))
# 
# ###### Soil.Moisture LM
# QUEGAR.Soil.Moisture.LM<-lm(Basal.Area~Soil.Moisture, data=Pts.QUEGAR)
# summary(QUEGAR.Soil.Moisture.LM)  
# plot(QUEGAR.Soil.Moisture.LM)
# 
# QUEDOU.Soil.Moisture.LM<-lm(Basal.Area~Soil.Moisture, data=Pts.QUEDOU)
# summary(QUEDOU.Soil.Moisture.LM) 
# plot(QUEDOU.Soil.Moisture.LM)
# 
# QUEAGR.Soil.Moisture.LM<-lm(Basal.Area~Soil.Moisture, data=Pts.QUEAGR)
# summary(QUEAGR.Soil.Moisture.LM)
# 
# PSEMEN.Soil.Moisture.LM<-lm(Basal.Area~Soil.Moisture, data=Pts.PSEMEN)
# summary(PSEMEN.Soil.Moisture.LM) 
# 
# QUEKEL.Soil.Moisture.LM<-lm(Basal.Area~Soil.Moisture, data=Pts.QUEKEL)
# summary(QUEKEL.Soil.Moisture.LM)
# plot((QUEKEL.Soil.Moisture.LM))
# 
# AMOCAL.Soil.Moisture.LM<-lm(Basal.Area~Soil.Moisture, data=Pts.AMOCAL)
# summary(AMOCAL.Soil.Moisture.LM)
# 
# UMBCAL.Soil.Moisture.LM<-lm(~Soil.Moisture, data=Pts.UMBCAL)
# summary(UMBCAL.Soil.Moisture.LM)
# 
# 
# ####### Total Biomass with Many Models --> Nothing significant
# Sum.Pts<-with(bioclim, aggregate(Basal.Area~Plot, FUN=sum))
# head(Sum.Pts)
# 
# # DEM
# Sum.DEM.Pts<- merge(Sum.Pts, dem.pts)
# head(Sum.DEM.Pts)
# with(Sum.DEM.Pts, plot(DEM,Basal.Area))
# 
# Sum.DEM.lm<-lm(Basal.Area~DEM, data=Sum.DEM.Pts) 
# summary(Sum.DEM.lm)
# 
# # March Radiation
# Sum.Marrad.Pts<- merge(Sum.Pts, marrad.pts)
# head(Sum.Marrad.Pts)
# with(Sum.Marrad.Pts, plot(MarchRadiation,Basal.Area))
# 
# # Max July Temp 
# Sum.TMax.Pts<- merge(Sum.Pts,tmaxJUL.pts)
# head(Sum.TMax.Pts)
# with(Sum.TMax.Pts, plot(TMaxJUL,Basal.Area))
# 
# #TPI
# Sum.TMax.Pts<- merge(Sum.Pts,tpi500.pts)
# head(Sum.TMax.Pts)
# with(Sum.TMax.Pts, plot(tpi500,Basal.Area))
# 
# 
# ### tpi500 MODEL PLOTS
# with(bioclim, plot(tpi500,Basal.Area, main="All Species"))
# with(Pts.QUEGAR, plot(tpi500,Basal.Area, main="QUEGAR"))
# with(Pts.QUEDOU, plot(tpi500,Basal.Area, main = "QUEDOU"))
# with(Pts.QUEKEL, plot(tpi500,Basal.Area, main = "QUEKEL"))
# 
# yvals3<-predict(QUEKEL.tpi500.LM)
# lines(Pts.QUEKEL$tpi500,yvals3)
# text(0,8000, labels= paste("Pval=", round(summary(QUEKEL.tpi500.LM)[[4]][2,4],4)), pos=4)
# 
# with(Pts.QUEAGR, plot(tpi500,Basal.Area, main = "QUEAGR"))
# with(Pts.PSEMEN, plot(tpi500,Basal.Area, main = "PSEMEN"))
# with(Pts.AMOCAL, plot(tpi500,Basal.Area, main = "AMOCAL"))
# 
# 
# ### tpi500 MODEL LM
# 
# QUEGAR.tpi500.LM<-lm(Basal.Area~tpi500, data=Pts.QUEGAR)
# summary(QUEGAR.tpi500.LM)
# 
# QUEDOU.tpi500.LM<-lm(Basal.Area~tpi500, data=Pts.QUEDOU)
# summary(QUEDOU.tpi500.LM)
# 
# QUEAGR.tpi500.LM<-lm(Basal.Area~tpi500, data=Pts.QUEAGR)
# summary(QUEAGR.tpi500.LM)
# 
# 
# QUEKEL.tpi500.LM<-lm(Basal.Area~tpi500, data=Pts.QUEKEL)
# summary(QUEKEL.tpi500.LM) # THIS IS SIG
# with(Pts.QUEKEL, plot(tpi500,Basal.Area, main = "QUEKEL"))
# 
# PSEMEN.tpi500.LM<-lm(Basal.Area~tpi500, data=Pts.PSEMEN)
# summary(PSEMEN.tpi500.LM) # this is significant 
# with(Pts.PSEMEN, plot(tpi500,Basal.Area, main="Doug-Fir", ylab= "Basal Area (cm2)",
#                       xlab="Percent Lower Pixels", pch=19))
# yvals3<-predict(PSEMEN.tpi500.LM)
# lines(Pts.PSEMEN$tpi500,yvals3)
# text(-40,30000, labels= paste("Pval=", round(summary(PSEMEN.tpi500.LM)[[4]][2,4],4)), pos=4)
# 
# #plot(PSEMEN.tpi500.LM)
# 
# 
# AMOCAL.tpi500.LM<-lm(Basal.Area~tpi500, data=Pts.AMOCAL)
# summary(AMOCAL.tpi500.LM)
# 
# 
# UMBCAL.tpi500.LM<-lm(Basal.Area~tpi500, data=Pts.UMBCAL)
# summary(UMBCAL.tpi500.LM)
# 
# 
# #######  CORRELATIONS??? 
# plot(bioclim$tminJAN, bioclim$TMaxJUL)
# plot(bioclim$DEM, bioclim$MarchRadiation)
# 
# ######## Multiple Regressions ---> Nothing significant
# 
# QUEDOU.Multi<-lm(Basal.Area ~ tminJAN * TMaxJUL * Soil.Moisture, data=Pts.QUEDOU)
# summary(QUEDOU.Multi)
# 
# QUEGAR.Multi<-lm(Basal.Area ~ tminJAN * TMaxJUL *Soil.Moisture, data=Pts.QUEGAR)
# summary(QUEGAR.Multi)
# 
# QUEKEL.Multi<-lm(Basal.Area ~ tminJAN * TMaxJUL * Soil.Moisture, data=Pts.QUEKEL)
# summary(QUEKEL.Multi)
# 
# QUEAGR.Multi<-lm(Basal.Area ~ tminJAN * TMaxJUL * Soil.Moisture, data=Pts.QUEAGR)
# summary(QUEAGR.Multi)
# 
# PSEMEN.Multi<-lm(Basal.Area ~ tminJAN * TMaxJUL * Soil.Moisture, data=Pts.PSEMEN)
# summary(PSEMEN.Multi)
# 
# AMOCAL.Multi<-lm(Basal.Area ~ tminJAN * TMaxJUL * Soil.Moisture, data=Pts.AMOCAL)
# summary(AMOCAL.Multi)
# 
# ############################################
# QUEDOU.Multi<-lm(Basal.Area ~ DEM * MarchRadiation * WD, data=Pts.QUEDOU)
# summary(QUEDOU.Multi)
# 
# QUEGAR.Multi<-lm(Basal.Area ~ DEM * MarchRadiation * WD, data=Pts.QUEGAR)
# summary(QUEGAR.Multi)
# 
# QUEAGR.Multi<-lm(Basal.Area ~ DEM * MarchRadiation * WD, data=Pts.QUEAGR)
# summary(QUEAGR.Multi)
# 
# 
# PSEMEN.Multi<-lm(Basal.Area ~ DEM * MarchRadiation * WD, data=Pts.PSEMEN)
# summary(PSEMEN.Multi)
# 
# AMOCAL.Multi<-lm(Basal.Area ~ DEM * MarchRadiation * WD, data=Pts.AMOCAL)
# summary(AMOCAL.Multi)
# 
# 
# ################################################
# QG.MultiB<-lm(Basal.Area ~ Soil.Moisture * tpi500, data=bioclim)
# QD.MultiB<-lm(Basal.Area ~ Soil.Moisture *tpi500, data=bioclim)
# QG.MultiN<-lm(Basal.Area ~ Soil.Moisture * tpi500, data=bioclim)
# QD.MultiN<-lm(Basal.Area ~ Soil.Moisture *tpi500, data=bioclim)
# summary (QG.MultiB)
# summary (QG.MultiN)
# summary (QD.MultiB)
# summary (QD.MultiN)
# # --> Nothing is significant