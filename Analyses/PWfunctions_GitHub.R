### FUNCTIONS TO WORK WITH GITHUB###
# Author: Meagan F. Oldfather
# Created: 20150526
# Last edited: 20160202
######################################################################
# clear workspace
rm(list=ls())

######################################################################
### get.plot() ###
######################################################################
get.plot<-function(){ 
# list of file names
plot.list<- paste("PPW",1301:1350, sep="")
return(plot.list)
}
######################################################################
### get.plot.info() ###
######################################################################
get.plot.info<-function(prefix='https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/'){
# forces character strings to not be treated as factors 
options(stringsAsFactors=FALSE) 
# list of file names
url.plotinfo<- paste(prefix,"2013/Woody2013/Data/OriginalCSV/PlotInfo/PlotSurvey2013_", sep='')
plot.list<-get.plot()  
# read in all files for plot info
plot.data<-lapply(paste(url.plotinfo,plot.list,".csv",sep=''), function(x) read.csv(text=getURL(x, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")), skip=5, nrows=5, header=F))
# make dataframe structure 
plot.info<-data.frame("Plot"=numeric(length(plot.data)),
                      "UTM.E"=numeric(length(plot.data)),
                      "UTM.N"=numeric(length(plot.data)),
                      "Slope"=numeric(length(plot.data)),
                      "Aspect"=numeric(length(plot.data))) 
plot.info$Plot<-plot.list 
for (i in 1:length(plot.data)){
  plot.info$UTM.E[i]<-plot.data[[i]][1,2]
  plot.info$UTM.N[i]<-plot.data[[i]][2,2]
  plot.info$Slope[i]<-plot.data[[i]][3,5]
  plot.info$Aspect[i]<-plot.data[[i]][3,8]
}
# Slope and aspect are missing for plot PPW1302 and not applicable for PPW1329 (on crest) 
plot.info$Slope[2]<-NA # changes slope for plot 1302 to NA
plot.info$Aspect[2]<-NA # same for 1329 aspect
plot.info$Slope[29]<-NA # changes slope for plot 1329 to NA
plot.info$Aspect[29]<-NA # same for 1329 aspect
# calculate Aspect from field data 
plot.info$Aspect<- as.numeric(plot.info$Aspect)
plot.info$Aspect<-cos(2*pi*(as.numeric(plot.info$Aspect)/360))

super.plots<-c("PPW1301","PPW1307","PPW1325","PPW1349","PPW1310","PPW1309","PPW1344","PPW1312","PPW1321","PPW1322","PPW1338","PPW1339","PPW1324","PPW1335","PPW1315","PPW1332","PPW1340")

plot.info$Super.Plot<-0
plot.info[plot.info$Plot%in%super.plots, "Super.Plot"]<-1  
  
return(plot.info)

}
######################################################################
### get.envr.data() ### 
######################################################################
get.envr.data<-function(year,rmv.missing=F,prefix='https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/'){
options(stringsAsFactors=FALSE) 
plot.list<-get.plot()
url.envrdata <-paste(prefix,year,"/Woody",year,"/Data/OriginalCSV/PlotInfo/PlotSurvey",year,"_", sep='')

# This error code is broken   
#if(exists(getURL(paste(url.envrdata,"PPW1301.csv", sep="")))==F) stop("ERROR! Hold your horses! We don't yet have environmental surveys for this year")
  
envr.data<-lapply(paste(url.envrdata,plot.list,".csv", sep=''), function(x) read.csv(text=getURL(x, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")), skip=16, header=T))
names(envr.data) <- plot.list 
 
 for (i in 1:length(envr.data)){
   Plot<-plot.list[i]
   envr.data[[i]]<-cbind(Plot=Plot, envr.data[[i]]) #Inserts a plot column 
   colnames(envr.data[[i]])<-c("Plot", "Quad", "Bedrock", "Soil", "Boulder", "Fine", "Herb", "Litter", "Total")
   envr.data[[i]]<-envr.data[[i]][,1:9]
   envr.data[[i]]<-subset(envr.data[[i]], subset=(!is.na(envr.data[[i]][,8])))
 }
 envr.data<-do.call(rbind, envr.data)
 envr.data$Year=year
 row.names(envr.data) <- NULL

 #remove plot 1301 because missing data if future analyses desired
if(rmv.missing == T) envr.data<-subset(envr.data, subset=(envr.data$Plot!="PPW1301"))
return(envr.data)

}

######################################################################
### kill.trees() ### 
######################################################################
kill.trees<-function(year,prefix='https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/'){
if(!is.numeric(year)) stop("ERROR! Year must be numeric, try again cowboy")
 file<-paste(prefix, year, "/Mortality", year, "/Dead_Inds.csv" ,sep="")

# This error code is broken  
#if(file.exists(files[length(files)])==F) stop("ERROR! We don't yet have mortality surveys for future years, this ol'horse can't time travel")
 
dead<-lapply(file,function(x) read.csv(text=getURL(x, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))))
names(dead)<-year
 for(i in 1:length(dead)){
   dead[[i]]<-cbind(dead[[i]], Year=year[i])
  }

all.dead<-do.call(rbind,dead)
all.dead
}
######################################################################
### get.indv.data() ### 
######################################################################
get.indv.data<-function(year, stump=F,orig.dead=F, branches=F,prefix='https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/'){
options(stringsAsFactors=FALSE)  
file.list <-(paste(prefix,"2013/Woody2013/Data/OriginalCSV/Woody/WoodySurvey2013_", sep='')) 
plot.list<-get.plot()

mega.data<-lapply(paste(file.list, plot.list, ".csv", sep=''), function(x) read.csv(text=getURL(x, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")), na.strings=c("","NA") , skip=3)) 
names(mega.data) <- plot.list 

for (i in 1:length(mega.data)) 
{
  Plot<-plot.list[i]
  mega.data[[i]]<-cbind(Plot=Plot, mega.data[[i]]) 
  colnames(mega.data[[i]])<-c("Plot", "Quad", "Type", "Num", "Species",                             
                              "Confidence", "Dead.Stump", "SA.Stump.Height_cm",
                              "SA.Stump.BD_cm", "SA.Branch.Num", "DBH_cm", "X_cm", "Y_cm", "Notes") 
  mega.data[[i]]<-mega.data[[i]][,1:14] 
}
# turn list of dataframes into a single large dataframe called indv.data for easier manipulations
indv.data<-do.call(rbind, mega.data)
# get rid of confidence column
indv.data<-indv.data[,-6]
# get rid of extra rows
indv.data<-indv.data[!is.na(indv.data$Type),]
# change SA.Stump.BD_cm into numeric
indv.data$SA.Stump.BD_cm<-suppressWarnings(as.numeric(indv.data$SA.Stump.BD_cm))
# make indv.data$TreeNum numeric
indv.data$Num<-as.numeric(indv.data$Num)
# cleaning up Types 
indv.data[indv.data$Type==" TR", "Type"]<-"TR"
indv.data[indv.data$Type=="SA ","Type"]<-"SA"
indv.data[indv.data$Type=="SA  ","Type"]<-"SA"
indv.data[indv.data$Type=="S","Type"]<-"SA"
indv.data[indv.data$Type=="AS","Type"]<-"SA"

# Change individuals originally identified as "QUEDEC" to species-level indentification 
AUG.ID<-read.csv(text=getURL(paste(prefix, "2013/OakID2013/AUG_Species.csv", sep=''), followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
for(i in 1:dim(AUG.ID)[1]){
  indv.data[indv.data$Num %in% AUG.ID$Num[i], "Species"] <-AUG.ID$Species[i]
}

# change CEOCUN and UNKN27 individuals to CEACUN
indv.data[which(indv.data$Species=="CEOCUN"), "Species"]<-"CEACUN"
indv.data[which(indv.data$Species=="UNKN27"), "Species"]<-"CEACUN"

# change coordinates to all be positive
indv.data$Y_cm<-suppressWarnings(as.numeric(indv.data$Y_cm))

indv.data[which(indv.data$X_cm<0),"X_cm"]<-indv.data[which(indv.data$X_cm<0),"X_cm"]+500
indv.data[which(indv.data$Y_cm<0),"Y_cm"]<-indv.data[which(indv.data$Y_cm<0),"Y_cm"]+500

if (year=="none"){
  indv.data<-subset(indv.data, subset=(indv.data$Dead.Stump=="D" | indv.data$Dead.Stum=="S"))  
} 
else {
  if(year!=2012){
  indv.data$Year<-year
  year<-2013:year
  dead<-kill.trees(year=year)
  indv.data<-indv.data[!(indv.data$Num %in% dead$Num),]}  
  }

# condense each individual into a single row 
if (branches==F){
indv.data$Num<-floor(indv.data$Num)
indv.data[indv.data$Type=="SA", "Basal.Area" ]<-(((indv.data[indv.data$Type=="SA", "SA.Stump.BD_cm"]/2)^2)*(pi))*(indv.data[indv.data$Type=="SA","SA.Branch.Num"])
indv.data[indv.data$Type=="TR", "Basal.Area"]<- (((indv.data[indv.data$Type=="TR","DBH_cm"]/2)^2)*(pi))
# get rid of TS rows
indv.data<-indv.data[indv.data$Type!="TS",]
library(data.table)
indv.data<-data.table(indv.data)
indv.data[,Basal.Area:=sum(Basal.Area),by="Num"]

# for(i in 1:length(unique(indv.data$Num)))
#  {
#    indv.data[which(indv.data$Num == unique(indv.data$Num)[i]) ,"Basal.Area"]<-
#     sum(indv.data[which(indv.data$Num == unique(indv.data$Num)[i]) ,"Basal.Area"],na.rm=T)
#  }

# get rid of replication
indv.data<-indv.data[!duplicated(indv.data$Num,incomparables=NA),] 

# Get rid of the columns "Basal.Area",... 
indv.data<-as.data.frame(indv.data)
indv.data<-indv.data[,c("Plot","Quad","Type","Num","Species","Dead.Stump","X_cm","Y_cm","Basal.Area")]

}
else{
  indv.data[indv.data$Type=="SA", "Basal.Area" ]<-(((indv.data[indv.data$Type=="SA", "SA.Stump.BD_cm"]/2)^2)*(pi))*(indv.data[indv.data$Type=="SA","SA.Branch.Num"])
  indv.data[indv.data$Type=="TR", "Basal.Area"]<- (((indv.data[indv.data$Type=="TR","DBH_cm"]/2)^2)*(pi))
}


# subset stumps and original dead
if(stump==F & orig.dead==T) indv.data<-subset(indv.data, subset=(indv.data$Dead.Stump=="D" | is.na(indv.data$Dead.Stump))) 
# subset original dead individuals
if(stump==T & orig.dead==F) indv.data<-subset(indv.data, subset=(indv.data$Dead.Stump=="S" | is.na(indv.data$Dead.Stump)))
#subset both 
if(stump==F & orig.dead==F) {indv.data<-subset(indv.data, subset=(is.na(indv.data$Dead.Stump)))
                             indv.data<-indv.data[,-6]}


row.names(indv.data) <- NULL
return(indv.data)
}

######################################################################
### get.dead() ### 
######################################################################
get.dead<-function(year,total=T,by.plot=T){
  if(min(year)<2013) {stop("ERROR! Sorry partner but we don't have mortality surveys before 2013; if you are looking for stumps or original dead change use get.indv.data()")}
  dead<-kill.trees(year=year)
  colnames(dead)<-c("Plot","Num","Year.Died")
  indv.data<-suppressWarnings(get.indv.data(year=2012,branches=T))
  dead.indvs<- merge(indv.data,dead[,-1], by="Num",all.y=T)
  if(total==T){
    indv.data<-suppressWarnings(get.indv.data(year=year))
    branches.lost<-dead.indvs[(floor(dead.indvs$Num) %in% indv.data$Num),]
    dead.indvs$Count<-1
    dead.indvs$Branches.Lost<-0
    dead.indvs[(dead.indvs$Num %in% branches.lost$Num),"Count"]<-0
    dead.indvs[(dead.indvs$Num %in% branches.lost$Num),"Branches.Lost"]<-1
    # get rid of TS rows; some indivuals may incorrectly be TS --> needs investigation
    dead.indvs<-dead.indvs[dead.indvs$Type!="TS",]
    dead.indvs<-with(dead.indvs, aggregate(cbind(Basal.Area,Count,Branches.Lost) ~ Type+Species+Plot, FUN=sum))      
    dead.indvs<-dead.indvs[,c(3,2,1,5,4,6)]
    dead.indvs$Year<-paste(year, collapse = '.') 
    colnames(dead.indvs)<-c("Plot","Species","Type", "Num.Died","Basal.Area.Lost","Branches.Lost","Year.Died")
  if (by.plot==F) {dead.indvs<-with(dead.indvs, aggregate(cbind(Num.Died,Basal.Area.Lost,Branches.Lost) ~ Type+Species, FUN=sum))  
                   dead.indvs$Year.Died<-paste(year, collapse = '.')
                   dead.indvs<-dead.indvs[,c(2,1,3:5,6)]
  }  
  }
  return(dead.indvs)
}

####################################################################
### PW.species() ###
####################################################################
PW.species<-function(unknown=T){
indv.data<-suppressWarnings(get.indv.data(year=2012))
seju.data<-get.seju.data(year=2015)  
species<-unique(c(seju.data$Species,indv.data$Species))
if(unknown==F) species<-subset(species, subset=(substr(species, 1,3)!="UNK"))
return(species)
}

####################################################################
## get.seju.data() ###
####################################################################
get.seju.data<-function(year,prefix='https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/'){
  plot.list<-get.plot()
  file.dir <-paste(prefix,year, "/Woody",year, "/Data/OriginalCSV/Seedling/SeedlingSurvey",year,"_", sep='')
  
  # this error code is broken
 # if(file.exists(paste(file.dir,files[1], sep=""))==F) stop("ERROR! Whoah Nelly! We don't yet have seedling/junvenile surveys for this year")

seju<-lapply(paste(file.dir,plot.list,".csv",sep=''), function (x) read.csv(text=getURL(x, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")),na.strings="", skip=3, header=T))
  names(seju) <- plot.list  
  for (i in 1:length(seju)){
    Plot<-plot.list[i]
    seju[[i]]<-cbind(Plot=Plot, seju[[i]]) #Inserts a plot column 
    colnames(seju[[i]])<-c("Plot", "Quad", "Species", "Num.Seedlings", "Mistake", "Num.Juveniles", "Mistake","Num.Dead")
    seju[[i]]<-seju[[i]][,c(1:4,6,8)]
    if (year==2013){
    # fixes species issues
    seju[[i]][(seju[[i]]$Species%in% c("QUEXXX", "QUEAGKE","QUEDOU","QUEGAR","QUEKEL")), "Species"]<-"QUEDEC" 
    seju[[i]][(seju[[i]]$Species%in%"UMCAL"), "Species"]<-"UMBCAL"
    seju[[i]][(seju[[i]]$Species%in%"COFFEE"), "Species"]<-"FRACAL"
    seju[[i]][(seju[[i]]$Species%in%"LITDEN"), "Species"]<-"NOTDEN"
    seju[[i]][(seju[[i]]$Species%in%"ARBMAN"), "Species"]<-"ARBMEN"
    seju[[i]][(seju[[i]]$Species%in%"CEOCUN"), "Species"]<-"CEACUN"}
  }
seju<-do.call(rbind, seju)  
# omit rows with missing species 
seju<-subset(seju, subset=(!is.na(seju$Quad)))
seju[is.na(seju$Num.Seedlings),"Num.Seedlings"]<-0
seju[is.na(seju$Num.Juveniles),"Num.Juveniles"]<-0
seju[is.na(seju$Num.Dead),"Num.Dead"]<-0
seju$Total.Number<-(seju$Num.Seedlings+seju$Num.Juveniles)
seju$Year<-year
row.names(seju) <- NULL
return(seju)
}

####################################################################
## plants.by.plot() ###
####################################################################
 plants.by.plot<-function(year, type){
 plot.list<- get.plot()
 if(type=="SEJU"){
   seju<-get.seju.data(year=year)
   seju<-with(seju, aggregate(cbind(Num.Seedlings,Num.Juveniles,Total.Number)~Species+Plot+Year, FUN=sum))        
   seju<-seju[,c(2,1,3:6)]
   seju$Year<-year
   return(seju)                 
 }
   else{
     indv.data<-suppressWarnings(get.indv.data(year=year))
  if(type!="SA.TR") {
     count<-with(indv.data[indv.data$Type==type,], aggregate(Num~Species+Plot+Type, FUN=function(x)  length(unique(x))))
     area<-with(indv.data[indv.data$Type==type,], aggregate(Basal.Area~Species+Plot+Type, FUN=sum))
     output<-merge(area,count, by=c("Plot","Species","Type"))
      output$Year<-year
      colnames(output)<-c("Plot", "Species", "Type", "Basal.Area","Count","Year")}
     else{
       count.sap<-with(indv.data[indv.data$Type=="SA",], aggregate(Num~Species+Plot+Type, FUN=function(x)  length(unique(x))))
       area.sap<-with(indv.data[indv.data$Type=="SA",], aggregate(Basal.Area~Species+Plot+Type, FUN=sum))
       output.sap<-merge(area.sap,count.sap,by=c("Plot","Species","Type"))
       count.tr<-with(indv.data[indv.data$Type=="TR",], aggregate(Num~Species+Plot+Type, FUN=function(x)  length(unique(x))))
       area.tr<-with(indv.data[indv.data$Type=="TR",], aggregate(Basal.Area~Species+Plot+Type, FUN=sum))
       output.tr<-merge(area.tr,count.tr,by=c("Plot","Species","Type"))
       output<-merge(output.sap,output.tr,by=c("Plot","Species"),all=T)
       output<-output[-c(3,6)]
       output$Year<-year
       colnames(output)<-c("Plot","Species","SA.Basal.Area","SA.Count","TR.Basal.Area","TR.Count","Year")
       output[is.na(output)] <- 0
      }
    return(output)
    }   
   }  
####################################################################
## get.clim.pts() ###
####################################################################
# this code creates a dataframe with the climate variables for each plot
get.clim.pts<-function(prefix='https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/'){
clim.pts<-read.csv(text=getURL(paste(prefix, "GIS/ClimatePlotPts.csv", sep=''), followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))) 
  return(clim.pts)
}

# the below function can create raster and spd for all climate variables for the Preserve, but does not currently work within GitHub
#get.clim.pts<-function(all=F){
# library(sp)
# library(maptools)
# library(raster)
# ta.project = '+proj=aea +datum=NAD83 +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000'
# utm.project = '+proj=utm +zone=10S +datum=WGS84'
#     
# # PW DEM raster
# dem <- raster('GIS/DEM/pw_10m_t2.asc')
# # PW March Radiation raster
# marrad <- raster('GIS/PW10m_topoclimate/marrad.asc')
# # PW Max July Temp raster
# tmaxJUL<- raster('GIS/PW10m_topoclimate/tmxavejul.asc')
# # PW Min July Temp raster
# tminJAN<- raster ('GIS/PW10m_topoclimate/tmnavejan.asc')
# # PW tpi 
# topoidx <- raster('GIS/PW10m_topoclimate/topoidx.asc')
# # PW tpi at 500 meters
# tpi500 <- raster('GIS/PW10m_topoclimate/tpi_500.asc')
# # CWD at 270 meters
# cwd_hist<-raster('GIS/CWD/cwd1951_1980_ave_HST.asc')
# 
# # Pepperwood Preserve outline, used for cropping
# PP <- readShapeSpatial('GIS/PPshapefiles/PPshapefile-teale-albers/Pepperwood')
# PPex <- extent(PP)
# 
# all.PW.rasters<-c(dem,marrad,tmaxJUL,tminJAN,topoidx,tpi500)
# 
# if (all==T){
#   # Get values for all Pepperwood pts
#   Dempts<-getValues(crop(dem,PP))
#   MarRadpts<-getValues(crop(marrad,PP))
#   TmaxJULpts<-getValues(crop(tmaxJUL,PP))
#   TminJANpts<-getValues(crop(tminJAN,PP))
#   Topopts<-getValues(crop(topoidx,PP))
#   TPIpts<-getValues(crop(tpi500,PP))
#   CWD_Hist<-getValues(crop(cwd_hist,PP))
#   all.pts<-as.data.frame(matrix(nrow=length(Dempts),ncol=7))
#   colnames(all.pts)<-c("DEM","March.Rad","MaxJulT","MinJanT","Topo","TPI","CWD_Hist")
#   all.pts[,1]<- Dempts
#   all.pts[,2]<- MarRadpts
#   all.pts[,3]<- TmaxJULpts
#   all.pts[,4]<-  TminJANpts
#   all.pts[,5]<- Topopts
#   all.pts[,6]<- TPIpts
#   all.pts[,7]<-CWD_Hist
#   #remove rows with NA
#   #all.pts<-all.pts[!is.na(all.pts$DEM),]
#   # get the X,Y coordinates
#   #dim(rasterToPoints(crop(dem,PP))[,1:2])
#   # why different lengths?
#   #all.pts[,7]<-coords[,1]
#   #all.pts[,8]<-coords[,2]
#   return(list(all.pts,all.PW.rasters))
# }
# else{
#   plot.info<-get.plot.info()
#   plot.list<-get.plot()
#   long <- as.numeric(plot.info$UTM.E)
#   lat <- as.numeric(plot.info$UTM.N)
#   plotsSP <- SpatialPoints(data.frame(long,lat),proj4string <- CRS(utm.project))
#   plotsSP.ta <- spTransform(plotsSP,CRS(ta.project))
# 
# clim.pts<-as.data.frame(matrix(nrow=50,ncol=7))  
# rasters<-c(dem,marrad,tmaxJUL,tminJAN,topoidx,tpi500,cwd_hist)  
# for (i in 1:length(rasters)){  
#     pts<-extract(rasters[[i]],plotsSP.ta)
#     pts<-as.numeric(pts)
#    clim.pts[,i]<-pts
#   }
# 
# clim.pts$Plot<-plot.list
# clim.pts<-clim.pts[,c(8,1:7)]
# colnames(clim.pts)<- c("Plot","DEM","March.Rad","MaxJulT","MinJanT","Topo","TPI", "CWD")
# clim.pts$UTM.E <- as.numeric(plot.info$UTM.E)
# clim.pts$UTM.N<- as.numeric(plot.info$UTM.N)
# clim.spdf<-SpatialPointsDataFrame( plotsSP.ta,data=clim.pts)
# }
# return(list(clim.pts,clim.spdf,all.PW.rasters))
# }

####################################################################
## PWordinate() ###
####################################################################
PWordinate<-function(year,type,metric, psemen.rmv=T){
library(picante)
require(vegan)
df<-plants.by.plot(year,type)
if(type=="SA.TR"){
  df$Count<-df$SA.Count+df$TR.Count
  df$Basal.Area<-df$SA.Basal.Area+df$TR.Basal.Area  
}
clim.pts<-as.data.frame(get.clim.pts())

  psemen.plots<-c("PPW1320", "PPW1349", "PPW1340", "PPW1341", "PPW1306")
if(psemen.rmv==T){df<-df[!(df$Plot%in%psemen.plots),]
                 clim.pts<-clim.pts[!(clim.pts$Plot%in%psemen.plots),]}
 
clim.pts<-clim.pts[clim.pts$Plot%in%unique(df$Plot),]
  
mat<-sample2matrix(df[,c("Plot", metric,"Species")])
ord <- metaMDS(mat)
plot(ord,type='t')
return(list(mat,clim.pts)) 
}

#######################################
## get.super.trees()
######################################
 #get.super.trees(){
   
 #}


########################################
# get.hobo.data()
########################################
# get.hobo.data(){
#  prefix='https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/HOBO/ HOBO Raw Data 20150820' 
# <-lapply(paste(url.plotinfo,plot.list,".csv",sep=''), function(x) read.csv(text=getURL(x, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")), skip=5, nrows=5, header=F))  
#   
#   
# plot.list <- as.character(c(1301:1350))
# biglist <- vector(mode="list", length=50)
# 
# 
# names(biglist)<-plot.list
# for (i in 1:50){
#   direct<-dir()[i]
#   temp <-list.files(direct,pattern="*.csv")
#   sublist<-lapply(paste(direct,"/",temp,sep=""), read.csv,skip=1,header=T) 
#   
#   sublist<-lapply(sublist, FUN=function(x) {
#      if (grepl(pattern= "+F", x= colnames(x)[3]))
#        x[, 3] <- (x[, 3]-32)/(1.8)
#       colnames(x)<-paste0("V",1:ncol(x))
#        return(x)
#     })
# biglist[[i]]<-sublist 
# }
# 
# sublist<-vector(mode="list", length=50)
# names(sublist)<-plot.list
# for (i in 1:50){
#   sub<-biglist[[i]]
#   sub<-lapply(sub, function(x) x[,2:3])
#   sub<-as.data.frame(do.call(rbind, sub))
#   colnames(sub)<-c("Date.Time","Temp")
#   #print(colnames(sub))
#   sublist[[i]]<-sub
# } 
# 
# for (i in 1:50){
#   sub <- sublist[[i]] 
#   ts <- as.POSIXct(sub[,1],format = "%m/%d/%Y %H:%M")
#   sublist[[i]][,1]<- ts
# return(sublist)
# }
# }
# 
