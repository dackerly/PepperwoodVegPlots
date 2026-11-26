### FUNCTIONS TO WORK WITH GITHUB###
# Author: Meagan F. Oldfather
# Created: 20150526
# Last edited: 20261018 David Ackerly
######################################################################
# clear workspace
rm(list=ls())

######################################################################
### get.plot() ###
######################################################################
get.plot<-function(year=2013){ 
  # list of file names
  if(year>=2018) plot.list<- paste("PPW",c(1301:1350,1851:1854), sep="")
  else plot.list<- paste("PPW",1301:1350, sep="")
  return(plot.list)
}

######################################################################
### get.plot.info() ###
######################################################################
get.plot.info<-function(prefix='https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/'){
  # forces character strings to not be treated as factors 
  options(stringsAsFactors=FALSE) 
  # list of file names
  url.plotinfo1<- paste(prefix,"2013/Woody2013/Data/OriginalCSV/PlotInfo/PlotSurvey2013_", sep='')
  url.plotinfo2<- paste(prefix,"2018/Woody2018/Data/OriginalCSV/PlotInfo/PlotSurvey2018_", sep='')
  plot.list1<-get.plot(year = 2013)
  plot.list2<-get.plot(year = 2018)[51:54]
  # read in all files for plot info
  plot.data1<-lapply(paste(url.plotinfo1,plot.list1,".csv",sep=''), function(x) read.csv(text=getURL(x, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")), skip=5, nrows=5, header=F))
  plot.data2<-lapply(paste(url.plotinfo2,plot.list2,".csv",sep=''), function(x) read.csv(text=getURL(x, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")), skip=5, nrows=5, header=F))
  # make dataframe structure 
  plot.info<-data.frame("Plot"=c(plot.list1,plot.list2),
                        "UTM.E"=NA,
                        "UTM.N"=NA,
                        "Slope"=NA,
                        "Aspect"=NA) 
  for (i in 1:length(plot.data1)){
    plot.info$UTM.E[i]<-plot.data1[[i]][1,2]
    plot.info$UTM.N[i]<-plot.data1[[i]][2,2]
    plot.info$Slope[i]<-plot.data1[[i]][3,5]
    plot.info$Aspect[i]<-plot.data1[[i]][3,8]
  }
  for (i in 1:4){
    plot.info$UTM.E[50+i]<-plot.data2[[i]][1,2]
    plot.info$UTM.N[50+i]<-plot.data2[[i]][2,2]
    plot.info$Slope[50+i]<-plot.data2[[i]][3,5]
    plot.info$Aspect[50+i]<-plot.data2[[i]][3,8]
  }
  # Slope and aspect are missing for plot PPW1302 and not applicable for PPW1329 (on crest) 
  # plot.info$Slope[2]<-NA # changes slope for plot 1302 to NA
  # plot.info$Aspect[2]<-NA # same for 1329 aspect
  # plot.info$Slope[29]<-NA # changes slope for plot 1329 to NA
  # plot.info$Aspect[29]<-NA # same for 1329 aspect
  # # calculate Aspect from field data 
  plot.info$Aspect<- as.numeric(plot.info$Aspect)
  plot.info$Slope<- as.numeric(plot.info$Slope)
  #plot.info$Aspect<-cos(2*pi*(as.numeric(plot.info$Aspect)/360))
  
  super.plots<-c("PPW1301","PPW1307","PPW1325","PPW1349","PPW1310","PPW1309","PPW1344","PPW1312","PPW1321","PPW1322","PPW1338","PPW1339","PPW1324","PPW1335","PPW1315","PPW1332","PPW1340","PPW1852")
  
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
  if(file.exists(file[length(file)])==F) stop("ERROR! We don't yet have mortality surveys for future years, this ol'horse can't time travel")
  
  dead<-lapply(file,function(x) read.csv(text=getURL(x, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))))
  names(dead)<-year
  for(i in 1:length(dead)){
    dead[[i]]<-cbind(dead[[i]], Year=year[i])
  }
  
  all.dead<-do.call(rbind,dead)
  all.dead
}

## for troubleshooting, set defaults
year=2018
stump=F
orig.dead=F
survival=F
bsprout=F
epicormic=F
apical=F
canopy=F
bsprout.height=F
bsprout.count=F
tag.pulled=F
keep.999=F
branches=F
prefix='https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/'
plot.list=NA
######################################################################
### get.indv.data() ### 
######################################################################
get.indv.data <- function(year, stump=F, orig.dead=F, survival=F, bsprout=F, epicormic=F, apical=F, canopy=F, bsprout.height=F, bsprout.count=F, tag.pulled=F, keep.999=F, branches=F, prefix='https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/',plot.list=NA)
{
  require(RCurl)
  require(lubridate)
  options(stringsAsFactors=FALSE) 
  #getURL('https://github.com/dackerly/PepperwoodVegPlots/tree/master/2020/Woody2020/Data/OriginalCSV/Woody')
  #dir.list <- dir(paste(prefix,year,"/Woody",year,"/Data/OriginalCSV/Woody/",sep=''))
  file.list <-(paste(prefix,year,"/Woody",year,"/Data/OriginalCSV/Woody/WoodySurvey",year,"_", sep='')) 
  if (is.na(plot.list[1])) plot.list<-get.plot(year=year)
  
  mega.data<-lapply(paste(file.list, plot.list, ".csv", sep=''), function(x) read.csv(text=getURL(x, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")), na.strings=c("","NA") , skip=3)) 
  names(mega.data) <- plot.list # assigns a name to each item of the list
  #print(names(mega.data[[1]]))
  i=40
  
  first.line<-lapply(paste(file.list, plot.list, ".csv", sep=''), function(x) read.csv(text=getURL(x, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")), na.strings=c("","NA") , nrow=1,header=F)) 
  names(first.line) <- plot.list # assigns a name to each item of the list
  
  sample.date <- NA
  for (i in 1:length(first.line)) sample.date[i] <- first.line[[i]][11]
  sample.date <- do.call(rbind,sample.date)
  
  sYear <- substr(sample.date,1,4)
  sDate <- round(decimal_date(mdy(paste(substr(sample.date,5,6),
                                        substr(sample.date,7,8)
                                        ,sYear,sep='/'))),2)
  
  i=1
  for (i in 1:length(mega.data)) 
  {
    Plot<-plot.list[i]
    mega.data[[i]]<-cbind(Plot=Plot, mega.data[[i]]) 
    #print(c(i,length(colnames(mega.data[[i]]))))
    
    if(year>=2018) 
    {
      # remove any excess columns created by excel
      mega.data[[i]] <- mega.data[[i]][,1:21]
      mega.data[[i]]$sYear <- sYear[i]
      mega.data[[i]]$sDate <- sDate[i]
      nm <- colnames(mega.data[[i]])
      #nm$SOD.on.Bay <- NA
      nm <-replace(nm,grep('X..cm',nm),'X_cm')
      nm <-replace(nm,grep('Y..cm',nm),'Y_cm')
      nm <-replace(nm,grep('Tree.Tag.No.',nm),'Num')
      nm <-replace(nm,grep('DBH..cm.',nm),'DBH_cm')
      nm <-replace(nm,which(nm=='Basal'),'bSprout')
      nm <-replace(nm,grep('Epicormic',nm),'Epicormic')
      nm <-replace(nm,grep('Apical',nm),'Apical')
      nm <-replace(nm,grep('X..Living.Canopy',nm),'Canopy.Percent') 
      nm <-replace(nm,grep('SA.or.Stump.Height..cm.',nm),'SA.Height_cm') 
      nm <-replace(nm,grep('SA.or.Stump.BD..cm.',nm),'SA.BD_cm') 
      nm <-replace(nm,grep('SA..Stem.',nm),'SA.Stems') 
      nm <-replace(nm,grep('Basal.resrpout..height.',nm),'Basal.Resprout.Height_cm')
      nm <- replace(nm,grep('Basal.Resprout.count',nm),'Basal.Resprout.Count')
      nm <-replace(nm,grep('Tag.pulled',nm),'Tag.Pulled')
      nm <-replace(nm,grep('SOD.on.Bay',nm),'SOD.on.Bay')
      
      colnames(mega.data[[i]]) <- nm
      rm('nm')
      
      # in 2018 only, newly killed trees were measured in 1851:1854 to recreate pre fire stands - these are numbered with 5 digits: 99###; so they are not removed by keep.999=F
      if (keep.999==F) mega.data[[i]] <- mega.data[[i]][mega.data[[i]]$Num>=1000,]
    } else {
      # THIS SECTION FOR 2013
      mega.data[[i]]<-mega.data[[i]][,1:14] 
      mega.data[[i]]$sYear <- sYear[i]
      mega.data[[i]]$sDate <- sDate[i]
      # CLEAN UP COLUMN NAMES - 2013 COLUMNS CAN **NEVER** BE REORDERED, OR THIS LINE NEEDS TO BE EDITED!
      colnames(mega.data[[i]])<-c("Plot", "Quad", "Type", "Num",
                                  "Species", "Confidence",
                                  "Dead.Stump", 
                                  "SA.Height_cm", "SA.BD_cm", 
                                  "SA.Stems", "DBH_cm", "X_cm", "Y_cm", "Notes","sYear","sDate") 
      if (TRUE) {
        mega.data[[i]]$Num[which(mega.data[[i]]$Dead.Stump %in% c("D","S"))] <- 999
        
        # subset no stumps and original dead
        if(stump==T & orig.dead==T) mega.data[[i]]<-subset(mega.data[[i]], subset=(mega.data[[i]]$Dead.Stump=="S" | mega.data[[i]]$Dead.Stump=="D" | is.na(mega.data[[i]]$Dead.Stump)))       
        # subset no stumps and original dead
        if(stump==F & orig.dead==T) mega.data[[i]]<-subset(mega.data[[i]], subset=(mega.data[[i]]$Dead.Stump=="D" | is.na(mega.data[[i]]$Dead.Stump))) 
        # subset original dead individuals
        if(stump==T & orig.dead==F) mega.data[[i]]<-subset(mega.data[[i]], subset=(mega.data[[i]]$Dead.Stump=="S" | is.na(mega.data[[i]]$Dead.Stump)))
        #subset both 
        if(stump==F & orig.dead==F) {mega.data[[i]]<-subset(mega.data[[i]], subset=(is.na(mega.data[[i]]$Dead.Stump)))
        }
      }
    }
  }
  
  # turn list of dataframes into a single large dataframe called indv.data for easier manipulations
  indv.data<-do.call(rbind, mega.data)
  
  # get rid of extra rows - 2013 only
  if (year==2013) indv.data<-indv.data[!is.na(indv.data$Type),]
  
  # remove rows with no entry for Num
  if (year>=2018) indv.data<-indv.data[which(!is.na(indv.data$Num)),]
  
  # change SA.Stump.BD_cm into numeric
  indv.data$SA.BD_cm <- suppressWarnings(as.numeric(indv.data$SA.BD_cm))
  indv.data$DBH_cm <- suppressWarnings(as.numeric(indv.data$DBH_cm))
  
  # make indv.data$TreeNum numeric
  indv.data$Num<-as.numeric(indv.data$Num)
  
  # cleaning up Types 
  indv.data$Type[indv.data$Type==" TR"]<-"TR"
  indv.data$Type[indv.data$Type=="SA "]<-"SA"
  indv.data$Type[indv.data$Type=="SA  "]<-"SA"
  indv.data$Type[indv.data$Type=="S"]<-"SA"
  indv.data$Type[indv.data$Type=="AS"]<-"SA"
  
  # fix up Species names for bad data entry, etc.
  indv.data[which(indv.data$Species=="CEOCUN"), "Species"]<-"CEACUN"
  indv.data[which(indv.data$Species=="UNKN27"), "Species"]<-"CEACUN"
  indv.data[which(indv.data$Species=="ARBMEN "), "Species"]<-"ARBMEN"
  indv.data[which(indv.data$Species=="ARCMAN "), "Species"]<-"ARCMAN"
  indv.data[which(indv.data$Species=="CERBET "), "Species"]<-"CERBET"
  indv.data[which(indv.data$Species=="QUEAGR "), "Species"]<-"QUEAGR"
  indv.data[which(indv.data$Species=="QUEAGRI"), "Species"]<-"QUEAGR"
  indv.data[which(indv.data$Species=="QUEAGARI"), "Species"]<-"QUEAGR"
  indv.data[which(indv.data$Species=="QUAGRI"), "Species"]<-"QUEAGR"
  indv.data[which(indv.data$Species=="QUEARG"), "Species"]<-"QUEAGR"
  indv.data[which(indv.data$Species=="QUERAGR"), "Species"]<-"QUEAGR"
  indv.data[which(indv.data$Species=="QUEBERGAR"), "Species"]<-"QUEBEGA"
  indv.data[which(indv.data$Species=="QUEBERG"), "Species"]<-"QUEBER" 
  indv.data[which(indv.data$Species=="QUEBAR"), "Species"]<-"QUEBER" 
  indv.data[which(indv.data$Species=="QUEDEO"), "Species"]<-"QUEDOU"
  indv.data[which(indv.data$Species=="QUEDO"), "Species"]<-"QUEDOU"
  indv.data[which(indv.data$Species=="QUEDOGA"), "Species"]<-"QUEDEC"
  indv.data[which(indv.data$Species=="QUEGAR "), "Species"]<-"QUEGAR"
  indv.data[which(indv.data$Species=="QUEGARI"), "Species"]<-"QUEAGR" 
  indv.data[which(indv.data$Species=="QUEKEL "), "Species"]<-"QUEKEL"
  indv.data[which(indv.data$Species=="QUEKAL"), "Species"]<-"QUEKEL"
  indv.data[which(indv.data$Species=="QUEKELL"), "Species"]<-"QUEKEL"
  indv.data[which(indv.data$Species=="QUEWIZ"), "Species"]<-"QUEWIS"
  indv.data[which(indv.data$Species=="PIEMEN"), "Species"]<-"PSEMEN"
  indv.data[which(indv.data$Species=="PSMEN"), "Species"]<-"PSEMEN"
  indv.data[which(indv.data$Species=="PSEMEM"), "Species"]<-"PSEMEN"
  indv.data[which(indv.data$Species=="SEMEN"), "Species"]<-"PSEMEN"
  indv.data[which(indv.data$Species=="SAL SP."), "Species"]<-"SAL_SP"
  indv.data[which(indv.data$Species=="UMBCAL "), "Species"]<-"UMBCAL"
  indv.data[which(indv.data$Species=="UMCAL"), "Species"]<-"UMBCAL"
  indv.data[which(indv.data$Species=="UMBAL"), "Species"]<-"UMBCAL"
  indv.data[which(indv.data$Species=="UNKN30"), "Species"]<-"UNK"
  indv.data[which(indv.data$Species=="QUEDEC"), "Species"]<-"QUEdec"
  indv.data[which(indv.data$Species=="UNKN47"), "Species"]<-"PRUCER"
  indv.data[which(indv.data$Species=="CEATHY"), "Species"]<-"CEAPAR"
  
  
  ##Used to run these lines in order to replace incorrect species ID's in 2013- changed these in the 2013 CSV files during 2022 QC- no need to use AUG_Species csv anymore- some names in that file are incorrect now anyway
  
  # Change individuals originally identified as "QUEDEC" to species-level identification 
  #if(year==2013) {
  # AUG.ID<-read.csv(text=getURL(paste(prefix, "2013/OakID2013/AUG_Species.csv", sep=''), followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
  #for(i in 1:dim(AUG.ID)[1]){
  #  indv.data[indv.data$Num %in% AUG.ID$Num[i], "Species"] <-AUG.ID$Species[i]
  #  }
  # }
  
  # change coordinates to all be positive
  indv.data$Y_cm<-suppressWarnings(as.numeric(indv.data$Y_cm))
  write.csv(indv.data[,c('Plot','Num','X_cm')],'tmp/xcm.csv')
  indv.data[which(indv.data$X_cm<c(-500)),"X_cm"]<-NA
  indv.data[which(indv.data$Y_cm<c(-500)),"Y_cm"]<-NA
  indv.data[which(indv.data$X_cm<0),"X_cm"]<-indv.data[which(indv.data$X_cm<0),"X_cm"]+500
  indv.data[which(indv.data$Y_cm<0),"Y_cm"]<-indv.data[which(indv.data$Y_cm<0),"Y_cm"]+500
  
  # create variables for TreeNum and Point separately
  indv.data$iNum<-floor(indv.data$Num) #strips away decimal point values
  indv.data$Point <- indv.data$Num - indv.data$iNum
  
  # For saplings, change NAs for stem number to 0, and then add 1 to all - CHECK 2013 question for the possibility that main stem was included in stem number
  indv.data$SA.Stems[indv.data$Type=="SA" & is.na(indv.data$SA.Stems)] <- 0
  indv.data$SA.Stems[which(indv.data$Type=="SA")] <- indv.data$SA.Stems[which(indv.data$Type=="SA")] + 1
  
  indv.data$dbh <- NA
  indv.data$dbh[which(indv.data$Type=="SA")] <- indv.data$SA.BD_cm[which(indv.data$Type=="SA")]
  indv.data$dbh[which(indv.data$Type=="TR")] <- indv.data$DBH_cm[which(indv.data$Type=="TR")]
  
  indv.data$Basal.Area <- ((indv.data$dbh/2)^2)*(pi)
  
  # For saplings adjust basal area by number of stems < 10cm. As done here it overestimates by multiplying by stem number, and the other stems are smaller than main stem
  #indv.data$Basal.Area[which(indv.data$Type=="SA")] <-(indv.data$Basal.Area[which(indv.data$Type=="SA")])*(indv.data$SA.Stems[which(indv.data$Type=="SA")])
  
  # CREATE FOUR STATUS VARIABLES AND ASSIGN
  indv.data$Dead <- NA
  indv.data$Live <- NA
  indv.data$Topkill <- NA
  indv.data$gCrown <- NA
  
  if (year==2013) {
    indv.data$Survival <- NA
    indv.data$Epicormic <- NA
    indv.data$Apical <- NA
  } else 
  {
    indv.data$Epicormic[which(indv.data$Type=='SA')] <- NA
    indv.data$Apical[which(indv.data$Type=='SA')] <- NA
  }
  
  indv.data$NPoints <- NA
  indv.data$NPoints[which(indv.data$Type=='SA')] <- indv.data$SA.Stems[which(indv.data$Type=='SA')]
  indv.data$NPoints[which(indv.data$Type=='TR')] <- 1
  
  if (year==2013) 
  {
    indv.data$Live <- 1
    indv.data$Live[!is.na(indv.data$Dead.Stump)] <- 0
    indv.data$gCrown <- indv.data$Live
    
    ## make list of individuals with TS attached
    indv.data$bSprout[indv.data$Point==0] <- 0
    TSlist <- unique(indv.data$iNum[which(indv.data$Type=='TS')])
    indv.data$bSprout[which(indv.data$Num %in% TSlist)] <- 1
  } else 
  {
    indv.data$Apical[which(indv.data$Type=='TR' & indv.data$Survival==0)] <- 0
    indv.data$Epicormic[which(indv.data$Type=='TR' & indv.data$Survival==0)] <- 0
    indv.data$Live <- apply(indv.data[,c('Survival','bSprout')],1,max,na.rm=T)
    
    indv.data$gCrown[which(indv.data$Type=='TR')] <- apply(indv.data[which(indv.data$Type=='TR'),c('Epicormic','Apical')],1,max,na.rm=T)
    
    indv.data$gCrown[which(indv.data$Type=='SA')] <- indv.data$Survival[which(indv.data$Type=='SA')]
    
    indv.data$Topkill <- 0
    indv.data$Topkill[which(indv.data$Survival==0)] <- 1
  }
  
  # convert TS which were main stems to SA. If not main stem, get collapsed in next step (if branches==F)
  indv.data$Type[which(indv.data$Type == 'TS' & indv.data$Point == 0)] <- "SA"
  
  if (branches==F)
  {
    # get rid of dead points - DIDN'T WORK
    # indv.data <- indv.data[-which(indv.data$Survival==0 & indv.data$bSprout==0  & indv.data$Num%%1==0),]
    
    # Convert to data.table to collapse rows 
    library(data.table)
    indv.data<-data.table(indv.data)
    indv.data[,dbh:=max(dbh),by="iNum"]
    indv.data[,Basal.Area:=sum(Basal.Area),by="iNum"]
    allZero <- function(x) if (all(x==0)) return(0) else return(1)
    allOne <- function(x) if (all(x==1)) return(1) else return(0)
    
    if(year>=2018) {
      indv.data[,Survival:=max(Survival),by="iNum"]
      indv.data[,Live:=max(Live),by="iNum"]
      indv.data[,Topkill:=min(Topkill),by="iNum"]
      indv.data[,bSprout:=max(bSprout),by="iNum"]
      indv.data[,Epicormic:=max(Epicormic),by="iNum"]
      indv.data[,Apical:=max(Apical),by="iNum"]
      indv.data[,gCrown:=max(gCrown),by="iNum"]
      #indv.data[,Canopy.Percent:=max(Canopy.Percent),by="iNum"] #### BIT OF A FUDGE
    }
    # Set Dead to 1-Live (after step above to allow for effects of collapsing branches)
    indv.data$Dead <- 1 - indv.data$Live
    
    # Keep only main stem rows
    indv.data<-indv.data[indv.data$Point==0,] 
    
    # Get rid of the extra columns "DBH_cm", ... 
    indv.data<-as.data.frame(indv.data)
    indv.data<-indv.data[,c("Plot","Quad","Type","Num","Species","X_cm","Y_cm","Basal.Area","dbh","Survival","Dead","Live","bSprout","Topkill","Epicormic","Apical","gCrown","sYear","sDate")]
  } 
  
  # else {
  #   indv.data[indv.data$Type=="SA", "Basal.Area" ]<-(((indv.data[indv.data$Type=="SA", "SA.Stump.BD_cm"]/2)^2)*(pi))*(indv.data[indv.data$Type=="SA","SA.Stem.Num"])
  #   indv.data[indv.data$Type=="TR", "Basal.Area"]<- (((indv.data[indv.data$Type=="TR","DBH_cm"]/2)^2)*(pi))
  # }
  # 
  
  if (year=="none"){
    indv.data<-subset(indv.data, subset=(indv.data$Dead.Stump=="D" | indv.data$Dead.Stump=="S"))  
  }
  
  row.names(indv.data) <- NULL
  
  # remove PSEMEN trees that were accidentally removed by chainsaw crews in winter 2017 in plot PPW1307 
  indv.data<-indv.data[which(!(indv.data$Plot=="PPW1307" & indv.data$Num==1108 | indv.data$Num==1115 |indv.data$Num==1118 |indv.data$Num==1121 | indv.data$Num==1127 | indv.data$Num==1169 | indv.data$Num==1170 |indv.data$Num==1174)),]
  
  return(indv.data)
}

#This is Meagan's original get.indv.data function. It was updated during preparation of K26 to the version above
get.indv.data_deprecated<-function(year, stump=F, orig.dead=F, survival=F, bsprout=F, epicormic=F, apical=F, canopy=F, bsprout.height=F, bsprout.count=F, tag.pulled=F, keep.999=F, branches=F, prefix='https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/')
  {
  options(stringsAsFactors=FALSE)  
  file.list <-(paste(prefix,year,"/Woody",year,"/Data/OriginalCSV/Woody/WoodySurvey",year,"_", sep='')) 
  plot.list<-get.plot(year=year)
  
  mega.data<-lapply(paste(file.list, plot.list, ".csv", sep=''), function(x) read.csv(text=getURL(x, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")), na.strings=c("","NA") , skip=3)) 
  names(mega.data) <- plot.list 
  
  for (i in 1:length(mega.data)) 
  {
    Plot<-plot.list[i]
    mega.data[[i]]<-cbind(Plot=Plot, mega.data[[i]]) 
    
    if(year>=2018) {
      mega.data[[i]] <- mega.data[[i]][,c(1:4,7,13:16,5:6,20,8:12,17:19)]
      colnames(mega.data[[i]])<-c("Plot", "Quad", "Type", "Num", "Species",  
                                  "SA.Stump.Height_cm","SA.Stump.BD_cm", "SA.Branch.Num",
                                  "DBH_cm", "X_cm", "Y_cm", "Notes", 
                                  "Survival", "Basal.Resprout", "Epicormic.Resprout", 
                                  "Apical.Growth", "Canopy.Percent", "Basal.Resprout.Height_cm", 
                                  "Basal.Resprout.Count", "Tag.Pulled")
      if(tag.pulled==F) mega.data[[i]] <- mega.data[[i]][,-20]
      if(bsprout.count==F) mega.data[[i]] <- mega.data[[i]][,-19]
      if(bsprout.height==F) mega.data[[i]] <- mega.data[[i]][,-18]
      if(canopy==F) mega.data[[i]] <- mega.data[[i]][,-17]
      if(apical==F) mega.data[[i]] <- mega.data[[i]][,-16]
      if(epicormic==F) mega.data[[i]] <- mega.data[[i]][,-15]
      if(bsprout==F) mega.data[[i]] <- mega.data[[i]][,-14]
      if(survival==F) mega.data[[i]] <- mega.data[[i]][,-13]    
      if(keep.999==F) mega.data[[i]] <- mega.data[[i]][mega.data[[i]]$Num>=1000,]
    } else {
      mega.data[[i]]<-mega.data[[i]][,c(1:5,7:14)] 
      colnames(mega.data[[i]])<-c("Plot", "Quad", "Type", "Num", "Species", "Dead.Stump", 
                                  "SA.Stump.Height_cm", "SA.Stump.BD_cm", "SA.Branch.Num", "DBH_cm", "X_cm", "Y_cm", "Notes") 
      # subset stumps and original dead
      if(stump==F & orig.dead==T) mega.data[[i]]<-subset(mega.data[[i]], subset=(mega.data[[i]]$Dead.Stump=="D" | is.na(mega.data[[i]]$Dead.Stump))) 
      # subset original dead individuals
      if(stump==T & orig.dead==F) mega.data[[i]]<-subset(mega.data[[i]], subset=(mega.data[[i]]$Dead.Stump=="S" | is.na(mega.data[[i]]$Dead.Stump)))
      #subset both 
      if(stump==F & orig.dead==F) {mega.data[[i]]<-subset(mega.data[[i]], subset=(is.na(mega.data[[i]]$Dead.Stump)))
      mega.data[[i]]<-mega.data[[i]][,-6]}
    }
  }
  
  # turn list of dataframes into a single large dataframe called indv.data for easier manipulations
  indv.data<-do.call(rbind, mega.data)
  # get rid of extra rows
  indv.data<-indv.data[!is.na(indv.data$Type),]
  # change SA.Stump.BD_cm into numeric
  indv.data$SA.Stump.BD_cm <- suppressWarnings(as.numeric(indv.data$SA.Stump.BD_cm))
  # make indv.data$TreeNum numeric
  indv.data$Num<-as.numeric(indv.data$Num)
  # cleaning up Types 
  indv.data[indv.data$Type==" TR", "Type"]<-"TR"
  indv.data[indv.data$Type=="SA ","Type"]<-"SA"
  indv.data[indv.data$Type=="SA  ","Type"]<-"SA"
  indv.data[indv.data$Type=="S","Type"]<-"SA"
  indv.data[indv.data$Type=="AS","Type"]<-"SA"
  
  # fix up Species names for bad data entry, etc.
  indv.data[which(indv.data$Species=="CEOCUN"), "Species"]<-"CEACUN"
  indv.data[which(indv.data$Species=="UNKN27"), "Species"]<-"CEACUN"
  indv.data[which(indv.data$Species=="ARBMEN "), "Species"]<-"ARBMEN"
  indv.data[which(indv.data$Species=="ARCMAN "), "Species"]<-"ARCMAN"
  indv.data[which(indv.data$Species=="CERBET "), "Species"]<-"CERBET"
  indv.data[which(indv.data$Species=="QUEAGR "), "Species"]<-"QUEAGR"
  indv.data[which(indv.data$Species=="QUEAGRI"), "Species"]<-"QUEAGR"
  indv.data[which(indv.data$Species=="QUEARG"), "Species"]<-"QUEAGR"
  indv.data[which(indv.data$Species=="QUEGAR "), "Species"]<-"QUEGAR"
  indv.data[which(indv.data$Species=="QUEKEL "), "Species"]<-"QUEKEL"
  indv.data[which(indv.data$Species=="PIEMEN"), "Species"]<-"PSEMEN"
  indv.data[which(indv.data$Species=="PSMEN"), "Species"]<-"PSEMEN"
  indv.data[which(indv.data$Species=="UMBCAL "), "Species"]<-"UMBCAL"
  indv.data[which(indv.data$Species=="UMCAL"), "Species"]<-"UMBCAL"
  indv.data[which(indv.data$Species=="UNKN30"), "Species"]<-"UNK"
  indv.data[which(indv.data$Species=="QUEDEC"), "Species"]<-"QUEdec"
  
  # Change individuals originally identified as "QUEDEC" to species-level indentification 
  if(year==2013) {
    AUG.ID<-read.csv(text=getURL(paste(prefix, "2013/OakID2013/AUG_Species.csv", sep=''), followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
    for(i in 1:dim(AUG.ID)[1]){
      indv.data[indv.data$Num %in% AUG.ID$Num[i], "Species"] <-AUG.ID$Species[i]
    }
  }
  
  # change coordinates to all be positive
  indv.data$Y_cm<-suppressWarnings(as.numeric(indv.data$Y_cm))
  indv.data[which(indv.data$X_cm<0),"X_cm"]<-indv.data[which(indv.data$X_cm<0),"X_cm"]+500
  indv.data[which(indv.data$Y_cm<0),"Y_cm"]<-indv.data[which(indv.data$Y_cm<0),"Y_cm"]+500
  
  # condense each individual into a single row 
  if (branches==F){
    indv.data$Num<-floor(indv.data$Num)
    if(year>=2018) {
      indv.data[indv.data$Type=="SA" & is.na(indv.data$SA.Stem.Num), "SA.Stem.Num" ] <- 0
      indv.data[indv.data$Type=="SA", "SA.Stem.Num" ]<- indv.data[indv.data$Type=="SA", "SA.Stem.Num" ] + 1
    }
    indv.data[indv.data$Type=="SA", "Basal.Area" ]<-(((indv.data[indv.data$Type=="SA", "SA.Stump.BD_cm"]/2)^2)*(pi))*(indv.data[indv.data$Type=="SA","SA.Branch.Num"])
    indv.data[indv.data$Type=="TR", "Basal.Area"]<- (((indv.data[indv.data$Type=="TR","DBH_cm"]/2)^2)*(pi))
    # get rid of TS rows
    indv.data<-indv.data[indv.data$Type!="TS",]
    library(data.table)
    indv.data<-data.table(indv.data)
    indv.data[,Basal.Area:=sum(Basal.Area),by="Num"]
    if(year>=2018) {
      if(survival==TRUE) indv.data[,Survival:=max(Survival),by="Num"]
      if(bsprout==TRUE) indv.data[,Basal.Resprout:=max(Basal.Resprout),by="Num"]
      if(epicormic==TRUE) indv.data[,Epicormic.Resprout:=max(Epicormic.Resprout),by="Num"]
      if(apical==TRUE) indv.data[,Apical.Growth:=max(Apical.Growth),by="Num"]
      if(canopy==TRUE) indv.data[,Canopy.Percent:=max(Canopy.Percent),by="Num"] #### BIT OF A FUDGE
    }
    # get rid of duplicated tag numbers (?)
    indv.data<-indv.data[!duplicated(indv.data$Num,incomparables=NA),] 
    
    # Get rid of the extra columns "DBH_cm", ... 
    indv.data<-as.data.frame(indv.data)
    indv.data<-indv.data[,c("Plot","Quad","Type","Num","Species","Dead.Stump","X_cm","Y_cm","Basal.Area")]
  } else {
    indv.data[indv.data$Type=="SA", "Basal.Area" ]<-(((indv.data[indv.data$Type=="SA", "SA.Stump.BD_cm"]/2)^2)*(pi))*(indv.data[indv.data$Type=="SA","SA.Branch.Num"])
    indv.data[indv.data$Type=="TR", "Basal.Area"]<- (((indv.data[indv.data$Type=="TR","DBH_cm"]/2)^2)*(pi))
  }
  
  if (year=="none"){
    indv.data<-subset(indv.data, subset=(indv.data$Dead.Stump=="D" | indv.data$Dead.Stum=="S"))  
  } else {
    if(year!=2012){
      indv.data$Year<-year
      #########
      # MORTALITY FUNCTIONS SHOULD NOT EVEN BE HERE SHOULD THEY?
      # MAKE A NEW FUNCTION get.indv.mortality.data()
      #    year<-2013:year
      #    dead<-kill.trees(year=year)
      #    indv.data<-indv.data[!(indv.data$Num %in% dead$Num),]
      #########
    }
  }
  
  row.names(indv.data) <- NULL
  
  # remove PSEMEN trees that were accidently removed by chainsaw crews in winter 2017 in plot PPW1307 
  indv.data<-indv.data[which(!(indv.data$Plot=="PPW1307" & indv.data$Num==1108 | indv.data$Num==1115 |indv.data$Num==1118 |indv.data$Num==1121 | indv.data$Num==1127 | indv.data$Num==1169 | indv.data$Num==1170 |indv.data$Num==1174)),]
  
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
plants.by.plot <- function(year, type)
{
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

#This is Meagan's original plants.by.plot function
plants.by.plot.deprecated<-function(year, type){
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


##################################################
# pbp.mm()
##################################################
pbp.mm<-function(min = 1, max = 300){
  plot.list<- get.plot()
  indv.data<-suppressWarnings(get.indv.data(year=2014, branches = T))
  indv.data <- subset(indv.data, Type == "TR")
  indv.data <- subset(indv.data, DBH_cm >= min & DBH_cm < max)
  
  count<-with(indv.data, aggregate(Num~Species+Plot+Type, FUN=function(x)  length(unique(x))))
  area<-with(indv.data, aggregate(Basal.Area~Species+Plot+Type, FUN=sum))
  output<-merge(area,count, by=c("Plot","Species","Type"))
  colnames(output)<-c("Plot", "Species", "Type", "Basal.Area","Count")
  
  return(output)
  
}   

####################################################################
## get.clim.pts() ###
####################################################################
# this code creates a dataframe with the climate variables for each plot
get.clim.pts<-function(prefix='https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/'){
  clim.pts<-read.csv(text=getURL(paste(prefix, "GIS/ClimatePlotPts.csv", sep=''), followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))) 
  return(clim.pts)
}


# A function to break up and label the plots by any size subplot 
applySubplots <- function(treepoints, size, centroid=FALSE, bbox=FALSE, fill=FALSE, plot_sub=FALSE) { 
  if(!(size %in% c(10,20,25,50,100))) stop("Only subplot sizes 10, 20, 25, 50, and 100m can be calculated.\n  Try again with one of these sizes.") 
  
  suppressPackageStartupMessages(library(raster)) 
  
  # Convert to a SpatialPointsDataFrame if necessary 
  df=FALSE 
  if(class(treepoints)!="SpatialPointsDataFrame") { 
    df=TRUE 
    coordinates(treepoints) <- 5:6 
    crs(treepoints) <- '+proj=utm +zone=10S +datum=WGS84 +units=m +ellps=WGS84 +towgs84=0,0,0' 
  } 
  
  coords <- read.csv('https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/2016/Hectares/hectare_corners.csv',header=TRUE,sep=",",stringsAsFactors=FALSE) 
  
  for(p in 1:nrow(coords)) { 
    for(x in 0:(100/size-1)) { 
      for(y in 0:(100/size-1)) { 
        ext <- extent(coords[p,4]+x*size, coords[p,4]+x*size+size, 
                      coords[p,5]+y*size, coords[p,5]+y*size+size) 
        letter <- c("Q","R","S","T","U","V","W","X","Y","Z") 
        temp <- crop(treepoints,ext) 
        
        #If fill=TRUE and there's not trees in the subplot, make a dummy
        if(fill & is.null(temp)) {
          temp <- treepoints@data[0,]
          temp[nrow(temp)+1,]$Plot <- coords$Plot[p]
          temp$UTM.E=mean(c(ext[1],ext[2]))
          temp$UTM.N=mean(c(ext[3],ext[4]))
          coordinates(temp) <- c("UTM.E","UTM.N")
          crs(temp) <- '+proj=utm +zone=10S +datum=WGS84 +units=m +ellps=WGS84 +towgs84=0,0,0' 
        }      
        if(length(temp)) { 
          # Write the new subplot ID 
          temp$Subplot <- paste(letter[10-(100/size-1)+x],y+1,sep="") 
          
          # If centroid=TRUE save the subplot's centroid
          if(centroid) { 
            temp$Center.UTM.E <- mean(c(ext[1],ext[2]))
            temp$Center.UTM.N <- mean(c(ext[3],ext[4]))
          } 
          
          # If bbox=TRUE save the bounding box coordinates 
          # for each subplot to the data frame 
          if(bbox) { 
            temp$Min.UTM.E <- ext[1] 
            temp$Max.UTM.E <- ext[2] 
            temp$Min.UTM.N <- ext[3] 
            temp$Max.UTM.N <- ext[4] 
          } 
          
          if(p==1 & x==0 & y==0) subtrees <- temp 
          else subtrees <- rbind(subtrees,temp) 
        } 
      } 
    } 
  } 
  
  # Clean up any trees that are right on a line! 
  subtrees <- subtrees[!duplicated(subtrees$Num,incomparables=NA),]
  
  # Rename the "Subplot" column to reflect the size selection 
  names(subtrees@data)[[2]] <- paste("Subplot_",size,"m",sep="") 
  
  # If plot_sub=TRUE add a column "Plot_sub" with plot and sub ID 
  if(plot_sub) subtrees@data$Plot_sub <- paste(subtrees[[1]],subtrees[[2]],sep="_") 
  
  # Return whichever object class was sent 
  if(df) as.data.frame(subtrees, stringsAsFactors=FALSE) 
  else subtrees 
} 


# Get the hectare survey data with trunks aggregated to basal area 
getHectareTrees <- function(na.omit=TRUE, basal.area = TRUE, agg.trunks=TRUE, filter.size=TRUE, trim.data=TRUE) { 
  if(agg.trunks & !basal.area) stop("Trunks can only be aggregated if basal area is also calculated.\n  If agg.trunks is set TRUE, basal.area must also be set TRUE.") 
  if(filter.size & !agg.trunks) stop("Small trees can only be filtered after aggregating trunks.\n  If filter.size is set TRUE, agg.trunks must also be set TRUE.") 
  
  suppressPackageStartupMessages(library(data.table)) 
  
  # Load the CSV of hectare surveys as a data frame 
  get.trees <- read.csv('https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/2016/Hectares/hectare_trees_all.csv',header=TRUE,sep=",",stringsAsFactors=FALSE)
  
  # Dirty fix to remove data with missing coordinates, species, or DBH 
  if(na.omit) get.trees <- get.trees[!(is.na(get.trees$DBH.cm) | is.na(get.trees$UTM.N) | is.na(get.trees$Species)),] 
  
  # Calculate basal area 
  if(basal.area) get.trees$Basal.Area <- (get.trees$DBH.cm/2)^2*pi 
  
  # Aggregate multiple trunks to single individuals 
  if(basal.area & agg.trunks) { 
    get.trees$Num <- trunc(get.trees$Num) 
    get.trees <- data.table(get.trees) 
    get.trees[,Basal.Area:=sum(Basal.Area),by="Num"] 
    get.trees <- data.frame(get.trees) 
    # get rid of duplications created by truncating 
    get.trees <- get.trees[!duplicated(get.trees$Num,incomparables=NA),] 
  } 
  
  # Get rid of trees less than 20cm DBH 
  if(basal.area & agg.trunks & filter.size) get.trees <- get.trees[get.trees$Basal.Area>=(100*pi),] 
  
  # Renumber data frame rows 
  rownames(get.trees) <- NULL 
  
  # Send the requested columns, trimmed or not 
  if(!trim.data) get.trees 
  else if(trim.data & basal.area) get.trees[,c("Plot","Subplot","Num","Species","UTM.E","UTM.N","Year","Basal.Area")] 
  else if(trim.data & !basal.area) get.trees[,c("Plot","Subplot","Num","Species","UTM.E","UTM.N","Year","DBH.cm")] 
} 

# Correct the UTM coordinates of the original veg plot survey trees
# based on the angle of the plot and slope and also select trees >20cm DBH
# THESE CORRECTIONS ARE ALREADY MADE FOR THE VEGPLOT TREES INCLUDED IN
# THE hectare_trees_all.csv FILE, THE FUNCTION IS INCLUDED AS DOCUMENTATION.
correctTreeCoordinates <- function(indivs) { 
  # Note: PPW1317 is included here, even though it's not a superplot,
  # because it falls entirely within the hectare around superplot PPW1315
  superplots <- c("PPW1301","PPW1307","PPW1309","PPW1310","PPW1312","PPW1315","PPW1321","PPW1322","PPW1324","PPW1325","PPW1332","PPW1335","PPW1338","PPW1339","PPW1340","PPW1344","PPW1349","PPW1317")
  
  # Load the differentially corrected coordinates for the superplots
  corners <- read.csv('https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/2016/Hectares/vegplot_corners_corrected.csv',header=TRUE,sep=",",stringsAsFactors=FALSE)
  
  attach(corners)
  # individual angles for each side of the plot
  corners$S.angle <- atan((SW.UTM.N - SE.UTM.N)/(SE.UTM.E - SW.UTM.E)) #* (180/pi)
  corners$N.angle <- atan((NW.UTM.N - NE.UTM.N)/(NE.UTM.E - NW.UTM.E)) #* (180/pi)
  corners$W.angle <- atan((NW.UTM.E - SW.UTM.E)/(NW.UTM.N - SW.UTM.N)) #* (180/pi)
  corners$E.angle <- atan((NE.UTM.E - SE.UTM.E)/(NE.UTM.N - SE.UTM.N)) #* (180/pi)
  
  # calculate a mean angle of declination for each plot
  corners$plot.angle <- rowMeans(corners[,11:14])
  
  # rough correction to adjust for the difference between on-the-ground
  # and aerial measurements due to the slope of the plot
  corners$X.correction <- (SE.UTM.E - SW.UTM.E) / 20
  corners$Y.correction <- (NW.UTM.N - SW.UTM.N) / 20
  
  detach(corners)
  
  # Aggregate the stems from the vegplot data to make a size cut-off
  indv.data <- indivs
  indv.data$Num <- trunc(indv.data$Num)
  indv.data <- indv.data[indv.data$Type!="TS",]
  library(data.table)
  indv.data <- data.table(indv.data)
  indv.data[,Basal.Area:=sum(Basal.Area),by="Num"]
  indv.data <- indv.data[!duplicated(indv.data$Num,incomparables=NA),] 
  indv.data <- subset(indv.data, indv.data$Basal.Area > pi*100)
  # subset them based on basal area and superplot identity
  indivs <- subset(indivs, trunc(indivs$Num) %in% indv.data$Num & indivs$Plot %in% superplots)
  
  # Set up the quad offsets to convert measurements to coordinates
  quad <- (c('A1','A2','A3','A4','B1','B2','B3','B4','C1','C2','C3','C4','D1','D2','D3','D4'))
  Xoffset <- c(0,0,0,0,5,5,5,5,10,10,10,10,15,15,15,15)
  Yoffset <- c(0,5,10,15,0,5,10,15,0,5,10,15,0,5,10,15)
  off <- data.frame(Quad=quad, X.offset=Xoffset, Y.offset=Yoffset)
  indivs <- merge(indivs, off, by="Quad")
  
  # Change the measured Xs and Ys from cm to m
  indivs$X <- indivs$X_cm / 100
  indivs$Y <- indivs$Y_cm / 100
  
  # Add all the measurements up for an uncorrected individual UTM
  indivs <- merge(indivs, corners, by="Plot")
  indivs$UTM.E <- indivs$SW.UTM.E + (indivs$X.offset + indivs$X) #* indivs$X.correct
  indivs$UTM.N <- indivs$SW.UTM.N + (indivs$Y.offset + indivs$Y) #* indivs$Y.correct
  
  # Now calculate angle and distance from the SW corner for each individual
  indivs$angle <- atan(((indivs$Y.offset + indivs$Y) * indivs$Y.correct)/((indivs$X.offset + indivs$X) * indivs$X.correct))
  indivs$dist <- sqrt(((indivs$X.offset + indivs$X) * indivs$X.correct)^2 + ((indivs$Y.offset + indivs$Y) * indivs$Y.correct)^2)
  
  # Calculate corrected UTM coordinates
  indivs$UTM.E.corrected <- indivs$SW.UTM.E + (indivs$dist * cos(indivs$angle - indivs$plot.angle))
  indivs$UTM.N.corrected <- indivs$SW.UTM.N + (indivs$dist * sin(indivs$angle - indivs$plot.angle))
  
  indivs <- indivs[order(indivs$Num),c("Plot","Quad","Num","Species","UTM.E.corrected","UTM.N.corrected","DBH_cm","Type","Year","Notes")]
  #write.csv(indivs,"center_plot_trees.csv",row.names=FALSE,quote=FALSE)
  return(indivs)
  
  # Compare original and corrected coordinates for a plot
  #  plot(indivs$UTM.E[indivs$Plot=="PPW1301"],indivs$UTM.N[indivs$Plot=="PPW1301"], pch=16)
  #  points(indivs$UTM.E.corrected, indivs$UTM.N.corrected, col="red", pch=16)
  
  
  # Some test code to check algorithms on a grid
  #  trees.all <- expand.grid(1:10,1:10)
  #  angle <- atan(trees.all[,2]/trees.all[,1]) #* 180/pi
  #  dist <- sqrt(trees.all[,1]^2 + trees.all[,2]^2)
  #  plot(dist*cos((angle-mean(angle))),dist*sin((angle-mean(angle))))
}

loadFireSeverity <- function(fs=fsall,fsmet='Tubbs.MTBS.RDNBR.30',fire='Tubbs',verbose=F)
{
  if (verbose) {
    head(fs)
    tail(fs)
    names(fs)
  }
  fs$fsvar <- fs[,fsmet]
  if (verbose) print(summary(fs$fsvar))
  if (verbose) print(hist(fs$fsvar))
  
  # RDNBR fire severity levels
  # Unburned
  # Low <170, but burned
  # Medium 170-700
  # High: >700
  # Tubbs unburned: 1308, 1309, 1311, 1312, 1327, 1344, 1347
  # Kincade unburned: 1305, 1306, 1320, 1325, 1328, 1330, 1340, 1341, 1342, 1346, 1349, 1351, 1352, 1352, 1354
  
  # Create a discrete FireSev variable
  if (verbose) hist(fs$fsvar,breaks=c(-400,0,50,100,150,200,300,400,500,600,700,800,1000))
  fs.breaks <- c(-400,135,430,1000) # Based on Parks et al. 2014, but not using intermediate split at 304
  # !!this is for RDNBR.30 !!
  fsCat <- cut(fs$fsvar,fs.breaks)
  if (verbose) print(table(fsCat))
  fs$fsLevel <- as.numeric(fsCat)
  if (verbose) print(table(fs$fsLevel))
  
  # manually code unburned as 0
  if (fire=='Tubbs') 
    unburned.plots <- c('PPW1308','PPW1309','PPW1311','PPW1312','PPW1327','PPW1344','PPW1347') else if (fire=='Kincade') 
      unburned.plots <- c('PPW1305', 'PPW1306', 'PPW1320', 'PPW1325', 'PPW1328', 'PPW1330', 'PPW1340', 'PPW1341', 'PPW1342', 'PPW1346', 'PPW1349', 'PPW1351', 'PPW1352', 'PPW1353', 'PPW1354')

  fs$fsLevel[which(fs$Plot %in% unburned.plots)] <- 0
  if (verbose) print(table(fs$fsLevel))
  fs$fsCat <- as.factor(fs$fsLevel)
  if (verbose) print(table(fs$fsCat))
  if (verbose) print(str(fs$fsCat))
  return(fs)
  ####
}

convertHybrids <- function(d=tAll)
{
  # hard code conversion of QUEBEGA to QUEBER
  d$Species.13[which(d$Species.13=='QUEBEGA')] <- 'QUEBER'
  d$Species.17[which(d$Species.17=='QUEBEGA')] <- 'QUEBER'
  d$Species.18[which(d$Species.18=='QUEBEGA')] <- 'QUEBER'
  d$Species.19[which(d$Species.19=='QUEBEGA')] <- 'QUEBER'
  d$Species.20[which(d$Species.20=='QUEBEGA')] <- 'QUEBER'
  d$Species[which(d$Species=='QUEBEGA')] <- 'QUEBER'
  
  d$Species.13[which(d$Species.13=='QUEDOGA')] <- 'QUEDOU'
  d$Species.17[which(d$Species.17=='QUEDOGA')] <- 'QUEDOU'
  d$Species.18[which(d$Species.18=='QUEDOGA')] <- 'QUEDOU'
  d$Species.19[which(d$Species.19=='QUEDOGA')] <- 'QUEDOU'
  d$Species.20[which(d$Species.20=='QUEDOGA')] <- 'QUEDOU'
  d$Species[which(d$Species=='QUEDOGA')] <- 'QUEDOU'
  
  d$Species.13[which(d$Species.13=='QUEdec')] <- 'QUEDOU'
  d$Species.17[which(d$Species.17=='QUEdec')] <- 'QUEDOU'
  d$Species.18[which(d$Species.18=='QUEdec')] <- 'QUEDOU'
  d$Species.19[which(d$Species.19=='QUEdec')] <- 'QUEDOU'
  d$Species.20[which(d$Species.20=='QUEdec')] <- 'QUEDOU'
  d$Species[which(d$Species=='QUEdec')] <- 'QUEDOU'
  
  d <- d[-which(d$Species=='UNK'),]
  
  d$Type.13[which(d$Type.13=='A2')] <- 'SA'
  d$Type.17[which(d$Type.17=='A2')] <- 'SA'
  #1112.1
  d$Type.18[which(d$Num==1112.1)] <- 'SA'
  return(d)
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
#PWordinate<-function(year,type,metric, psemen.rmv=T){
#library(picante)
#require(vegan)
#df<-plants.by.plot(year,type)
#if(type=="SA.TR"){
#  df$Count<-df$SA.Count+df$TR.Count
#  df$Basal.Area<-df$SA.Basal.Area+df$TR.Basal.Area  
#}
#clim.pts<-as.data.frame(get.clim.pts())

#psemen.plots<-c("PPW1320", "PPW1349", "PPW1340", "PPW1341", "PPW1306")
#if(psemen.rmv==T){df<-df[!(df$Plot%in%psemen.plots),]
#                 clim.pts<-clim.pts[!(clim.pts$Plot%in%psemen.plots),]}

#clim.pts<-clim.pts[clim.pts$Plot%in%unique(df$Plot),]

#mat<-sample2matrix(df[,c("Plot", metric,"Species")])
#ord <- metaMDS(mat)
#plot(ord,type='t')
#return(list(mat,clim.pts)) 
#}

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