# Intent: Brings in the environmental variables (eg. % soil, bedrock, herb) for each plot measured in 2013
# Author: M.F. Oldfather
# Date Created: 2013?
# Date Last Edited: 20141020

# use the below script for multiple users, enter your own pathway for setting the working directory in the open brackets after "else"
if (Sys.getenv("USER")=='meaganoldfather') setwd("/Users/meaganoldfather/Box Sync/PepperwoodVegPlots/Database/2013/Woody2013/Data/OriginalCSV/PlotInfo/") else setwd("")

file.list <-dir(paste(getwd(),"/", sep=''))
strlen <- nchar(file.list[1]) # strlen is the length of all of the file names
plot.list <- substr(file.list,strlen-10,strlen-4) # extracts plot numbers from file name vector

# read in data from 
envr.data<-lapply(file.list, read.csv, skip=16, header=T)
head(envr.data) 
names(envr.data) <- plot.list # labels each element with the plot name

for (i in 1:length(envr.data)){
  Plot<-plot.list[i]
  envr.data[[i]]<-cbind(Plot=Plot, envr.data[[i]]) #Inserts a plot column 
  colnames(envr.data[[i]])<-c("Plot", "Quad", "Bedrock", "Soil", "Boulder", "Fine", "Herb", "Litter", "Total")
  envr.data[[i]]<-envr.data[[i]][,1:9]
  envr.data[[i]]<-subset(envr.data[[i]], subset=(!is.na(envr.data[[i]][,8])))
  }
head(envr.data)
envr.data<-do.call(rbind, envr.data)
#remove plot 1301 because missing data if future analyses desired
envr.data<-subset(envr.data, subset=(envr.data$Plot!="PPW1301"))
head(envr.data)
tail(envr.data)

# regressions with climate.pts and the total Herb cover have been explored, but
# clear pattern have arisen yet 

