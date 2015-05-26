# Intent: Bring in densiometer data taken in May 2013 for potential analyses
# Author: M.F. Oldfather
# Date Created: 20131111
# Date Last Edited: 20141020

# use the below script for multiple users, enter your own pathway for setting the working directory in the open brackets after "else"
if (Sys.getenv("USER")=='meaganoldfather') setwd("/Users/meaganoldfather/Box Sync/PepperwoodVegPlots/Database/2013/Light2013/Data/OriginalCSV/") else setwd("")
dir()



light<-read.csv("LightMeasurments2013.csv")
head(light)
light<-light[,-6]
light<-light[,-1]
colnames(light)<-c("Plot", "Quad", "Dir", "Open")
light<-as.data.frame(light)
light$Sq<-light$Open/4  # divide by four to get number of open total squares
light$Density<- (100 - (light$Sq*4.17)) # multiple by 4.17 to get overstory density

light<-aggregate(light$Density~light$Plot + light$Quad, FUN=mean) # aggregate for each of the 4 pts in each quad
colnames(light)<-c("Plot", "Quad", "CanopyDensity")
head(light)

light.plot<-aggregate(light$CanopyDensity ~ light$Plot, FUN=mean)
colnames(light.plot)<-c("Plot", "CanopyDensity")
head(light.plot)

library("lattice")
boxplot(light.plot$CanopyDensity~light.plot$Plot, ylab="Overstory Density %", xlab="Plot ID")

