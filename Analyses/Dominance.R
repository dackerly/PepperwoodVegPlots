# Intent: Look at domimant species in each plot by calculating the percentage of each species
# relative to the total biomass of the plot
# Author: M.F. Oldfather
# Date Created: 2013?
# Date Last Edited: 20141021

# use the below script for multiple users, enter your own pathway for setting the working directory in the open brackets after "else"
if (Sys.getenv("USER")=='meaganoldfather') setwd("/Users/meaganoldfather/Box Sync/PepperwoodVegPlots/Database/Analyses") else setwd("")

# create data freame "bioclim"
source("climate.pts.R")

total.basal<-aggregate(bioclim$Basal.Area~bioclim$Plot, FUN=sum)
colnames(total.basal)<-c("Plot","Total.BA")
head(total)
dominance<-merge(total, bioclim , by="Plot")
# Calculate the percentage of that species in that plot relative to total biomass
dominance$Percent<-(dominance$Basal.Area/dominance$Tot.BA)
head(dominance)

# Subset the dataframe for only the species with the max "percent" for each plot
dominance.max<-data.frame()
for(i in 1:length(plot.list)){
  test<-dominance[dominance$Plot==plot.list[i],]
  dominance.max<-rbind(dominance.max, test[which.max(test$Percent),])
}
head(dominance.max)
# list of dominant species
dominance.max$Species
