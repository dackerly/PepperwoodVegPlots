# Intent: Calculate the percent each plot is descidious versus evergreen of a plot 
# Author: M.F. Oldfather
# Date Created: 20141020
# Date Last Edited: 20141020

# use the below script for multiple users, enter your own pathway for setting the working directory in the open brackets after "else"
if (Sys.getenv("USER")=='meaganoldfather') setwd("/Users/meaganoldfather/Box Sync/PepperwoodVegPlots/Database/") else setwd("")

# source MasterData.R in order to use the dataframe "SA.TR" and "plot.info"
source("Analyses/MasterData.R")

# THIS SCRIPT IS NOT YET FUCNTIONAL

head(SA.TR)

# aggregate total number or basal area of all trees 

All.Number<-with(SA.TR, aggregate(Number~Plot, FUN=sum))
head(All.Number)
All.Cover<-with(dec, aggregate(Basal.Area~Plot, FUN=sum))
head(All.Cover)

# list of descidious species present in our plots
dec.species<- c("QUEGAR", "QUEDOU", "QUELOB", "QUEKEL", "QUEDEC", "AMOCAL")


# subset the "SA.TR" dataframe to just be the descidious individual 
dec<- subset(SA.TR, subset=(SA.TR$Species %in% dec.species))
head(dec)

# create an aggregate of the number of all descidious individuals in each plot 
dec.num<-aggregate(dec$Number ~ dec$Plot, FUN=sum)
colnames(dec.num)<- c("Plot", "Dec.Number")
head(dec.num)

# create an aggregate of the basal area of all descidious individuals in each plot 
dec.cover<-aggregate(dec$Basal.Area ~ dec$Plot, FUN=sum)
colnames(dec.cover)<- c("Plot", "Dec.Basal.Area")
head(dec.cover)





########### 

basal<- aggregate(TR$Basal.Area ~ TR$Plot, FUN=sum)
head(basal)
colnames(basal)<- c("Plot", "All")
Tree<-merge(TR, basal, by= "Plot") 
head(Tree)

dec<- subset(Tree, subset=(Tree$Species %in% dec.species)) 
head(dec)
dec$Percent<- dec$Basal.Area / dec$All *100
dec.ag<- aggregate(dec$Percent ~ dec$Plot, FUN=sum)  
dec.ag
colnames(dec.ag)<- c("Plot", "Percent")
dec.ag<- subset(dec.ag, subset=(dec.ag$Percent > 75))
dec.ag
dim(dec.ag)