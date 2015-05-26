# Intent: Make the 2013 seedling and juvenile count data usable for analyses; creates dataframe SEJU.Plot 
# Author: M.F. Oldfather
# Date Created: 20130603
# Date Last Edited: 20141020

options(stringsAsFactors=FALSE) 
#Setting this option makes sure that character strings aren't treated as factors. 

# use the below script for multiple users, enter your own pathway for setting the working directory in the open brackets after "else"
if (Sys.getenv("USER")=='meaganoldfather') setwd("/Users/meaganoldfather/Box Sync/PepperwoodVegPlots/Database/2013/Woody2013/Data/OriginalCSV/Seedling/") else setwd("")

file.list <-dir(paste(getwd(),"/", sep=''))
strlen <- nchar(file.list[1]) # strlen is the length of all of the file names
plot.list <- substr(file.list,strlen-10,strlen-4) # extracts plot numbers from file name vector

seedling.data<-lapply(file.list, read.csv, skip=3, header=T)
head(seedling.data) 

names(seedling.data) <- plot.list # labels each element with correct plot ID

for (i in 1:length(seedling.data)){
  Plot<-plot.list[i]
  seedling.data[[i]]<-cbind(Plot=Plot, seedling.data[[i]]) #Inserts a plot column 
  colnames(seedling.data[[i]])<-c("Plot", "Quad", "Species", "Num.Seedlings", "Mistake", "Num.Juveniles")
  seedling.data[[i]]<-seedling.data[[i]][,c(1:4,6)]
  # fixes species issues
  seedling.data[[i]][(seedling.data[[i]]$Species=="QUEXXX"), "Species"]<-"QUEDEC"  # lumps all decidious oaks
  seedling.data[[i]][(seedling.data[[i]]$Species=="QUEAGKE"), "Species"]<-"QUEDEC"
  seedling.data[[i]][(seedling.data[[i]]$Species=="QUEDOU"), "Species"]<-"QUEDEC"
  seedling.data[[i]][(seedling.data[[i]]$Species=="QUEKEL"), "Species"]<-"QUEDEC"
  seedling.data[[i]][(seedling.data[[i]]$Species=="QUEGAR"), "Species"]<-"QUEDEC"
  seedling.data[[i]][(seedling.data[[i]]$Species=="UMCAL"), "Species"]<-"UMBCAL"
  seedling.data[[i]][(seedling.data[[i]]$Species=="COFFEE"), "Species"]<-"FRACAL"
  seedling.data[[i]][(seedling.data[[i]]$Species=="LITDEN"), "Species"]<-"NOTDEN"
  seedling.data[[i]][(seedling.data[[i]]$Species=="ARBMAN"), "Species"]<-"ARBMEN"
}

head(seedling.data)
seed.juv.data<-do.call(rbind, seedling.data)
head(seed.juv.data)

# omit rows with missing species 
seed.juv.data<-subset(seed.juv.data, subset=(seed.juv.data$Species!=''))

# put a zero in when no number entered
seed.juv.data[is.na(seed.juv.data$Num.Seedlings),"Num.Seedlings"]<-0
seed.juv.data[is.na(seed.juv.data$Num.Juveniles),"Num.Juveniles"]<-0

# Aggregate the number of seedling and inviduals so it seperated by plot and not by quad
SEJU.Plot<- aggregate(seed.juv.data$Num.Seedlings ~ seed.juv.data$Plot + seed.juv.data$Species, FUN=sum)
SEJU.Plot.JU<- aggregate(seed.juv.data$Num.Juveniles ~ seed.juv.data$Plot + seed.juv.data$Species, FUN=sum)
SEJU.Plot<-cbind(SEJU.Plot, SEJU.Plot.JU[,3])
colnames(SEJU.Plot)<-c("Plot", "Species", "Num.Seedlings", "Num.Juveniles")
SEJU.Plot$Total.Number<-(SEJU.Plot$Num.Seedlings + SEJU.Plot$Num.Juveniles)

# final form of "SEJU.Plot
head(SEJU.Plot)

