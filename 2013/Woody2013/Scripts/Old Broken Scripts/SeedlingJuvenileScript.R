# June 3,2013
# Intention of this script is make seedling and juvenile data for TBC Pepperwood plot usable for analyses 


options(stringsAsFactors=FALSE) 
#Setting this option makes sure that character strings aren't treated as factors. 

if (Sys.getenv("USER")=='meaganoldfather') setwd("/Users/meaganoldfather/Dropbox/PepperwoodVegPlots/Database/2013/Woody2013/Data/OriginalCSV/Seedling/") else setwd("/Users/david/Documents/Projects/Dropbox/PepperwoodVegPlots_2013/Database/2013/Woody2013Data/OriginalCSV/Seedling/")
# In order to use the same script with multiple users; Final folder is 'Seedling'

file.list <-dir(paste(getwd(),"/", sep=''))
file.list # list of file names of csv's in 'PlotInfo'

strlen <- nchar(file.list[1]) # strlen is the length of all of the file names
plot.list <- substr(file.list,strlen-10,strlen-4) # extracts plot numbers from file name vector
plot.list # list of plot names

seedling.data<-lapply(file.list, read.csv, skip=3, header=T)
head(seedling.data) 

names(seedling.data) <- plot.list # labels each element with correct plot ID

#juveniles<-vector("list", length(seedling.data)) 
#names(juveniles) <- plot.list
#seedlings<-vector("list", length(seedling.data))
#names(seedlings) <- plot.list

for (i in 1:length(seedling.data)){
  Plot<-plot.list[i]
  seedling.data[[i]]<-cbind(Plot=Plot, seedling.data[[i]]) #Inserts a plot column 
  colnames(seedling.data[[i]])<-c("Plot", "Quad", "Species", "Num.Seedlings", "Mistake", "Num.Juveniles")
  seedling.data[[i]]<-seedling.data[[i]][,1:6]
  
  #seedling.data[[i]][(seedling.data[[i]]$Species=="QUEXXX"), "Species"]<-"QUEDEC"  # lumps all decidious oaks
  #seedling.data[[i]][(seedling.data[[i]]$Species=="QUEAGKE"), "Species"]<-"QUEDEC"
  seedling.data[[i]][(seedling.data[[i]]$Species=="QUEDOU"), "Species"]<-"QUEDEC"
  #seedling.data[[i]][(seedling.data[[i]]$Species=="QUEKEL"), "Species"]<-"QUEDEC"
  seedling.data[[i]][(seedling.data[[i]]$Species=="QUEGAR"), "Species"]<-"QUEDEC"
  seedling.data[[i]][(seedling.data[[i]]$Species=="UMCAL"), "Species"]<-"UMBCAL"
  seedling.data[[i]][(seedling.data[[i]]$Species=="COFFEE"), "Species"]<-"FRACAL"
  seedling.data[[i]][(seedling.data[[i]]$Species=="LITDEN"), "Species"]<-"NOTDEN"
  seedling.data[[i]][(seedling.data[[i]]$Species=="ARBMAN"), "Species"]<-"ARBMEN"
}

head(seedling.data)


little.guys<-do.call(rbind, seedling.data)
little.guys$Mistake<-NULL # gets rid of 'Mistake' column 
head(little.guys)

little.guys<-subset(little.guys, subset=(little.guys$Species!=''))

little.guys[is.na(little.guys$Num.Seedlings),"Num.Seedlings"]<-0
little.guys[is.na(little.guys$Num.Juveniles),"Num.Juveniles"]<-0

# Aggregate the number of seedling and inviduals so it seperated by plot and not by quad
SEJU.Plot<- aggregate(little.guys$Num.Seedlings ~ little.guys$Plot + little.guys$Species, FUN=sum)
SEJU.Plot.JU<- aggregate(little.guys$Num.Juveniles ~ little.guys$Plot + little.guys$Species, FUN=sum)
SEJU.Plot<-cbind(SEJU.Plot, SEJU.Plot.JU[,3])
colnames(SEJU.Plot)<-c("Plot", "Species", "Num.Seedlings", "Num.Juveniles")
head(SEJU.Plot)

SEJU.Plot$Total.Number<-(SEJU.Plot$Num.Seedlings + SEJU.Plot$Num.Juveniles)

# Mean number of juveniles and seedlings across all plots
head(SEJU.Plot)
x<- aggregate(SEJU.Plot$Num.Seedlings ~ SEJU.Plot$Species, FUN=sum)
x<-mean(SEJU.Plot$Num.Juveniles)
x

## Descidious Little Ones
SEJU.DEC<-subset(SEJU.Plot, subset=(SEJU.Plot$Species=="QUEDEC"))
SEJU.DEC
x<-merge(SEJU.DEC, bioclim, by = "Plot")
with(x, plot(Soil.Moisture, Total.Number))
with(x, plot(Soil.Moisture, Num.Juveniles))
with(x, plot(Soil.Moisture, Num.Seedlings))

with(x, plot(tpi500, Total.Number))
with(x, plot(tpi500, Num.Juveniles))
with(x, plot(tpi500, Num.Seedlings))
library("lme4")

y<-(with(x,lmer(Num.Seedlings ~ MarchRadiation * Soil.Moisture)))
a<-anova(y)
summary(y)

# bar chart
head(SEJU.Plot)
sj<- aggregate(SEJU.Plot$Total.Number~ SEJU.Plot$Species, FUN=sum)
colnames(sj)<-c("Species", "SJNUM" )
sj$Species<-reorder(sj$Species, -sj$SJNUM)
barchart(Species~SJNUM, data=sj, scales=list(cex=0.75, x=list(log=10)) , 
         col="black", xlab="Seedling+Juvenile Number", ylab="Species")