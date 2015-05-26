# Intent: Pull in environemental variables acorss plots for PW
# columns - plot, % of each type ; can up to over 100

if (Sys.getenv("USER")=='meaganoldfather') setwd("/Users/meaganoldfather/Dropbox/PepperwoodVegPlots/Database/2013/Woody2013/Data/OriginalCSV/PlotInfo/") else setwd("/Users/david/Documents/Projects/Dropbox/PepperwoodVegPlots_2013/Database/2013/Woody2013/Data/OriginalCSV/PlotInfo/")

file.list <-dir(paste(getwd(),"/", sep=''))
file.list # list of file names of csv's in 'PlotInfo'
strlen <- nchar(file.list[1]) # strlen is the length of all of the file names
plot.list <- substr(file.list,strlen-10,strlen-4) # extracts plot numbers from file name vector
plot.list # list of plot names

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
ed<-do.call(rbind, envr.data)
#remove plot 1301 because missing data
ed<-subset(ed, subset=(ed$Plot!="PPW1301"))
head(ed)
tail(ed)
bedrock[i]<-mean(ed[ed$Plot[i]==ed$Plot[i], "Bedrock"])

bedrock<-(numeric(50))

for (i in length(50)){
  bedrock[i]<-mean(ed[ed$Plot[i]==ed$Plot[i], "Bedrock"[i]])  
}
library("lattice")
par(mfrow=c(2,1))
boxplot(Bedrock~Plot, data=ed, xlab="Plot ID", ylab="Percent Cover",
        main="Bedrock")
boxplot(Soil~Plot, data=ed, xlab="Plot ID", ylab="Percent Cover",
        main="Soil")
boxplot(Boulder~Plot, data=ed, xlab="Plot ID", ylab="Percent Cover",
        main="Boulder")

boxplot(Fine~Plot, data=ed, xlab="Plot ID", ylab="Percent Cover",
        main="Fine", ylim=c(0,100))
boxplot(Herb~Plot, data=ed, xlab="Plot ID", ylab="Percent Cover",
        main="Herb", ylim=c(1,100))
boxplot(Litter~Plot, data=ed, xlab="Plot ID", ylab="Percent Cover",
        main="Litter", ylim=c(1,100))


max(ed$Litter)
min(ed$Soil)
mean(ed$Soil)

# x<-TR[TR$Species=="QUEGAR", "Plot"]
# y<-TR[TR$Species=="QUEDOU", "Plot"]
# x
# y
# match(x,y) #1310, 1311, 1325, 1335, 1345

head(envr.data)
head(ed)

Herb<-(numeric(50))

for (i in length(Herb)){
  Herb[i]<-mean(ed[ed$Plot[i]==ed$Plot[i], "Herb"[i]])  
}
Herb


mean(ed[ed$Plot[3]==ed$Plot[3], "Herb"])


####

Herb<-aggregate(ed$Herb ~ed$Plot , FUN=mean)
colnames(Herb)<-c("Plot", "HerbCover")
Herb

head(climate.pts)
h1<-merge(climate.pts, Herb, by="Plot")
h1<-merge(plot.info, h1, by="Plot")
head(h1)
# nothing significant
lm1<-lm(CC~Soil.Moisture, data=h2)
lm1<-lm(CC~TMaxJUL, data=h2)
lm1<-lm(CC~tminJAN, data=h2) # significant
lm1<-lm(CC~MarchRadiation, data=h2)
lm1<-lm(CC~DEM, data=h2)
lm1<-lm(CC~tpi500, data=h2)
lm1<-lm(CC~Slope, data=h2)
lm1<-lm(CC~Aspect, data=h2)
summary(lm1)
plot(h1$DEM, h1$Herb)

plot.info$slope<-as.numeric(plot.info$Slope)

head(light)
h2<-merge(h1, light, by="Plot")
head(h2)

lm1<-lm(HerbCover~CC, data=h2)
plot(lm1)
with(h2, plot(CC,HerbCover, pch=19))
abline(lm1)


with(h2, plot(CC, TMaxJUL))
with(h2, plot(CC, tminJAN))


lm2<-lm(HerbCover~TMaxJUL * tminJAN, data=h2)
summary(lm2)

with(h2, plot(CC, Soil.Moisture))
 with (h2, plot(tminJAN, CC))
abline(lm1)