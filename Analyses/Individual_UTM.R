# Intent: Make UTM coordinates for each alive individual & visualize the individuals (sizes, species) in the plots 
# Author: Meagan F. Oldfather
# Date Created: 20150627

# clear workspace
rm(list=ls())

#Packages
library("RCurl")
library("data.table")
library("picante")

# sources in all functions
source_https <- function(url, ...) { 
  # parse and evaluate each .R script
  sapply(c(url, ...), function(u) {
    eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
  })
}
source_https('https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/Analyses/PWfunctions_GitHub.R')

# get individual data
indv.data<-get.indv.data(2014)
head(indv.data)
tail(indv.data)

# get UTM coordinates for each plots SW corner, add to to indv df
sw.coords<-get.plot.info()
sw.coords$UTM.E<-as.numeric(sw.coords$UTM.E)
sw.coords$UTM.N<-as.numeric(sw.coords$UTM.N)
df<-merge(indv.data, sw.coords, by="Plot")
df<-df[,1:10]
colnames(df)<-c("Plot","Quad","Type","Num","Species", "X", "Y", "Basal.Area", "SW.Easting", "SW.Northing")
head(df)

# Calculate the amount necessary to add to the UTM for each quad
# A1: 0 added to X, 0 added to Y 
# A2: 0 added to X, 5 added to Y 
# A3: 0 added to X, 10 added to Y 
# A4: 0 added to X, 15 added to Y 
# B1: 5 added to X, 0 added to Y 
# B2: 5 added to X, 5 added to Y 
# B3: 5 added to X, 10 added to Y 
# B4: 5 added to X, 15 added to Y 
# C1: 10 added to X, 0 added to Y 
# C2: 10 added to X, 5 added to Y 
# C3: 10 added to X, 10 added to Y 
# C4: 10 added to X, 15 added to Y 
# D1: 15 added to X, 0 added to Y 
# D2: 15 added to X, 5 added to Y 
# D3: 15 added to X, 10 added to Y 
# D4: 15 added to X, 15 added to Y 

quads<-unique(df[!is.na(df$Quad), "Quad"])
x.adds<-c(0,0,5,5,5,10,10,10,15,15,0,5,10,15,15,0) 
y.adds<-c(5,15,15,10,0,0,5,10,15,5,0,5,15,0,10,10)  
adds<-data.frame(Quad=quads, X.add=x.adds, Y.add=y.adds)

# add additional amount to add to each for specific quad
df<- merge(df,adds, by="Quad")

# change the measured X's and Y's to cm 
df$X<-df$X/100
df$Y<-df$Y/100
head(df)
str(df)
# add all measurments up for an individual UTM
df$Indv.UTM.E<-df$SW.Easting+df$X.add+df$X
df$Indv.UTM.N<-df$SW.Northing+df$Y.add+df$Y
head(df)
tail(df)

# reduce down to necessary columns
indv.UTM<-df[,c(2,1,3,4,5,8,13,14)]

# NOTE: when using head you will only see 7 digits for the coordinates, but the data does go out to more decimal places in the below csv file
head(indv.UTM)

# writes csv
write.csv(indv.UTM, "Desktop/IndvUTM.csv")

# visualize a plot
# colors for main species, black for rest of species
dom<-c("QUEAGR", "PSEMEN", "QUEGAR", "QUEDOU", "QUEKEL", "ARBMEN", "UMBCAL" ,"ARCMAN", "HETARB","AESCAL")
sp.col<-c("brown", "red","darkgreen", "dodgerblue", "purple","orange","green","pink","lightgrey","yellow", "black") 
# size depends on Basal. Area

see.plot<-function(data, plot){ 
# subset by a single plot
 df<-subset(data, data$Plot==plot) 
 col<-sp.col[match(df$Species, dom, nomatch = 11)]
plot(df$Indv.UTM.E, df$Indv.UTM.N,xlab="",xlim=c(df$SW.Easting[1],df$SW.Easting[1]+20), ylim=c(df$SW.Northing[1], df$SW.Northing[1]+20), ylab="",yaxt="n", xaxt="n",col=col,pch=19, cex=(sqrt(df$Basal.Area/pi)/5), asp=1)
abline(v=df$SW.Easting[1]+c(5,10,15), lty="dashed", col="grey")  
abline(h=df$SW.Northing[1]+c(5,10,15), lty="dashed", col="grey") 
axis(side=1, at = df$SW.Easting[1]+c(2.5,7.5,12.5, 17.5), labels=LETTERS[1:4], tick=F)
  axis(side=2, at = df$SW.Northing[1]+c(2.5,7.5,12.5, 17.5), labels=1:4, tick=F)
  }
# an example
see.plot(df,"PPW1302")
