head(bioclim)
bio.QG<-subset(bioclim, bioclim$Species=="QUEGAR")
bio.QD<-subset(bioclim, bioclim$Species=="QUEDOU")



with(bio.QD, plot(Soil.Moisture, Number, col="black", pch=19))
with(bio.QG, points(Soil.Moisture, Number, col="green", pch=19))




with(bio.QD, points(Soil.Moisture, Basal.Area, col="black", pch=19))
with(bio.QG, plot(Soil.Moisture, Basal.Area, col="black", xlab="Soil Moisture (%)",
                  ylab="Basal Area (cm2)"))

x<-lm(Basal.Area ~ MarchRadiation, data=bio.QG)
summary(x)


#############

head(SA)
head(TR)
head(SA.TR)
#SA.TR<-SA.TR[,-5]
all<-aggregate(Basal.Area ~ Plot, data=SA.TR, FUN=sum)
head(all)

# if percentage of plot is greater than QUEDEC

d<-SA.TR$Species[]

###########

ag<-aggregate(TR$Basal.Area ~ TR$Species, FUN=sum)
agS<-aggregate(SA$Basal.Area ~ SA$Species, FUN=sum)
agTS<-aggregate(SA.TR$Basal.Area ~ SA.TR$Species, FUN=sum)

###### TR and SA
agTS<-agTS[1:22,]
colnames(agTS)<-c("Species", "Basal.Area")
agTS
#xy$x <- reorder( xy$x, xy$y )
agTS$Species<-reorder(agTS$Species, -agTS$Basal.Area)
agTS1<-agTS[(agTS$Basal.Area>50000),]
barchart(Basal.Area~Species,data=agTS,scales=list(cex=0.5), col="black")
barchart(Species~Basal.Area,data=agTS1, scales=list(cex=0.75), 
         col="black", xlab="Basal Area (cm2)")


## only T
ag
colnames(ag)<-c("Species", "Basal.Area")
#xy$x <- reorder( xy$x, xy$y )
ag1$Species<-reorder(ag1$Species, ag1$Basal.Area)
ag$Species<-reorder(ag$Species, -ag$Basal.Area)
ag1<-ag[(ag$Basal.Area>1000),]
#barchart(Basal.Area~Species,data=agTS,scales=list(cex=0.5), col="black")
barchart(Species~Basal.Area,data=ag1, scales=list(cex=0.75), 
         col="black", xlab="Basal Area (cm2)")
barchart(Species~log(Basal.Area),data=ag, scales=list(cex=0.75), 
         col="black", ylab="Species", xlab="Basal Area (cm2)", main="Tree Basal Area")

# what percentace of the basal area is in top 11 species with over 1000 cm2?
a<-sum(ag1$Basal.Area) # greater than 1000
a
b<-sum(ag$Basal.Area) # all 
b
c<-b-a # less than 1000
# ratio of greater than 1000 to all
d<- a/b
d
e<-c/b
e
#### only S
agS
colnames(agS)<-c("Species", "Basal.Area")
#xy$x <- reorder( xy$x, xy$y )
agS1<-agS[(agS$Basal.Area>1000),]
agS1$Species<-reorder(agS1$Species, -agS1$Basal.Area)
agS$Species<-reorder(agS$Species, -agS$Basal.Area)

#barchart(Basal.Area~Species,data=agSTS,scales=list(cex=0.5), col="black")
barchart(Basal.Area~Species,data=agS1, scales=list(cex=0.75), 
         col="black", ylab="Basal Area (cm2)")
barchart(Species~Basal.Area,data=agS, scales=list(cex=0.75, x=list(log=10)), 
         col="black", ylab="Species", xlab="Basal Area (cm2)", main="Sapling Basal Area"
         , xmin=0)


#barchart for number of individuals (TR+SA)
numTS<-aggregate(SA.TR$Number ~ SA.TR$Species, FUN=sum)
head(numTS)
colnames(numTS)<-c("Species", "Number")
numTS$Species<-reorder(numTS$Species, -numTS$Number)
barchart(Species~Number, data=numTS, scales=list(cex=0.75, x=list(log=10)) , 
col="black", xlab="Tree+Sapling Number", ylab="Species")