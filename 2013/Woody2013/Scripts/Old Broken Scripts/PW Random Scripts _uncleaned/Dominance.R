# Create 'bioclim'


total<-aggregate(bioclim$Basal.Area~bioclim$Plot, FUN=sum)
colnames(total)<-c("Plot","Tot.BA")
head(total)
bio<-merge(total, bioclim , by="Plot")
head(bio)
bio$percent<-(bio$Basal.Area/bio$Tot.BA)
bio[,c(1,2,9,10,16)]

dom<-bio[,c(1,2,9,10,16)]
dom<-subset(dom, subset=(bio$percent >.75))
dim(dom)
dom$Plot
dom$Species
unique(dom$Species)


#### How many oaks? 
head(bioclim)
head(plot.SA.TR) 
dec<-subset(plot.SA.TR, subset=(plot.SA.TR$Species == "QUEGAR" | plot.SA.TR$Species == "QUEDOU" ))
dim(dec)
head(dec)
n.both<-aggregate(dec$Number ~ dec$Species, FUN=sum)
n.both
n.qd<-
n.qg<-  

