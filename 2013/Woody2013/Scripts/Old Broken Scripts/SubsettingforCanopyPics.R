# create TR from AllDataCreation20131016.R 
TR
x<-c(1,2,3,4,5)
x
class(x)
class(TR)
head(TR)
tail(TR)
colnames(TR)
unique(TR$Species)
dec.species<- c("QUEGAR", "QUEDOU", "QUELOB", "QUEKEL", "QUEDEC")

dec<- subset(TR, subset=(TR$Species %in% dec.species))
head(dec)

dim(dec)
dec.ag<-aggregate(dec$Number ~ dec$Plot, FUN=sum)
dec.ag

dim(dec.ag)

colnames(dec.ag)<- c("Plot", "Number")

dec.lots<-subset(dec.ag, subset=(dec.ag$Number < 10))
dec.lots
dim(dec.lots)

dec[dec$Plot=="PPW1344", ]


########### 
head(TR)
TR[2,1]
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