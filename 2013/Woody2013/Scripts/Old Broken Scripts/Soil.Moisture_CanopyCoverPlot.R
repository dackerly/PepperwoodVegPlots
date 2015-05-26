head(hydro) # made in Climate.Pts.R
head(light2) # made in LightMeasures20131111
light3<-aggregate(light2$Density ~ light2$Plot, FUN=mean)
colnames(light3)<-c("Plot", "Density")
hl<-merge(light3, hydro, by="Plot")
head(hl)
hl$DEC<-ugh4$DEC
plot(Density~Soil.Moisture, data=hl[hl$DEC==0,], pch=19, xlab="Soil Moisture %", 
       ylab="Overstory Density %",col="red")
points(Density~Soil.Moisture, data=hl[hl$DEC==1,], pch=19,col="blue")

maybe<-lm(Density~Soil.Moisture, data=hl)
summary(maybe) # not significant

# need to include which plot are greater than 50% descidious! 
# use TotalperSpeciesperPlot.R script to make 'total' than use the below code
# descidious oak will be forever blue

ugh<-total 
ugh[ugh$Species == "QUEGAR", "Species"]<-"QUEDEC"             
ugh[ugh$Species == "QUEDOU", "Species"]<-"QUEDEC"     
ugh[ugh$Species == "QUEKEL", "Species"]<-"QUEDEC" 

ugh1<-aggregate(ugh$BA ~ugh$Plot+ugh$Species, FUN=sum)
colnames(ugh1)<-c("Plot", "Species", "BA")

ugh2<-aggregate(ugh1$BA ~ ugh1$Plot, FUN=sum) # all species
ugh2
colnames(ugh2)<-c("Plot", "BA")
ugh3<-aggregate(ugh1[ugh1$Species=="QUEDEC","BA"] ~ ugh1[ugh1$Species=="QUEDEC", "Plot"], FUN=sum) # just QUEDEC
ugh3
colnames(ugh3)<-c("Plot", "BA.DEC" )

ugh4<-merge(ugh2,ugh3, by="Plot")
ugh4$Ratio<-ugh4$BA.DEC/ugh4$BA
ugh4$DEC<-0
ugh4[ugh4$Ratio>.5,"DEC"]<-1
ugh4
