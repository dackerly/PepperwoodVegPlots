

#### 
head(h)
boxplot(h$WD~h$Plot.ID, ylab="SOIL MOISTURE (%)", xlab="PLOT ID")

#####
head(bioclim)
max(bioclim$DEM)
min(bioclim$DEM)
# sum across all species for basal area
b<-aggregate(bioclim$Basal.Area ~ bioclim$Plot, FUN=sum)
colnames(b)<-c("Plot","Basal.Area")
b
max(b$Basal.Area)
min(b$Basal.Area)

# range of soil moistures
head(hydro)
max(hydro$Soil.Moisture)
min(hydro$Soil.Moisture)

# number of individuals tagged
head(TR)
sum(TR$Number)
sum(SA$Number)
sum(SA.TR$Number)

# number of species
species<-length(unique(SA.TR$Species))

#WORK ON THIS
#a table of species, with total abundance, 
#basal area, sapling number, juvenile number and seedling number,
#and number of plots in which they occur

(unique(SA.TR$Species))
Species<-(unique(SA.TR$Species))
write.csv(species, file="~/Desktop/species.csv")


# densities
head(TR)
TR1<-aggregate(TR$Number ~ TR$Plot, FUN=sum)
TR2<-aggregate(TR$Basal.Area ~ TR$Plot, FUN=sum)
colnames(TR1)<-c("Plot", "Number")
colnames(TR2)<-c("Plot", "Basal.Area")
TR2$Basal.Area<-round(TR2$Basal.Area,2)
TR3<- merge(TR1, TR2, by="Plot")
dim(TR3)
max(TR1$Number)
min(TR1$Number)

head(SA)
SA1<-aggregate(SA$Number ~ SA$Plot, FUN=sum)
colnames(SA1)<-c("Plot", "NumberS")
head(SA1)
max(SA1$Number)
min(SA1$Number)

write.csv(SA1, file="~/Desktop/SA1.csv")
# most abundant species by stems
SA.TR1<-aggregate(SA.TR$Number ~ SA.TR$Species, FUN=sum)
head(SA.TR1)
colnames(SA.TR1)<-c("Species", "Number")
class(SA.TR1)
SA.TR1
write.csv(SA.TR1, file="~/Desktop/SATR1.csv")

# most abundant tree species by basal area
TR2<-aggregate(TR$Basal.Area~TR$Species, FUN=sum)
colnames(TR2)<-c("Species", "Basal.Area")
head(TR2)
max(TR1$Basal.Area)

############## Abundance of seedlings and juveniles
head(SEJU.Plot)
# by plot
s<-aggregate(SEJU.Plot$Num.Seedlings~SEJU.Plot$Plot, FUN=sum)
j<-aggregate(SEJU.Plot$Num.Juveniles~SEJU.Plot$Plot, FUN=sum)
colnames(s)<-c("Plot", "NumS")
colnames(j)<-c("Plot", "NumJ")
max(s$NumS)
max(j$NumJ)
min(s$NumS)
min(j$NumJ)
sj<-merge(s,j)
write.csv(sj, file="~/Desktop/sj.csv")
# by species
sj1<-aggregate(SEJU.Plot$Total.Number~SEJU.Plot$Species, FUN=sum)
sj1

dem.pts
write.csv(dem.pts, file="~/Desktop/dem.csv")

####################### Species table
head(TR)
TRSB<-aggregate(TR$Basal.Area~TR$Species, FUN=sum)
colnames(TRSB)<-c("Species", "Basal.Area")
head(TRSB)

TRSN<-aggregate(TR$Number~TR$Species, FUN=sum)
colnames(TRSN)<-c("Species", "Number")
head(TRSN)
TRS<- merge(TRSB,TRSN, by="Species")
head(TRS)
TRS$Basal.Area<-round(TRS$Basal.Area,2)
write.csv(TRS, file="~/Desktop/TRS.csv")


head(SA)
SASB<-aggregate(SA$Basal.Area~SA$Species, FUN=sum)
colnames(SASB)<-c("Species", "Basal.Area")
head(SASB)

SASN<-aggregate(SA$Number~SA$Species, FUN=sum)
colnames(SASN)<-c("Species", "Number")
head(SASN)
SAS<- merge(SASB,SASN, by="Species")
head(SAS)
SAS$Basal.Area<-round(SAS$Basal.Area,2)
write.csv(SAS, file="~/Desktop/SAS.csv")

head(SEJU.Plot)
SN<-aggregate(SEJU.Plot$Num.Seedlings~SEJU.Plot$Species, FUN=sum)
JN<-aggregate(SEJU.Plot$Num.Juveniles~SEJU.Plot$Species, FUN=sum)
colnames(SN)<-c("Species", "Num.Seedlings")
colnames(JN)<-c("Species", "Num.Juveniles")
head(SN)
head(JN)
SJN<-merge(SN,JN, by="Species")
write.csv(SJN, file="~/Desktop/SJN.csv")


##### calculating number of plots each species is present, SHOULD BUILD A LOOP
head(SA.TR)
sum(SA.TR$Species=="UNK47")
