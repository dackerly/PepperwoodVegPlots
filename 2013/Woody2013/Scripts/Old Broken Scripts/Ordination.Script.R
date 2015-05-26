#March 9, 2013
source("/Users/meaganoldfather/Dropbox/PepperwoodVegPlots/Database/2013/Woody2013/Scripts/AllDataCreation20131016.R")

library(picante)
require(vegan)
require(ggplot2)


############## COMMUNITY ORDINATION ##############
head(plot.SA)
head(plot.TR)
head(plot.SA.TR)



#Obvious Doug Fir Removal Plots 1320, 1349, 1340, 1341, 1306, Maybe also 1321 and 1325
# Removing PSEMEN removal plots from ordinations for all three dataframes 
plot.SA.noP<-subset(plot.SA, subset=(plot.SA$Plot != "PPW1320"))
plot.SA.noP<-subset(plot.SA.noP, subset=(plot.SA.noP$Plot != "PPW1349"))
plot.SA.noP<-subset(plot.SA.noP, subset=(plot.SA.noP$Plot != "PPW1340"))
plot.SA.noP<-subset(plot.SA.noP, subset=(plot.SA.noP$Plot != "PPW1341"))
plot.SA.noP<-subset(plot.SA.noP, subset=(plot.SA.noP$Plot != "PPW1306"))

plot.TR.noP<-subset(plot.TR, subset=(plot.TR$Plot != "PPW1320"))
plot.TR.noP<-subset(plot.TR.noP, subset=(plot.TR.noP$Plot != "PPW1349"))
plot.TR.noP<-subset(plot.TR.noP, subset=(plot.TR.noP$Plot != "PPW1340"))
plot.TR.noP<-subset(plot.TR.noP, subset=(plot.TR.noP$Plot != "PPW1341"))
plot.TR.noP<-subset(plot.TR.noP, subset=(plot.TR.noP$Plot != "PPW1306"))

plot.SA.TR.noP<-subset(plot.SA.TR, subset=(plot.SA.TR$Plot != "PPW1320"))
plot.SA.TR.noP<-subset(plot.SA.TR.noP, subset=(plot.SA.TR.noP$Plot != "PPW1349"))
plot.SA.TR.noP<-subset(plot.SA.TR.noP, subset=(plot.SA.TR.noP$Plot != "PPW1340"))
plot.SA.TR.noP<-subset(plot.SA.TR.noP, subset=(plot.SA.TR.noP$Plot != "PPW1341"))
plot.SA.TR.noP<-subset(plot.SA.TR.noP, subset=(plot.SA.TR.noP$Plot != "PPW1306"))

TR.mat.cover<- sample2matrix(plot.TR.noP[,c(1,3,2)])# makes a matrix for TR basal area  
TR.mat.num<- sample2matrix(plot.TR.noP[,c(1,4,2)]) # makes a matrix for TR number
head(TR.mat.cover)
head(TR.mat.num)

# Plot of QUEDOU versus QUEGAR basal area across all plots
plot(TR.mat.cover$QUEDOU, TR.mat.cover$QUEGAR, 
     main="Basal Area(cm2) Per Plot", xlab="Blue Oak", ylab="Oregon Oak", pch=19)

# Plot of QUEDOU versus QUEGAR number across all plots
plot(TR.mat.num$QUEDOU, TR.mat.num$QUEGAR, 
     main="Number of Individuals Per Plot", xlab="Blue Oak", ylab="Oregon Oak", pch=19)


SA.mat.cover<- sample2matrix(plot.SA.noP[,c(1,3,2)])# makes a matrix for Sa basal area  
SA.mat.num<- sample2matrix(plot.SA.noP[,c(1,4,2)]) # makes a matrix for SA number
head(SA.mat.cover)
head(SA.mat.num)

SA.TR.mat.cover<- sample2matrix(plot.SA.TR.noP[,c(1,3,2)])# makes a matrix for SA.TR basal area  
SA.TR.mat.num<- sample2matrix(plot.SA.TR.noP[,c(1,4,2)]) # makes a matrix for SA.TR number
# same for all plots
#SA.TR.mat.cover<- sample2matrix(plot.SA.TR[,c(1,3,2)])# makes a matrix for SA.TR basal area  
#SA.TR.mat.num<- sample2matrix(plot.SA.TR[,c(1,4,2)]) # makes a matrix for SA.TR number

head(SA.TR.mat.cover)
head(SA.TR.mat.num)

ord <- metaMDS(TR.mat.cover)
summary(ord)
plot(ord,type='t')
attributes(ord)
x<-dist(TR.mat.cover)
wcmdscale(x)
#wascores(ord, climate.noP)

# Now time to use analyses of these ordination with help from Bier's script!

heatmap(as.matrix(SA.TR.mat.num), Rowv = NA, Colv = NA) #hot color = low values, light color = high value
heatmap(as.matrix(SA.TR.mat.cover), Rowv = NA, Colv = NA)
heatmap(as.matrix(SA.TR.mat.cover))
heatmap(as.matrix(SA.TR.mat.num))

plot.rich <- apply(SA.TR.mat.num > 0, MARGIN = 1, FUN = sum) # calculates species richness
plot.rich


# Source in climate data from all plots
source("/Users/meaganoldfather/Dropbox/PepperwoodVegPlots/Database/2013/Woody2013/Scripts/GISLayersScript20130904.R")
head(climate.pts)
head(bioclim)
plot.H <- diversity(SA.TR.mat.num) # Shannon entropy
plot.N1 <- exp(plot.H) ## Shannon number of diversity
plot.N1
plot.N2 <- diversity(plot.N1, "inv") ## Simpson Diversity
plot.N2 # The simpson diversity across our plot is 40.23945

plot.pca <- prcomp(TR.mat.num)
plot.pca
summary(plot.pca)
biplot(plot.pca) 

plot.mds <- metaMDS(TR.mat.cover) # function of "metaMDS" , can change matrix used
plot.mds
stressplot(plot.mds)

# need to also remove PSEMEN plots from climate data
climate.noP<-subset(climate.pts, subset=(climate.pts$Plot != "PPW1320"))
climate.noP<-subset(climate.noP, subset=(climate.noP$Plot != "PPW1349"))
climate.noP<-subset(climate.noP, subset=(climate.noP$Plot != "PPW1341"))
climate.noP<-subset(climate.noP, subset=(climate.noP$Plot != "PPW1306"))

plot.mds<-metaMDS(TR.mat.cover, trymax=100) #can change matrix used. or distance="c"

fit1 <- envfit(plot.mds, climate.noP$Soil.Moisture, perm = 99)
fit2<- envfit(plot.mds,climate.noP$tpi500, perm=99)
fit3<- envfit(plot.mds,climate.noP$tminJAN, perm=99)
plot(plot.mds)
plot(fit1)
plot(fit2)
plot(fit3)
fit1 # 0.06 --> close! With SA.TR. cover
fit2 # 0.68
fit3 # 0.82
# fits are worse for just TR.cover and for TR.num and SA.TR.NUM 
mds.score <- scores(plot.mds)
############################
# CCA Attempts
pw.cca <- cca(TR.mat.cover ~ MarchRadiation + tpi500 + Soil.Moisture, data=climate.noP)
pw.cca <- cca(TR.mat.cover ~ MarchRadiation + Soil.Moisture, data=climate.noP)

pw.cca
plot(pw.cca)

## ‘Partialling out’ and ‘negative components of variance’
cca(varespec ~ Ca, varechem)
cca(varespec ~ Ca + Condition(pH), varechem)


attempt<-cca(TR.mat.cover, climate.noP)

############################


# ordN <- metaMDS(tree.number.mat)
# plot(ordN,type='t')
# 
# # Ordination for saplings
# head(sapling.cover)
# # remove PSEMEN plots 
# sapling.cover.noremoval<- subset(sapling.cover, subset=(sapling.cover$Plot != "PPW1320")) 
# sapling.cover.noremoval<- subset(sapling.cover.noremoval, subset=(sapling.cover.noremoval$Plot != "PPW1349"))
# sapling.cover.noremoval<- subset(sapling.cover.noremoval, subset=(sapling.cover.noremoval$Plot != "PPW1341"))
# sapling.cover.noremoval<- subset(sapling.cover.noremoval, subset=(sapling.cover.noremoval$Plot != "PPW1306"))
# sapling.cover.noremoval
# 
# 
# sap.cover.mat <- sample2matrix(sapling.cover.noremoval[,c(1,4,2)])
# sap.number.mat <- sample2matrix(sapling.cover.noremoval[,c(1,3,2)])
# head(sap.cover.mat)
# head(sap.number.mat)
# 
# ord <- metaMDS(sap.cover.mat)
# summary(ord)
# plot(ord,type='t')
# 
# attributes(ord)
# 
# ordN <- metaMDS(tree.number.mat)
# plot(ordN,type='t')
# 



######################################################################################################
# removing plots that are PSEMEN removal plots, presently 1320, 1349, 1349,1341,1306 (maybe also add 1321 and 1306)    
#tree.cover.noremoval<- subset(tree.cover, subset=(tree.cover$Plot != "PPW1320")) 
#tree.cover.noremoval<- subset(tree.cover.noremoval, subset=(tree.cover.noremoval$Plot != "PPW1349"))
#tree.cover.noremoval<- subset(tree.cover.noremoval, subset=(tree.cover.noremoval$Plot != "PPW1341"))
#tree.cover.noremoval<- subset(tree.cover.noremoval, subset=(tree.cover.noremoval$Plot != "PPW1306"))
#tree.cover.noremoval

#tba <- tapply(tree.cover.noremoval$Basal.Area_cm2,tree.cover.noremoval$Species,sum)
#tba <- tba[order(tba)]
#tba 

# tree.cover.mat <- sample2matrix(tree.cover.noremoval[,c(1,4,2)])
# tree.number.mat <- sample2matrix(tree.cover.noremoval[,c(1,3,2)])
# head(tree.cover.mat)
# plot(tree.cover.mat$QUEDOU, tree.cover.mat$QUEGAR, main="Basal Area of QUEDOU & QUEGAR", xlab="QUEDOU", ylab="QUEGAR")
# head(tree.number.mat)
# 
# ord <- metaMDS(tree.cover.mat)
# summary(ord)
# plot(ord,type='t')
# 
# attributes(ord)
# 
# ordN <- metaMDS(tree.number.mat)
# plot(ordN,type='t')
# 
# # Ordination for saplings
# head(sapling.cover)
# # remove PSEMEN plots 
# sapling.cover.noremoval<- subset(sapling.cover, subset=(sapling.cover$Plot != "PPW1320")) 
# sapling.cover.noremoval<- subset(sapling.cover.noremoval, subset=(sapling.cover.noremoval$Plot != "PPW1349"))
# sapling.cover.noremoval<- subset(sapling.cover.noremoval, subset=(sapling.cover.noremoval$Plot != "PPW1341"))
# sapling.cover.noremoval<- subset(sapling.cover.noremoval, subset=(sapling.cover.noremoval$Plot != "PPW1306"))
# sapling.cover.noremoval
# 
# 
# sap.cover.mat <- sample2matrix(sapling.cover.noremoval[,c(1,4,2)])
# sap.number.mat <- sample2matrix(sapling.cover.noremoval[,c(1,3,2)])
# head(sap.cover.mat)
# head(sap.number.mat)
# 
# ord <- metaMDS(sap.cover.mat)
# summary(ord)
# plot(ord,type='t')
# 
# attributes(ord)
# 
# ordN <- metaMDS(tree.number.mat)
# plot(ordN,type='t')
# 

################ HISTOGRAMS OF SIZE DISTRIBUTION ###################
################ HISTOGRAMS OF DENSITY PER PLOT ###################
# Plot size distribution histograms, and plot density histograms
species.list <- names(tree.cover.mat)
par(mfrow=c(2,1))
par(ask=TRUE)
hist(master.trees$DBH)

i=1
for (i in 1:length(species.list)) {
    hist(master.trees$DBH[master.trees$Species==species.list[i]],
         xlab='diameter (cm)',
         main=paste(species.list[i],'size distribution'))
    nzero <- length(which(tree.number.mat[,i]==0))
    hist(tree.number.mat[tree.number.mat[,i]!=0,i],xlab='density',
         main=paste(species.list[i],'density:',nzero,'zeros'))
}

### compare frequency of tree in the plots to frequency of salpings
### per species

species.list <- names(sap.cover.mat)
par(mfrow=c(2,1))
par(ask=TRUE)

master.saplings$SA.Stump.BD_cm<- as.numeric(master.saplings$SA.Stump.BD_cm)
master.saplings$BD.Total<- (master.saplings$SA.Stump.BD_cm)*(master.saplings$SA.Branch.Num)

hist(master.saplings$BD.Total)

i=1
for (i in 1:length(species.list)) {
  hist(master.saplings$BD.Total[master.saplings$Species==species.list[i]],
       xlab='diameter (cm)',
       main=paste(species.list[i],'size distribution'))
  nzero <- length(which(sap.number.mat[,i]==0))
  hist(sap.number.mat[sap.number.mat[,i]!=0,i],xlab='density',
       main=paste(species.list[i],'density:',nzero,'zeros'))
}


###################################################
sapling.cover<-do.call(rbind, sapling.cover.by.plot)
#write.csv(tree.cover, file="/Users/meaganoldfather/Dropbox/PepperwoodVegPlots_2013 (2)/Veg Survey Data/Data Processing/Tree_Area.csv") # Meagan's machine
write.csv(tree.cover, file="/Users/mattb/Dropbox/Berkeley Projects/PepperwoodVegPlots_2013/Veg Survey Data/Data Processing/Tree_Area.csv") # Matt's machine
#write.csv(sapling.cover, file="/Users/meaganoldfather/Dropbox/PepperwoodVegPlots_2013 (2)/Veg Survey Data/Data Processing/Sapling_Area.csv")

total.cover.by.plot<-vector("list", length=length(tree.cover.by.plot))
names(total.cover.by.plot)<-names(tree.cover.by.plot)

for (i in 1: length(tree.cover.by.plot))
{  
  Plot.id<-names(tree.cover.by.plot)[[i]]
  species.in.plot<-unique(c(tree.cover.by.plot[[i]]$Species, sapling.cover.by.plot[[i]]$Species))	
  total.cover.by.plot[[i]]<-data.frame(Plot=character(0), Species=character(0), Total.cover=numeric(0))
  
  
  if(length(species.in.plot)>0)
  {
    # names(total.cover.by.plot[[i]])<-c("Plot", "Species", "Total cover (cm^2)")
    for (j in 1:length(species.in.plot))
    {
      total.cover.by.plot[[i]][j, 1]<-Plot.id
      total.cover.by.plot[[i]][j, 2]<-species.in.plot[j]
      total.cover.by.plot[[i]][j, 3]<-sum(tree.cover.by.plot[[i]][tree.cover.by.plot[[i]]$Species==species.in.plot[j],3], sapling.cover.by.plot[[i]][sapling.cover.by.plot[[i]]$Species==species.in.plot[j],3])
    }
    
    
  }
  colnames(total.cover.by.plot[[i]])<-c("Plot", "Species", "Total cover (cm^2)")
}

total.cover.by.plot

total.cover<-do.call(rbind, total.cover.by.plot)
total.cover

#write.csv(total.cover, file="/Users/meaganoldfather/Dropbox/PepperwoodVegPlots_2013 (2)/Veg Survey Data/Data Processing/Total_Area.csv") # Meagan's machine
#write.csv(total.cover, file="/Users/mattb/Dropbox/Berkeley Projects/PepperwoodVegPlots_2013/Veg Survey Data/Data Processing/Total_Area.csv") # Matt's machine

# CAN MAKE TOTAL COVER ALSO WITH BELOW CODE; CALLED 'all.area'

all.area<-rbind(tree.cover, sapling.cover)
all.area

##  Make a csv file with the percent cover QUEDEC for just trees  
#tree<-read.csv("/Users/meaganoldfather/Dropbox/PepperwoodVegPlots_2013 (2)/Veg Survey Data/Data Processing/Tree_Area.csv") # Meagan's machine
tree<-read.csv("/Users/mattb/Dropbox/Berkeley Projects/PepperwoodVegPlots_2013/Veg Survey Data/Data Processing/Tree_Area.csv") # Matt's machine


tree
tree[,c(2,3,4)]
attach(tree)

allspecies<-sort(unique(tree$Species))
allspecies

FT<-c(rep("ew", 9),rep("do",4), "ew")
ft.table<-data.frame(FT, allspecies)

sf2<-match(tree$Species, ft.table$allspecies)
tree$FT<- ft.table$FT[sf2]
names(tree)[4]="basalarea"
ftbasalarea<-data.frame(tapply(tree$basalarea, list(tree$Plot, tree$FT), sum))
ftbasalarea

ftbasalarea[is.na(ftbasalarea)]=0
ftbasalarea
ftbasalarea$total<-apply(ftbasalarea,1,sum)
ftbasalarea

ftbasalarea$doperc<-ftbasalarea[,1]/ftbasalarea[,3]
ftbasalarea
#write.csv<-(ftbasalarea, file="/Users/meaganoldfather/Dropbox/PepperwoodVegPlots_2013 (2)/Veg Survey Data/Data Processing/ftbasalarea.csv")
#write.csv(ftbasalarea, file="/Users/mattb/Dropbox/Berkeley Projects/PepperwoodVegPlots_2013/Veg Survey Data/Data Processing/ftbasalarea.csv") #Matt's machine

######################################





