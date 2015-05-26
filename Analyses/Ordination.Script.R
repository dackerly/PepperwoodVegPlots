# Intent: Analyze woody community data with ordinations, focusing on trees and saplings only 
# Author: M.F. Oldfather
# Date Created: 20130309
# Date Last Edited: 20141021

# packages
library(picante)
require(vegan)
require(ggplot2)


# use the below script for multiple users, enter your own pathway for setting the working directory in the open brackets after "else"
if (Sys.getenv("USER")=='meaganoldfather') setwd("/Users/meaganoldfather/Box Sync/PepperwoodVegPlots/Database/Analyses/") else setwd("")

source("MasterData.R")

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

# Make matrices! 
TR.mat.cover<- sample2matrix(plot.TR.noP[,c(1,3,2)])# makes a matrix for TR basal area  
TR.mat.num<- sample2matrix(plot.TR.noP[,c(1,4,2)]) # makes a matrix for TR number
head(TR.mat.cover)
head(TR.mat.num)

SA.mat.cover<- sample2matrix(plot.SA.noP[,c(1,3,2)])# makes a matrix for Sa basal area  
SA.mat.num<- sample2matrix(plot.SA.noP[,c(1,4,2)]) # makes a matrix for SA number
head(SA.mat.cover)
head(SA.mat.num)

SA.TR.mat.cover<- sample2matrix(plot.SA.TR.noP[,c(1,3,2)])# makes a matrix for SA.TR basal area  
SA.TR.mat.num<- sample2matrix(plot.SA.TR.noP[,c(1,4,2)]) # makes a matrix for SA.TR number
head(SA.TR.mat.cover)
head(SA.TR.mat.num)

# quick attempt at ordination (nonmetric multidimensionalscaling)
ord <- metaMDS(TR.mat.cover)
summary(ord)
plot(ord,type='t')
attributes(ord)
x<-dist(TR.mat.cover)
wcmdscale(x)

# Exploratory heatmaps for similarity in community composition across plots
#hot color = low values, light color = high value
heatmap(as.matrix(SA.TR.mat.num), Rowv = NA, Colv = NA) 
heatmap(as.matrix(SA.TR.mat.cover), Rowv = NA, Colv = NA)
heatmap(as.matrix(SA.TR.mat.cover))
heatmap(as.matrix(SA.TR.mat.num))

# Metrics of diversity
# calculate species richness
plot.rich <- apply(SA.TR.mat.num > 0, MARGIN = 1, FUN = sum)
plot.rich

plot.H <- diversity(SA.TR.mat.num) # Shannon entropy
plot.N1 <- exp(plot.H) ## Shannon number of diversity
plot.N1
plot.N2 <- diversity(plot.N1, "inv") ## Simpson Diversity
plot.N2 

# Explortory work with PCA's
plot.pca <- prcomp(TR.mat.num)
plot.pca
summary(plot.pca)
biplot(plot.pca) # PSEMEN adundance seems to largely drive the difference in community composition

plot.mds <- metaMDS(TR.mat.cover) # function of "metaMDS" , can change matrix used
plot.mds
stressplot(plot.mds)

# Source in climate data from all plots to explore how the climate drives the community differences 
# use the below script for multiple users, enter your own pathway for setting the working directory in the open brackets after "else"
if (Sys.getenv("USER")=='meaganoldfather') setwd("/Users/meaganoldfather/Box Sync/PepperwoodVegPlots/Database/Analyses/") else setwd("")
source("Climate.Pts.R")

# need to also remove PSEMEN plots from climate data
climate.noP<-subset(climate.pts, subset=(climate.pts$Plot != "PPW1320"))
climate.noP<-subset(climate.noP, subset=(climate.noP$Plot != "PPW1349"))
climate.noP<-subset(climate.noP, subset=(climate.noP$Plot != "PPW1340"))
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
mds.score # notr sure what these mean entirely


# Attemping at CCA 
pw.cca <- cca(TR.mat.cover ~ MarchRadiation + tpi500 + Soil.Moisture, data=climate.noP)
pw.cca <- cca(TR.mat.cover ~ MarchRadiation + Soil.Moisture, data=climate.noP)
pw.cca
plot(pw.cca)


