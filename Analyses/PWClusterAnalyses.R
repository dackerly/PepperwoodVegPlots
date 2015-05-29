# Intent: Cluster analyses for PW Veg Plots 
# Author: Meagan F. Oldfather
# Date Created: 20150528
# Date Last Edited: 20150828

# clear workspace
rm(list=ls())

#packages
library("RCurl")
library("data.table")
library("picante")

# source in PW functions
source_https <- function(url, ...) { 
  # parse and evaluate each .R script
  sapply(c(url, ...), function(u) {
    eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
  })
}

source_https('https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/Analyses/PWfunctions_GitHub.R')

#### seedling/juvenile ####
# load in 2015 seedling/juvenile data
SJ15<-plants.by.plot(year = 2015,type = "SEJU")
SJ15$Plot<-substr(SJ15$Plot,6,8)
head(SJ15)

# make a communtiy matrix
SJ15.mat<-sample2matrix(SJ15[,c("Plot","Total.Number","Species")])

# make a dissimilarity matrix by plot
SJ15.dis<-vegdist(SJ15.mat,method="bray")

# make a dissimilarity matrix by species
SJ15.dis.sp<-vegdist(t(SJ15.mat),method="bray")

# cluster analyses
hcS.plot<-hclust(SJ15.dis)
plot(hcS.plot)
hcS<-hclust(SJ15.dis.sp)
plot(hcS)

#### tree/sapling ####
# load in 2015 tree data
T15<-plants.by.plot(year = 2014,type = "TR") 
T15$Plot<-substr(T15$Plot,6,8)
head(T15)

# get rid of QUEAGKE & QUEDEC individuals; only 1 entry each
T15[T15$Species =="QUEAGKE",]
T15[T15$Species =="QUEDEC",]
T15[T15$Species =="QUEWIS",]

T15<-subset(T15, T15$Species!="QUEAGKE")
T15<-subset(T15, T15$Species!="QUEDEC")
T15<-subset(T15, T15$Species!="QUEWIS")

# make a communtiy matrix
T15.mat<-sample2matrix(T15[,c("Plot","Basal.Area","Species")])

# make a dissimilarity matrix by plot
T15.dis<-vegdist(T15.mat,method="bray")

# make a dissimilarity matrix by species
T15.dis.sp<-vegdist(t(T15.mat),method="bray")

# cluster analyses
hc.plot<-hclust(T15.dis)
plot(hc.plot)
hc<-hclust(T15.dis.sp)
plot(hc)

### Heatmaps ###
#colors.row<-c(rep("black",20), rep("dodgerblue", 20), rep("slategray2", 10))
heatmap(as.matrix(SJ15.mat), col = terrain.colors(12), main="Seedling/Juvenile Counts")
heatmap(as.matrix(T15.mat), col = terrain.colors(12), main="Tree Basal Area")

### Colored Dendogram
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R",verbose=T)
# colored dendrogram
par(bg = 'white')

colors<-c("dodgerblue","mediumorchid2","black","brown3","orange","turquoise","darkgreen","darkolivegreen3","slategray2")

# seedling/juvenile species
A2Rplot(hcS, k = 9, boxes = FALSE, col.up = "gray50", col.down =colors, main= "Seedling/Juvenile Counts")
A2Rplot(hcS.plot, k = 5, boxes = FALSE, col.up = "gray50", col.down =colors, main= "Seedling/Juvenile Counts")


# tree species
A2Rplot(hc, k = 9, boxes = FALSE, col.up = "gray50", col.down =colors, main="Tree Basal Area")

A2Rplot(hc.plot, k =5, boxes = FALSE, col.up = "gray50", col.down =colors, main= "Tree Basal Area")

