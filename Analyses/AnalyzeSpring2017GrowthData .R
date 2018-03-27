# clear workspace
rm(list=ls())

#Packages
library("RCurl")
library("data.table")
library("picante")
library("vegan")
library("raster")
library("maptools")
library("rgdal")
library("pscl")
library("arm")
library("knitr")
library("AICcmodavg")
library("FactoMineR")
library("lmtest")
library("ggplot2")

# sources in all functions
source_https <- function(url, ...) { 
  # parse and evaluate each .R script
  sapply(c(url, ...), function(u) {
    eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
  })
}
source_https('https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/Analyses/PWfunctions_GitHub.R')

# bring in plot info
plot.info <- get.plot.info()
head(plot.info)

# bring in tree data
trees<- get.indv.data(year=2014, stump=F,orig.dead=F,branches=T)
head(trees)

# subset to superplots
tree.super<-merge(trees, plot.info[,c(1,6)], by="Plot")
tree.super<-subset(tree.super, Super.Plot == 1)
head(tree.super)

# subset to trees
tree.super<-subset(tree.super, Type == "TR")
head(tree.super)

# bring in 2017 survey data (on github)
growth2017<-read.csv("Pepperwood/GitDatabase/2017/GrowthMortality/Data/TreeGrowthMortality.csv")
colnames(growth2017)<-c("Date","Plot","Num","Species","Alive","DBH17","Notes")
head(growth2017)

# subset by alives, measured trees
growth2017<-subset(growth2017, Alive == 1)

growth13.17<-merge(tree.super, growth2017, by=c("Plot","Num","Species"), all.y=T) 
head(growth13.17)

growth13.17<-growth13.17[!is.na(growth13.17$DBH17),]

ggplot(growth13.17, aes(DBH_cm, DBH17))+geom_point(pch=".")+theme_classic()+xlab("2013 DBH")+ylab("2017 DBH")+geom_abline(intercept = 0, slope = 1)

fit<-lm(growth13.17$DBH17~growth13.17$DBH_cm)
summary(fit)

growth13.17$Growth<-growth13.17$DBH17 - growth13.17$DBH_cm 

ggplot(growth13.17, aes(DBH_cm, Growth))+geom_point()+theme_classic()+xlab("2013 DBH")+ylab("2017 DBH  - 2013 DBH")+geom_abline(intercept = 0, slope = 0)

growth13.17[growth13.17$Growth<(-5),]

# remove plots with ridiculous changes in DBH; most likeyly due to previously incorrect measurements
growth13.17.clean<-subset(growth13.17, Growth >= -5)

ggplot(growth13.17.clean, aes(DBH_cm, DBH17))+geom_point(pch=19)+theme_bw()+xlab("2013 DBH")+ylab("2017 DBH")+geom_abline(intercept = 0, slope = 1)

ggplot(growth13.17.clean, aes(DBH_cm, Growth))+geom_point()+theme_bw()+xlab("2013 DBH")+ylab("Growth")+geom_abline(intercept = 0, slope = 0, lty="dashed")+ylim(-5,5)


ggplot(growth13.17.clean, aes(DBH_cm, Growth))+geom_point()+theme_bw()+xlab("2013 DBH (cm)")+ylab("Growth (cm)")+ylim(0,5)

length(unique(growth13.17$Num))
length(unique(growth13.17.clean$Num))

mean(growth13.17.clean$Growth)
sum(growth13.17.clean$Growth == 0)
sum(growth13.17.clean$Growth > 0)
sum(growth13.17.clean$Growth < 0)
growth13.17.clean[growth13.17.clean$Growth >= 5,]

plot(density(growth13.17.clean[growth13.17.clean$Growth < 0,"Growth"]),main="")

# look how growth measurements vary by species and CWD (N vs. S)
species.of.interest<-c("QUEDOU","QUEGAR","QUEAGR","QUEKEL","PSEMEN","UMBCAL", "ARBMEN")
growth.species<-subset(growth13.17.clean, Species %in% species.of.interest)

ggplot(growth.species, aes(Species, Growth))+geom_boxplot()+geom_abline(intercept = 0, slope = 0, lty="dashed")+ylim(-4,4)+theme_bw()

growth.mean<-aggregate(Growth~Species, data=growth.species, FUN=mean)
growth.mean$Growth/4

head(growth.species)
climate<-read.csv("Pepperwood/GitDatabase/GIS/ClimatePlotPts.csv") #on github
head(climate)
growth.species<-merge(growth.species,climate,by="Plot", all.x=T)
head(growth.species)

growth.species.mean<-aggregate(cbind(Growth, CWD)~Plot+Species, data=growth.species, FUN=mean, na.rm=T)
growth.species.mean

ggplot(growth.species, aes(CWD, Growth,col=Species))+geom_point()
ggplot(growth.species.mean, aes(CWD, Growth, col=Species))+geom_smooth(se=F, method="lm")
ggplot(growth.species.mean, aes(CWD, Growth,group=Species))+geom_smooth(se=F, method="lm")+geom_point()

fit<-lm(Growth~CWD,data=growth.species.mean[growth.species.mean$Species=="QUEGAR",])
summary(fit)


fit<-lm(Growth~CWD,data=growth.species.mean)
summary(fit)

# not-sig: QUEAGR,PSEMEN,QUEKEL, UMBCAL, ARBMEN
# QUEGAR is positive (very small effect)

# Try splitting out the plots into "dry" and "wet"
head(growth.species)
ggplot(growth.species, aes(DBH_cm, Growth, col=CWD))+geom_point()+theme_bw()+xlab("2013 DBH")+ylab("Growth")+geom_abline(intercept = 0, slope = 0, lty="dashed")+ylim(0,5)

growth.species$Category<-1
growth.species[growth.species$CWD > quantile(growth.species$CWD)[3], "Category"]<-2

ggplot(growth.species, aes(DBH_cm, Growth, col=Species))+theme_bw()+xlab("2013 DBH")+ylab("Growth")+geom_point()+ylim(0,3)+geom_smooth(aes(group=as.factor(Category)), method=lm)

ggplot(growth.species, aes(DBH_cm, Growth))+geom_point()+theme_bw()+xlab("2013 DBH (cm)")+ylab("Growth (cm)")+ylim(0,5)+geom_smooth(aes(group=as.factor(Category)), method=lm)

aggregate(Growth~Category+Species, data=growth.species, FUN=mean)

# Calculate change in basal area
library(data.table)

growth13.17.clean$Num.A<- floor(growth13.17.clean$Num)
growth13.17.clean$Basal.Area # 2013 Basal Area
growth13.17.clean$Basal.Area.17<- (((growth13.17.clean$DBH17/2)^2)*(pi))

growth13.17.clean$Delta.BA<-growth13.17.clean$Basal.Area.17 - growth13.17.clean$Basal.Area 

ggplot(growth13.17.clean, aes(Basal.Area, Delta.BA, color=Species))+geom_point()+theme_bw()+xlab("2013 Basal Area")+ylab("2017 Basal Area  - 2013 Basal Area")+geom_abline(intercept = 0, slope = 0)

basal.area<-aggregate(Delta.BA~Species, data=growth13.17.clean, FUN=sum)
basal.area
sum(basal.area$Delta.BA)

# remove QUEDEC
basal.area<-subset(basal.area, Species != "QUEDEC")

ggplot(basal.area, aes(Species, Delta.BA))+geom_bar(stat="identity")+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8))+xlab("")+ylab("Change in Basal Area (2017 - 2013)")


