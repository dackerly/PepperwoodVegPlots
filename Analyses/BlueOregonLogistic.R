# Logistic Regression with QUEDOU,QUEGAR Seedlings #
# Author: Meagan F. Oldfather
# Date Created: 20150530

# I added one line to try a push and commit

# clear workspace
rm(list=ls())

library("RCurl")
library("data.table")
library("picante")

#function to source html 
source_https <- function(url, ...) { 
  # parse and evaluate each .R script
  sapply(c(url, ...), function(u) {
    eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
  })
}

# sources in all PW functions 
source_https('https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/Analyses/PWfunctions_GitHub.R')

# get plot climate data
clim<-get.clim.pts()
clim$Slope<-as.numeric(clim$Slope)
# get plot info
info<-get.plot.info()
# merge climate and info data
clim<-cbind(clim, info[,4:5])

# get aggregated by plot, species seedling/juvenile data 
SJ15<-plants.by.plot(year = 2015,type = "SEJU")
head(SJ15)
# get aggregated by plot,species tree data
T15<-plants.by.plot(year = 2014,type = "TR") 
head(T15)

#subset by species of interest, only currently looking at QUEGAR & QUEDOU though
species.interest<-c("ARBMEN", "FRACAL", "PSEMEN", "QUEAGR", "QUEDOU", "QUEGAR", "QUEKEL", "UMBCAL")
SJ15<-subset(SJ15, SJ15$Species%in%species.interest)
T15<-subset(T15, T15$Species%in%species.interest)

# turn into matrix where all plot,species combination are represented based on counts for seedling/juveniles and basal area for trees
SJ15.mat<-sample2matrix(SJ15[,c("Plot","Total.Number","Species")])
T15.mat<-sample2matrix(T15[,c("Plot","Basal.Area","Species")])
# adding "BA" to tree columns to distinquish from counts
colnames(T15.mat)<-paste(colnames(T15.mat),".BA",sep="")
SJ15.mat$Plot<-rownames(SJ15.mat)
T15.mat$Plot<-rownames(T15.mat)

# merge tree and seedling together
all.mat<-merge(SJ15.mat,T15.mat)
head(all.mat)

# add on clim/info data
all<-merge(all.mat,clim)

# scale basal area and climate data
all.s<-all
# combine with raw seedling data
all.s<-as.data.frame(cbind(all[,c(1:9)],apply(all.s[,10:ncol(all.s)], 2, scale)))

# logistic regression with QUEDOU seedling = successs and QUEGAR seedling = failure; additive model with QUEDOU basal area, QUEGAR basal area and CWD
m1 <- glm(cbind(QUEDOU, QUEGAR) ~ QUEDOU.BA + QUEGAR.BA + CWD, data=all.s, family=binomial)
summary(m1)

# considered adding in Plot as a random effect, but decided it didn't make sense as only one data point for covariates
#library(lme4)
#m2 <- glmer(cbind(QUEDOU, QUEGAR) ~ QUEDOU.BA + QUEGAR.BA + CWD + (1|Plot), data=all.s, family=binomial)
#summary(m2)

# plot the probability of a Blue Oak seedling with changing CWD, not sure how to interpret these marginal effects of a single predictor... this doesn't feel useful for intrepetation. 
pred.CWD<-predict(m1,type="response")
# plot model predictions
plot(all.s$CWD, pred.CWD, xlab="CWD (SD from Mean)",ylab="Blue Oak", ylim=c(0,1), pch=19)
# plot caculated proportion 
points(all.s$CWD, (all.s$QUEDOU/(all.s$QUEGAR+all.s$QUEDOU)), col="dodgerblue",pch=19)
newdata<-data.frame(CWD=seq(-2.5,2.5, length.out=100), QUEDOU.BA=rep(0,100), QUEGAR.BA=rep(0,100))
# best fit lines
best.fit.CWD<-predict(m1,newdata=newdata,type="response")
lines(newdata$CWD, best.fit.CWD,col="red")
# add legend
legend("topleft",legend = c("Probability", "Proportion"), col=c("black","dodgerblue"), pch=19)


# looking at general relationships 
pairs(all[,c("QUEDOU.BA","QUEGAR.BA")])
pairs(all.s[,c("QUEGAR.BA","CWD")])
pairs(all.s[,c("QUEDOU.BA","CWD")])
pairs(all.s[c("QUEGAR", "CWD")])
pairs(all.s[c("QUEDOU", "CWD")])
pairs(all.s[c("QUEGAR","QUEDOU")])

# linear models of Basal Area and CWD for both species; not significant but inverse effects
summary(lm(QUEDOU.BA~CWD,data=all.s))
summary(lm(QUEGAR.BA~CWD,data=all.s))

# removed Basal Areas from model; effect of CWD flips to positive
m2<- glm(cbind(QUEDOU, QUEGAR) ~ CWD, data=all.s, family=binomial)
summary(m2)

# Looking at tree counts as binomial response (not basal area)
Adult.mat<-sample2matrix(T15[,c("Plot","Count","Species")])
colnames(Adult.mat)<-paste(colnames(Adult.mat),".C",sep="")
Adult.mat$Plot<-rownames(Adult.mat)
all.s.A<- merge(all.s,Adult.mat)


# logistic regression with QUEDOU tree counts = successs and QUEGAR tree counts = failure; additive model with CWD
m3<-glm(cbind(QUEDOU.C, QUEGAR.C) ~ CWD, data=all.s.A, family=binomial)
summary(m3)

# looking at how individually the species tree counts change with CWD
summary(lm(QUEGAR.C+QUEDOU.C~CWD,data=all.s.A))
summary(lm(QUEDOU.C~CWD,data=all.s.A))
summary(lm(QUEGAR.C~CWD,data=all.s.A))

