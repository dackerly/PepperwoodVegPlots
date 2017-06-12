# clear workspace
rm(list=ls())

# The following packaged need to be installed the following functions to work: "RCurl", "data.table","picante"
library("RCurl")
library("data.table")
library("picante")
library("vegan")
library("ggplot2")

# this code is from some delightful human on the internet who figured out how to source from GitHub; run this function in order to source in the script with all the Pepperwood Functions 
source_https <- function(url, ...) { 
  # parse and evaluate each .R script
  sapply(c(url, ...), function(u) {
    eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
  })
}

# sources in all functions (described below) that allow access to the PPW Vegetation Plot Data
source_https('https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/Analyses/PWfunctions_GitHub.R')

# bring in 
babies<-plants.by.plot(year = 2015, type = "SEJU")
head(babies)

seju.rich<-aggregate(Species~Plot, data = babies, FUN = function(x) length(unique(x)))
seju.rich

plot(density(seju.rich$Species))

# bring in 10m CWD data
clim<-get.clim.pts()
head(clim)


seju.rich$CWD<-clim$CWD

ggplot(seju.rich, aes (CWD, Species))+geom_point()+theme_classic()+xlim(700,1200) + ylab("Seedling + Juvenile Richness")
fit<-lm(Species~CWD, data=seju.rich)
summary(fit) # not significant

# DIVERSITY
div.mat<-sample2matrix(babies[,c(1,6,2)])
div<-diversity(div.mat, index = "shannon")
clim$div<-div

ggplot(clim, aes(CWD, div))+geom_point()+theme_classic()+xlim(700,1200)+ylab(" Seedling+Juvenile Diversity")
fit<-lm(div~CWD, data=clim)
summary(fit) # not significant

# density verssus diversity

