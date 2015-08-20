# Lichens!
# MFO
# 20150820

# clear workspace
rm(list=ls())

#Packages
library("RCurl")
library("data.table")
library("picante")
library(colortools)
library(plotrix)

# sources in all functions
source_https <- function(url, ...) { 
  # parse and evaluate each .R script
  sapply(c(url, ...), function(u) {
    eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
  })
}
source_https('https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/Analyses/PWfunctions_GitHub.R')

# get plot environmental data
clim<-get.clim.pts()
head(clim)

