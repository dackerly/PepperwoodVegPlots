# Intent: Tutorial for PW Functions working with GitHub 
# Author: Meagan F. Oldfather
# Date Created: 20150527
# Date Last Edited: 20161026

# add in simple metadata to this code 

# clear workspace
rm(list=ls())

# The following packaged need to be installed the following functions to work: "RCurl", "data.table","picante"
library("RCurl")
library("data.table")
library("picante")

# this code is from some delightful human on the internet who figured out how to source from GitHub; run this function in order to source in the script with all the Pepperwood Functions 
source_https <- function(url, ...) { 
  # parse and evaluate each .R script
  sapply(c(url, ...), function(u) {
    eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
  })
}

# sources in all functions (described below) that allow access to the PPW Vegetation Plot Data
source_https('https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/Analyses/PWfunctions_GitHub.R')

# Functions to test out: 
# Functions sourced:
# get.plot(): creates a list of plot names
# get.plot.info(): creates dataframe of plot-specific characteristics & UTM coords
# get.envr.data(): creates dataframe of plot-specific environmental variables
# get.indv.data(): creates dataframe of all tagged indivuals
# get.dead(): creates dataframe of all dead indivudals in a specific year(s)
# PW.species(): create list of species found in all plots at the preserve
# get.seju.data() : creates dataframe of seedling and juveniles counts in a specific year
# plants.by.plot(): creates dataframes of basal area and count data for different types of woody individuals
# get.clim.pts(): creates dataframe of values of multiple raster layers for the plots 
# PWordinate(): creates matrix of count or basal area data for all species in all plots

# Create list of plot names with get.plot()
###################################################
# there are currently no inputs to this function
plot.list<-get.plot()

# look at the result!
plot.list
length(plot.list)

# Create dataframe with plot specific data (coordinates, slope, aspect) with get.plot.info()
###################################################
# there are currently no inputs to this function
plot.info<-get.plot.info()

# look at the result!
head(plot.info)
dim(plot.info)

# Create dataframe with with plot specific environmental variables (eg. % soil, bedrock, herb) with get.envr.data()
###################################################
# Inputs
##### rmv.missing = T  will remove plot PPW1301 which has missing data; default is rvm=F    
#### year
# the year of the survey, first one is 2013
envr.data<-get.envr.data(year=2013)

# look at the result!
head(envr.data)
tail(envr.data)
dim(envr.data)

# Create dataframe with all tagged indivuals,coordinates, and basal area with get.indv.data()
###################################################
# Inputs 
##### year(necessary)
# can be a year from 2012-2014 currently; mortality surveys are done every fall (~Sept), use the year of the 
# survey to only have include individuals that were alive at the survey in that year. For example, to include all individuals
# tagged when the plots were first established make "year=2012". #If you input year="none" you will create a dataframe with 
#only stumps or already dead indivuals (if you also change default to T for those parameters)
##### stump (optional)
# if T, stumps found during establishment are included; default is F
##### orig.dead (optional)
#if T, dead indiviuduals found during establishment are included;default is F
#### branches (optional)
# if T, each tagged branches are kept as seperate rows; default is F where each row is a single individual
# indviduals have an aggregated basal area (considering of also calculating number of associated tagged branches if useful?)
### NOTE: the basal area for trees and saplings are calculated differently due to different methods for size measurements, in addition
# the diameter of the branches are taken at different heights (10 cm for SA and stumps and 1.4 m for TR) so the basal area of these 
#different types of individuals should not be summed
# default is only see individuals 

indv.data<-get.indv.data(year=2012, stump=F,orig.dead=F,branches=F)
indv.data<-get.indv.data(year=2012, stump=T,orig.dead=T,branches=T)

#look at the results!
dim(indv.data)
head(indv.data)
tail(indv.data)

# Create dataframe with all tagged individuals that died in a certain year with get.dead()
###################################################
# Inputs
#### year(necessary)
# the year of interest for when individuals died, can be multiple years 
#### total (optional)
# if T(default) then the number of dead indivuduals is summed across plot and species at the indidual level, 
# also includes a column for the number of branches lost for that species  
# if F, the the specific information for each indivual/branch that died is created  
#### by.plot (optional)
## if T(default) then the number of dead indviduals/lost basal area/branches are aggregated by species,type and plot
# if F then the variable columns are aggregated by species,type only

dead.indvs<-get.dead(year=2013:2014,total=F,by.plot=T)

# look at the results!
head(dead.indvs)
dim(dead.indvs)

# Create a vector of all woody species (in all size classes) found in our plots with PW.species()
############################################################################
# Inputs
#### unknown(optional)
# default is T, but if F then species with unknown identifications are not included
species<-PW.species()
# look at the results!
species

# Create dataframe with counts of seedling or juveniles of all species across the plots with get.seju.data()
################################################################################
#Inputs
### year(necessary)
# the year of the seedling/juveniles survey of interest 
seedling.juvenile<-get.seju.data(year=2015)

#look at results!
head(seedling.juvenile)
dim(seedling.juvenile)
sum(seedling.juvenile$Total.Number) 

# Create dataframes with count or size information (for saplings,trees, seedlings/juveniles)
# aggregated by plot with plants.by.plot()
###################################################
# Inputs:
#### year
# year of alive individuals included (same as for get.indv.data)
#### type (necessary)
# types may include "SA" (only saplings), "TR" (only trees), "SA.TR" (both saplings and trees), "SEJU" (seedlings and juveniles)

tree<-plants.by.plot(year=2014, type="TR")
sapling<-plants.by.plot(year=2014,type="SA")
all<-plants.by.plot(year=2014,type="SA.TR")
babies<-plants.by.plot(year = 2015, type = "SEJU")

# look at the results!
head(tree)
head(babies)
dim(tree)
head(sapling)
dim(sapling)
dim(all)
head(all)

# Create dataframe with climate pts for each plot.location with get.clim.pts 
# Values are pulled out for the following rasters:"DEM","March.Rad","MaxJulT","MinJunT","Topo","TPI"
#######################################################
# Inputs:none,for now 
clim.pts<-get.clim.pts()

# look at the results!
head(clim.pts)

# Create a matrix of species counts or basal area in each plot with PWordinate
# Also creates a dataframe of climate data for plot points for all included plots
# Plots with heavy douglas-fir removal are not kept in for analyses
# Also produces a NMDS plot of the specified matrix 
###################################################
# Inputs:
#### year (necessary)
# year of alive individuals included (same as for get.indv.data)
#### type (necessary)
# types may include "SA" (only saplings), "TR" (only trees), "SA.TR" (both saplings and trees), "SEJU" (seedlings and juveniles)
### metric (necessary)
# "Count" or "Basal.Area"
### psemen.rmv
# default is T, where the 5 plots with doug-fir removal are removed
mat<- PWordinate(2014,"TR","Count")[1]

clim<-PWordinate(2014,"TR","Count")[2]
head(mat)
head(clim)

# Have fun!



