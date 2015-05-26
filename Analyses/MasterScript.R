# Intent: Master Script for exploring the functions created for creation and analyses of the TBC3 Veg Plot data 
# Author: M.F. Oldfather
# Date Created: 20141022
# Date Last Edited: 20141112

# Set up your own "if" statement for your own working directory; 
#for your wd go to "Database/" for all functions to work properly (see MFO example below)
# Please feel free to save this code with your own "if" statement
if (Sys.getenv("USER")=='meaganoldfather') setwd("/Users/meaganoldfather/Box Sync/PepperwoodVegPlots/Database/")
dir() 
# source in all functions
source("Analyses/PWFunctions.R")
# Functions sourced:
# get.plot(): creates a list of plot names
# get.plot.info(): creates dataframe of plot-specific characteristics & UTM coords
# get.envr.data(): creates dataframe of plot-specific environmental variables
# get.indv.data(): creates dataframe of all tagged indivuals
# get.dead(): creates dataframe of all dead indivudals in a specific year(s)
# PW.species(): create list of species found in all plots at the preserve
# get.seju.data() : creates dataframe of seedling and juveniles counts in a specific year
# plants.by.plot(): creates dataframes of basal area and count data for different types of woody individuals
# get.clim.pts(): creates dataframes of values of multiple raster layers for the plots (and all PW points),as well as the raster layers themselves
# PWordinate(): creates matrix of count or basal area data for all species in all plots

# Note about functions script: Please do not alter these functions in any way, if you are having an issue please contact MFO
# at meagan_oldfather@berkeley.edu. Functions may be added, but then also should be fully documented in the "FunctionsNotes.txt". Also, 
# I apologive about the old west theme for the error messages, it was funny at the time. 

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
#### There will be an warning that NA coercian is taking place, this is an issue MFO is working on, but doesn't seem to alter the data
# default is only see individuals 
indv.data<-get.indv.data(year=2014, stump=F,orig.dead=F,branches=F)

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
year=2014

# Create a vector of all woody species found in our plots with PW.species()
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
seedling.juvenile<-get.seju.data(year=2013)

#look at results!
head(seedling.juvenile)









dim(seedling.juvenile)

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

# look at the results!
head(tree)
dim(tree)
head(sapling)
dim(sapling)
dim(all)
head(all)

# Create dataframe with climate pts for each plot.location with get.clim.pts 
# Values are pulled out for the following rasters:"DEM","March.Rad","MaxJulT","MinJunT","Topo","TPI"
# Both a simple dataframe (element 1) and a SpatialPointsDataFrame(element 2) are created
# Regardless of your input all rasters for the area surrounding PW, as well as the PW outline("PP") are created
#as additional return elements
#######################################################
# Inputs:
#### all (optional)
#all=F is default, if changed to true then all pts (not just plots) in the preserve are summarized  
clim.pts<-get.clim.pts(all=F)[1]

# look at the results!
head(clim.pts)

# to get a single raster layer
clim.pts[[1]][2]

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


