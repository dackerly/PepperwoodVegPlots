# Intent: Pull in all data for the plots and individuals from the datasheets used to establish 
# the plots in the Spring of 2013. Mortality can also be incorparted for all years. Creates three main 
# dataframes: "plot.info", "tree", "sapling" and then other dataframes "SA", "TR", "SA.TR" & "plot.SA", "plot.TR", "plot.SA.TR"
# that has condensed the data to cover and number of individuals by species by plot and with the associated "plot.info" data
# This data has extensively cleaned, and the erros have been fixed in the main datafiles after field confirmation. 
# Author: M.F. Oldfather
# Date Created: 20130611
# Date Last Edited: 20141020

# Part 1: Create plot.info with plot specific data (coordinates, slope, aspect)
###################################################

# forces character strings to not be treated as factors 
options(stringsAsFactors=FALSE) 

# use the below script for multiple users, enter your own pathway for setting the working directory in the open brackets after "else"
if (Sys.getenv("USER")=='meaganoldfather') setwd("/Users/meaganoldfather/Box Sync/PepperwoodVegPlots/Database/2013/Woody2013/Data/OriginalCSV/PlotInfo/") else setwd("")

# list of file names 
file.list <-dir(paste(getwd(),"/", sep=''))
file.list

# extracts plot numbers from file name vector
strlen <- nchar(file.list[1]) # strlen is the length of all of the file names
plot.list <- substr(file.list,strlen-10,strlen-4) 
plot.list

# read in all files for plot info
plot.data<-lapply(file.list, read.csv, skip=5, nrows=5, header=F)
head(plot.data) 

# make dataframe structure 
plot.info<-data.frame("Plot"=numeric(length(plot.data)),
                      "UTM.E"=numeric(length(plot.data)),
                      "UTM.N"=numeric(length(plot.data)),
                      "Slope"=numeric(length(plot.data)),
                      "Aspect"=numeric(length(plot.data))) 

# fills in first colums with the plot names
plot.info$Plot<-plot.list 

for (i in 1:length(plot.data)){
  plot.info$UTM.E[i]<-plot.data[[i]][1,2]
  plot.info$UTM.N[i]<-plot.data[[i]][2,2]
  plot.info$Slope[i]<-plot.data[[i]][3,5]
  plot.info$Aspect[i]<-plot.data[[i]][3,8]
}
plot.info

# Slope and aspect are missing for plot PPW1302 and not applicable for PPW1329 (on crest) 
plot.info$Slope[2]<-NA # changes slope for plot 1302 to NA
plot.info$Aspect[2]<-NA # same for 1329 aspect
plot.info$Slope[29]<-NA # changes slope for plot 1329 to NA
plot.info$Aspect[29]<-NA # same for 1329 aspect
# calculate Aspect from field data 
plot.info$Aspect<- as.numeric(plot.info$Aspect)
plot.info$Aspect<-cos(2*pi*(as.numeric(plot.info$Aspect)/365))

# final form of plot.info
head(plot.info)

# Part 2: Pull in data on the all tagged tree or sapling individuals, creates a dataframe called mega.data
########################################
 # In order to use the same script with multiple users enter your own pathway for setting the working directory in the open brackets after "else"
options(stringsAsFactors=FALSE)
if (Sys.getenv("USER")=='meaganoldfather') setwd("/Users/meaganoldfather/Box Sync/PepperwoodVegPlots/Database/2013/Woody2013/Data/OriginalCSV/Woody/") else setwd("")
 
 file.list <-dir(paste(getwd(),"/", sep='')) # file list is a character vector of the names of woody csv file
 strlen <- nchar(file.list[1]) # strlen is the length of all of the file names
 plot.list <- substr(file.list,strlen-10,strlen-4) # extracts plot numbers from file name vector
 
#Reads all .csv files in a specified folder into one list of data.frames; theoretically it should ignore .xls files and only read files with the .csv extension; skips top tree lines and just returns data block
mega.data<-lapply(file.list, read.csv,skip=3) 
length(mega.data) # check that mega.data is 50 (total number of plots)
names(mega.data) <- plot.list # make each entry have the name of the plots
 

#iterates through the whole list of data.frames corresponding to the number of plots worth of .csv files entered
 for (i in 1:length(mega.data)) 
 {
   print(i)
   Plot<-plot.list[i] #Pulls the plot out of the plot.list variable made above
   mega.data[[i]]<-cbind(Plot=Plot, mega.data[[i]]) #Inserts a plot column before the rest of the columns in the data.frame
   colnames(mega.data[[i]])<-c("Plot", "Quad", "Type", "TreeNum", "Species", "Confidence", "Dead.Stump", "SA.Stump.Height_cm", "SA.Stump.BD_cm", "SA.Branch.Num", "DBH_cm", "X_cm", "Y_cm", "Notes") #Sets column names of data.frame to the names defined above (some .csv files had different column names (SA BD (cm) versus SA or Stump BD (cm) which made rbind challenging so I just explicitly names all of the columns))  #mega.data[[i]]<-mega.data[[i]][-1,] #Removes the first row in the data.frame (had the column labels in it-- no longer needed because they have become the actual column names, rather than the first row) #commented out as csv read is now fixed
   mega.data[[i]]<-mega.data[[i]][,1:14] 
}
head(mega.data) 
 
 
# Part 3 (optional): Changes individuals originally identified as "QUEDEC", but now identified to species 
##########################################
source("/Users/meaganoldfather/Box Sync/PepperwoodVegPlots/Database/2013/OakID2013/AUG2013ResurveyQUEDEC.R")
head(mega.data)

# Part 5: Pull out information from mega.data; creates tree & sapling dataframes
###########################################
# Make all the lists for the tree vectors
trees<-vector("list", length(mega.data)) 
names(trees) <- plot.list

# And for saplings
saplings<-vector("list", length(mega.data))
names(saplings) <- plot.list

#iterates through the whole list of data.frames corresponding to the number of plots worth of .csv files entered
for (i in 1:length(mega.data)) 
{
  Plot<-plot.list[i] #Pulls the plot out of the plot.list variable made above 
  colnames(mega.data[[i]])<-c("Plot", "Quad", "Type", "TreeNum", "Species", "Confidence", "Dead.Stump", "SA.Stump.Height_cm", "SA.Stump.BD_cm", "SA.Branch.Num", "DBH_cm", "X_cm", "Y_cm", "Notes") 
  mega.data[[i]]<-mega.data[[i]][,1:14] 
  # for trees
  trees[[i]]<-mega.data[[i]][intersect(grep('TR',mega.data[[i]]$Type),(which(is.na(mega.data[[i]]$Dead.Stump)|(mega.data[[i]]$Dead.Stump=='')))), c(1:5,11,14)] 
  trees[[i]]<-trees[[i]][order(trees[[i]]$Species),] #alphabetizes according to Species column
  trees[[i]]<-subset(trees[[i]], subset=(!is.na(trees[[i]][,6]))) # removes trees with NA for DBH
  # for saplings
  saplings[[i]]<-mega.data[[i]][intersect(grep('SA',mega.data[[i]]$Type),(which(is.na(mega.data[[i]]$Dead.Stump)|(mega.data[[i]]$Dead.Stump=='')))), c(1:5,8:10)]
  saplings[[i]]<-saplings[[i]][order(saplings[[i]]$Species),] #alphabetizes according to Species column 
}
head(trees)
tree<-do.call(rbind, trees) # make list into one dataframe
head(tree)

head(saplings)
sapling<-do.call(rbind,saplings) # make list into one dataframe
head(sapling)

TR.species<-unique(tree$Species) # unique tree species
TR.species
length(TR.species) # 20

SA.species<-unique(sapling$Species) # unique sapling species
SA.species
length(SA.species) #22

# round the Tag numbers
tree$Tag<-floor(tree$TreeNum)
sapling$Tag<-floor(sapling$TreeNum)

TR.info<-tree[, c(1,3,5,6,8)] # TR.info is only Plot, Type, Species, DBH_cm, Tag  
head(TR.info)
SA.info<-sapling[, c(1,3,5,7:9)] # TR.info is only Plot, Type, Species,Tag
head(SA.info)


### Calculating Basal Area per species per plot 
# For TR
TR.info$Basal.Area<-(pi/4)*(TR.info$DBH_cm^2)
TR.cover<-with(TR.info, aggregate(Basal.Area ~ Plot + Species, FUN=sum))  # with this
head(TR.cover)

# For SA
SA.info$SA.Stump.BD_cm<- as.numeric(SA.info$SA.Stump.BD_cm)
SA.info$SA.Branch.Num<-as.numeric(SA.info$SA.Branch.Num)
SA.info$Calc<-((SA.info$SA.Stump.BD_cm)*(SA.info$SA.Branch.Num))
SA.info$Basal.Area<-(pi/4)*(SA.info$Calc^2)
SA.cover<-with(SA.info, aggregate(Basal.Area ~ Plot + Species, FUN=sum))
head(SA.cover)


### Calculating Number Indv per species per plot
# For TR
TR.num<-with(TR.info, aggregate(Tag ~ Plot + Species, FUN= function(x)  length(unique(x)))) 
colnames(TR.num)<-c("Plot","Species", "Number")
head(TR.num)

# For SA
SA.num<-with(SA.info, aggregate(Tag ~ Plot + Species, FUN= function(x)  length(unique(x)))) 
colnames(SA.num)<-c("Plot","Species", "Number")
head(SA.num)

#### Merging cover and num for each type
# For TR
TR<- merge(TR.cover,TR.num)
head(TR)

# For SA
SA<-merge(SA.cover, SA.num)
head(SA)

### Combining Trees and Sapling with cover and num info per species per plot 
SA.TR<-rbind(TR,SA) 
SA.TR.cover<-with(SA.TR, aggregate(Basal.Area ~ Plot+Species, FUN=sum))
SA.TR.num<-with(SA.TR, aggregate(Number ~ Plot+Species, FUN=sum))
SA.TR<-merge(SA.TR.cover, SA.TR.num)
head(SA.TR)

# Merging the plot info with the tree | & sapling info
plot.TR<-merge(TR,plot.info, by="Plot") #only TR individuals included
plot.SA<-merge(SA, plot.info, by="Plot") # only SA individuals included
plot.SA.TR<-merge(SA.TR, plot.info, by= "Plot")

head(plot.TR)
head(plot.SA)   
head(plot.SA.TR) 


# Part X (optional): Creates binary columns for mortality surveys for each year (ex. Resurvey2013), which 0 = dead
# This part needs to be done differently due to the need of multiple years of data 
# ;possible after the trees and sapling dataframes have been created, maybe worth building a function to incoperate yearly mortality 
########################################
source("/Users/meaganoldfather/Box Sync/PepperwoodVegPlots/Database/2013/Mortality2013/AUG2013ResurveyMortality.R")
head(mega.data)
# more sources made be added to include multiple years of mortality OR the above script can be changed to include multiple years 









