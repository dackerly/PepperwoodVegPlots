# June 11, 2013 MFO
# Intention of script is to pull plot characters (SW lat/long, slope, aspect) out of 'PlotInfo'
# csv and combine with tree data by plot;
# Columns: 
# Plot.ID, UTM.E (SW corner), UTM.N(SW corner), Slope (field collected), Aspect (field collected) 
# Tree.Species, Tree.Number(per species, DBH > 1cm), Basal.Area_cm2(per species, DBH>1cm), 
#Total.Basal.Area_cm2 (per plot, all species combined, DBH > 1cm), Percent.Cover (per species) 

options(stringsAsFactors=FALSE) 
#Setting this option makes sure that character strings aren't treated as factors. 

if (Sys.getenv("USER")=='meaganoldfather') setwd("/Users/meaganoldfather/Dropbox/PepperwoodVegPlots/Database/2013/Woody2013/Data/OriginalCSV/PlotInfo/") else setwd("/Users/david/Documents/Projects/Dropbox/PepperwoodVegPlots_2013/Database/2013/Woody2013Data/OriginalCSV/PlotInfo/")
# In order to use the same script with multiple users; Final folder is 'PlotInfo'

file.list <-dir(paste(getwd(),"/", sep=''))
file.list # list of file names of csv's in 'PlotInfo'

strlen <- nchar(file.list[1]) # strlen is the length of all of the file names
plot.list <- substr(file.list,strlen-10,strlen-4) # extracts plot numbers from file name vector
plot.list # list of plot names

plot.data<-lapply(file.list, read.csv, skip=5, nrows=5, header=F)
head(plot.data) 

plot.data[[1]] # return first element in plot.data (PPW1301)

plot.info<-data.frame("Plot.ID"=numeric(length(plot.data)),
                      "UTM.E"=numeric(length(plot.data)),
                      "UTM.N"=numeric(length(plot.data)),
                      "Slope"=numeric(length(plot.data)),
                      "Aspect"=numeric(length(plot.data))) 

plot.info$Plot.ID<-plot.list 
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
plot.info

# Now go into woody plot data in order to pull out the tree (and sapling) species present in each plot,
# the number of tree of that species, and the total cover of each species in each plot

#if (Sys.getenv("USER")=='meaganoldfather') setwd("/Users/meaganoldfather/Dropbox/PepperwoodVegPlots/Database/2013/Woody2013/Data/OriginalCSV/Woody/") else setwd("/Users/david/Documents/Projects/Dropbox/PepperwoodVegPlots_2013/Database/2013/Woody2013")
# final folder is 'Woody' 

#file.list <-dir(paste(getwd(),"/", sep='')) # file list is a character vector of the names of woody csv file
#strlen <- nchar(file.list[1]) # strlen is the length of all of the file names
#plot.list <- substr(file.list,strlen-10,strlen-4) # extracts plot numbers from file name vector

#mega.data<-lapply(file.list, read.csv,skip=3)  
#names(mega.data) <- plot.list

# Get mega.data from the R script that specifically creates it (within mortality script) 
#and than run it the scripts that renames the QUEDECs and adds a mortality columns 

source("/Users/meaganoldfather/Dropbox/PepperwoodVegPlots/Database/2013/Woody2013/AUG2013Resurvey/AUG2013ResurveyMortailty.R") # mortality is spelled wrong
head(mega.data)
source("/Users/meaganoldfather/Dropbox/PepperwoodVegPlots/Database/2013/Woody2013/AUG2013Resurvey/AUG2013ResurveyQUEDEC.R")
head(mega.data)

# Make all the lists of tree vectors
trees<-vector("list", length(mega.data)) 
names(trees) <- plot.list
tree.cover.by.plot<-vector("list", length(mega.data))
names(tree.cover.by.plot) <- plot.list
trees.clean<-vector("list", length(mega.data))
names(trees.clean)<-plot.list

# And for saplings
saplings<-vector("list", length(mega.data))
names(saplings) <- plot.list
sapling.cover.by.plot<-vector("list", length(mega.data))
names(sapling.cover.by.plot) <- plot.list

#############################################################################################

for (i in 1:length(mega.data)) #iterates through the whole list of data.frames corresponding to the number of plots worth of .csv files entered
{
  Plot<-plot.list[i] #Pulls the plot out of the plot.list variable made above
  #mega.data[[i]]<-cbind(Plot=Plot, mega.data[[i]]) #Inserts a plot column before the rest of the columns in the data.frame ... already done in previous creation of mega.data
  colnames(mega.data[[i]])<-c("Plot", "Quad", "Type", "TreeNum", "Species", "Confidence", "Dead.Stump", "SA.Stump.Height_cm", "SA.Stump.BD_cm", "SA.Branch.Num", "DBH_cm", "X_cm", "Y_cm", "Notes", "AUG.Resurvey") #Sets column names of data.frame to the names defined above 
  mega.data[[i]]<-mega.data[[i]][,1:15] 
  
  trees[[i]]<-mega.data[[i]][intersect(grep('TR',mega.data[[i]]$Type),(which(is.na(mega.data[[i]]$Dead.Stump)|(mega.data[[i]]$Dead.Stump=='')))), c(1:5,11,15)] # modified version of command below because 1302 had TR listed as ' TR' so wasn't matching with first condition
  trees[[i]]<-subset(trees[[i]], subset=(trees[[i]]$AUG.Resurvey != 0)) # gets rids of the indvs that died to calculate the species area
  trees[[i]]<-trees[[i]][order(trees[[i]]$Species),] #alphabetizes according to Species column
  trees[[i]]<-subset(trees[[i]], subset=(!is.na(trees[[i]][,6]))) # removes trees with NA for DBH
  saplings[[i]]<-mega.data[[i]][intersect(grep('SA',mega.data[[i]]$Type),(which(is.na(mega.data[[i]]$Dead.Stump)|(mega.data[[i]]$Dead.Stump=='')))), c(1:5,8:10)]
  saplings[[i]]<-saplings[[i]][order(saplings[[i]]$Species),] #alphabetizes according to Species column
 
}
head(trees)
tree<-do.call(rbind, trees)
head(saplings)
sapling<-do.call(rbind,saplings)

head(tree)
head(sapling)

TR.species<-unique(tree$Species) # unique tree species
TR.species
length(TR.species) # 20

SA.species<-unique(sapling$Species) # unique sapling species
SA.species
length(SA.species) #22

tree$Tag<-floor(tree$TreeNum)
sapling$Tag<-floor(sapling$TreeNum)

TR.info<-tree[, c(1,3,5,6,8)] # TR.info is only Plot, Type, Species, DBH_cm, Tag  
head(TR.info)
SA.info<-sapling[, c(1,3,5,7:9)] # TR.info is only Plot, Type, Species,Tag
head(SA.info)


### Calculating Basal Area per species per plot --> not necessary to combine indvs 
# For TR
TR.cover<-with(TR.info, aggregate(DBH_cm ~ Plot + Species, FUN=sum)) 
TR.cover$Basal.Area<-(pi/4)*(TR.cover$DBH_cm^2)
TR.cover<-TR.cover[,c(1,2,4)]
head(TR.cover)

# For SA
SA.info$SA.Stump.BD_cm<- as.numeric(SA.info$SA.Stump.BD_cm)
SA.info$SA.Branch.Num<-as.numeric(SA.info$SA.Branch.Num)
SA.info$Calc<-((SA.info$SA.Stump.BD_cm)*(SA.info$SA.Branch.Num))
SA.cover<-with(SA.info, aggregate(Calc ~ Plot + Species, FUN=sum)) 
SA.cover$Basal.Area<-(pi/4)*(SA.cover$Calc^2)
SA.cover<-SA.cover[,c(1,2,4)]
head(SA.cover)


### Calculating Number Indv per species per plot
# For TR
head(TR.info)
TR.num<-with(TR.info, aggregate(Tag ~ Plot + Species, FUN= function(x)  length(unique(x)))) 
colnames(TR.num)<-c("Plot","Species", "Number")
head(TR.num)

# For SA
head(SA.info)
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
dim(TR)
dim(SA)
dim(SA.TR)
head(SA.TR)
tail(SA.TR)

SA.TR.cover<-with(SA.TR, aggregate(Basal.Area ~ Plot+Species, FUN=sum))
dim(SA.TR.cover)
SA.TR.num<-with(SA.TR, aggregate(Number ~ Plot+Species, FUN=sum))
dim(SA.TR.cover)

SA.TR<-merge(SA.TR.cover, SA.TR.num)
head(SA.TR)
dim(SA.TR)

#####################################
# Merging the plot info with the tree | & sapling info
head(TR)
head(SA)
head(SA.TR)
head(plot.info)
colnames(plot.info)[1]<-("Plot")
head(plot.info)

plot.TR<-merge(TR,plot.info, by="Plot") #only TR individuals included
plot.SA<-merge(SA, plot.info, by="Plot") # only SA individuals included
plot.SA.TR<-merge(SA.TR, plot.info, by= "Plot")

head(plot.TR)
head(plot.SA)   
head(plot.SA.TR) 
# # # # # # # # # # # # # # # # # # # # # #

# sapling.cover<-do.call(rbind, sapling.cover.by.plot)
# head(sapling.cover) # Plot. 1330 is broken for the sapling cover
# 
# colnames(sapling.cover)[1]<-"Plot.ID" # change the name of the first column form 'Plot' to 'Plot.ID'
# 
# Plot.Info.Sapling<- merge(sapling.cover, plot.info, by="Plot.ID")  # merges tree cover and plot info by the Plot ID column                           
# Plot.Info.Sapling$Type<-'SA'
# Plot.Info.Sapling  
# ########################
# Plot.Info.All<-rbind(Plot.Info.Tree, Plot.Info.Sapling)
# head(Plot.Info.All) #Yay!
# ##########################
# total.area.tree<-aggregate(tree.cover$Basal.Area_cm2~tree.cover$Plot.ID, FUN=sum) # sums tree cover of all species by plot
# colnames(total.area.tree)[1]<- "Plot.ID"
# colnames(total.area.tree)[2]<-"Tree.Basal.Area_cm2"
# total.area.tree
# 
# Plot.Info.All<-merge(Plot.Info.All, total.area.tree, by="Plot.ID") # add column in Plot.Info.Tree that is the total basal area of all tree species in each plot 
# 
# total.area.sap<-aggregate(sapling.cover$Basal.Area_cm2~sapling.cover$Plot.ID, FUN=sum)
# colnames(total.area.sap)[1]<- "Plot.ID"
# colnames(total.area.sap)[2]<-"Sapling.Basal.Area_cm2"
# total.area.sap
# 
# Plot.Info.All<-merge(Plot.Info.All, total.area.sap, by="Plot.ID", all=T) # add column in Plot.Info.Tree that is the total basal area of all tree species in each plot 
# 
# Plot.Info.All[which(is.na(Plot.Info.All$Sapling.Basal.Area_cm2)), "Sapling.Basal.Area_cm2"]<-0
# 
# 
# Plot.Info.All[Plot.Info.All$Type=="TR", "Percent.Tree.Cover"]<-round((Plot.Info.All[Plot.Info.All$Type=="TR","Basal.Area_cm2"]/Plot.Info.All[Plot.Info.All$Type=="TR","Tree.Basal.Area_cm2"])*100,4)
# 
# Plot.Info.All[Plot.Info.All$Type=="SA", "Percent.Sapling.Cover"]<-round((Plot.Info.All[Plot.Info.All$Type=="SA","Basal.Area_cm2"]/Plot.Info.All[Plot.Info.All$Type=="SA","Sapling.Basal.Area_cm2"])*100,4)
# 
# Plot.Info.All$TreeSap.Basal.Area_cm2<-Plot.Info.All$Tree.Basal.Area_cm2 + Plot.Info.All$Sapling.Basal.Area_cm2 # This is the most useful metric as of now
# 
# Plot.Info.All$Aspect<- as.numeric(Plot.Info.All$Aspect)
# Plot.Info.All$AspectCos<-cos(2*pi*(as.numeric(Plot.Info.All$Aspect)/365))
# 
# # adds a column of the percent of that species (of both types) in a plot of the total basal area (both types) -> not finished
# #Percent.bySpecies<- with(Plot.Info.All, aggregate(Basal.Area_cm2~ Plot.ID + Species, FUN=sum))
# #colnames(Percent.bySpecies)[3]<-"Basal.Area.byspecies"
# 
# #Plot.Info.All$Percent.bySpecies<-round((Plot.Info.All$Basal.Area_cm2/Plot.Info.All$.Basal.Area_cm2)*100,4)
# 
# # adds a column of the percent of that species and its type (ie. UMBCAL sapling) in a plot of the total basal area (both types)
# Plot.Info.All$Percent.bySpeciesType<-round((Plot.Info.All$Basal.Area_cm2/Plot.Info.All$TreeSap.Basal.Area_cm2)*100,4)
# 
# # To swap columns order
# #Plot.Info.Tree<-cbind(Plot.Info.Tree[,1], Plot.Info.Tree[,5:8], Plot.Info.Tree[,2], Plot.Info.Tree[,3:4], Plot.Info.Tree[,9:10])
# #colnames(Plot.Info.Tree)[1]<-"Plot.ID"
# #colnames(Plot.Info.Tree)[6]<- "Species"
# 
# #Plot.Info.Tree
# # # # # # # # # # # # # # # # # # # # # # #
# # To swap columns order
# #Plot.Info.Sapling<-cbind(Plot.Info.Sapling[,1], Plot.Info.Sapling[,5:8], Plot.Info.Sapling[,2], Plot.Info.Sapling[,3:4], Plot.Info.Sapling[,9:10])
# #colnames(Plot.Info.Sapling)[1]<-"Plot.ID"
# #colnames(Plot.Info.Sapling)[6]<- "Species"
# 
# 
# # Plot attempts to view relationship between sapling abundace and tree abundance &  sapling cover and tree cover 
# # Also might be very interesting to bring in seedling/juvenile counts
# #Sapling.Species<-unique(Plot.Info.Sapling$Species) # missing QUELOB, QUEDOU, QUEKEL, QUEWIS --> MAKE ALL QUEDEC? Yes- in loop (done)
# #Tree.Species<-unique(Plot.Info.Tree$Species) # missing CEOCUN, UNK21, UNK28, UNK30,UNK32, TORCAL, UNK47
# #match(Sapling.Species, Tree.Species)
# #match(Tree.Species, Sapling.Species)
# 
# Species.Both<-c("PSEMEN", "QUEAGR", "UMBCAL", "AESCAL", "QUEDEC", "HETARB", "ARBMEN", "ARCMAN", "AMOCAL",
#                "FRACAL", "BACPIL", "QUEBER", "NOTDEN", "ADEFAS")
# 
# #Plot.Info.All<-subset(Plot.Info.All, subset=(Plot.Info.All$Species!="CEOCUN")) 
# #Plot.Info.All<-subset(Plot.Info.All, subset=(Plot.Info.All$Species!="UNK21")) 
# #Plot.Info.All<-subset(Plot.Info.All, subset=(Plot.Info.All$Species!="UNK28")) 
# #Plot.Info.All<-subset(Plot.Info.All, subset=(Plot.Info.All$Species!="UNK30")) 
# #Plot.Info.All<-subset(Plot.Info.All, subset=(Plot.Info.All$Species!="UNK32"))
# #Plot.Info.All<-subset(Plot.Info.All, subset=(Plot.Info.All$Species!="TORCAL"))
# #Plot.Info.All<-subset(Plot.Info.All, subset=(Plot.Info.All$Species!="UNK47")) 
# 
# # par(mfrow=c(1,1))
# # par(ask=TRUE)
# #  
# # for (i in 1:length(Species.Both)) {
# #   plot(as.numeric(substr(Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="TR","Plot.ID"],6,7)), 
# #        Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="TR","Percent.bySpeciesType"]
# #                      , col="blue", xlab="Plot", xlim=c(1,50)
# #        , ylab="Percent by Species", ylim=c(0,100), main=Species.Both[i])
# #   points(as.numeric(substr(Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="SA", "Plot.ID"],6,7)), 
# #        Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="SA","Percent.bySpeciesType"]
# #                      , col = "green")
# # }
# # 
# # 
# # 
# # 
# # ############
# # for (i in 1:length(Species.Both)) {
# #   plot(Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="TR","Slope"], 
# #        Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="TR","Percent.bySpeciesType"]
# #        , col="blue", xlab="Slope", ylab="Percent by Species", ylim=c(0,100), main=Species.Both[i])
# #   points(Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="SA", "Slope"], 
# #          Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="SA","Percent.bySpeciesType"]
# #          , col = "green")
# #   
# # }
# # 
# # #plot(Plot.Info.All$Aspect, Plot.Info.All$TreeSap.Basal.Area_cm2)
# # ###################################
# # 
# # for (i in 1:length(Species.Both)) {
# #   plot(as.numeric(substr(Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="TR","Plot.ID"],6,7)), 
# #        Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="TR","Basal.Area_cm2"]
# #        , col="blue", xlab="Plot", xlim=c(1,50)
# #        , ylab="Basal Area (cm2)", main=Species.Both[i])
# #   points(as.numeric(substr(Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="SA", "Plot.ID"],6,7)), 
# #          Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="SA","Basal.Area_cm2"]
# #          , col = "green")
# # }
# # 
# # for (i in 1:length(Species.Both)) {
# #   plot(Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="TR","AspectCos"], 
# #        Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="TR","Basal.Area_cm2"]
# #        , col="blue", xlab="AspectCos", ylab="Basal Area (cm2)",  main=Species.Both[i])
# #   points(Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="SA", "AspectCos"], 
# #          Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="SA","Basal.Area_cm2"]
# #          , col = "green")
# # }
# # 
# # 
# # 
# # Plot.Info.All$Tree.Basal.Area_cm2 
# # max(Plot.Info.All$Tree.Basal.Area_cm2)
# # min(Plot.Info.All$Tree.Basal.Area_cm2)
# # max(Plot.Info.All$Sapling.Basal.Area_cm2 )
# # min(Plot.Info.All$Sapling.Basal.Area_cm2 )
# # Plot.Info.All$Sapling.Basal.Area_cm2 
# # max(Plot.Info.All$Basal.Area_cm2)
# # min(Plot.Info.All$Basal.Area_cm2)
# # 
# # #################################
# # for (i in 1:length(Species.Both)) {
# #   plot(as.numeric(substr(Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="TR","Plot.ID"],6,7)), 
# #        Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="TR","Basal.Area_cm2"]
# #        , col="blue", xlab="Plot", xlim=c(1,50)
# #        , ylab="Basal Area (cm2)", main=Species.Both[i])
# #   points(as.numeric(substr(Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="SA", "Plot.ID"],6,7)), 
# #          Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="SA","Basal.Area_cm2"]
# #          , col = "green")
# #   
# #   plot(as.numeric(substr(Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="TR","Plot.ID"],6,7)), 
# #        Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="TR","Number"]
# #        , col="blue", xlab="Plot", xlim=c(1,50)
# #        , ylab="Number", main=Species.Both[i])
# #   points(as.numeric(substr(Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="SA", "Plot.ID"],6,7)), 
# #          Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="SA","Number"]
# #          , col = "green")
# # 
# # }
# # 
# # ######### 
# # #I want to try and add in the seedling and juvenile individuals 
# # #(see script 'SeedlingJuvenileScript.R')
# #  head(SEJU.Plot)
# # 
# # SEJU.Plot<- merge(SEJU.Plot, plot.info, by=("Plot.ID")) # adds in coordinates, Slope, Aspect
# # SEJU.Plot$Aspect<- as.numeric(SEJU.Plot$Aspect)
# # SEJU.Plot$AspectCos<-cos(2*pi*(as.numeric(SEJU.Plot$Aspect)/365))
# # #Plots the the Number of individuals of each species in each plot 
# # 
# # par(mfrow=c(1,1))
# # par(ask=TRUE)
# # # Plot x Number 
# # # Trees are blue, Saplings are green, Juveniles are orange, and Seedlings are red
# # 
# # for (i in 1:length(Species.Both)) {
# #   plot(as.numeric(substr(Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="TR","Plot.ID"],6,7)), 
# #        Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="TR","Number"]
# #        , col="blue", xlab="Plot", xlim=c(1,50)
# #        , ylab="Number", main=Species.Both[i]) 
# #   points(as.numeric(substr(Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="SA", "Plot.ID"],6,7)), 
# #          Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="SA","Number"]
# #          , col = "green")
# #   points(as.numeric(substr(SEJU.Plot[SEJU.Plot$Species==Species.Both[i], "Plot.ID"],6,7)),
# #          SEJU.Plot[SEJU.Plot$Species==Species.Both[i],"Num.Seedlings"]
# #          , col ="red")
# #   points(as.numeric(substr(SEJU.Plot[SEJU.Plot$Species==Species.Both[i], "Plot.ID"],6,7)),
# #          SEJU.Plot[SEJU.Plot$Species==Species.Both[i],"Num.Juveniles"]
# #          , col ="orange")
# #   
# # }
# # 
# # #Slope x Number
# # 
# # for (i in 1:length(Species.Both)) {
# #   plot(Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="TR","Slope"], 
# #        Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="TR","Number"]
# #        , col="blue", xlab="Slope"
# #        , ylab="Number", main=Species.Both[i]) 
# #   points(Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="SA", "Slope"], 
# #          Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="SA","Number"]
# #          , col = "green")
# #   points(SEJU.Plot[SEJU.Plot$Species==Species.Both[i], "Slope"],
# #          SEJU.Plot[SEJU.Plot$Species==Species.Both[i],"Num.Seedlings"]
# #          , col ="red")
# #   points(SEJU.Plot[SEJU.Plot$Species==Species.Both[i], "Slope"],
# #          SEJU.Plot[SEJU.Plot$Species==Species.Both[i],"Num.Juveniles"]
# #          , col ="orange")
# #   
# # }
# 
# #Number x Aspect
# 
# # for (i in 1:length(Species.Both)) {
# #   plot(Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="TR","AspectCos"], 
# #        Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="TR","Number"]
# #        , col="blue", xlab="Aspect"
# #        , ylab="Number", main=Species.Both[i]) 
# #   points(Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="SA", "AspectCos"], 
# #          Plot.Info.All[Plot.Info.All$Species==Species.Both[i] & Plot.Info.All$Type=="SA","Number"]
# #          , col = "green")
# #   points(SEJU.Plot[SEJU.Plot$Species==Species.Both[i], "AspectCos"],
# #          SEJU.Plot[SEJU.Plot$Species==Species.Both[i],"Num.Seedlings"]
# #          , col ="red")
# #   points(SEJU.Plot[SEJU.Plot$Species==Species.Both[i], "AspectCos"],
# #          SEJU.Plot[SEJU.Plot$Species==Species.Both[i],"Num.Juveniles"]
# #          , col ="orange")
# #   
# # }
# 
# 
# 
# # Attempt at ordination with seedling and juveniles
#  library("picante")
# orSE<- sample2matrix(SEJU.Plot[,c(1,3,2)]) # DO I NEED TO REMOVE PLOTS WITH NO SEEDLINGS?
# orJU<- sample2matrix(SEJU.Plot[,c(1,4,2)])
# 
# head(orJU)
# 
# ord <- metaMDS(orSE)
# summary(ord)
# plot(ord,type='t')
# 
# # What if sum the SE and JU and then ordinate with that? 
# 
# head(SEJU.Plot)
# SEJU.Plot$Num.SEJU<- SEJU.Plot$Num.Seedlings + SEJU.Plot$Num.Juveniles
#  ordSEJU<- sample2matrix(SEJU.Plot[,c(1,5,2)])
# 
# ord<-metaMDS(ordSEJU)
# summary(ord)
# plot(ord, type='t')
# 
# 
# 
# ordSEJU<- sample2matrix(SEJU.Plot[,c(1,5,2)])
# 
# # Would be interesting to see if certain species as adults fall out in the same places
# # as that species in a different age class? Not sure how to do ths exactly with an ordination 
# 
# head(SEJU.Plot)
# 
# 
# # Interested in the a way to compare the ratio between different age classes (numbers first?)
# # TR/SA
# # SA/JU
# #JU/SE
# 
# # Could also incorperate SE and JU age classes into basal.area percent if have each indv an average basal area. 
# # .4 cm basal for JU and 0.3 cm basal for SE --> brings up issue with comparing basal area for TR and SA  --> Measure a couple
# # --> need to find a conversion from DBH to 10-cm Basal Area
# 
# Plot.Info.All$Aspect<- as.numeric(Plot.Info.All$Aspect)
# Plot.Info.All$AspectCos<-cos(2*pi*(as.numeric(Plot.Info.All$Aspect)/365))
# 
# 
