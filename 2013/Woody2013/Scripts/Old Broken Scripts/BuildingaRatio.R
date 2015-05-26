# August 26th, 2013 MFO
# Intent of Script: Build a Ratio that is number (individuals) of Trees to number(individuals) of Sapling for each Plot
# Will only be possible if both are present as both types??? Maybe need to force values that are only trees to be one and
# plots that are only sapling to  be a zero

library(plyr)
library(reshape)

#source("/Users/meaganoldfather/Dropbox/PepperwoodVegPlots/Database/2013/Woody2013/Scripts/Plot.Info.All.R")
# Do not worry about the plots that the above scripts try to make --> ignore and press enter
# Eventually take out ordination plots from the above script and make a new one specifically for that only


## More complicated that I orginally thought bc needs to be for each plot. 

Ratio.Build<- Plot.Info.All[, c(1:3,9)] 
head(Ratio.Build)
dim(Ratio.Build)
trees <- subset(Ratio.Build, subset= Ratio.Build$Type == "TR")
                   saplings <-subset(Ratio.Build, subset= Ratio.Build$Type == "SA")
                   
                   #trees <- trees[ , -1]
                   #saplings <- saplings[ , -1]
                   
                   ratio <- trees
                   names(ratio) <- c("Plot.ID", "Species", "TR", "X")
                   ratio <- ratio[ , -4]
                   
                   ratio2 <- saplings
                   names(ratio2) <- c("Plot.ID", "Species", "SA", "X")
                   ratio2 <- ratio2[ , -4]
                   
                   final.ratio <- merge(ratio, ratio2, all=T)
                   
                   final.ratio[which(is.na(final.ratio$TR)), "TR"] <- 0
                   final.ratio[which(is.na(final.ratio$SA)), "SA"] <- 0
                   
                   final.ratio <- final.ratio[order(final.ratio$Plot.ID), ]
         head(final.ratio)         
         
                   
        #final.ratio$Ratio<-final.ratio$TR/final.ratio$SA            
        final.ratio$Sum<- final.ratio$TR + final.ratio$SA   
        final.ratio$Percent.TR<-final.ratio$TR/final.ratio$Sum  
        final.ratio$Percent.SA<- final.ratio$SA/final.ratio$Sum     

# This metric does not really work for individuals that often have many 'TR' branches, as it 
# greatly increases the number of individuals trees and therefore skews the metric
