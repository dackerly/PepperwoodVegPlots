# Intent:Combines tree,sapling,juvenile, and seedling data for each species in each plot 
# Builds correlations between different size classes
# Author: M.F. Oldfather
# Date Created: 2013?
# Date Last Edited: 20141021

# use the below script for multiple users, enter your own pathway for setting the working directory in the open brackets after "else"
if (Sys.getenv("USER")=='meaganoldfather') setwd("/Users/meaganoldfather/Box Sync/PepperwoodVegPlots/Database/Analyses/") else setwd("")
source("MasterData.R")
if (Sys.getenv("USER")=='meaganoldfather') setwd("/Users/meaganoldfather/Box Sync/PepperwoodVegPlots/Database/Analyses/") else setwd("")
source("SeedlingJuvenileScript.R")

# get all species of all size classes
x<-unique(TR$Species)
y<-unique(SA$Species)
z<-unique(SEJU.Plot$Species)
species<-unique(unlist(list(x,y,z)))

# make a template for the vector of dataframes with indivuals split into 4 size 
# categories: Adult, Sapling, Juvenile, Seedling, also lists the Basal Area (BA) of the Adult trees of that species in that particular plot
df.template <- data.frame(Plot=numeric(length(species)), Species=species, Adult=0, BA=0, Sapling=0, Juvenile=0, Seedling=0)
all <- vector("list", length=50)


for (i in 1:50)
{
	all[[i]] <- df.template
	all[[i]]$Plot <- plot.list[i]
	all[[i]]$Adult[match(TR[TR$Plot == plot.list[i], "Species"], species)] <- TR$Number[TR$Plot==plot.list[i]]
	all[[i]]$BA[match(TR[TR$Plot == plot.list[i], "Species"], species)] <-  TR$Basal.Area[TR$Plot==plot.list[i]]
	all[[i]]$Sapling[match(SA[SA$Plot == plot.list[i], "Species"], species)] <- SA$Number[SA$Plot==plot.list[i]]
	all[[i]]$Juvenile[match(SEJU.Plot[SEJU.Plot$Plot == plot.list[i], "Species"], species)] <- SEJU.Plot$Num.Juveniles[SEJU.Plot$Plot==plot.list[i]]
	all[[i]]$Seedling[match(SEJU.Plot[SEJU.Plot$Plot == plot.list[i], "Species"], species)] <- SEJU.Plot$Num.Seedlings[SEJU.Plot$Plot==plot.list[i]]
}

# check it out!
head(all)
tail(all)

# make it one giant dataframe
total<-do.call(rbind, all)

# add a column that is the total number of seedlings and juveniles 
total$JS<-total$Juvenile+total$Seedling

# These lines will create csv's with these base dataframes
# write.csv(total, "~/Desktop/AllInfo.csv")
# write.csv(TR, "~/Desktop/TR.csv")
# write.csv(SA, "~/Desktop/SA.csv")
# write.csv(SEJU.Plot, "~/Desktop/SEJU.csv")

###################################### BUILDING CORRELATIONS ###########################
# Need to form a species list where QUEDEC = all oak species due to that the smaller age classes were often only identified to the sub-genus level 
species.list <- list(PSEMEN="PSEMEN", QUEAGR="QUEAGR", UMBCAL="UMBCAL", AESCAL="AESCAL", HETARB="HETARB", ARBMEN="ARBMEN", ARCMAN="ARCMAN", AMOCAL="AMOCAL", FRACAL="FRACAL", BACPIL="BACPIL", NOTDEN="NOTDEN", ADEFAS="ADEFAS", QUEWIS="QUEWIS", QUEBER="QUEBER" ,QUEDEC=c("QUEDEC", "QUEGAR", "QUEDOU", "QUELOB", "QUEAGKE", "QUEXXX", "QUEKEL"), CEOCUN="CEOCUN", TORCAL="TORCAL", UNKN27="UNKN27", UNK28="UNK28", UNKN30="UNKN30", UNKN47="UNKN47", UNK21="UNK21", UNKN42="UNKN42")

# Make a dataframe that will contain all correlation between adults and seedlings/juveniles
correlation.df<- data.frame(Species=names(species.list), Adults.by.JS=numeric(length(species.list)))
head(correlation.df)
layout(matrix(1:12, nrow=3))
                            
for (i in 1:length(species.list)) 
  {
  Adults.by.plot <- aggregate(Adult ~ Plot, data=total[total$Species %in% species.list[[i]], ], FUN=sum)$Adult
  BA.by.plot <- aggregate(BA ~ Plot, data=total[total$Species %in% species.list[[i]], ], FUN=sum)$BA
  JS.by.plot <- aggregate(JS ~ Plot, data= total[total$Species %in% species.list[[i]], ], FUN=sum)$JS
                              
  #Assigns the appropriate row of our data frame with the correlation between these two vectors
  correlation.df$Adults.by.JS[i] <- cor(Adults.by.plot, JS.by.plot)
  correlation.df$BA.by.JS[i] <- cor(BA.by.plot, JS.by.plot)
                              
                              
                              
    # #Adds a correlation scatter plot to our quartz display using Adults by JS
    if (!is.na(correlation.df$Adults.by.JS[i]))
    {plot(Adults.by.plot, JS.by.plot, pch=19, main=names(species.list)[i], xlab="Adults", ylab="Juveniles + Seedlings")
    legend("top", box.lwd=0, legend=paste("r= ", round(correlation.df$Adults.by.JS[i], 2), sep=""))}
                              
     #Adds a correlation scatter plot to our quartz display using BA by JS
      #if (!is.na(correlation.df$BA.by.JS[i]))
      #{plot(BA.by.plot, JS.by.plot, pch=19, main=names(species.list)[i], xlab=expression(paste("Basal Area (", cm^2, ")", sep="")),  ylab="Juveniles + Seedlings")
      #legend("top", box.lwd=0, legend=paste("r= ", round(correlation.df$BA.by.JS[i], 2), sep=""))}
}
                            
                            
                            
                        
                            
                            





