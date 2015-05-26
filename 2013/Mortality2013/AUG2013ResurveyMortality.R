# Intent: Incoperates 2013 over-summer deaths of individuals into mega.data
# For more detailed notes see the associated txt file
# Author: M.F. Oldfather
# Date Created: 20130810 
# Date Last Edited: 20141016

for(i in 1:length(mega.data)){
mega.data[[i]]$Resurvey2013<-0
mega.data[[i]][(is.na(mega.data[[i]]$Dead.Stump)
              | mega.data[[i]]$Dead.Stump == ""), "Resurvey2013"] <- 1 
}
head(mega.data)

# Changes Resurvey2013 column to a "0" in the corresponding row of newly dead individuals  
Dead2013<- read.csv("/Users/meaganoldfather/Box Sync/PepperwoodVegPlots/Database/2013/Mortality2013/Aug_Dead_Inds.csv")
Dead2013$Plot<-as.numeric(Dead2013$Plot)

for(i in 1:dim(Dead2013)[1]){
mega.data[[Dead2013$Plot[i]]][which(mega.data[[Dead2013$Plot[i]]]$TreeNum == Dead2013$Num[i]), "Resurvey2013"] <-0
}
head(mega.data)
head(Dead2013) # lists all dead individuals from 2013 resurvey 


