 # Intent: Change QUEDEC individuals into respective species after AUG2013 re-identification and collections    
# For more detailed notes see the associated txt file
# Author: M.F. Oldfather
# Date Created: 20130810
# Date Last Edited: 20150626

AUG.ID<-read.csv(paste(year,"/OakID2013/Aug_Species.csv", sep=""))
head(AUG.ID)
AUG.ID$Plot<-as.numeric(AUG.ID$Plot)

for(i in 1:dim(AUG.ID)[1]){
  mega.data[[AUG.ID$Plot[i]]][which(mega.data[[AUG.ID$Plot[i]]]$TreeNum == AUG.ID$Num[i]), "Species"] <-AUG.ID$Species[i]
}
head(mega.data)


