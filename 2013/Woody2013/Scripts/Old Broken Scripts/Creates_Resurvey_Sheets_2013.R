# Intent: Creates Resurvey Sheets for all PW plots MFO 20130802

# Create mega.data
source("/Users/meaganoldfather/Desktop/Datasheets/Creates_MegaData.R")

head(mega.data)

for(i in 1:length(mega.data)){
  # reoders columns
  
# add columns 'Dead0813?' and 'LeavesPresent?',  'NumLeavesCollected' and 'AcornsPresent? , 'NumAcornCollected' 

  mega.data[[i]]$"Dead?"<-"" 
  mega.data[[i]]$"Lvs?"<-""
  mega.data[[i]]$"NumLvsCol"<-""
  mega.data[[i]]$"Acorns?"<-""
  mega.data[[i]]$"NumAcornCol"<-""
  
  mega.data[[i]]<-cbind(mega.data[[i]][,2:5],mega.data[[i]][,12:13],mega.data[[i]][,7:11], mega.data[[i]][,15:19], mega.data[[i]][4], mega.data[[i]][,14])
  
  
  # Gets rid of extra rows
  mega.data[[i]]<-(mega.data[[i]][(mega.data[[i]]$Quad != "" & mega.data[[i]]$Type != "" ),])
  
 # Renames columns
  colnames(mega.data[[i]])[18]<-"Notes"
  colnames(mega.data[[i]])[7]<-"D/S"
  colnames(mega.data[[i]])[8]<-"Height"
  colnames(mega.data[[i]])[9]<-"BD"
  colnames(mega.data[[i]])[10]<-"BNum"
  colnames(mega.data[[i]])[11]<-"DBH"
  
 # creates 50 datasheets
  write.csv(mega.data[[i]], file=(paste("/Users/meaganoldfather/Desktop/Datasheets/", names(mega.data)[i], ".csv",sep="")), row.names=F)}


