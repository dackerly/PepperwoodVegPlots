# Intent: Make datasheets for 2014 mortality surveys at Pepperwood Preserve 
# Author:MFO 
# Date Created: 201409102
# Date Last Modified: 201409102

library(gridExtra)
library(ggplot2)


source("/Users/meaganoldfather/Dropbox/PepperwoodVegPlots/Database/2013/Woody2013/Scripts/AllDataCreation20131016.R")
head(mega.data)
tail(mega.data)

# Create empty list of dataframes
x<-vector("list", length(mega.data)) 

for (i in 1:length(mega.data)) 
          {
  # remove stumps
      x[[i]]<- subset(mega.data[[i]], subset=(mega.data[[i]]$AUG.Resurvey == 1))
      # restrict columns to necessary for resurvey
      x[[i]]<-x[[i]][,c(1:5,8:13)]  # ?
      #Change columns names to shorter
      colnames(x[[i]])<-c("PLOT", "QUAD", "TYPE", "NUM", "SPECIES","HEIGHT","SA.BD","SA.NUM","DBH","X","Y")
      x[[i]]$STATUS<-("")
      x[[i]]$DAMAGE<-("")
      x[[i]]$NOTES<-("")
        }
head(x)     
# turn list into one giant dataframe
x<-do.call(rbind,x)
head(x)


# make all x,y coordinates be positive

x<-subset(x, subset=(x$X!="NA" & x$Y!="NA"))
x[is.na(x$X), ]
x[is.na(x$Y), ]


# Force x$Y to be numeric
x$Y<-as.numeric(x$Y)

head(x)
for(i in 1:dim(x)[1]){
if (x$X[i]<0) {x$X[i] <-500+x$X[i]}
if (x$Y[i]<0) {x$Y[i] <-500+x$Y[i]}
}


# sort x$NUM in each quad of every plot
x<-x[order(x$PLOT,x$QUAD,x$NUM),]

# create pdf's 
head(plot.list)

for(i in 1:length(plot.list)){
  current.sheet<-x[x$PLOT==plot.list[i],]
  # removed NA's due to seemenly unnecessary rows in 1311
  current.sheet<-current.sheet[!is.na(current.sheet$NUM),]
  rows.sheet<-nrow(current.sheet)
  pages<-ceiling(rows.sheet/48)
  for(j in 1:pages){
  part<-current.sheet[(((j-1)*48)+1:48),]
  
  # Get rid of the NA's at the end of the plots
  part<-part[!is.na(part$NUM),]
  
  pdf(paste("~/Desktop/TEST/",plot.list[i],".",j, ".pdf", sep=""),height=11, width=8.5)
  grid.table(part, show.vlines=T,show.hlines=T ,separator = "black", gp=gpar(fontsize=5.5))
  grid.text("DATE:                         OBSERVERS:", vjust=-45)
  grid.text("DC=DEAD CANOPY, P=PRONE, M=MISSING, D=DYING, R=REGRESSION", vjust=46)
  dev.off()
  } 
}


### EXTRA CODE ###



