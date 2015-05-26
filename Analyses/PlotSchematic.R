# Intent: Draw a skematic of the location of each tree individual tree for each plot 
# Author: M.F. Oldfather
# Date Created: 20130611
# Date Last Edited: 20141020

# use the below script for multiple users, enter your own pathway for setting the working directory in the open brackets after "else"
if (Sys.getenv("USER")=='meaganoldfather') setwd("/Users/meaganoldfather/Box Sync/PepperwoodVegPlots/Database") else setwd("")

# THIS SCRIPT IS NOT YET FUCNTIONAL

pts <- read.csv()
i<-1
for (i in 1:50){
t2q <- match(pt[[i]]$Quad,qxy$quad)

pt[[i]]$qx <- qxy$swX[t2q]
pt[[i]]$qy <- qxy$swY[t2q]

### NEED TO DEAL WITH NEGATIVES ###
pt[[i]]$AX_cm <- pt[[i]]$X_cm
pt[[i]]$AY_cm <- pt[[i]]$Y_cm

pt[[i]]$AX_cm[pt[[i]]$X_cm<0] <- 500 + pt[[i]]$X_cm[pt[[i]]$X_cm<0]
pt[[i]]$AY_cm[pt[[i]]$Y_cm<0] <- 500 + pt[[i]]$Y_cm[pt[[i]]$Y_cm<0]
#plot(pt[[i]]$X_cm,pt[[i]]$AX_cm)

pt[[i]]$pX <- pt[[i]]$qx + pt[[i]]$AX_cm/100
pt[[i]]$pY <- pt[[i]]$qy + pt[[i]]$AY_cm/100

plot(pt[[i]]$pX,pt[[i]]$pY)

plot(pt[[i]]$pX,pt[[i]]$pY,cex=pt[[i]]$DBH_cm/25,
     asp=1,bty='n',xaxt='n',xlab='',
     yaxt='n',ylab='',xlim=c(-1,21),ylim=c(-1,21))
points(pt[[i]]$pX,pt[[i]]$pY,cex=pt[[i]]$SA.or.Stump.BD_cm/25,pch=22)

}

rect(0,0,20,20)
for (i in c(5,10,15)) lines(x=c(0,20),y=c(i,i),lty=2)
for (i in c(5,10,15)) lines(x=c(i,i),y=c(0,20),lty=2)
