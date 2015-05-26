## plot mapping script[[i]]
#pt[[i]] <- read.csv('/Users/david/Documents/Projects/Dropbox/PepperwoodVegPlots_2013/Veg Survey Data/Data Processing/plotTest/TBC3 FieldDataSheets 13-02-13_PPW1340.csv',comment.char='#')

#pt[[i]] <- read.csv("/Users/meaganoldfather/Dropbox/PepperwoodVegPlots/Database/2013/Woody2013/Data/OriginalCSV/Woody/WoodySurvey2013_PPW1301.csv",comment.char='#')
#head(pt[[i]])
#tail(pt[[i]])

source("/Users/meaganoldfather/Desktop/Datasheets/Creates_MegaData.R")
pt<-mega.data

qxy <- read.csv("/Users/meaganoldfather/Dropbox/PepperwoodVegPlots/Database/2013/Woody2013/Scripts/plotTest/quadXY.csv")
qxy

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
