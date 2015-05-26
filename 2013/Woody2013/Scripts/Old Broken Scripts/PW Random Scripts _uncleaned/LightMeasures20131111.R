# Intent: Create excel sheet to enter light measurements, read in data points, make boxplot 

# 
plot.list
quads<-c("B2", "B4", "C4", "C2")
dir<-c("N","E", "S", "W")

light<-data.frame("Plot"=numeric(800),
                  "Quad"=numeric(800),
                  "Dir"=numeric(800),
                  "Light"=numeric(800))
head(light)
                  
counter=1
for(i in 1:50){
  for(j in 1:4){
   for(k in 1:4){
      light$Plot[counter]<-plot.list[i]
      light$Quad[counter]<-quads[j]
      light$Dir[counter]<-dir[k]
    counter=counter+1
    }
  }
}

#write.csv(light, file="~/Desktop/Light.csv")

light<-read.csv("~/Desktop/Light.csv")
head(light)
light<-light[,-6]
light<-light[,-1]
colnames(light)<-c("Plot", "Quad", "Dir", "Open")
light<-as.data.frame(light)
light$Sq<-light$Open/4  # divide by four to get number of open total squares
light$Density<- (100 - (light$Sq*4.17)) # multiple by 4.17 to get overstory density

head(light)
light2<-aggregate(light$Density~light$Plot + light$Quad, FUN=mean) # aggregate for each of the 4 pts in each quad
head(light2)
colnames(light2)<-c("Plot", "Quad", "Density")
dim(light2)
light<-aggregate(light2$Density ~ light2$Plot, FUN=mean)
light
colnames(light)<-c("Plot", "CC")
library("lattice")
boxplot(light2$Density~light2$Plot, ylab="Overstory Density %", xlab="Plot ID")
       # main="Overstory Density  Across All Plots")