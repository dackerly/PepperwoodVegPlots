
# need to source Plot.Info.All.R script

head(Plot.Info.All)
# Across all plots what is the basal area of each species? 
Species.Area<- with(Plot.Info.All, aggregate(Basal.Area_cm2 ~ Species, FUN=sum)) 
#Species.Area.Ordered<-Species.Area[order(-Species.Area$Basal.Area_cm2),]
head(Species.Area.Ordered)

Species.Area.Type<- with(Plot.Info.All, aggregate(Basal.Area_cm2 ~ Species + Type, FUN=sum)) 
head(Species.Area.Type)
Species.Area.Type.Order<-Species.Area.Type[order(Species.Area.Type$Basal.Area_cm2),]
#Species.Area.Type.Order<- transform(Species.Area.Type, Species.Area.Type=Species.Area.Type[order(Species.Area.Type$Basal.Area_cm2),] 
head(Species.Area.Type.Order)

                           
df<-data.frame(Species.Area.Type.Order[1:3])
df                                 
rownames(df) <- seq(1:dim(df)[1])
                                    
                                    
Species.Num<- with(Plot.Info.All, aggregate(Number ~ Species, FUN=sum)) 
Species.Num.Type<- with(Plot.Info.All, aggregate(Number ~ Species + Type, FUN=sum)) 
head(Species.Num)
head(Species.Num.Type)

length(Species.Num.Type[Species.Num.Type$Type=="TR", ])
library(ggplot2)
ggplot(data=Species.Area.Ordered, aes(x=Species, y=Basal.Area_cm2)) + geom_bar(stat= "identity") + opts(axis.text.x=theme_text(angle=-90))
ggplot(data=df, aes(x=Species, y=Basal.Area_cm2, fill=Type)) + geom_bar(stat= "identity") + opts(axis.text.x=theme_text(angle=-90))+ scale_fill_brewer(palette = "Set1")

ggplot(data=Species.Num, aes(x=Species, y=Number)) + geom_bar(stat= "identity") + opts(axis.text.x=theme_text(angle=-90))
ggplot(data=Species.Num.Type.Ordered, aes(x=Species, y=Number, fill=Type)) + geom_bar(stat= "identity") + opts(axis.text.x=theme_text(angle=-90))+ scale_fill_brewer(palette = "Set1")



colnames(Species.Area.Ordered)
#write.csv(Species.Area.Ordered, file= "/Users/meaganoldfather/Dropbox/PepperwoodVegPlots/Database/2013/Woody2013/Outputs/ Species.Area.Total.csv")
     
     
     
Species.Area.Type<- with(Plot.Info.All, aggregate(Basal.Area_cm2 ~ Species + Type, FUN=sum)) 
head(Species.Area.Type)
dim(Species.Area.Type)
#Species.Area.Type<-as.matrix(Species.Area.Type)  
#head(Species.Area.Type)     
     
barplot(Species.Area.Type$Basal.Area_cm2)    
     
area.table<-table(Species.Area.Type)
barplot(area.table)          
#barplot(Species.Area.Type$Basal.Area_cm2, names.arg = Species.Area.Type$Species) 
     
    
     

     