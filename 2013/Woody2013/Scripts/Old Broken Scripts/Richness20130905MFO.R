x.GAR<-merge(Pts.QUEGAR,climate.pts)
x.DOU<-merge(Pts.QUEDOU, climate.pts)
x.PSE<-merge(Pts.PSEMEN, climate.pts)
head(x.GAR)
head(x.DOU)
head(x.PSE)

with(x.DOU, points(SoilMoisture, Slope, col="blue", pch=19))

with(x.GAR, plot(SoilMoisture, Slope, col="green", pch=19))

with(x.DOU, points(tpi500,Aspect, col="blue", pch=19))

with(x.GAR, plot(tpi500, Aspect, col="green", pch=19))   





with(x.PSE, plot(Aspect, Basal.Area, col="red", pch=19)) 

# Richness calculated in Ordination Scrips

x.rich<-cbind(climate.noP, rich)
head(x.rich)
with(x.rich, plot(tpi500, plot.rich))

rich.lm<-lm(x.rich$plot.rich ~ x.rich$Soil.Moisture)
summary(rich.lm)

head(plot.SA.TR)

try<-merge(x.rich,plot.SA.TR)
head(try)
try<-aggregate(Number ~ Plot, FUN=sum, data=try)
try<-merge(x.rich, try)

with(try, plot(Number, plot.rich))  # do not include P removal plots in this - dim of try should be 46 -5

