# 20130916 
# Shift from Blue to Oregon Oak over Soil Moisture gradient

# Need to run AllDataCreation20130830.R and GISLayersScript20130904
head(bioclim)
# subset to only include plots that have either Blue or Oregon Oak 
bioclim.dec<-subset(bioclim, subset=(bioclim$Species == "QUEGAR" | bioclim$Species == "QUEDOU" ))
head(bioclim.dec)

bioclim.gar<-subset(bioclim, subset=(bioclim$Species == "QUEGAR"))
bioclim.dou<-subset(bioclim, subset=(bioclim$Species == "QUEDOU"))

with(bioclim.dec, plot(Soil.Moisture, Number, pch=19))
with(bioclim.dou, points(Soil.Moisture, Number, col="blue", pch=19))
with(bioclim.gar, points(Soil.Moisture, Number, col="green", pch=19))

dec.num<-lm(bioclim.dec$Number ~ bioclim.dec$Soil.Moisture)
summary(dec.num)
gar.num<-lm(bioclim.gar$Number ~ bioclim.gar$Soil.Moisture)
summary(gar.num)
dou.num<-lm(bioclim.dou$Number ~ bioclim.dou$Soil.Moisture)
summary(dou.num)


with(bioclim.dec, plot(Soil.Moisture, Basal.Area, pch=19, log="xy"))
with(bioclim.dou, points(Soil.Moisture, Basal.Area, col="blue", pch=19, log="xy"))
with(bioclim.gar, points(Soil.Moisture, Basal.Area, col="green", pch=19, log="xy"))

dec.bas<-lm(bioclim.dec$Basal.Area ~ bioclim.dec$Soil.Moisture)
summary(dec.bas)
gar.bas<-lm(bioclim.gar$Basal.Area ~ bioclim.gar$Soil.Moisture)
summary(gar.bas)
dou.bas<-lm(bioclim.dou$Basal.Area ~ bioclim.dou$Soil.Moisture)
summary(dou.bas)

# Nothing is significant  for Soil.Moisture.... how about for tpi500

with(bioclim.dec, plot(tpi500, Number, pch=19))
with(bioclim.dou, points(tpi500, Number, col="blue", pch=19))
with(bioclim.gar, points(tpi500, Number, col="green", pch=19))

dec.num<-lm(bioclim.dec$Number ~ bioclim.dec$tpi500)
summary(dec.num) # significant
gar.num<-lm(bioclim.gar$Number ~ bioclim.gar$tpi500)
summary(gar.num) # significant
# could be driven by either soil moisture or cooler temperature--> need 
# fine-scale climate data!  Need to consider all variable to look at effects at contrasting edges of
#ecological similiar species. 
dou.num<-lm(bioclim.dou$Number ~ bioclim.dou$tpi500)
summary(dou.num) # not significant

with(bioclim.dec, plot(tpi500, Basal.Area, pch=19))
with(bioclim.dou, points(tpi500, Basal.Area, col="blue", pch=19))
with(bioclim.gar, points(tpi500, Basal.Area, col="green", pch=19))

dec.bas<-lm(bioclim.dec$Basal.Area ~ bioclim.dec$tpi500)
summary(dec.bas) # not significant
gar.bas<-lm(bioclim.gar$Basal.Area ~ bioclim.gar$tpi500)
summary(gar.bas) # significant
dou.bas<-lm(bioclim.dou$Basal.Area ~ bioclim.dou$tpi500)
summary(dou.bas) # not significant



###### Aspect
dou.num<-lm(bioclim.dou$Number ~ bioclim.dou$Aspect)
summary(dou.num) # not significant

with(bioclim.dec, plot(Aspect, Basal.Area, pch=19))
with(bioclim.dou, points(Aspect, Basal.Area, col="blue", pch=19))
with(bioclim.gar, points(Aspect, Basal.Area, col="green", pch=19))

dec.bas<-lm(bioclim.dec$Basal.Area ~ bioclim.dec$Aspect)
summary(dec.bas) # not significant
gar.bas<-lm(bioclim.gar$Basal.Area ~ bioclim.gar$Aspect)
summary(gar.bas) # significant
dou.bas<-lm(bioclim.dou$Basal.Area ~ bioclim.dou$Aspect)
summary(dou.bas) # not significant


plot(1:100,1:100, log="xy")
abline(v=seq(0,100,10), lty=3)
abline(h=seq(0,100,10), lty=3)
#or

plot(1:100,1:100, log="xy", axes=F)
axis(1, at=(seq(0,100,20)))
axis(2, at=(seq(0,100,10)))
abline(v=seq(0,100,20), lty=3)
abline(h=seq(0,100,10), lty=3)

