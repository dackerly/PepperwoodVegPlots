head(bioclim)
class(bioclim$Slope)
bioclim$Slope<-as.numeric(bioclim$Slope)
bioclim$Slope<-as.numeric(bioclim$Aspect)


x<-lm(bioclim$Basal.Area~bioclim$Slope)
summary(x) # not significant

x<-lm(bioclim$Basal.Area~bioclim$Aspect)
summary(x) # not significant


x<-lm(bioclim$Basal.Area~bioclim$tpi500)
summary(x) # not significant

####
x<-lm(bioclim$Number~bioclim$Slope)
summary(x) # not significant

x<-lm(bioclim$Number~bioclim$Aspect)
summary(x) # not significant


x<-lm(bioclim$Number~bioclim$tpi500)
summary(x) # not significant
#######

x<-lm(bioclim$Basal.Area ~bioclim$Slope * bioclim$Aspect)
summary(x) # not significant 

x<-lm(bioclim$Basal.Area ~bioclim$tpi500 * bioclim$Aspect)
summary(x) # not significant 

######
qg<-subset(bioclim, subset=(bioclim$Species == "QUEGAR"))
ad<-subset(bioclim, subset=(bioclim$Species == "QUEDOU"))

x<-lm(ad$Basal.Area~ad$Slope)
summary(x) # not significant

x<-lm(ad$Basal.Area~ad$Aspect)
summary(x) # not significant


x<-lm(ad$Basal.Area~ad$tpi500)
summary(x) # not significant

####
x<-lm(ad$Number~ad$Slope)
summary(x) # not significant

x<-lm(ad$Number~ad$Aspect)
summary(x) # not significant


x<-lm(ad$Number~ad$tpi500)
summary(x) # not significant
#######

x<-lm(ad$Basal.Area ~ad$Slope * ad$Aspect)
summary(x) # not significant 

x<-lm(ad$Basal.Area ~ad$tpi500 * ad$Aspect)
summary(x) # not significant 


x<-lm(ad$Basal.Area~ad$Slope)
summary(x) # not significant

x<-lm(ad$Basal.Area~ad$Aspect)
summary(x) # not significant


x<-lm(ad$Basal.Area~ad$tpi500)
summary(x) # not significant

####

######
x<-lm(ag$Number~ag$Slope)
summary(x) # not significant

x<-lm(ag$Number~ag$Aspect)
summary(x) # not significant


x<-lm(ag$Number~ag$tpi500)
summary(x) # not significant
#######

x<-lm(ag$Basal.Area ~ag$Slope * ag$Aspect)
summary(x) # not significant 

x<-lm(ag$Basal.Area ~ag$tpi500 * ag$Aspect)
summary(x) # not significant 
