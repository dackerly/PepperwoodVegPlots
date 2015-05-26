
#################  Number!


### DEM MODEL PLOTS

#with(Total.Pts, plot(DEM,Number, main="All Species"))
with(Pts.QUEGAR, plot(DEM,Number, main="QUEGAR"))
with(Pts.QUEDOU, plot(DEM,Number, main = "QUEDOU"))
with(Pts.QUEKEL, plot(DEM,Number, main = "QUEKEL"))
with(Pts.QUEAGR, plot(DEM,Number, main = "QUEAGR"))
with(Pts.PSEMEN, plot(DEM,Number, main = "PSEMEN"))
with(Pts.AMOCAL, plot(DEM,Number, main = "AMOCAL"))
with(Pts.UMBCAL, plot(DEM,Number, main = "UMBCAL"))
### DEM MODEL LM

QUEGAR.DEM.LM<-lm(Number~DEM, data=Pts.QUEGAR)
summary(QUEGAR.DEM.LM)

QUEDOU.DEM.LM<-lm(Number~DEM, data=Pts.QUEDOU)
summary(QUEDOU.DEM.LM)

QUEAGR.DEM.LM<-lm(Number~DEM, data=Pts.QUEAGR)
summary(QUEAGR.DEM.LM)

PSEMEN.DEM.LM<-lm(Number~DEM, data=Pts.PSEMEN)
summary(PSEMEN.DEM.LM)

QUEKEL.DEM.LM<-lm(Number~DEM, data=Pts.QUEKEL)
summary(QUEKEL.DEM.LM)

AMOCAL.DEM.LM<-lm(Number~DEM, data=Pts.AMOCAL)
summary(AMOCAL.DEM.LM)


####  March Radiation Plots 
with(Total.Pts, plot(MarchRadiation,Number))
with(Pts.QUEGAR, plot(MarchRadiation,Number))
with(Pts.QUEDOU, plot(MarchRadiation,Number))
with(Pts.QUEKEL, plot(MarchRadiation,Number))
with(Pts.QUEAGR, plot(MarchRadiation,Number))
with(Pts.PSEMEN, plot(MarchRadiation,Number))
with(Pts.AMOCAL, plot(MarchRadiation,Number))


#### March Radiation LM
QUEGAR.MARR.LM<-lm(Number~MarchRadiation, data=Pts.QUEGAR)
summary(QUEGAR.MARR.LM) 

QUEDOU.MARR.LM<-lm(Number~MarchRadiation, data=Pts.QUEDOU)
summary(QUEDOU.MARR.LM)  

QUEAGR.MARR.LM<-lm(Number~MarchRadiation, data=Pts.QUEAGR)
summary(QUEAGR.MARR.LM)  

PSEMEN.MARR.LM<-lm(Number~MarchRadiation, data=Pts.PSEMEN)
summary(PSEMEN.MARR.LM)  

QUEKEL.MARR.LM<-lm(Number~MarchRadiation, data=Pts.QUEKEL)
summary(QUEKEL.MARR.LM)  

AMOCAL.MARR.LM<-lm(Number~MarchRadiation, data=Pts.AMOCAL)
summary(AMOCAL.MARR.LM)  

### July Max Temp Plots
with(Total.Pts, plot(TMaxJUL,Number))
with(Pts.QUEGAR, plot(TMaxJUL,Number)) 
with(Pts.QUEDOU, plot(TMaxJUL,Number))
with(Pts.QUEKEL, plot(TMaxJUL,Number))
with(Pts.QUEAGR, plot(TMaxJUL,Number))
with(Pts.PSEMEN, plot(TMaxJUL,Number))
with(Pts.AMOCAL, plot(TMaxJUL,Number))

#### July Max Temp LM
QUEGAR.TMAXJ.LM<-lm(Number~TMaxJUL, data=Pts.QUEGAR)
summary(QUEGAR.TMAXJ.LM)  
plot(QUEGAR.TMAXJ.LM)

QUEDOU.TMAXJ.LM<-lm(Number~TMaxJUL, data=Pts.QUEDOU)
summary(QUEDOU.TMAXJ.LM) 

QUEKEL.TMAXJ.LM<-lm(Number~TMaxJUL, data=Pts.QUEKEL)
summary(QUEKEL.TMAXJ.LM)

QUEAGR.TMAXJ.LM<-lm(Number~TMaxJUL, data=Pts.QUEAGR)
summary(QUEAGR.TMAXJ.LM) 

PSEMEN.TMAXJ.LM<-lm(Number~TMaxJUL, data=Pts.PSEMEN)
summary(PSEMEN.TMAXJ.LM)

AMOCAL.TMAXJ.LM<-lm(Number~TMaxJUL, data=Pts.AMOCAL)
summary(AMOCAL.TMAXJ.LM)

### Min Jan Temp Plots
with(Total.Pts, plot(tminJAN,Number))
with(Pts.QUEGAR, plot(tminJAN,Number))
with(Pts.QUEDOU, plot(tminJAN,Number))
with(Pts.QUEKEL, plot(tminJAN,Number, main="QUEKEL"))
with(Pts.QUEAGR, plot(tminJAN,Number))
with(Pts.PSEMEN, plot(tminJAN,Number))
with(Pts.AMOCAL, plot(tminJAN,Number))

### Min Jan Temp LM
QUEGAR.TMinJAN.LM<-lm(Number~tminJAN, data=Pts.QUEGAR)
summary(QUEGAR.TMinJAN.LM)  
plot(QUEGAR.TMinJAN.LM)

QUEDOU.TMinJAN.LM<-lm(Number~tminJAN, data=Pts.QUEDOU)
summary(QUEDOU.TMinJAN.LM) 
plot(QUEDOU.TMinJAN.LM)

QUEAGR.TMinJAN.LM<-lm(Number~tminJAN, data=Pts.QUEAGR)
summary(QUEAGR.TMinJAN.LM)

PSEMEN.TMinJAN.LM<-lm(Number~tminJAN, data=Pts.PSEMEN)
summary(PSEMEN.TMinJAN.LM) 

QUEKEL.TMinJAN.LM<-lm(Number~tminJAN, data=Pts.QUEKEL)
summary(QUEKEL.TMinJAN.LM)
plot((QUEKEL.TMinJAN.LM))

AMOCAL.TMinJAN.LM<-lm(Number~tminJAN, data=Pts.AMOCAL)
summary(AMOCAL.TMinJAN.LM) 
with(Pts.AMOCAL, plot(tminJAN,Number, main = "Amorpha californica", ylab="Number of Individuals"
                      , xlab= "Min January Temperature C", pch=19))
yvals3<-predict(AMOCAL.TMinJAN.LM)
lines(Pts.AMOCAL$tminJAN,yvals3)
text(3.9,4, labels= paste("Pval=", round(summary(AMOCAL.TMinJAN.LM)[[4]][2,4],4)), pos=4)






###### WD Plots
with(Total.Pts, plot(Soil.Moisture,Number, main="All Species"))
with(Pts.QUEGAR, plot(Soil.Moisture,Number, main = "QUEGAR"))
with(Pts.QUEDOU, plot(Soil.Moisture,Number, main="QUEDOU"))
with(Pts.QUEKEL, plot(Soil.Moisture,Number, main="QUEKEL"))
with(Pts.QUEAGR, plot(Soil.Moisture,Number, main= "QUEAGR"))
with(Pts.PSEMEN, plot(Soil.Moisture,Number, main= "PSEMEN"))
with(Pts.AMOCAL, plot(Soil.Moisture,Number, main= "AMOCAL"))

###### Soil.Moisture LM
QUEGAR.Soil.Moisture.LM<-lm(Number~Soil.Moisture, data=Pts.QUEGAR)
summary(QUEGAR.Soil.Moisture.LM)  
plot(QUEGAR.Soil.Moisture.LM)

QUEDOU.Soil.Moisture.LM<-lm(Number~Soil.Moisture, data=Pts.QUEDOU)
summary(QUEDOU.Soil.Moisture.LM) 
plot(QUEDOU.Soil.Moisture.LM)

QUEAGR.Soil.Moisture.LM<-lm(Number~Soil.Moisture, data=Pts.QUEAGR)
summary(QUEAGR.Soil.Moisture.LM)

PSEMEN.Soil.Moisture.LM<-lm(Number~Soil.Moisture, data=Pts.PSEMEN)
summary(PSEMEN.Soil.Moisture.LM) 

QUEKEL.Soil.Moisture.LM<-lm(Number~Soil.Moisture, data=Pts.QUEKEL)
summary(QUEKEL.Soil.Moisture.LM)
plot((QUEKEL.Soil.Moisture.LM))

AMOCAL.Soil.Moisture.LM<-lm(Number~Soil.Moisture, data=Pts.AMOCAL)
summary(AMOCAL.Soil.Moisture.LM) 

### tpi500 MODEL LM

QUEGAR.tpi500.LM<-lm(Number~tpi500, data=Pts.QUEGAR)
summary(QUEGAR.tpi500.LM) # very significant
with(Pts.QUEGAR, plot(tpi500,Number, main = "Quercus garryana", ylab="Number of Individuals per Plot"
                      , xlab= "Percent Lower Pixels", pch=19))
yvals3<-predict(QUEGAR.tpi500.LM)
lines(Pts.QUEGAR$tpi500,yvals3)
text(0,40, labels= paste("Pval=", round(summary(QUEGAR.tpi500.LM)[[4]][2,4],4)), pos=4)


QUEDOU.tpi500.LM<-lm(Number~tpi500, data=Pts.QUEDOU)
summary(QUEDOU.tpi500.LM)

QUEAGR.tpi500.LM<-lm(Number~tpi500, data=Pts.QUEAGR)
summary(QUEAGR.tpi500.LM)


QUEKEL.tpi500.LM<-lm(Number~tpi500, data=Pts.QUEKEL)
summary(QUEKEL.tpi500.LM) 

PSEMEN.tpi500.LM<-lm(Number~tpi500, data=Pts.PSEMEN)
summary(PSEMEN.tpi500.LM) #

AMOCAL.tpi500.LM<-lm(Number~tpi500, data=Pts.AMOCAL)
summary(AMOCAL.tpi500.LM)

UMBCAL.tpi500.LM<-lm(Number~tpi500, data=Pts.UMBCAL)
summary(UMBCAL.tpi500.LM)

####### Total Number with Many Models
## Not working below this... 
Sum.Pts<-with(Total.Pts, aggregate(Number~Plot.ID, FUN=sum))
head(Sum.Pts)

# DEM
Sum.DEM.Pts<- merge(Sum.Pts, dem.pts)
head(Sum.DEM.Pts)
with(Sum.DEM.Pts, plot(DEM,Number))

Sum.DEM.lm<-lm(Number~DEM, data=Sum.DEM.Pts) 
summary(Sum.DEM.lm)

# March Radiation
Sum.Marrad.Pts<- merge(Sum.Pts, marrad.pts)
head(Sum.Marrad.Pts)
with(Sum.Marrad.Pts, plot(MarchRadiation,Number, main = "All Species"))

yvals5<-predict(Sum.MAR.lm)
lines(Sum.Marrad.Pts$MarchRadiation,yvals5)
text(600,600, labels= paste("Pval=", round(summary(Sum.MAR.lm)[[4]][2,4],4)), pos=4)

Sum.MAR.lm<-lm(Number~MarchRadiation, data=Sum.Marrad.Pts) 
summary(Sum.MAR.lm) # ---> this is Significant

# Max July Temp 
Sum.TMax.Pts<- merge(Sum.Pts,tmaxJUL.pts)
head(Sum.TMax.Pts)
with(Sum.TMax.Pts, plot(TMaxJUL,Number, main= "All Species"))

yvals4<-predict(Sum.TMax.lm)
lines(Sum.TMax.Pts$TMaxJUL,yvals4)
text(30.6,600, labels= paste("Pval=", round(summary(Sum.TMax.lm)[[4]][2,4],4)), pos=4)

Sum.TMax.lm<-lm(Number~TMaxJUL, data=Sum.TMax.Pts) 
summary(Sum.TMax.lm) # ---> this is Significant


# Min Jan Temp 
Sum.TMinJAN.Pts<- merge(Sum.Pts,tminJAN.pts)
head(Sum.TMinJAN.Pts)
with(Sum.TMinJAN.Pts, plot(tminJAN,Number))

Sum.TMinJAN.lm<-lm(Number~tminJAN, data=Sum.TMinJAN.Pts) 
summary(Sum.TMinJAN.lm)


######## Multiple Regressions 

QUEDOU.Multi<-lm(Number ~ tminJAN * TMaxJUL * Soil.Moisture, data=Pts.QUEDOU)
summary(QUEDOU.Multi) #--> significance!  

QUEGAR.Multi<-lm(Number ~ tminJAN * TMaxJUL *Soil.Moisture, data=Pts.QUEGAR)
summary(QUEGAR.Multi)

QUEKEL.Multi<-lm(Number ~ tminJAN * TMaxJUL * Soil.Moisture, data=Pts.QUEKEL)
summary(QUEKEL.Multi)

QUEAGR.Multi<-lm(Number ~ tminJAN * TMaxJUL * Soil.Moisture, data=Pts.QUEAGR)
summary(QUEAGR.Multi)

PSEMEN.Multi<-lm(Number ~ tminJAN * TMaxJUL * Soil.Moisture, data=Pts.PSEMEN)
summary(PSEMEN.Multi)


############################################
QUEDOU.Multi<-lm(Number ~ DEM * MarchRadiation * Soil.Moisture, data=Pts.QUEDOU)
summary(QUEDOU.Multi)

QUEGAR.Multi<-lm(Number ~ DEM * MarchRadiation * Soil.Moisture, data=Pts.QUEGAR)
summary(QUEGAR.Multi)

QUEAGR.Multi<-lm(Number ~ DEM * MarchRadiation * Soil.Moisture, data=Pts.QUEAGR)
summary(QUEAGR.Multi)

