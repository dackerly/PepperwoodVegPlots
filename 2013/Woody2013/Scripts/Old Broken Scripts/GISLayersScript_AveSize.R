
# Average Size!

### DEM MODEL PLOTS

with(Total.Pts, plot(DEM,Ave.Size, main="All Species"))
with(Pts.QUEGAR, plot(DEM,Ave.Size, main="QUEGAR"))
with(Pts.QUEDOU, plot(DEM,Ave.Size, main = "QUEDOU"))
with(Pts.QUEKEL, plot(DEM,Ave.Size, main = "QUEKEL"))
with(Pts.QUEAGR, plot(DEM,Ave.Size, main = "QUEAGR"))
with(Pts.PSEMEN, plot(DEM,Ave.Size, main = "PSEMEM"))

### DEM MODEL LM

QUEGAR.DEM.LM<-lm(Ave.Size~DEM, data=Pts.QUEGAR)
summary(QUEGAR.DEM.LM)

QUEDOU.DEM.LM<-lm(Ave.Size~DEM, data=Pts.QUEDOU)
summary(QUEDOU.DEM.LM)

QUEAGR.DEM.LM<-lm(Ave.Size~DEM, data=Pts.QUEAGR)
summary(QUEAGR.DEM.LM)

PSEMEN.DEM.LM<-lm(Ave.Size~DEM, data=Pts.PSEMEN)
summary(PSEMEN.DEM.LM)

####  March Radiation Plots 

with(Total.Pts, plot(MarchRadiation,Ave.Size))
with(Pts.QUEGAR, plot(MarchRadiation,Ave.Size))
with(Pts.QUEDOU, plot(MarchRadiation,Ave.Size))
with(Pts.QUEKEL, plot(MarchRadiation,Ave.Size))
with(Pts.QUEAGR, plot(MarchRadiation,Ave.Size))
with(Pts.PSEMEN, plot(MarchRadiation,Ave.Size))

#### March Radiation LM

QUEGAR.MARR.LM<-lm(Ave.Size~MarchRadiation, data=Pts.QUEGAR)
summary(QUEGAR.MARR.LM) 

QUEDOU.MARR.LM<-lm(Ave.Size~MarchRadiation, data=Pts.QUEDOU)
summary(QUEDOU.MARR.LM)  

QUEAGR.MARR.LM<-lm(Ave.Size~MarchRadiation, data=Pts.QUEAGR)
summary(QUEAGR.MARR.LM)  

QUEKEL.MARR.LM<-lm(Ave.Size~MarchRadiation, data=Pts.QUEKEL)
summary(QUEKEL.MARR.LM)  

PSEMEN.MARR.LM<-lm(Ave.Size~MarchRadiation, data=Pts.PSEMEN)
summary(PSEMEN.MARR.LM)  

AMOCAL.MARR.LM<-lm(Ave.Size~MarchRadiation, data=Pts.AMOCAL)
summary(AMOCAL.MARR.LM)  

### July Max Temp Plots
with(Total.Pts, plot(TMaxJUL,Ave.Size))
with(Pts.QUEGAR, plot(TMaxJUL,Ave.Size)) 
with(Pts.QUEDOU, plot(TMaxJUL,Ave.Size))
with(Pts.QUEKEL, plot(TMaxJUL,Ave.Size))
with(Pts.QUEAGR, plot(TMaxJUL,Ave.Size))
with(Pts.PSEMEN, plot(TMaxJUL,Ave.Size))

#### July Max Temp LM

QUEGAR.TMAXJ.LM<-lm(Ave.Size~TMaxJUL, data=Pts.QUEGAR)
summary(QUEGAR.TMAXJ.LM)  
plot(QUEGAR.TMAXJ.LM)

QUEDOU.TMAXJ.LM<-lm(Ave.Size~TMaxJUL, data=Pts.QUEDOU)
summary(QUEDOU.TMAXJ.LM) 

QUEKEL.TMAXJ.LM<-lm(Ave.Size~TMaxJUL, data=Pts.QUEKEL)
summary(QUEKEL.TMAXJ.LM) # --> significant

QUEAGR.TMAXJ.LM<-lm(Ave.Size~TMaxJUL, data=Pts.QUEAGR)
summary(QUEAGR.TMAXJ.LM) 

PSEMEN.TMAXJ.LM<-lm(Ave.Size~TMaxJUL, data=Pts.PSEMEN)
summary(PSEMEN.TMAXJ.LM)

### Min Jan Temp Plots

with(Total.Pts, plot(tminJAN,Ave.Size))
with(Pts.QUEGAR, plot(tminJAN,Ave.Size))
with(Pts.QUEDOU, plot(tminJAN,Ave.Size))
with(Pts.QUEKEL, plot(tminJAN,Ave.Size, main="QUEKEL"))
with(Pts.QUEAGR, plot(tminJAN,Ave.Size))
with(Pts.PSEMEN, plot(tminJAN,Ave.Size))

### Min Jan Temp LM
QUEGAR.TMinJAN.LM<-lm(Ave.Size~tminJAN, data=Pts.QUEGAR)
summary(QUEGAR.TMinJAN.LM)  
plot(QUEGAR.TMinJAN.LM)

QUEDOU.TMinJAN.LM<-lm(Ave.Size~tminJAN, data=Pts.QUEDOU)
summary(QUEDOU.TMinJAN.LM) 
plot(QUEDOU.TMinJAN.LM)

QUEAGR.TMinJAN.LM<-lm(Ave.Size~tminJAN, data=Pts.QUEAGR)
summary(QUEAGR.TMinJAN.LM)

PSEMEN.TMinJAN.LM<-lm(Ave.Size~tminJAN, data=Pts.PSEMEN)
summary(PSEMEN.TMinJAN.LM) 

QUEKEL.TMinJAN.LM<-lm(Ave.Size~tminJAN, data=Pts.QUEKEL)
summary(QUEKEL.TMinJAN.LM)
plot((QUEKEL.TMinJAN.LM))

AMOCAL.TMinJAN.LM<-lm(Ave.Size~tminJAN, data=Pts.AMOCAL)
summary(AMOCAL.TMinJAN.LM)
plot((AMOCAL.TMinJAN.LM))

###### WD Plots

with(Total.Pts, plot(WD,Ave.Size, main="All Species"))
with(Pts.QUEGAR, plot(WD,Ave.Size, main = "QUEGAR",))
yvals1<-predict(QUEGAR.WD.LM)
lines(Pts.QUEGAR$WD,yvals1)
text(10,2000, labels= paste("Pval=", round(summary(QUEGAR.WD.LM)[[4]][2,4],4)), pos=4)

with(Pts.QUEDOU, plot(WD,Ave.Size, main="QUEDOU"))
with(Pts.QUEKEL, plot(WD,Ave.Size, main="QUEKEL"))
with(Pts.QUEAGR, plot(WD,Ave.Size, main= "QUEAGR"))
with(Pts.PSEMEN, plot(WD,Ave.Size, main= "PSEMEN"))
with(Pts.AMOCAL, plot(WD,Ave.Size, main= "AMOCAL"))

###### WD LM
QUEGAR.WD.LM<-lm(Ave.Size~WD, data=Pts.QUEGAR)
summary(QUEGAR.WD.LM)  #--> very significant! 
plot(QUEGAR.WD.LM)

QUEDOU.WD.LM<-lm(Ave.Size~WD, data=Pts.QUEDOU)
summary(QUEDOU.WD.LM) 
plot(QUEDOU.WD.LM)

QUEAGR.WD.LM<-lm(Ave.Size~WD, data=Pts.QUEAGR)
summary(QUEAGR.WD.LM)

PSEMEN.WD.LM<-lm(Ave.Size~WD, data=Pts.PSEMEN)
summary(PSEMEN.WD.LM) 

QUEKEL.WD.LM<-lm(Ave.Size~WD, data=Pts.QUEKEL)
summary(QUEKEL.WD.LM)
plot((QUEKEL.WD.LM))

AMOCAL.WD.LM<-lm(Ave.Size~WD, data=Pts.AMOCAL)
summary(AMOCAL.WD.LM)
plot((AMOCAL.WD.LM))

### tpi500 MODEL LM

QUEGAR.tpi500.LM<-lm(Ave.Size~tpi500, data=Pts.QUEGAR)
summary(QUEGAR.tpi500.LM) # significant
with(Pts.QUEGAR, plot(tpi500, Ave.Size, main = "QUEGAR"))


QUEDOU.tpi500.LM<-lm(Ave.Size~tpi500, data=Pts.QUEDOU)
summary(QUEDOU.tpi500.LM)

QUEAGR.tpi500.LM<-lm(Ave.Size~tpi500, data=Pts.QUEAGR)
summary(QUEAGR.tpi500.LM)


QUEKEL.tpi500.LM<-lm(Ave.Size~tpi500, data=Pts.QUEKEL)
summary(QUEKEL.tpi500.LM) 

PSEMEN.tpi500.LM<-lm(Ave.Size~tpi500, data=Pts.PSEMEN)
summary(PSEMEN.tpi500.LM) #

AMOCAL.tpi500.LM<-lm(Ave.Size~tpi500, data=Pts.AMOCAL)
summary(AMOCAL.tpi500.LM)

####### Total Ave>Size with Many Models
Sum.Pts<-with(Total.Pts, aggregate(Ave.Size~Plot.ID, FUN=sum))
head(Sum.Pts)

# DEM
Sum.DEM.Pts<- merge(Sum.Pts, dem.pts)
head(Sum.DEM.Pts)
with(Sum.DEM.Pts, plot(DEM,Ave.Size))

Sum.DEM.lm<-lm(Ave.Size~DEM, data=Sum.DEM.Pts) 
summary(Sum.DEM.lm)

# March Radiation
Sum.Marrad.Pts<- merge(Sum.Pts, marrad.pts)
head(Sum.Marrad.Pts)
with(Sum.Marrad.Pts, plot(MarchRadiation,Ave.Size))

# Max July Temp 
Sum.TMax.Pts<- merge(Sum.Pts,tmaxJUL.pts)
head(Sum.TMax.Pts)
with(Sum.TMax.Pts, plot(TMaxJUL,Ave.Size))

#######   
plot(Total.Pts$tminJAN, Total.Pts$TMaxJUL)
plot(Total.Pts$DEM, Total.Pts$MarchRadiation)


######## Multiple Regressions 

QUEDOU.Multi<-lm(Ave.Size ~ tminJAN * TMaxJUL * WD, data=Pts.QUEDOU)
summary(QUEDOU.Multi)

QUEGAR.Multi<-lm(Ave.Size ~ tminJAN * TMaxJUL *WD, data=Pts.QUEGAR)
summary(QUEGAR.Multi)

QUEKEL.Multi<-lm(Ave.Size ~ tminJAN * TMaxJUL * WD, data=Pts.QUEKEL)
summary(QUEKEL.Multi)

QUEAGR.Multi<-lm(Ave.Size ~ tminJAN * TMaxJUL * WD, data=Pts.QUEAGR)
summary(QUEAGR.Multi)

PSEMEN.Multi<-lm(Ave.Size ~ tminJAN * TMaxJUL * WD, data=Pts.PSEMEN)
summary(PSEMEN.Multi)

############################################
QUEDOU.Multi<-lm(Ave.Size ~ DEM * MarchRadiation * WD, data=Pts.QUEDOU)
summary(QUEDOU.Multi)

QUEGAR.Multi<-lm(Ave.Size ~ DEM * MarchRadiation * WD, data=Pts.QUEGAR)
summary(QUEGAR.Multi)

QUEAGR.Multi<-lm(Ave.Size ~ DEM * MarchRadiation * WD, data=Pts.QUEAGR)
summary(QUEAGR.Multi)




