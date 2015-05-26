#################  Percent Treeness! 

Pts.QUEGAR<- subset(Total.Pts,subset=(Total.Pts$Species == "QUEGAR"))
Pts.QUEDOU<- subset(Total.Pts,subset=(Total.Pts$Species == "QUEDOU"))
Pts.QUEKEL<- subset(Total.Pts,subset=(Total.Pts$Species == "QUEKEL"))
Pts.QUEAGR<- subset(Total.Pts,subset=(Total.Pts$Species == "QUEAGR"))
Pts.PSEMEN<- subset(Total.Pts,subset=(Total.Pts$Species == "PSEMEN"))
Pts.AMOCAL<- subset(Total.Pts,subset=(Total.Pts$Species == "AMOCAL"))


# Histogrmas
hist(Pts.QUEKEL$Percent.TR)
hist(Pts.QUEGAR$Percent.TR)
hist(Pts.QUEDOU$Percent.TR)
hist(Pts.QUEAGR$Percent.TR)
hist(Pts.PSEMEN$Percent.TR)
hist(Pts.AMOCAL$Percent.TR)

############################################# 

### DEM MODEL PLOTS
with(Total.Pts, plot(DEM,Percent.TR, main="All Species"))
with(Pts.QUEGAR, plot(DEM,Percent.TR, main="QUEGAR"))
with(Pts.QUEDOU, plot(DEM,Percent.TR, main = "QUEDOU"))
with(Pts.QUEKEL, plot(DEM,Percent.TR, main = "QUEKEL"))
with(Pts.QUEAGR, plot(DEM,Percent.TR, main = "QUEAGR"))
with(Pts.PSEMEN, plot(DEM,Percent.TR, main = "PSEMEN"))
with(Pts.AMOCAL, plot(DEM,Percent.TR, main = "AMOCAL"))


### DEM MODEL LM

QUEGAR.DEM.LM<-lm(Percent.TR~DEM, data=Pts.QUEGAR)
summary(QUEGAR.DEM.LM)

QUEDOU.DEM.LM<-lm(Percent.TR~DEM, data=Pts.QUEDOU)
summary(QUEDOU.DEM.LM)

QUEAGR.DEM.LM<-lm(Percent.TR~DEM, data=Pts.QUEAGR)
summary(QUEAGR.DEM.LM)

PSEMEN.DEM.LM<-lm(Percent.TR~DEM, data=Pts.PSEMEN)
summary(PSEMEN.DEM.LM)

AMOCAL.DEM.LM<-lm(Percent.TR~DEM, data=Pts.AMOCAL)
summary(AMOCAL.DEM.LM)

####  March Radiation Plots 

with(Total.Pts, plot(MarchRadiation,Percent.TR))
with(Pts.QUEGAR, plot(MarchRadiation,Percent.TR))
with(Pts.QUEDOU, plot(MarchRadiation,Percent.TR))
with(Pts.QUEKEL, plot(MarchRadiation,Percent.TR))
with(Pts.QUEAGR, plot(MarchRadiation,Percent.TR, main="QUEAGR"))

yvals2<-predict(QUEAGR.MARR.LM)
lines(Pts.QUEAGR$MarchRadiation,yvals2)
text(600,17000, labels= paste("Pval=", round(summary(QUEAGR.MARR.LM)[[4]][2,4],4)), pos=4)


with(Pts.PSEMEN, plot(MarchRadiation,Percent.TR))
with(Pts.AMOCAL, plot(MarchRadiation,Percent.TR))

#### March Radiation LM
QUEGAR.MARR.LM<-lm(Percent.TR~MarchRadiation, data=Pts.QUEGAR)
summary(QUEGAR.MARR.LM) 

QUEDOU.MARR.LM<-lm(Percent.TR~MarchRadiation, data=Pts.QUEDOU)
summary(QUEDOU.MARR.LM)  

QUEAGR.MARR.LM<-lm(Percent.TR~MarchRadiation, data=Pts.QUEAGR)
summary(QUEAGR.MARR.LM)   

PSEMEN.MARR.LM<-lm(Percent.TR~MarchRadiation, data=Pts.PSEMEN)
summary(PSEMEN.MARR.LM)  

AMOCAL.MARR.LM<-lm(Percent.TR~MarchRadiation, data=Pts.AMOCAL)
summary(AMOCAL.MARR.LM) 

### July Max Temp Plots
with(Total.Pts, plot(TMaxJUL,Percent.TR))
with(Pts.QUEGAR, plot(TMaxJUL,Percent.TR)) 
with(Pts.QUEDOU, plot(TMaxJUL,Percent.TR))
with(Pts.QUEKEL, plot(TMaxJUL,Percent.TR, main = "QUEKEL"))

yvals3<-predict(QUEKEL.TMAXJ.LM)
lines(Pts.QUEKEL$TMaxJUL,yvals3)
text(29.8,6000, labels= paste("Pval=", round(summary(QUEKEL.TMAXJ.LM)[[4]][2,4],4)), pos=4)


with(Pts.QUEAGR, plot(TMaxJUL,Percent.TR))

with(Pts.PSEMEN, plot(TMaxJUL,Percent.TR, main="PSEMEN"))
yvals4<-predict(PSEMEN.TMAXJ.LM)
lines(Pts.PSEMEN$TMaxJUL,yvals4)
text(30.5,0.8, labels= paste("Pval=", round(summary(PSEMEN.TMAXJ.LM)[[4]][2,4],4)), pos=4)

#### July Max Temp LM
QUEGAR.TMAXJ.LM<-lm(Percent.TR~TMaxJUL, data=Pts.QUEGAR)
summary(QUEGAR.TMAXJ.LM)  
plot(QUEGAR.TMAXJ.LM)

QUEDOU.TMAXJ.LM<-lm(Percent.TR~TMaxJUL, data=Pts.QUEDOU)
summary(QUEDOU.TMAXJ.LM) 

QUEKEL.TMAXJ.LM<-lm(Percent.TR~TMaxJUL, data=Pts.QUEKEL)
summary(QUEKEL.TMAXJ.LM) 

QUEAGR.TMAXJ.LM<-lm(Percent.TR~TMaxJUL, data=Pts.QUEAGR)
summary(QUEAGR.TMAXJ.LM) 

PSEMEN.TMAXJ.LM<-lm(Percent.TR~TMaxJUL, data=Pts.PSEMEN)
summary(PSEMEN.TMAXJ.LM) # --> this is significant
plot(PSEMEN.TMAXJ.LM)

AMOCAL.TMAXJ.LM<-lm(Percent.TR~TMaxJUL, data=Pts.AMOCAL)
summary(AMOCAL.TMAXJ.LM)

### Min Jan Temp Plots

with(Total.Pts, plot(tminJAN,Percent.TR))
with(Pts.QUEGAR, plot(tminJAN,Percent.TR))
with(Pts.QUEDOU, plot(tminJAN,Percent.TR))
with(Pts.QUEKEL, plot(tminJAN,Percent.TR, main="QUEKEL"))
with(Pts.QUEAGR, plot(tminJAN,Percent.TR))
with(Pts.PSEMEN, plot(tminJAN,Percent.TR))
with(Pts.AMOCAL, plot(tminJAN,Percent.TR))


### Min Jan Temp LM

QUEGAR.TMinJAN.LM<-lm(Percent.TR~tminJAN, data=Pts.QUEGAR)
summary(QUEGAR.TMinJAN.LM)  
plot(QUEGAR.TMinJAN.LM)

QUEDOU.TMinJAN.LM<-lm(Percent.TR~tminJAN, data=Pts.QUEDOU)
summary(QUEDOU.TMinJAN.LM) 
plot(QUEDOU.TMinJAN.LM)

QUEAGR.TMinJAN.LM<-lm(Percent.TR~tminJAN, data=Pts.QUEAGR)
summary(QUEAGR.TMinJAN.LM)

PSEMEN.TMinJAN.LM<-lm(Percent.TR~tminJAN, data=Pts.PSEMEN)
summary(PSEMEN.TMinJAN.LM) 

QUEKEL.TMinJAN.LM<-lm(Percent.TR~tminJAN, data=Pts.QUEKEL)
summary(QUEKEL.TMinJAN.LM)
plot((QUEKEL.TMinJAN.LM))

AMOCAL.TMinJAN.LM<-lm(Percent.TR~tminJAN, data=Pts.AMOCAL)
summary(AMOCAL.TMinJAN.LM)
plot((AMOCAL.TMinJAN.LM))

###### WD Plots

with(Total.Pts, plot(WD,Percent.TR, main="All Species"))
with(Pts.QUEGAR, plot(WD,Percent.TR, main = "QUEGAR"))
with(Pts.QUEDOU, plot(WD,Percent.TR, main="QUEDOU"))
with(Pts.QUEKEL, plot(WD,Percent.TR, main="QUEKEL"))
with(Pts.QUEAGR, plot(WD,Percent.TR, main= "QUEAGR"))
with(Pts.PSEMEN, plot(WD,Percent.TR, main= "PSEMEN"))
with(Pts.AMOCAL, plot(WD,Percent.TR, main= "AMOCAL"))

###### WD LM
QUEGAR.WD.LM<-lm(Percent.TR~WD, data=Pts.QUEGAR)
summary(QUEGAR.WD.LM)  
plot(QUEGAR.WD.LM)

QUEDOU.WD.LM<-lm(Percent.TR~WD, data=Pts.QUEDOU)
summary(QUEDOU.WD.LM) 
plot(QUEDOU.WD.LM)

QUEAGR.WD.LM<-lm(Percent.TR~WD, data=Pts.QUEAGR)
summary(QUEAGR.WD.LM)

PSEMEN.WD.LM<-lm(Percent.TR~WD, data=Pts.PSEMEN)
summary(PSEMEN.WD.LM) 

QUEKEL.WD.LM<-lm(Percent.TR~WD, data=Pts.QUEKEL)
summary(QUEKEL.WD.LM)
plot((QUEKEL.WD.LM))


AMOCAL.WD.LM<-lm(Percent.TR~WD, data=Pts.AMOCAL)
summary(AMOCAL.WD.LM)


###### TPI500

QUEGAR.tpi500.LM<-lm(Percent.TR~tpi500, data=Pts.QUEGAR)
summary(QUEGAR.tpi500.LM) # significant
with(Pts.QUEGAR, plot(tpi500, Percent.TR, main = "QUEGAR"))


QUEDOU.tpi500.LM<-lm(Percent.TR~tpi500, data=Pts.QUEDOU)
summary(QUEDOU.tpi500.LM)

QUEAGR.tpi500.LM<-lm(Percent.TR~tpi500, data=Pts.QUEAGR)
summary(QUEAGR.tpi500.LM)


QUEKEL.tpi500.LM<-lm(Percent.TR~tpi500, data=Pts.QUEKEL)
summary(QUEKEL.tpi500.LM) 

PSEMEN.tpi500.LM<-lm(Percent.TR~tpi500, data=Pts.PSEMEN)
summary(PSEMEN.tpi500.LM) #

AMOCAL.tpi500.LM<-lm(Percent.TR~tpi500, data=Pts.AMOCAL)
summary(AMOCAL.tpi500.LM)








######## Multiple Regressions 

QUEDOU.Multi<-lm(Percent.TR ~ tminJAN * TMaxJUL * WD, data=Pts.QUEDOU)
summary(QUEDOU.Multi) # All significant

QUEGAR.Multi<-lm(Percent.TR ~ tminJAN * TMaxJUL *WD, data=Pts.QUEGAR)
summary(QUEGAR.Multi)

QUEKEL.Multi<-lm(Percent.TR ~ tminJAN * TMaxJUL * WD, data=Pts.QUEKEL)
summary(QUEKEL.Multi)

QUEAGR.Multi<-lm(Percent.TR ~ tminJAN * TMaxJUL * WD, data=Pts.QUEAGR)
summary(QUEAGR.Multi)

PSEMEN.Multi<-lm(Percent.TR ~ tminJAN * TMaxJUL * WD, data=Pts.PSEMEN)
summary(PSEMEN.Multi) #-->min/max temp seperatly and interaction significant

AMOCAL.Multi<-lm(Percent.TR ~ tminJAN * TMaxJUL * WD, data=Pts.AMOCAL)
summary(AMOCAL.Multi)

############################################
QUEDOU.Multi<-lm(Percent.TR ~ DEM * MarchRadiation * WD, data=Pts.QUEDOU)
summary(QUEDOU.Multi) #--> Very significant


QUEGAR.Multi<-lm(Percent.TR ~ DEM * MarchRadiation * WD, data=Pts.QUEGAR)
summary(QUEGAR.Multi)

QUEAGR.Multi<-lm(Percent.TR ~ DEM * MarchRadiation * WD, data=Pts.QUEAGR)
summary(QUEAGR.Multi)


PSEMEN.Multi<-lm(Percent.TR ~ DEM * MarchRadiation * WD, data=Pts.PSEMEN)
summary(PSEMEN.Multi)

AMOCAL.Multi<-lm(Percent.TR ~ DEM * MarchRadiation * WD, data=Pts.AMOCAL)
summary(AMOCAL.Multi)
