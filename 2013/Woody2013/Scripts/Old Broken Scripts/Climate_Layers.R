setwd("/Users/Queso/Documents/Rfiles/gltemp")
load(file="alldata.Rda")
load(file="catdata.Rda")
load(file="snddata.Rda")
load(file="wdsdata.Rda")
load(file="wimdata.Rda")
load(file="dailysum.Rda")

library(chron)
library(ggplot2)
library(gridExtra)
library(plyr)


	adp = qplot(alldata$date, alldata$Temp, data=alldata, facets=summit~aspect)
	grid.arrange(adp)
	
	
	save(alldata, file="alldata.Rda")
	load(file="alldata.Rda")

	pdf("alldata.pdf", width=6,height=12)
	adp
	dev.off()
	
	
	# one graph
	oneg= qplot(alldata$date, alldata$Temp, data=alldata)

	pdf("oneg.pdf", width=6,height=12)
	oneg
	dev.off()
	
	
### --------------------------------------------------------------
### linear regressions

tester=catdata

testR<-lm(tester$Temp~tester$date)

summary(testR)

plot(tester$date,tester$Temp)
abline(testR)



#daily maximum

### --------------------------------------------------------------
### extract max, min, average daily values --- takes about 40 minutes!!

alld<-alldata
dailysum<-ddply(alld,c(.(region),.(summit),.(aspect),.(date)), .fun = summarise, maxTemp = max(Temp), minTemp = min(Temp), aveTemp = mean(Temp))
            
dailysum$date<-as.Date(dailysum$date)       
                        
save(dailysum, file="dailysum.Rda")
load(file="dailysum.Rda")

### --------------------------------------------------------------
### linear regressions

## max values

maxR<-lm(dailysum$maxTemp~dailysum$date)
summary(maxR)

jdmax=as.numeric(max(dailysum$date))
jdmin=as.numeric(min(dailysum$date))
a04=.000438*jdmin-1.415
a12=.000438*jdmax-1.415
a04
a12

plot(dailysum$date,dailysum$maxTemp)
abline(maxR)

##max values minus the zeros
maxsub=subset(dailysum, !dailysum$maxTemp==0)

## min values

minR<-lm(dailysum$minTemp~dailysum$date)
summary(minR)

b04=-.0003904*jdmin+4.897
b12=-.0003904*jdmax+4.897
b04
b12

plot(dailysum$date,dailysum$minTemp)
abline(minR)

## alldata

allR<-lm(alldata$Temp~alldata$date)
summary(allR)

c04=-.00005492*jdmin+2.442
c12=-.00005492*jdmax+2.442
c04
c12

plot(dailysum$date,dailysum$minTemp)
abline(minR)

## by target region

wdsR=lm(wdsdata$Temp~wdsdata$date)
summary(wdsR)

plot(wimdata$date,wimdata$Temp)
abline(wimR)

##by peak

rnasub=subset(dailysum, dailysum$summit=="RNA")
rna=lm(rnasub$maxTemp~rnasub$date)
summary(rna)





### --------------------------------------------------------------
### max graph

maxG= ggplot(dailysum, aes(date, maxTemp)) +
		geom_point(size=.5) +
		geom_abline(intercept=-1.415, slope=.0004383) +
		opts(title = expression(paste("Daily Maximum Soil Temperatures"))) +
		opts(plot.title=theme_text(size=24)) +
		scale_y_continuous(expression(paste("Temperature (C)"))) +
		xlab("Date") +
		opts(axis.text.x=theme_blank()) +
		opts(panel.grid.minor = theme_blank()) +
		opts(axis.title.x=theme_blank()) +
		opts(axis.text.y=theme_text(size=16)) +
		opts(axis.title.y=theme_blank())

minG= ggplot(dailysum, aes(date, minTemp)) +
		geom_point(size=.5) +
		geom_abline(intercept=4.897, slope=-.0003904) +
		opts(title = expression(paste("Daily Minimum Soil Temperatures"))) +
		opts(plot.title=theme_text(size=24)) +
		scale_y_continuous(expression(paste("Temperature (C)"))) +
		xlab("Date") +
		opts(axis.text.x=theme_blank()) +
		opts(panel.grid.minor = theme_blank()) +
		opts(axis.title.x=theme_blank()) +
		opts(axis.text.y=theme_text(size=16)) +
		opts(axis.title.y=theme_blank())



##---------aspect graph

##max values minus the zeros
rnasub=subset(dailysum, dailysum$summit=="RNA")

aMax = ggplot(rnasub, aes(date, maxTemp, colour=factor(aspect))) +
		stat_smooth(size=.4, se=F, method="loess", span=.1) +
		#geom_abline(intercept=-1.415, slope=.0004383) +
		opts(title = expression(paste("Daily Maximum, RNA Summit by Aspect"))) +
		opts(plot.title=theme_text(size=24)) +
		scale_y_continuous(expression(paste("Temperature (C)"))) +
		xlab("Date") +
		opts(axis.text.x=theme_blank()) +
		opts(panel.grid.minor = theme_blank()) +
		opts(axis.title.x=theme_blank()) +
		opts(axis.text.y=theme_text(size=16)) +
		opts(axis.title.y=theme_blank()) +
		opts(legend.title=theme_blank()) +
		opts(legend.justification=c(0,0), legend.position=c(.07,.04)) +
		opts(legend.background = theme_rect(col=0))  # gets rids of box around the legend
		
aMin = ggplot(rnasub, aes(date, minTemp, colour=factor(aspect))) +
		stat_smooth(size=.4, se=F, method="loess", span=.1) +
		#geom_abline(intercept=-1.415, slope=.0004383) +
		opts(title = expression(paste("Daily Minimum, RNA Summit by Aspect"))) +
		opts(plot.title=theme_text(size=24)) +
		scale_y_continuous(expression(paste("Temperature (C)"))) +
		scale_y_continuous(limits=c(-10,15)) +
		xlab("Date") +
		opts(panel.grid.minor = theme_blank()) +
		opts(axis.title.x=theme_blank()) +
		opts(axis.text.y=theme_text(size=16)) +
		opts(axis.text.x=theme_text(size=16, angle=45)) +
		opts(axis.title.y=theme_blank()) +
		opts(legend.title=theme_blank())+
		opts(legend.justification=c(0,0), legend.position=c(.07,.15)) +
		opts(legend.background = theme_rect(col=0)) # gets rids of box around the legend



pdf("maxmin.pdf", width=8,height=14)
	grid.arrange(maxG,minG,aMax,aMin, ncol=1, heights=c(24/100,24/100,24/100,27/100))
dev.off()


ggplot(dailysum, aes(date, maxTemp, colour=factor(summit))) +
		stat_smooth(size=.4, se=F, method="lm", span=.1) +
		#geom_abline(intercept=-1.415, slope=.0004383) +
		opts(title = expression(paste("Daily Maximum, RNA Summit by Aspect"))) +
		opts(plot.title=theme_text(size=24)) +
		scale_y_continuous(expression(paste("Temperature (C)"))) +
		xlab("Date") +
		opts(axis.text.x=theme_blank()) +
		opts(panel.grid.minor = theme_blank()) +
		opts(axis.title.x=theme_blank()) +
		opts(axis.text.y=theme_text(size=16)) +
		opts(axis.title.y=theme_blank()) +
		
  opts(legend.title=theme_blank()) +
		opts(legend.justification=c(0,0), legend.position=c(.07,.04)) +
		opts(legend.background = theme_rect(col=0))  # gets rids of box around the legend
		
		