
april.rad<-read.csv("~/Desktop/AprilRad.csv")
head(april.rad)

range(april.rad$Pepperwood, na.rm=T)
range(april.rad$SUM, na.rm=T)

with(april.rad, plot(density(Pepperwood, na.rm=T), main="April Radiation"))
with(april.rad, lines(density(SUM,na.rm=T), col="red"))
legend("topleft",legend=c("All Pepperwood Pts", "Veg Plot Pts"), col=c("black","red"),lty=1)

ecdf(april.rad$Pepperwood)(600) # 2%
ecdf(april.rad$SUM)(600) # 0%
