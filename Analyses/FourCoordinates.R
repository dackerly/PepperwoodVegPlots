#  Creates a csv file of the UTM coordinates for all 4 corners of each plot
# Author: Meagan F. Oldfather
# Date Created: 20150627

# clear workspace
rm(list=ls())

# library needed
library("RCurl")

# sources in all functions (described below) that allow access to the PPW Vegetation Plot Data
  sapply(c(url, ...), function(u) {
    eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
  })
}
source_https('https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/Analyses/PWfunctions_GitHub.R')

# forces character strings to not be treated as factors 
options(stringsAsFactors=FALSE) 

prefix='https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/'

# list of file names
url.plotinfo<- paste(prefix,"2013/Woody2013/Data/OriginalCSV/PlotInfo/PlotSurvey2013_", sep='')
plot.list<-get.plot()  

plot.data<-lapply(paste(url.plotinfo,plot.list,".csv",sep=''), function(x) read.csv(text=getURL(x, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")), skip=5, nrows=5, header=F))

plot.info<-data.frame("Plot"=numeric(length(plot.data)),
                      "SW_E"=numeric(length(plot.data)),
                      "SW_N"=numeric(length(plot.data)),
                      "NW_E"=numeric(length(plot.data)),
                      "NW_N"=numeric(length(plot.data)),
                      "NE_E"=numeric(length(plot.data)),
                      "NE_N"=numeric(length(plot.data)),
                      "SE_E"=numeric(length(plot.data)),
                      "SE_N"=numeric(length(plot.data))) 

plot.info$Plot<-plot.list 
for (i in 1:length(plot.data)){
  plot.info$SW_E[i]<-plot.data[[i]][1,2]
  plot.info$SW_N[i]<-plot.data[[i]][2,2]
  plot.info$NW_E[i]<-plot.data[[i]][1,4]
  plot.info$NW_N[i]<-plot.data[[i]][2,4]
  plot.info$NE_E[i]<-plot.data[[i]][1,6]
  plot.info$NE_N[i]<-plot.data[[i]][2,6]
  plot.info$SE_E[i]<-plot.data[[i]][1,9]
  plot.info$SE_N[i]<-plot.data[[i]][2,9]
}

