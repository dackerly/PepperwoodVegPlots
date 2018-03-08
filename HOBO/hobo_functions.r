# Intent: Functions for compiling and QC of TBC3 Veg Plot HOBO data 
# Authors: P.D. Papper, M.F. Oldfather
# Date created: 20151208
# Last edited: 20180308


# Main function to get HOBO data from individual files locally or on github
HOBO.getFromRaw <- function(plots=c(1301:1350), sensor="both", location="github", shuttleclean=TRUE, convertF=TRUE, allPST=TRUE, roll30=TRUE, roll1=TRUE, posixtimestamp=TRUE) {

  # add "PPW" to the plot numbers
  plots <- paste("PPW", plots, sep="")

  # Set up downloading data from Github
  if(location=="github") {
    github <- TRUE
    location <- "https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/HOBO/Raw_Data"
    files <- read.csv(paste(location, "hobo_csv_index.csv", sep="/"), header=FALSE, stringsAsFactors=FALSE)
  } else github <- FALSE

  # Create an empty list that will be filled
  hobos <- vector(mode="list", length=length(plots))
  names(hobos) <- plots

  for(plot in plots) {
    directory <- paste(location, substr(plot, 4, 7), sep="/")
    plot.files <- if(github) files[files[,1]==substr(plot, 4, 7),2] else list.files(directory, pattern=".csv")
    plot.list <- lapply(paste(directory, plot.files, sep="/"), read.csv, skip=1, header=TRUE, stringsAsFactors=FALSE) 

    plot.list <- lapply(plot.list, FUN=function(one.file) {
      # Only the first four columns have interesting info
      one.file <- one.file[,1:4]
      # Convert the date and time from character to POSIXct (local time)
      # Pacific/Pitcairn is used as the time zone because it doesn't observe Daylight Saving Time
      one.file$Date.Time <- as.POSIXct(one.file[,2], format="%m/%d/%y %I:%M:%S %p", tz="Pacific/Pitcairn")

# Remove empty entries (NAs) created by HOBO shuttle connection
      if(shuttleclean) one.file <- one.file[!is.na(one.file[,3]),]

# Identify temperature data stored as Fahrenheit and convert to Celsius
      if(convertF & grepl(pattern="+F", x=colnames(one.file)[3])) one.file[,3] <- (one.file[,3]-32)/(1.8)

# Identify PDT (GMT-07:00) timestamps and convert them to PST (GMT-08:00)
      if(allPST & grepl(pattern="GMT.07.00", x=colnames(one.file)[2])) one.file$Date.Time <- one.file$Date.Time - 3600

# Identify previously flagged files that need timestamp rolled back 30 days
      if(roll30 & grepl(pattern="30.Days", x=colnames(one.file)[1])) one.file$Date.Time <- one.file$Date.Time - 2592000

# Identify previously flagged files that need timestamp rolled forward 1 day
      if(roll1 & grepl(pattern="1.Day", x=colnames(one.file)[1])) one.file$Date.Time <- one.file$Date.Time + 86400

# Return the requested sensor columns: temp, rh, or both
      colnames(one.file) <- c("Record","Timestamp","Temp","RH","Date.Time")
      if(sensor=="temp") return(one.file[,c(5,3)])
      if(sensor=="rh") return(one.file[,c(5,4)])
      if(sensor=="both") return(one.file[,c(5,3:4)])
    })

    # Join the list into a single data frame for the plot
    plot.frame <- as.data.frame(do.call(rbind, plot.list))

    # Add this plot to the list
    hobos[[plot]] <- plot.frame
  }

  return(hobos)
}

# Generates a single .csv file that lists all the HOBO .csv files
# This file needs to be updated on GitHub to allow remote access
HOBO.generateGitHubIndex <- function(location=getwd()) {
  files <- list.files(location, pattern=".csv", recursive=TRUE)
  files <- t(as.data.frame(strsplit(files, "/")))
  write.table(files, paste(location, "hobo_csv_index.csv", sep="/"), sep=",", row.names=FALSE, col.names=FALSE, quote=FALSE)  
}


# Manually adjust timestamps at three plots to align them to other plots
HOBO.syncTimestamps <- function(dat) {
  # PPW1302: -160 seconds
  if("PPW1302" %in% names(dat)) dat$PPW1302$Date.Time[which(substr(dat$PPW1302$Date.Time,18,19) == "40")] <- hobos.raw$PPW1302$Date.Time[which(substr(dat$PPW1302$Date.Time, 18, 19) == "40")] - 160
  # PPW1321: +136 seconds
  if("PPW1321" %in% names(dat)) dat$PPW1321$Date.Time[which(substr(dat$PPW1321$Date.Time,18,19) == "44")] <- hobos.raw$PPW1321$Date.Time[which(substr(dat$PPW1321$Date.Time, 18, 19) == "44")] + 136
  # PPW1328: +19 seconds
  if("PPW1328" %in% names(dat)) dat$PPW1328$Date.Time[which(substr(dat$PPW1328$Date.Time,18,19) == "41")] <- hobos.raw$PPW1328$Date.Time[which(substr(dat$PPW1328$Date.Time, 18, 19) == "41")] + 19
  return(dat)
}


# Function to calculate concordance and return a full matrix
HOBO.plotConcordance <- function(dat) {
  concordance <- function(x, y, na.rm=TRUE) (2 * sqrt(summary(lm(y~x,na.action=na.omit))$r.squared) * sd(x,na.rm=na.rm) * sd(y,na.rm=na.rm)) / (var(x,na.rm=na.rm) + var(y,na.rm=na.rm) + ((mean(x,na.rm=na.rm) - mean(y,na.rm=na.rm))^2))
  concord <- apply(combn(ncol(dat), 2), 2, FUN=function(x) concordance(dat[,x[1]], dat[,x[2]]))
  # Some juggling to turn the vector of concordances into a symmetric matrix
  b <- matrix(1, length(dat), length(dat))
  b[lower.tri(b, diag=FALSE)] <- concord
  b <- t(b)
  b[lower.tri(b, diag=FALSE)] <- concord
  # Matrix to data frame
  b <- as.data.frame(b)
  rownames(b) <- colnames(dat)
  colnames(b) <- colnames(dat)
  return(b)
}


# Turn the list into a wide data frame to fill in the NAs
HOBO.listToWide <- function(dat) {
  newnames <- names(dat)
  dat <- Reduce(function(x, y) merge(x, y, by="Date.Time", all=TRUE, suffixes=runif(2)), dat)
  colnames(dat) <- c("Date.Time",newnames)
  return(dat)
}


# Turn a wide data frame back into a list..wheee!
HOBO.wideToList <- function(dat) {
  newnames <- colnames(dat)[-1]
  dat <- lapply(as.list(dat)[-1], function(x) data.frame(Date.Time=as.list(dat)[[1]],x=x,stringsAsFactors=FALSE))
  names(dat) <- newnames
  return(dat)
}


# De-sensortize a sensorQC sensor object
HOBO.sensorToList <- function(dat, newnames=c("Date.Time","x")) {
  lapply(dat, FUN=function(x) { colnames(x$sensor) <- newnames ; x$sensor } )
}


# De-sensortize a sensor QC object, keeping the flags as columns
HOBO.exportSensorData <- function(dat, rm.na, type) {
  flagColumn <- function(x) {
    for(f in x$flags) {
      newnames <- c(colnames(x$sensor),f$expression)
      x$sensor$newflag <- logical(nrow(x$sensor))
      x$sensor$newflag[f$flag.i] <- TRUE
      colnames(x$sensor) <- newnames
    }
    x$sensor
  }
  dat <- lapply(dat, FUN=flagColumn)
  dat <- lapply(names(dat), function(x) cbind(dat[[x]], site=x, stringsAsFactors=FALSE))
  dat <- do.call("rbind", dat)
  if(rm.na) dat <- dat[which(!is.na(dat$x)),]
  return(dat)
}


# A script to get the difference vector for any plot
dif <- function(dat, dat2=NULL) {
  if(is.null(dat2)) dat$sensor$x[-1] - dat$sensor$x[-length(dat$sensor$x)]
    else dat$sensor$x - dat2$sensor$x
}

# and one to work with flag()
timeDelta <- function(dat) c(NA, dat[-1] - dat[-length(dat)])

# Script to remove data following 1 bad point, with resetting counter
extendFlag <- function(dat, flag, span) {
  ovals <- flag(dat, flag)$flags[[1]]$flag.i
  if(length(ovals)>0) nvals <- sort(unique(unlist(lapply(ovals,function(x)return(seq(x,x+span,1))))))
    else nvals <- NULL
  ndat <- logical(length(dat))
  ndat[nvals] <- TRUE
  return(ndat)
}


# Method wrappers for sensorQC functions
# An easy method to flag() a list of sensor objects
flag.list <- function(dat, conc=NULL, flags) {
  for(p in 1:length(dat)) {
    if(!is.null(conc)) spatial.mean <<- rowMeans(Reduce(function(x,y) cbind(x,y), lapply(dat[order(conc[p])[c(47:49)]], function(x) x[[1]][,2])),na.rm=TRUE)
    dat[[p]] <- flag(dat[[p]], flags)
  }
  if(exists('spatial.mean')) rm(spatial.mean, pos=.GlobalEnv)
  return(dat)
}

# An easy method to create a sensor() object from a list of data frames
sensor.list <- function(dat, ...) lapply(dat, FUN=function(x) sensor(x, ...))

# An easy method to plot() a sensor object
plot.sensor <- function(dat, range=TRUE, ...) plot(dat$sensor$x[range], ...)

# An easy method to clean() a list of sensor objects
clean.list <- function(dat, ...) lapply(dat, FUN=function(x) clean(x, ...))

