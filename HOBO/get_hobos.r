# Intent: Compile and Plot TBC3 Veg Plot Hobo Data 
# Authors: M.F. Oldfather, P.D. Papper
# Date created: 20151208
# Last edited: 20170804

#location="Z:/Box/Desktop/Weather/Raw Data/"
#plots=1308; convertF=TRUE; timesync=TRUE; roll30=TRUE; roll1=TRUE; allPST=TRUE; shuttleclean=TRUE; dirtyfix=TRUE
#plot="PPW1308"
getHOBOsFromRaw <- function(plots=c(1301:1350), output="list", location="github", convertF=TRUE, allPST=TRUE, roll30=TRUE, roll1=TRUE, shuttleclean=TRUE, dirtyfix=TRUE, timesync=TRUE) {

  # add "PPW" to the plot numbers
  plots <- paste("PPW", plots, sep="")

  # Set up downloading data from Github
  if(location=="github") {
    github <- TRUE
    location <- "https://raw.githubusercontent.com/pdpapper/PepperwoodVegPlots/patch-2/HOBO/Raw%20Data"
    files <- read.csv(paste(location, "hobo_csv_index.csv", sep="/"), header=FALSE, stringsAsFactors=FALSE)
  } else github <- FALSE

  hobos <- vector(mode="list", length=length(plots))
  names(hobos) <- plots

  for(plot in plots) {
    direct <- paste(location, substr(plot, 4, 7), sep="/")
    temp <- if(github) files[files[,1]==substr(plot, 4, 7),2] else list.files(direct, pattern=".csv")
    templist <- lapply(paste(direct, temp, sep="/"), read.csv, skip=1, header=TRUE, stringsAsFactors=FALSE) 
    # Convert the date and time from character to POSIXlt (LMT)
    templist <- lapply(templist, FUN=function(x) {
      x <- x[,1:4]
      x$Timestamp <- as.POSIXlt(x[,2], format="%m/%d/%y %I:%M:%S %p", tz="Pacific/Pitcairn")

# Identify and convert temperature data stored as Fahrenheit
      if(convertF & grepl(pattern="+F", x=colnames(x)[3])) x[,3] <- (x[,3]-32)/(1.8)

# Identify PDT (GMT-07:00) timestamps and convert them to PST (GMT-08:00)
      if(allPST & grepl(pattern="GMT.07.00", x=colnames(x)[2])) x$Timestamp$hour <- x$Timestamp$hour - 1

# Pick out previously flagged files that need timestamp rolled back 30 days
      if(roll30 & grepl(pattern="30.Days", x=colnames(x)[1])) x$Timestamp$mday <- x$Timestamp$mday - 30

# Pick out previously flagged files that need timestamp rolled forward 1 day
      if(roll1 & grepl(pattern="1.Day", x=colnames(x)[1])) x$Timestamp$mday <- x$Timestamp$mday + 1

      colnames(x) <- paste0("V", 1:ncol(x))
      return(x[,c(5,3:4)])
    })

    # Join them all ino a single data frame
    sub <- as.data.frame(do.call(rbind, templist))
    colnames(sub) <- c("Date.Time", "Temp", "RH")

# Remove empty lines created by HOBO shuttle connection
    if(shuttleclean) { sub <- sub[!is.na(sub[,2]),] }

# Basic filter to remove extremely bad temperature data (> 50*C or < -20*C)
    if(dirtyfix) { sub <- sub[!(sub[,2]>50 | sub[,2]<(-20)),] }

# Manually adjust timestamps at three plots to align them with to other plots
    if(timesync) {
      # PPW1302: -160 seconds
      sub$Date.Time$sec <- ifelse(sub$Date.Time$min %in% c(02,32), sub$Date.Time$sec - 160, sub$Date.Time$sec)
      # PPW1321: +136 seconds
      sub$Date.Time$sec <- ifelse(sub$Date.Time$min %in% c(57,27), sub$Date.Time$sec + 136, sub$Date.Time$sec)
      # PPW1328: +19 seconds
      sub$Date.Time$sec <- ifelse(sub$Date.Time$min %in% c(59,29), sub$Date.Time$sec - 41, sub$Date.Time$sec)
      sub$Date.Time$min <- ifelse(sub$Date.Time$min %in% c(59,29), sub$Date.Time$min + 1, sub$Date.Time$min)
    }

# Get rid of the damned POSIX Date.Times
    sub$Date.Time <- as.character(sub$Date.Time)
    if(output=="list") sub$Date.Time <- as.POSIXlt(sub$Date.Time, tz="Pacific/Pitcairn")
    
# Add this plot to the list
    hobos[[plot]] <- sub
  }

# Create a wide form data frame
  if(output=="wide") {
    newnames <- "Date.Time"; for(p in names(hobos)) { newnames <- c(newnames, paste("Temp.", p, sep=""), paste("RH.", p, sep="")) }
    hobos <- Reduce(function(x, y) merge(x, y, by="Date.Time", all=TRUE, suffixes=runif(2)), hobos)
    colnames(hobos) <- newnames
    hobos$Date.Time <- as.POSIXlt(hobos$Date.Time, tz="Pacific/Pitcairn")
  }
  
# Create a long form data frame
  if(output=="long") {
    for(i in 1:length(hobos)) hobos[[i]]$Plot <- names(hobos)[[i]]
    hobos <- as.data.frame(do.call(rbind, hobos))
    hobos$Date.Time <- as.POSIXlt(hobos$Date.Time, tz="Pacific/Pitcairn")
  }

  return(hobos)
}


createGitHubHOBOsIndex <- function(location=getwd()) {
  files <- list.files(location, pattern=".csv", recursive=TRUE)
  files <- t(as.data.frame(strsplit(files, "/")))
  write.table(files, paste(location, "hobo_csv_index.csv", sep="/"), sep=",", row.names=FALSE, col.names=FALSE, quote=FALSE)  
}
