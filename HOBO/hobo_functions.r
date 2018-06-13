# Intent: Compile and Plot TBC3 Veg Plot HOBO Data 
# Authors: M.F. Oldfather, P.D. Papper
# Date created: 20151208
# Last edited: 20180618
#
# This source file has functions for working with Veg Plot HOBO data:
#
# HOBO.getFromRaw()
# HOBO.repairTimestamps()
# HOBO.flagIncidents()
# HOBO.flagData()
# HOBO.flagData.threshold()
# HOBO.flagData.persist()
# HOBO.flagData.slope()
# HOBO.flagData.extend()
# HOBO.generateGitHubIndex()
# HOBO.syncTimestamps()
# HOBO.plotConcordance()
#

# Get HOBO data from individual files locally or on github
HOBO.getFromRaw <- function(plots=c(1301:1350), sensorType, location="github", convertF=TRUE, allPST=TRUE) {

# Set up downloading data from Github or else use local files
  if(location=="github") {
    location <- "https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/HOBO"
    files <- paste(location, "Raw_Data", apply(read.csv(paste(location, "hobo_csv_index.csv", sep="/"), header=FALSE, stringsAsFactors=FALSE)[,1:2], 1, paste, collapse="/" ), sep="/")
  } else {
    files <- list.files(paste(location, "Raw_Data", plots, sep="/"), pattern=".csv", full.names=TRUE)
  }

# Open all the individual raw HOBO files and store them in a list  
  filelist <- lapply(files, read.table, sep=",", fill=TRUE, stringsAsFactors=FALSE) 

# Create a (blank) tests run table
  tests_run <- data.frame(deployment_id=character(), flag=character(), start_date=as.POSIXct(character()), end_date=as.POSIXct(character()), dateApplied=as.POSIXct(character()), dataFlagger=character())

# Open and set up the flag definitions table
  flag_definitions <- read.csv(paste(location, "flag_definitions.csv", sep="/"), stringsAsFactors=FALSE)

# Open and set up the deployment log table
  deployment_log <- read.csv(paste(location, "deployment_log.csv", sep="/"), stringsAsFactors=FALSE)
  deployment_log$start_date <- as.POSIXct(deployment_log$start_date, format="%m/%d/%y %H:%M", tz="Pacific/Pitcairn")
  deployment_log$end_date <- as.POSIXct(deployment_log$end_date, format="%m/%d/%y %H:%M", tz="Pacific/Pitcairn")

# Open and set up the incident reports table
  incident_reports <- read.csv(paste(location, "incident_reports.csv", sep="/"), stringsAsFactors=FALSE)
  incident_reports$start_date <- as.POSIXct(incident_reports$start_date, format="%m/%d/%y %H:%M", tz="Pacific/Pitcairn")
  incident_reports$end_date <- as.POSIXct(incident_reports$end_date, format="%m/%d/%y %H:%M", tz="Pacific/Pitcairn")

# Apply to each individual file
  sensor_data <- lapply(filelist, FUN=function(x) {

  # Preserve the column names
    colnames(x) <- x[2,]
  
  # Fill the first column with the sensor_id
    id <- strsplit(colnames(x)[3], " ", fixed=TRUE)[[1]][8]
    x[,1] <- as.integer(substr(id, 1, nchar(id)-1))

  # Remove extraneous rows and columns from raw HOBO file format
    x <- x[-(1:2),1:4]
    x <- x[!(x[3]==""),]      

  # Convert the date and time from character to POSIXlt (LMT)
    x[,5] <- as.POSIXct(x[,2], format="%m/%d/%y %I:%M:%S %p", tz="Pacific/Pitcairn")

  # Add site_id and deployment_id field (from the deployment log)
    x[,6:7] <- NA
    for(r in 1:nrow(deployment_log)) {
      if(deployment_log[r,1] %in% unique(x[,1])) {
        x[which(x[,1]==deployment_log[r,1] & x[,5]>=deployment_log[r,8] & (x[,5]<=deployment_log[r,9] | is.na(deployment_log[r,9]))),6:7] <- deployment_log[r,c('deployment_id','site_id')]
      }
    }

  # Convert sensor readings to numeric (default read as character)
    x[,3] <- as.numeric(x[,3])
    x[,4] <- as.numeric(x[,4])

  # Identify PDT (GMT-07:00) timestamps and convert them to PST (GMT-08:00)
    if(allPST & grepl(pattern="GMT.07.00", x=colnames(x)[2])) x[,5] <- x[,5] - 60*60
    
  # Identify and convert temperature data stored as Fahrenheit
    if(sensorType=="AirTemp" & convertF & grepl(pattern="+F", x=colnames(x)[3])) x[,3] <- (x[,3]-32)/(1.8)

  # Return the requested sensor columns: temp or rh
    colnames(x) <- paste0("V", seq(1, length(x)), sep="")
    if(sensorType=="AirTemp") return(x[,c(1,6,7,5,3)])
    if(sensorType=="RH") return(x[,c(1,6,7,5,4)])
  })
  
# Join them all into a single data frame
  sensor_data <- as.data.frame(do.call(rbind, sensor_data))
  colnames(sensor_data) <- c("sensor_id","deployment_id","site_id","Timestamp","observedValue")
  # Reset row numbering
  rownames(sensor_data) <- NULL
  # Append the sensorType
  sensor_data$sensorType <- sensorType
  # Append the "U flag to all rows
  sensor_data$flags <- "U"

# Add all the dataframes to a single list object and return it
  list(sensor_data=sensor_data, tests_run=tests_run, flag_definitions=flag_definitions, deployment_log=deployment_log, incident_reports=incident_reports)
}


# Repairing issues identified in HOBO timestamps
# (Probably caused by syncing with a bad HOBO shuttle in the field)
HOBO.repairTimestamps <- function(dat) {
# Select just the incidents we want to flag
  repair <- dat$incident_reports[which(dat$incident_reports$dataQualityLevel=="REPAIRED"),]

# For efficiency, select just the sites that need repair
  x <- dat$sensor_data[which(dat$sensor_data$deployment_id %in% repair[repair$incidentType=="roll30",'deployment_id']),]
# Run the date-time fix (for timestamps that are +30 days)
  for(r in 1:nrow(repair)) {
    if(repair[r,'incidentType']=="roll30" & repair[r,'deployment_id'] %in% unique(x[,'deployment_id'])) { x[which(x[,'deployment_id']==repair[r,'deployment_id'] & x[,'Timestamp']>=repair[r,'start_date'] & x[,'Timestamp']<=repair[r,'end_date']),'Timestamp'] <- x[which(x[,'deployment_id']==repair[r,'deployment_id'] & x[,'Timestamp']>=repair[r,'start_date'] & x[,'Timestamp']<=repair[r,'end_date']),'Timestamp'] - 30*24*60*60 }
  }
# Merge the repaired rows back in to the sensor data
  dat$sensor_data <- rbind(dat$sensor_data[which(!(dat$sensor_data$deployment_id %in% repair[repair$incidentType=="roll30",'deployment_id'])),], x)

# As above but for timestamps that are -24 hours
  x <- dat$sensor_data[which(dat$sensor_data$deployment_id %in% repair[repair$incidentType=="roll1",'deployment_id']),]
  for(r in 1:nrow(repair)) {
    if(repair[r,'incidentType']=="roll1" & repair[r,'deployment_id'] %in% unique(x[,'deployment_id'])) { x[which(x[,'deployment_id']==repair[r,'deployment_id'] & x[,'Timestamp']>=repair[r,'start_date'] & x[,'Timestamp']<=repair[r,'end_date']),'Timestamp'] <- x[which(x[,'deployment_id']==repair[r,'deployment_id'] & x[,'Timestamp']>=repair[r,'start_date'] & x[,'Timestamp']<=repair[r,'end_date']),'Timestamp'] + 24*60*60 }
  }
  dat$sensor_data <- rbind(dat$sensor_data[which(!(dat$sensor_data$deployment_id %in% repair[repair$incidentType=="roll1",'deployment_id'])),], x)

  return(dat)
}


# Flag data from the incident table using deployment_id + date range
HOBO.flagIncidents <- function(dat, qual) {
# Select just the incidents we want to flag
  incident <- dat$incident_reports[which(dat$incident_reports$dataQualityLevel==qual & dat$incident_reports$sensorType%in%dat$sensor_data$sensorType),]

# Select just the deployments from the sensor data that match the flag
  x <- dat$sensor_data[which(dat$sensor_data$deployment_id %in% incident$deployment_id),]

# Flag rows based on time stamp falling within date range
  for(r in 1:nrow(incident)) {
    if(incident[r,'deployment_id'] %in% unique(x[,'deployment_id'])) { 
      x[which(!(grepl(incident[r,'flags'],x$flags)) & x[,'deployment_id']==incident[r,'deployment_id'] & x[,'Timestamp']>=incident[r,'start_date'] & x[,'Timestamp']<=incident[r,'end_date']),'flags'] <- paste(x[which(!(grepl(incident[r,'flags'],x$flags)) & x[,'deployment_id']==incident[r,'deployment_id'] & x[,'Timestamp']>=incident[r,'start_date'] & x[,'Timestamp']<=incident[r,'end_date']), 'flags'], incident[r,'flags'], sep=",")
    }
  }

# Merge the flagged rows back to the data and return
  dat$sensor_data <- rbind(dat$sensor_data[which(!(dat$sensor_data$deployment_id %in% incident$deployment_id)),], x)
  return(dat)
}


# A wrapper function that can take a vector of any type of flags and call them appropriately
HOBO.flagData <- function(dat, flags) {
# Remove the "U" flag from all rows in the sensor data
  dat$sensor_data[which(dat$sensor_data$flags=="U"),"flags"] <- NA
 
  for(flag in flags) {
    if(dat$flag_definitions[which(dat$flag_definitions$flags==flag),'testType'] == "threshold") dat <- HOBO.flagData.threshold(dat, flag)
    if(dat$flag_definitions[which(dat$flag_definitions$flags==flag),'testType'] == "persist") dat <- HOBO.flagData.persist(dat, flag)
    if(dat$flag_definitions[which(dat$flag_definitions$flags==flag),'testType'] == "slope") dat <- HOBO.flagData.slope(dat, flag)
    if(dat$flag_definitions[which(dat$flag_definitions$flags==flag),'testType'] == "extend") dat <- HOBO.flagData.extend(dat, flag)
  }
  
  return(dat)
}
#
HOBO.flagData.threshold <- function(dat, flag) {
# Define 'x' as the observedValues
  x <- dat$sensor_data$observedValue
# Separate tests for rows previously flagged by another test versus those still marked NA
  dat$sensor_data[which(!(grepl(flag,dat$sensor_data$flags)) & eval(parse(text=dat$flag_definitions[which(dat$flag_definitions$flags==flag),'testStatement'])) & is.na(dat$sensor_data$flags)),'flags'] <- flag
  dat$sensor_data[which(!(grepl(flag,dat$sensor_data$flags)) & eval(parse(text=dat$flag_definitions[which(dat$flag_definitions$flags==flag),'testStatement'])) & !is.na(dat$sensor_data$flags)),'flags'] <- paste(dat$sensor_data[which(!(grepl(flag,dat$sensor_data$flags)) & eval(parse(text=dat$flag_definitions[which(dat$flag_definitions$flags==flag),'testStatement'])) & !is.na(dat$sensor_data$flags)),'flags'], flag, sep=",")

  return(dat)
}
#
HOBO.flagData.persist <- function(dat, flag) {
# define 'x' as the vector of observedValue persistence
  x <- rep(rle(dat$sensor_data$observedValue)$lengths, rle(dat$sensor_data$observedValue)$lengths)
# Separate tests for rows previously flagged by another test versus those still marked NA
  dat$sensor_data[which(!(grepl(flag,dat$sensor_data$flags)) & eval(parse(text=dat$flag_definitions[which(dat$flag_definitions$flags==flag),'testStatement'])) & is.na(dat$sensor_data$flags)),'flags'] <- flag
  dat$sensor_data[which(!(grepl(flag,dat$sensor_data$flags)) & eval(parse(text=dat$flag_definitions[which(dat$flag_definitions$flags==flag),'testStatement'])) & !is.na(dat$sensor_data$flags)),'flags'] <- paste(dat$sensor_data[which(!(grepl(flag,dat$sensor_data$flags)) & eval(parse(text=dat$flag_definitions[which(dat$flag_definitions$flags==flag),'testStatement'])) & !is.na(dat$sensor_data$flags)),'flags'], flag, sep=",")

  return(dat)
}
#
HOBO.flagData.slope <- function(dat, flag) {
# define 'x' as the vector of differences between successive observedValues
  x <- c(NA, abs(dat$sensor_data$observedValue[-1] - dat$sensor_data$observedValue[-length(dat$sensor_data$observedValue)]))
# Separate tests for rows previously flagged by another test versus those still marked NA
  dat$sensor_data[which(!(grepl(flag,dat$sensor_data$flags)) & eval(parse(text=dat$flag_definitions[which(dat$flag_definitions$flags==flag),'testStatement'])) & is.na(dat$sensor_data$flags)),'flags'] <- flag
  dat$sensor_data[which(!(grepl(flag,dat$sensor_data$flags)) & eval(parse(text=dat$flag_definitions[which(dat$flag_definitions$flags==flag),'testStatement'])) & !is.na(dat$sensor_data$flags)),'flags'] <- paste(dat$sensor_data[which(!(grepl(flag,dat$sensor_data$flags)) & eval(parse(text=dat$flag_definitions[which(dat$flag_definitions$flags==flag),'testStatement'])) & !is.na(dat$sensor_data$flags)),'flags'], flag, sep=",")

  return(dat)
}
# Flag a series of observations following one flagged observation
HOBO.flagData.extend <- function(dat, flag) {
  span <- as.numeric(strsplit(dat$flag_definitions[which(dat$flag_definitions$flags==flag),'testStatement'], " ")[[1]][3])
  old <- strsplit(dat$flag_definitions[which(dat$flag_definitions$flags==flag),'testStatement'], " ")[[1]][1]

  old.flags <- which(grepl(old,dat$sensor_data$flags))
  
# Generate a sequence by adding the "span" value to each old flag
  if(length(old.flags)>0) new.flags <- sort(unique(unlist(lapply(old.flags, FUN=function(x) return(seq(x,x+span,1)))))) else new.flags <- NULL

# Separate tests for rows previously flagged by another test versus those still marked NA
  dat$sensor_data[ which(!(grepl(flag,dat$sensor_data$flags)) & (rownames(dat$sensor_data) %in% new.flags) & is.na(dat$sensor_data$flags)),'flags'] <- flag
  dat$sensor_data[which(!(grepl(flag,dat$sensor_data$flags)) & (rownames(dat$sensor_data) %in% new.flags) & !is.na(dat$sensor_data$flags)),'flags'] <- paste(dat$sensor_data[which(!(grepl(flag,dat$sensor_data$flags)) & (rownames(dat$sensor_data) %in% new.flags) & !is.na(dat$sensor_data$flags)),'flags'], flag, sep=",")

  return(dat)
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
  if("PPW1302" %in% unique(dat$sensor_data$site_id)) dat$sensor_data$Timestamp[which(substr(dat$Timestamp,18,19) == "40")] <- dat$sensor_data$Timestamp[which(substr(dat$Timestamp, 18, 19) == "40")] - 160
  # PPW1321: +136 seconds
  if("PPW1321" %in% unique(dat$sensor_data$site_id)) dat$sensor_data$Timestamp[which(substr(dat$Timestamp,18,19) == "44")] <- dat$sensor_data$Timestamp[which(substr(dat$Timestamp, 18, 19) == "44")] + 136
  # PPW1328: +19 seconds
  if("PPW1328" %in% unique(dat$sensor_data$site_id)) dat$sensor_data$Timestamp[which(substr(dat$Timestamp,18,19) == "41")] <- dat$sensor_data$Timestamp[which(substr(dat$Timestamp, 18, 19) == "41")] + 19

  return(dat)
}


# Function to calculate concordance factors and return a full matrix
HOBO.plotConcordance <- function(dat) {
  concordance <- function(x, y) (2 * cor(x,y,use='pairwise.complete.obs') * sd(x,na.rm=TRUE) * sd(y,na.rm=TRUE)) / (var(x,na.rm=TRUE) + var(y,na.rm=TRUE) + ((mean(x,na.rm=TRUE) - mean(y,na.rm=TRUE))^2))
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

