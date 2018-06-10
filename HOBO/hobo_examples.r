##########################################################################
# this code is from some delightful human on the internet who figured out 
# how to source from GitHub; run this function in order to source in the 
# script with all the Pepperwood functions (vegplots, hectares, and HOBOs)
if(!require(RCurl)){install.packages("RCurl");library(RCurl)}
url <- "https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/source_files.r"
eval(parse(text = getURL(url, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
##########################################################################


# The code above will source the current HOBO functions from Github
# This sources them locally, whihc is useful when actively working on them
source('C:/GitHub/PepperwoodVegPlots/HOBO/hobo_functions.r')


# First, compile the data into a long format dataframe from the raw HOBO files
hobos.temp <- HOBO.getFromRaw(plots=c(1301:1350), sensorType="AirTemp", location="C:/GitHub/PepperwoodVegPlots/HOBO")
# For HOBO data, the sensorType can be either "AirTemp" or "RH"
# Location can be "Github", which will access the files remotely,
# or a local directory that has the same file structure as the Github repo

# HOBO.getFromRaw() returns a list of dataframes as a relational database
str(hobos.temp)

# The function is pretty fast now, but it's still nice to have a quick backup
hobos.back <- hobos.temp
hobos.back -> hobos.temp


# Incident reports with dataQualityLevel=REPAIRED can be directly fixed (no flag is applied)
hobos.temp <- HOBO.repairTimestamps(hobos.temp)


# Observations can be flagged from the flag descriptions table
hobos.temp <- HOBO.flagData(hobos.temp, c("VB","VE","P4","S"))
hobos.temp <- HOBO.flagData(hobos.temp, c("VBx","VEx"))

# Or they can be flagged form the incident reports table
hobos.temp <- HOBO.flagIncidents(hobos.temp, "BAD")
hobos.temp <- HOBO.flagIncidents(hobos.temp, "SUSPECT")

# Note that neither of these flagging functions currently appends the
# dataQualityLevel info and they also don't add to the tests run table.
# There's also still some issues with the U and NA flags to clean up
unique(hobos.temp$sensor_data$flags)


# Like with sensorQC, it's pretty easy to quickly look at flagged data
hobos.temp$sensor_data[!(grepl("VE",hobos.temp$sensor_data$flags)) & grepl("P4",hobos.temp$sensor_data$flags),3:6]


# Plotting is easy enough, too, but dataQualityLevel is still needed for easy filtering
hist(hobos.temp$sensor_data[which(hobos.temp$sensor_data$site_id=="PPW1328"),'observedValue'])
plot(hobos.temp$sensor_data[which(hobos.temp$sensor_data$site_id=="PPW1328"),'Timestamp'], hobos.temp$sensor_data[which(hobos.temp$sensor_data$site_id=="PPW1328"),'observedValue'], type="l")

# Even after runing all these flags,there still some work to be done on QC
plot(hobos.temp$sensor_data[which(hobos.temp$sensor_data$site_id=="PPW1328" & is.na(hobos.temp$sensor_data$flags)),'Timestamp'], hobos.temp$sensor_data[which(hobos.temp$sensor_data$site_id=="PPW1328" & is.na(hobos.temp$sensor_data$flags)),'observedValue'], type="l")


# Timestamps that are out of 00 and 30 sync can be manually synced
hobos <- HOBO.syncTimestamps(hobos)


# Running the whole workflow is currently very quickly
# Adding more incidents and flags will make it take longer, though
system.time({
  hobos <- HOBO.getFromRaw(plots=c(1301:1350), sensor="AirTemp", location="C:/GitHub/PepperwoodVegPlots/HOBO")

  hobos <- HOBO.repairTimestamps(hobos)

  hobos <- HOBO.flagData(hobos, c("VB","VE","P4","S"))
  hobos <- HOBO.flagData(hobos, c("VBx","VEx"))

  hobos <- HOBO.flagIncidents(hobos, "BAD")
  hobos <- HOBO.flagIncidents(hobos, "SUSPECT")

  hobos <- HOBO.syncTimestamps(hobos)
})