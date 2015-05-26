# Intent: Creates mega.data 2013 MFO 20130802

options(stringsAsFactors=FALSE)

# In order to use the same script with multiple users
if (Sys.getenv("USER")=='meaganoldfather') setwd("/Users/meaganoldfather/Dropbox/PepperwoodVegPlots/Database/2013/Woody2013/Data/OriginalCSV/Woody/") else setwd("/Users/david/Documents/Projects/Dropbox/PepperwoodVegPlots_2013/Database/2013/Woody2013")

# or can just run one of the two below scripts: 
#file.list <- paste("/Users/david/Documents/Projects/Dropbox/PepperwoodVegPlots/Database/2013/Woody2013/Data/OriginalCSV/Woody/",dir("/Users/david/Documents/Projects/Dropbox/PepperwoodVegPlots/Database/2013/Woody2013/Data/OriginalCSV/Woody/"),sep='')
#file.list <- paste("/Users/meaganoldfather/Dropbox/PepperwoodVegPlots/Database/2013/Woody2013/Data/OriginalCSV/Woody/", dir("/Users/meaganoldfather/Dropbox/PepperwoodVegPlots/Database/2013/Woody2013/Data/OriginalCSV/Woody/") ,sep='')

file.list <-dir(paste(getwd(),"/", sep='')) # file list is a character vector of the names of woody csv file

strlen <- nchar(file.list[1]) # strlen is the length of all of the file names
plot.list <- substr(file.list,strlen-10,strlen-4) # extracts plot numbers from file name vector

mega.data<-lapply(file.list, read.csv,skip=3) #Reads all .csv files in a specified folder into one list of data.frames; theoretically it should ignore .xls files and only read files with the .csv extension; skips top tree lines and just returns data block
mega.data[[1]]
length(mega.data)
names(mega.data) <- plot.list

for (i in 1:length(mega.data)) #iterates through the whole list of data.frames corresponding to the number of plots worth of .csv files entered
{
  print(i)
  Plot<-plot.list[i] #Pulls the plot out of the plot.list variable made above
  mega.data[[i]]<-cbind(Plot=Plot, mega.data[[i]]) #Inserts a plot column before the rest of the columns in the data.frame
  colnames(mega.data[[i]])<-c("Plot", "Quad", "Type", "TreeNum", "Species", "Confidence", "Dead.Stump", "SA.Stump.Height_cm", "SA.Stump.BD_cm", "SA.Branch.Num", "DBH_cm", "X_cm", "Y_cm", "Notes") #Sets column names of data.frame to the names defined above (some .csv files had different column names (SA BD (cm) versus SA or Stump BD (cm) which made rbind challenging so I just explicitly names all of the columns))	#mega.data[[i]]<-mega.data[[i]][-1,] #Removes the first row in the data.frame (had the column labels in it-- no longer needed because they have become the actual column names, rather than the first row) #commented out as csv read is now fixed
  mega.data[[i]]<-mega.data[[i]][,1:14] 

}
