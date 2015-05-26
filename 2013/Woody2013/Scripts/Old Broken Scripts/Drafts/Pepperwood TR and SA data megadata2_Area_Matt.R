#March 9, 2013
#Intention of the program is to take a folder's worth of tree and sapling .csv files from Ackerly's Pepperwood demography study and calculate total cover (sum of all DBH for trees; sum of branch #'s and basal diameters for saplings) for each species for each plot
#MFO, MJK

#NOTES: 

#Be sure to replace the file path explicitly written in this code to that of the folder containing the relevant .csv files
#NA's will be returned for cover calculations if there are any blanks in the cells that should have measurements
#Each list element is given the name of the plot that the data.frame it contains refers to; therefore any plot that has no sapling or tree cover will have a list entry that still will be identified as its particular plot, but the data.frame within will only have column names (and no data). This should allow rbind later, if that is desired

#Five main variables are created:

#mega.data: a list containing each plot's full data sheet (including all non-trivial columns). One list element represents one plot
#trees: a list containing each plot's full census of trees measured. One list element represents one plot
#saplings: a list containing each plot's full census of saplings measured. One list element represents one plot
#tree.cover.by.plot: a list containing unique tree species in each plot and their corresponding sum of DBH
#sapling.cover.by.plot: a list containing unique sapling species in each plot and their corresponding sum of BD and number of branches

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

options(stringsAsFactors=FALSE) #Setting this option makes sure that character strings aren't treated as factors. Essentially, this makes it so we can more easily force the character strings of the DBH measurements into actual numbers that we can sum
#mega.data<-lapply(paste("/Users/meaganoldfather/Dropbox/PepperwoodVegPlots_2013 (2)/Veg Survey Data/Data Processing/Woody Cover CSV Files/Test/",list.files("/Users/meaganoldfather/Dropbox/PepperwoodVegPlots_2013 (2)/Veg Survey Data/Data Processing/Woody Veg CSV Files/Test/", pattern="*.csv"), sep=""), read.csv) #Reads all .csv files in a specified folder into one list of data.frames; theoretically it should ignore .xls files and only read files with the .csv extension
#mega.data<-lapply(paste("/Users/meaganoldfather/Dropbox/PepperwoodVegPlots_2013 (2)/Veg Survey Data/Field data sheet_copies/Woody Cover csv/",list.files("/Users/meaganoldfather/Dropbox/PepperwoodVegPlots_2013 (2)/Veg Survey Data/Data Processing/Woody Veg CSV Files/", pattern="*.csv"), sep=""), read.csv) #Reads all .csv files in a specified folder into one list of data.frames; theoretically it should ignore .xls files and only read files with the .csv extension
mega.data<-lapply(paste("/Users/mattb/Dropbox/Berkeley Projects/PepperwoodVegPlots_2013/Veg Survey Data/Field data sheet_copies/Woody Cover csv/",list.files("/Users/mattb/Dropbox/Berkeley Projects/PepperwoodVegPlots_2013/Veg Survey Data/Field data sheet_copies/Woody Cover csv", pattern="*.csv"), sep=""), read.csv) #Reads all .csv files in a specified folder into one list of data.frames; theoretically it should ignore .xls files and only read files with the .csv extension
trees<-vector("list", length(mega.data)) #creates 4 lists for each kind of data.frame we want; number of elements in list is equivalent to number of files read into R
saplings<-vector("list", length(mega.data))
tree.cover.by.plot<-vector("list", length(mega.data))
sapling.cover.by.plot<-vector("list", length(mega.data))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

for (i in 1:length(mega.data)) #iterates through the whole list of data.frames corresponding to the number of plots worth of .csv files entered
{
	Plot<-colnames(mega.data[[i]])[2] #Pulls the plot out of the B1 cell on the .csv file
 # need to add in a line to also pull out the date from the plot csv file and add as column
 # Date <- colnames)(mega.data[[i]][11])  Pulls out date from the B11 cell and makes a date column
  names(mega.data)[i]<-Plot
	mega.data[[i]]<-mega.data[[i]][-(1:2),] #removes first two rows (the date, initials, notes, etc. rows)
	# mega.data[[i]]<-mega.data[[i]][,apply(apply(mega.data[[i]], 2, is.na), 2, sum)==0 ] #only keeps the columns in the data.frame that aren't full of NA's (there seem to be a bunch after the "Notes" column)
	# names<-mega.data[[i]][1,] #These lines used to take the column names from the computer file, but some column names are slightly different in different spreadsheets (apparently that matters later)
	# names<-cbind("Plot", names)
	# names<-lapply(names, as.character) #Recasts the (now first) row as a vector of characters to rename the columns of the data.frame as something useful (rather than X.6, etc.)
	mega.data[[i]]<-cbind(Plot=Plot, mega.data[[i]]) #Inserts a plot column before the rest of the columns in the data.frame
	colnames(mega.data[[i]])<-c("Plot", "Quad", "Type", "Tree Tag No.", "Species", "Confidence", "Dead/ Stump", "SA or Stump Height (cm)", "SA or Stump BD (cm)", "SA Branch #", "DBH (cm)", "X (cm)", "Y (cm)", "Notes") #Sets column names of data.frame to the names defined above (some .csv files had different column names (SA BD (cm) versus SA or Stump BD (cm) which made rbind challenging so I just explicitly names all of the columns))
	mega.data[[i]]<-mega.data[[i]][-1,] #Removes the first row in the data.frame (had the column labels in it-- no longer needed because they have become the actual column names, rather than the first row)
	mega.data[[i]]<-mega.data[[i]][,1:14]

	trees[[i]]<-cbind(mega.data[[i]][mega.data[[i]]$Type=='TR'&mega.data[[i]]$"Dead/ Stump"=='', 1:5], DBH=mega.data[[i]][mega.data[[i]]$Type=='TR'&mega.data[[i]]$"Dead/ Stump"=='', 11]) #Combines the desired columns into a new data.frame for trees; first part of cbind returns columns 1 through 5 and every row such that the "Type" column is equal to 'TR' and the "Dead/ Stump" column is NOT equal to 'D'; second part of cbind returns column 11 (the DBH column) and all rows such that the "Type" column is equal to 'TR' and the "Dead/ Stump" column is NOT equal to 'D'. Also names the DBH column as 'DBH'
  trees[[i]]<-trees[[i]][order(trees[[i]]$Species),] #alphabetizes according to Species column
	names(trees)[i]<-Plot
	
	tree.species<-unique(trees[[i]]$Species) #tree.species is defined as a vector of the unique values of the species
	tree.cover.by.plot[[i]]<-as.data.frame(matrix(NA, nrow=length(tree.species), ncol=3))

	names(tree.cover.by.plot[[i]])<-c("Plot", "Species", "Sum Area (cm^2)")
	names(tree.cover.by.plot)[i]<-Plot
	
	if(length(tree.species)>0)
	{
		tree.cover.by.plot[[i]][,1]<-Plot
		tree.cover.by.plot[[i]][,2]<-tree.species
		
		for(j in 1:length(tree.species))
		{	
			
			tree.cover.by.plot[[i]]$"Sum Area (cm^2)"[j]<-sum(pi*(1/4)*(as.numeric(trees[[i]][trees[[i]]$Species==tree.species[j],6]))^2)
		}
	}
	saplings[[i]]<-cbind(mega.data[[i]][mega.data[[i]]$Type=='SA'&mega.data[[i]]$"Dead/ Stump"=='', 1:5], mega.data[[i]][mega.data[[i]]$Type=='SA'&mega.data[[i]]$"Dead/ Stump"=='', 9:10]) #Same idea for the saplings; First part of cbind returns all rows such that "Type" column equals 'SA' AND such that the "Dead/ Stump" column is NOT 'D' but only the first 4 columns; second part of cbind returns all rows such that "Type" column equals 'SA' but only the 8th and 9th columns
	saplings[[i]]<-saplings[[i]][order(saplings[[i]]$Species),]
	names(saplings)[i]<-Plot
	
	
	sapling.species<-unique(saplings[[i]]$Species)
	#if(length(sapling.species)==0) {sapling.cover.by.plot[[i]]<-data.frame(Plot=Plot, Species=NA); sapling.cover.by.plot[[i]]<-cbind(sapling.cover.by.plot[[i]], "Sum SA BD (cm)"=NA, "Sum SA Branches"=NA)}
	
	sapling.cover.by.plot[[i]]<-as.data.frame(matrix(NA, nrow=length(sapling.species), ncol=3))

	names(sapling.cover.by.plot[[i]])<-c("Plot", "Species", "Sum Area (cm^2)")
	names(sapling.cover.by.plot)[i]<-Plot
	
	if(length(sapling.species)>0)
	{
		sapling.cover.by.plot[[i]][,1]<-Plot
		sapling.cover.by.plot[[i]][,2]<-sapling.species
		for(j in 1:length(sapling.species))
		{	
		  sapling.cover.by.plot[[i]]$"Sum Area (cm^2)"[j]<-sum(as.numeric(saplings[[i]][saplings[[i]]$Species==sapling.species[j],7])*pi/4*(as.numeric(saplings[[i]][saplings[[i]]$Species==sapling.species[j],6]))^2)
			#sapling.cover.by.plot[[i]]$"Sum SA BD (cm)"[j]<-sum(as.numeric(saplings[[i]][saplings[[i]]$Species==sapling.species[j],6])) #here we add two columns: one for BD and one for number of branches
			#sapling.cover.by.plot[[i]]$"Sum SA Branches"[j]<-sum(as.numeric(saplings[[i]][saplings[[i]]$Species==sapling.species[j],7]))
		}
	}
	
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

master.trees<-do.call(rbind, trees) #This creates a single data.frame with all of the different trees represented for all the different plots
master.trees
master.saplings<-do.call(rbind, saplings) #Same for saplings
master.saplings
#write.csv(master.trees, file="/Users/meaganoldfather/Dropbox/PepperwoodVegPlots_2013 (2)/Veg Survey Data/Data Processing/Tree_Master.csv")
#write.csv(master.saplings, file="/Users/meaganoldfather/Dropbox/PepperwoodVegPlots_2013 (2)/Veg Survey Data/Data Processing/Sapling_Master.csv")
tree.cover.by.plot #Just calls the list with tree cover per plot (one plot per list element); you can use the same do.call(rbind, tree.cover.by.plot) kind of function as used above to put all this info into one data frame if needed; might be easier to read as a list?
sapling.cover.by.plot
tree.cover<-do.call(rbind, tree.cover.by.plot)
sapling.cover<-do.call(rbind, sapling.cover.by.plot)
#write.csv(tree.cover, file="/Users/meaganoldfather/Dropbox/PepperwoodVegPlots_2013 (2)/Veg Survey Data/Data Processing/Tree_Area.csv") # Meagan's machine
write.csv(tree.cover, file="/Users/mattb/Dropbox/Berkeley Projects/PepperwoodVegPlots_2013/Veg Survey Data/Data Processing/Tree_Area.csv") # Matt's machine
#write.csv(sapling.cover, file="/Users/meaganoldfather/Dropbox/PepperwoodVegPlots_2013 (2)/Veg Survey Data/Data Processing/Sapling_Area.csv")

total.cover.by.plot<-vector("list", length=length(tree.cover.by.plot))
names(total.cover.by.plot)<-names(tree.cover.by.plot)

for (i in 1: length(tree.cover.by.plot))
{  
  Plot.id<-names(tree.cover.by.plot)[[i]]
  species.in.plot<-unique(c(tree.cover.by.plot[[i]]$Species, sapling.cover.by.plot[[i]]$Species))	
  total.cover.by.plot[[i]]<-data.frame(Plot=character(0), Species=character(0), Total.cover=numeric(0))
  
  
  if(length(species.in.plot)>0)
  {
    # names(total.cover.by.plot[[i]])<-c("Plot", "Species", "Total cover (cm^2)")
    for (j in 1:length(species.in.plot))
    {
      total.cover.by.plot[[i]][j, 1]<-Plot.id
      total.cover.by.plot[[i]][j, 2]<-species.in.plot[j]
      total.cover.by.plot[[i]][j, 3]<-sum(tree.cover.by.plot[[i]][tree.cover.by.plot[[i]]$Species==species.in.plot[j],3], sapling.cover.by.plot[[i]][sapling.cover.by.plot[[i]]$Species==species.in.plot[j],3])
    }
    
    
  }
  colnames(total.cover.by.plot[[i]])<-c("Plot", "Species", "Total cover (cm^2)")
}

total.cover.by.plot

total.cover<-do.call(rbind, total.cover.by.plot)
total.cover

#write.csv(total.cover, file="/Users/meaganoldfather/Dropbox/PepperwoodVegPlots_2013 (2)/Veg Survey Data/Data Processing/Total_Area.csv") # Meagan's machine
#write.csv(total.cover, file="/Users/mattb/Dropbox/Berkeley Projects/PepperwoodVegPlots_2013/Veg Survey Data/Data Processing/Total_Area.csv") # Matt's machine

# CAN MAKE TOTAL COVER ALSO WITH BELOW CODE; CALLED 'all.area'

all.area<-rbind(tree.cover, sapling.cover)
all.area

##  Make a csv file with the percent cover QUEDEC for just trees  
#tree<-read.csv("/Users/meaganoldfather/Dropbox/PepperwoodVegPlots_2013 (2)/Veg Survey Data/Data Processing/Tree_Area.csv") # Meagan's machine
tree<-read.csv("/Users/mattb/Dropbox/Berkeley Projects/PepperwoodVegPlots_2013/Veg Survey Data/Data Processing/Tree_Area.csv") # Matt's machine


tree
tree[,c(2,3,4)]
attach(tree)

allspecies<-sort(unique(tree$Species))
allspecies

FT<-c(rep("ew", 9),rep("do",4), "ew")
ft.table<-data.frame(FT, allspecies)

sf2<-match(tree$Species, ft.table$allspecies)
tree$FT<- ft.table$FT[sf2]
names(tree)[4]="basalarea"
ftbasalarea<-data.frame(tapply(tree$basalarea, list(tree$Plot, tree$FT), sum))
ftbasalarea

ftbasalarea[is.na(ftbasalarea)]=0
ftbasalarea
ftbasalarea$total<-apply(ftbasalarea,1,sum)
ftbasalarea

ftbasalarea$doperc<-ftbasalarea[,1]/ftbasalarea[,3]
ftbasalarea
#write.csv<-(ftbasalarea, file="/Users/meaganoldfather/Dropbox/PepperwoodVegPlots_2013 (2)/Veg Survey Data/Data Processing/ftbasalarea.csv")
write.csv(ftbasalarea, file="/Users/mattb/Dropbox/Berkeley Projects/PepperwoodVegPlots_2013/Veg Survey Data/Data Processing/ftbasalarea.csv") #Matt's machine


