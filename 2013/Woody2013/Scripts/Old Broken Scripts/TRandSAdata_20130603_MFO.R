#March 9, 2013
#Intention of the program is to take a folder's worth of tree and sapling .csv files from Pepperwood study and calculate total cover (sum of all DBH for trees; sum of branch #'s and basal diameters for saplings) for each species for each plot

# packages:
library(picante)

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

# In order to use the same script with multiple users
# DA I fixed it so it works for me - some parentheses and the exact path description. I think it will work for you now Meagan
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

#creates 4 lists for each kind of data.frame we want; number of elements in list is equivalent to number of files read into R
trees<-vector("list", length(mega.data)) 
names(trees) <- plot.list
saplings<-vector("list", length(mega.data))
names(saplings) <- plot.list
tree.cover.by.plot<-vector("list", length(mega.data))
names(tree.cover.by.plot) <- plot.list
sapling.cover.by.plot<-vector("list", length(mega.data))
names(sapling.cover.by.plot) <- plot.list

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
for (i in 1:length(mega.data)) #iterates through the whole list of data.frames corresponding to the number of plots worth of .csv files entered
{
    print(i)
	Plot<-plot.list[i] #Pulls the plot out of the plot.list variable made above
	mega.data[[i]]<-cbind(Plot=Plot, mega.data[[i]]) #Inserts a plot column before the rest of the columns in the data.frame
	colnames(mega.data[[i]])<-c("Plot", "Quad", "Type", "TreeNum", "Species", "Confidence", "Dead.Stump", "SA.Stump.Height_cm", "SA.Stump.BD_cm", "SA.Branch.Num", "DBH_cm", "X_cm", "Y_cm", "Notes") #Sets column names of data.frame to the names defined above (some .csv files had different column names (SA BD (cm) versus SA or Stump BD (cm) which made rbind challenging so I just explicitly names all of the columns))	#mega.data[[i]]<-mega.data[[i]][-1,] #Removes the first row in the data.frame (had the column labels in it-- no longer needed because they have become the actual column names, rather than the first row) #commented out as csv read is now fixed
	mega.data[[i]]<-mega.data[[i]][,1:14] 
  
	
  trees[[i]]<-mega.data[[i]][intersect(grep('TR',mega.data[[i]]$Type),(which(is.na(mega.data[[i]]$Dead.Stump)|(mega.data[[i]]$Dead.Stump=='')))), c(1:5,11)] # modified version of command below because 1302 had TR listed as ' TR' so wasn't matching with first condition
  head(trees[[i]])
    
  trees[[i]]<-trees[[i]][order(trees[[i]]$Species),] #alphabetizes according to Species column

	tree.species<-unique(trees[[i]]$Species) #tree.species is defined as a vector of the unique values of the species
	
  tree.cover.by.plot[[i]]<-as.data.frame(matrix(NA, nrow=length(tree.species), ncol=4))

	names(tree.cover.by.plot[[i]])<-c("Plot", "Species", "Ntrees","Sum.Area_cm2")
	
	if(length(tree.species)>0) 
	{
		tree.cover.by.plot[[i]][,1]<-Plot # First column is the Plot
		tree.cover.by.plot[[i]][,2]<-tree.species # Second column is the Tree Species
		
		for(j in 1:length(tree.species))
		{	
			tree.cover.by.plot[[i]]$Ntrees[j] <- length(which(trees[[i]]$Species==tree.species[j]))
			tree.cover.by.plot[[i]]$Sum.Area_cm2[j]<-sum(pi*(1/4)*(as.numeric(trees[[i]][trees[[i]]$Species==tree.species[j],6]))^2) # area calculated with diameter
		}

#### For Saplings

      saplings[[i]]<-mega.data[[i]][intersect(grep('SA',mega.data[[i]]$Type),(which(is.na(mega.data[[i]]$Dead.Stump)|(mega.data[[i]]$Dead.Stump=='')))), c(1:5,8:10)]
      saplings[[i]]<-saplings[[i]][order(saplings[[i]]$Species),]
      
      sapling.species<-unique(saplings[[i]]$Species)
      
      sapling.cover.by.plot[[i]]<-as.data.frame(matrix(NA, nrow=length(sapling.species), ncol=3))
      names(sapling.cover.by.plot[[i]])<-c("Plot", "Species", "Sum.Area_cm2")
     
      
      if(length(sapling.species)>0)
      {
        Plot<-plot.list[i]
        sapling.cover.by.plot[[i]][,1]<-Plot
        sapling.cover.by.plot[[i]][,2]<-sapling.species
        for(j in 1:length(sapling.species))
        {	
          sapling.cover.by.plot[[i]]$"Sum.Area_cm2"[j]<-sum(as.numeric(saplings[[i]][saplings[[i]]$Species==sapling.species[j],7])*pi/4*(as.numeric(saplings[[i]][saplings[[i]]$Species==sapling.species[j],8]))^2)
          sapling.cover.by.plot[[i]]$"Sum.SA.BD_cm)"[j]<-sum(as.numeric(saplings[[i]][saplings[[i]]$Species==sapling.species[j],7])) #here we add two columns: one for BD and one for number of branches
          sapling.cover.by.plot[[i]]$"Sum.SA.Branches"[j]<-sum(as.numeric(saplings[[i]][saplings[[i]]$Species==sapling.species[j],8]))
        }
      }
    }
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

master.trees<-do.call(rbind, trees) #This creates a single data.frame with all of the different trees represented for all the different plots
master.trees$BA_cm2 <- pi*(1/4)*master.trees$DBH_cm^2 # calculated area with diameter 
dim(master.trees)
head(master.trees)
head(tree.cover.by.plot) # list of tree cover per plot (one plot per list element) can use the same do.call(rbind, tree.cover.by.plot) 
#function as used above to put all this info into one data frame if needed; might be easier to read as a list?

master.saplings<-do.call(rbind, saplings) #Same for saplings; but area was already calculated within the loop
head(master.saplings)
head(sapling.cover.by.plot) 

#write.csv(master.trees, file="/Users/meaganoldfather/Dropbox/ # needs correct pathway")
#write.csv(master.saplings, file="/Users/meaganoldfather/Dropbox/# needs correct pathway")

tree.cover<-do.call(rbind, tree.cover.by.plot)
dim(tree.cover)
head(tree.cover)
unique(tree.cover$Species)  # gives the unique species back that make up the tree cover
which(is.na(tree.cover$Sum.Area_cm2))

sapling.cover<-do.call(rbind, sapling.cover.by.plot)
head(sapling.cover)
which(is.na(sapling.cover$Sum.Area_cm2)) # returns the index numbers of the offending saplings! 
unique(sapling.cover$Species) # gives species that make up sapling cover for all

################
# Cleaning the Data

check<-subset(master.saplings, subset=(master.saplings$Plot=='PPW1330'&master.saplings$Species=="PSEMEN"))
species.check<-subset(master.saplings, subset=(master.saplings$Species=='QUEARG'))
species.check<-subset(master.trees, subset=(master.trees$Species=='QUODOU'))

species.check

tree.cover.by.plot
sapling.cover.by.plot 

# to get rid of -999's
mega.data.combo<-do.call(rbind, mega.data)
head(mega.data)
head(mega.data.combo)

check.missing<- subset(mega.data.combo, subset=(mega.data.combo$Quad=="-999"|
                                                mega.data.combo$TreeNum=="-999"|
                                                mega.data.combo$Species=="-999"|
                                                mega.data.combo$SA.Stump.Height_cm=="-999"|
                                                mega.data.combo$SA.Stump.BD_cm=="-999"|
                                                mega.data.combo$SA.Branch.Num=="-999"|
                                                mega.data.combo$DBH_cm=="-999"))  

check.missing # Could we remove 'check missing' from the analyses?
# Plot Quad Type TreeNum Species Confidence Dead.Stump SA.Stump.Height_cm SA.Stump.BD_cm SA.Branch.Num  DBH_cm X_cm Y_cm
#PPW1320.196 PPW1320   B1   SA    -999  UMBCAL          1                           223           1.56             1      NA  -43  110
#PPW1320.260 PPW1320   C4   TR    4711  FRACAL          1                            NA           <NA>            NA -999.00 -243   15
#PPW1327.74  PPW1327   B3   TR    4397  PSEMEN          1                            NA           <NA>            NA -999.00  -39  219
#PPW1327.204 PPW1327   D1   SA    -999  BACPIL          1                           212           1.63             1      NA -152 -133
#PPW1330.73  PPW1330   A3   TR    -999  UMBCAL          1                            NA           <NA>            NA    7.50  193   11
#PPW1330.373 PPW1330   C2   TR    -999  UMBCAL          1                            NA           <NA>            NA    1.69  225   38
#PPW1330.524 PPW1330   D4   TS  4141.6  HETARB          1                          -999           1.12             9      NA  -78  500

# '-999' code for missing left in Treenum column, but changed to '' if in DBH or SA Height, BD, Branch.Num; do the individuals with NA's
# need to be removed before tree. master is formed? (only three individuals:4711, 4397,4141.6 )
############## COMMUNITY ORDINATION ##############
tba <- tapply(tree.cover$Sum.Area_cm2,tree.cover$Species,sum)
tba <- tba[order(tba)]
tba  # Why are some species 'NA'? There is an issue in the data ITSELF --> 
#the data has now been cleaned so that there are no longer any NA's for any TR individual;

tree.cover.mat <- sample2matrix(tree.cover[,c(1,4,2)])
tree.number.mat <- sample2matrix(tree.cover[,c(1,3,2)])
head(tree.cover.mat)
head(tree.number.mat)

ord <- metaMDS(tree.cover.mat)
summary(ord)
plot(ord,type='t')

attributes(ord)

ordN <- metaMDS(tree.number.mat)
plot(ordN,type='t')

# Ordination for saplings

sap.cover.mat <- sample2matrix(sapling.cover[,c(1,4,2)])
sap.number.mat <- sample2matrix(sapling.cover[,c(1,3,2)])
head(sap.cover.mat)
head(sap.number.mat)

ord <- metaMDS(sap.cover.mat)
summary(ord)
plot(ord,type='t')

attributes(ord)

#ordN <- metaMDS(tree.number.mat)
#plot(ordN,type='t')


################ HISTOGRAMS OF SIZE DISTRIBUTION ###################
################ HISTOGRAMS OF DENSITY PER PLOT ###################
# Plot size distribution histograms, and plot density histograms
species.list <- names(tree.cover.mat)
par(mfrow=c(2,1))
par(ask=TRUE)
hist(master.trees$DBH)

i=1
for (i in 1:length(species.list)) {
    hist(master.trees$DBH[master.trees$Species==species.list[i]],
         xlab='diameter (cm)',
         main=paste(species.list[i],'size distribution'))
    nzero <- length(which(tree.number.mat[,i]==0))
    hist(tree.number.mat[tree.number.mat[,i]!=0,i],xlab='density',
         main=paste(species.list[i],'density:',nzero,'zeros'))
}

### compare frequency of tree in the plots to frequency of salpings
### per species

species.list <- names(sap.cover.mat)
par(mfrow=c(2,1))
par(ask=TRUE)

master.saplings$SA.Stump.BD_cm<- as.numeric(master.saplings$SA.Stump.BD_cm)
master.saplings$BD.Total<- (master.saplings$SA.Stump.BD_cm)*(master.saplings$SA.Branch.Num)

hist(master.saplings$BD.Total)

i=1
for (i in 1:length(species.list)) {
  hist(master.saplings$BD.Total[master.saplings$Species==species.list[i]],
       xlab='diameter (cm)',
       main=paste(species.list[i],'size distribution'))
  nzero <- length(which(sap.number.mat[,i]==0))
  hist(sap.number.mat[sap.number.mat[,i]!=0,i],xlab='density',
       main=paste(species.list[i],'density:',nzero,'zeros'))
}


###################################################
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
#write.csv(ftbasalarea, file="/Users/mattb/Dropbox/Berkeley Projects/PepperwoodVegPlots_2013/Veg Survey Data/Data Processing/ftbasalarea.csv") #Matt's machine

######################################





