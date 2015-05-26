#setwd("/Users/mikoontz/Desktop/Pepperwood")
species.list <- list(PSEMEN="PSEMEN", QUEAGR="QUEAGR", UMBCAL="UMBCAL", AESCAL="AESCAL", HETARB="HETARB", ARBMEN="ARBMEN", ARCMAN="ARCMAN", AMOCAL="AMOCAL", FRACAL="FRACAL", BACPIL="BACPIL", NOTDEN="NOTDEN", ADEFAS="ADEFAS", QUEWIS="QUEWIS", QUEBER="QUEBER" ,QUEDEC=c("QUEDEC", "QUEGAR", "QUEDOU", "QUELOB", "QUEAGKE", "QUEXXX", "QUEKEL"), CEOCUN="CEOCUN", TORCAL="TORCAL", UNKN27="UNKN27", UNK28="UNK28", UNKN30="UNKN30", UNKN47="UNKN47", UNK21="UNK21", UNKN42="UNKN42")

total <- read.csv("/Desktopn/AllInfo.csv")
total <- total[,-1]
head(total)
tail(total)


R <- data.frame(Species=names(species.list), R.Adults.by.JS=numeric(length(species.list)), R.BA.by.JS=numeric(length(species.list))) #Creates an empty data frame for storing each species' correlation value. Because we gave the whole QUEDEC group the name "QUEDEC" (even though the individual elements of that group have different names), we can use the overall name of each group for our label

R


layout(matrix(1:12, nrow=3))

for (i in 1:length(species.list)) #Here's the machinery for calculating R (and doing the scatterplots too, if you like)
{
	#I broke this apart so we could see what's going on. Uses the aggregate function to sum all of the adults by plot but only using a subset of data corresponding to the particular species that we are interested in (or group of species) The %in% function works by returning a vector of TRUE/FALSE that is the same length as the vector on the left of the %in% operator with the elements evaluating to TRUE whenever the element in the left vector matches to the element in the right vector. This works like "match" when the right vector is only one element long (as in the case of "AMOCAL" for instance) but it can work when the right vector has more than 1 argument (as in the case of the "QUEDEC" group) and then it will return TRUE any time ANY of those elements match to anything in the left vector
	
	Adults.by.plot <- aggregate(Adult ~ Plot, data=total[total$Species %in% species.list[[i]], ], FUN=sum)$Adult
	
	BA.by.plot <- aggregate(BA ~ Plot, data=total[total$Species %in% species.list[[i]], ], FUN=sum)$BA
	
	#Same thing for Sapling. See below outside of the loop for an example of what the %in% function is doing in both cases using "AMOCAL" and using "QUEDEC"
	JS.by.plot <- aggregate(JS ~ Plot, data= total[total$Species %in% species.list[[i]], ], FUN=sum)$JS
	
	#Assigns the appropriate row of our R data frame with the correlation between these two vectors
	R$R.Adults.by.JS[i] <- cor(Adults.by.plot, JS.by.plot)
	R$R.BA.by.JS[i] <- cor(BA.by.plot, JS.by.plot)
	
	
	
	# #Adds a correlation scatter plot to our quartz display using Adults by JS
	# if (!is.na(R$R.Adults.by.JS[i]))
	# {plot(Adults.by.plot, JS.by.plot, pch=19, main=names(species.list)[i], xlab="Adults", ylab="Juveniles + Seedlings")
	# legend("top", box.lwd=0, legend=paste("r= ", round(R$R.Adults.by.JS[i], 2), sep=""))}
	
	#Adds a correlation scatter plot to our quartz display using BA by JS
	if (!is.na(R$R.BA.by.JS[i]))
	{plot(BA.by.plot, JS.by.plot, pch=19, main=names(species.list)[i], xlab=expression(paste("Basal Area (", cm^2, ")", sep="")),  ylab="Juveniles + Seedlings")
	legend("top", box.lwd=0, legend=paste("r= ", round(R$R.BA.by.JS[i], 2), sep=""))}
}


R


Adults.by.plot <- aggregate(Adult ~ Plot, data=total[total$Species %in% species.list[[i]], ], FUN=sum)$Adult
Saplings.by.plot <- aggregate(Sapling ~ Plot, data= total[total$Species %in% species.list[[i]], ], FUN=sum)$Sapling

