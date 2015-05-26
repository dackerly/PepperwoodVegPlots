species.list <- list(AMOCAL="AMOCAL", PSEMEN="PSEMEN", QUEDEC=c("QUEKEL", "QUEGAR", "QUEDEC", "QUEDOU"), FRACAL="FRACAL")
#By setting up your species list as a list instead of a character vector, you can add or subtract different species from within the QUEDEC group as you see fit. This way you never have to change the "total" data frame every time such that you rename your deciduous oaks. This framework lets you use the %in% function (part of match() ) to collectively aggregate all of the rows in "total" whose species is part of any of those list elements (Whether it be a list element that is a single character or a list element that is a character vector)

names(species.list) #The way I wrote the list, each group has a name also. The single species groups keep their name, and the group of QUEDEC are all now called "QUEDEC"

total <- data.frame(Plot=c(rep(1,7), rep(2, 7), rep(3, 7), rep(4, 7), rep(5, 7), rep(6, 7), rep(7, 7), rep(8, 7), rep(9, 7), rep(10, 7)), Species=unlist(species.list), Adult=rpois(70, lambda=17), Sapling=rpois(70, lambda=6)) #Simulating data for the "total" data frame

total

R <- data.frame(Species=names(species.list), R=numeric(length(species.list))) #Creates an empty data frame for storing each species' correlation value. Because we gave the whole QUEDEC group the name "QUEDEC" (even though the individual elements of that group have different names), we can use the overall name of each group for our label

R

layout(matrix(1:4, nrow=2, ncol=2)) #This function is fucking awesome. takes in a matrix corresponding to where you want each plot to go. If you just use a sequence (like I did here 1 through 4), it puts that many plots on a single quartz display in the order you specify (going down columns first). If you put the same number a couple of times in your matrix, then it allows two spaces for that graph so you could have longer or wider plots mixed with little ones (see attached image in the email)

for (i in 1:length(species.list)) #Here's the machinery for calculating R (and doing the scatterplots too, if you like)
{
	#I broke this apart so we could see what's going on. Uses the aggregate function to sum all of the adults by plot but only using a subset of data corresponding to the particular species that we are interested in (or group of species) The %in% function works by returning a vector of TRUE/FALSE that is the same length as the vector on the left of the %in% operator with the elements evaluating to TRUE whenever the element in the left vector matches to the element in the right vector. This works like "match" when the right vector is only one element long (as in the case of "AMOCAL" for instance) but it can work when the right vector has more than 1 argument (as in the case of the "QUEDEC" group) and then it will return TRUE any time ANY of those elements match to anything in the left vector
	
	Adults.by.plot <- aggregate(Adult ~ Plot, data=total[total$Species %in% species.list[[i]], ], FUN=sum)$Adult
	
	#Same thing for Sapling. See below outside of the loop for an example of what the %in% function is doing in both cases using "AMOCAL" and using "QUEDEC"
	Saplings.by.plot <- aggregate(Sapling ~ Plot, data= total[total$Species %in% species.list[[i]], ], FUN=sum)$Sapling
	
	#Assigns the appropriate row of our R data frame with the correlation between these two vectors
	R$R[i] <- cor(Adults.by.plot, Saplings.by.plot)
	
	#Adds a correlation scatter plot to our quartz display
	plot(Adults.by.plot, Saplings.by.plot, pch=19, main=names(species.list)[i], xlab="Adults", ylab="Saplings")
}

R


total$Species %in% species.list[[1]] #When the right vector is a single element ("AMOCAL")
total$Species %in% species.list[[3]] #When the rigth vector has more than 1 element (QUEDEC=c("QUEKEL", "QUEGAR", "QUEDEC", "QUEDOU"))


