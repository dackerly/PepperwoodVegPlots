# What you had

# THE BELOW LINE IS ATTEPMTING TO ONLY COUNT INDV WITH MULTIPLE BRANCHES ONCE FOR 'NUMBER', runs without error, but gives all zeros
      # For now only run ordinations on basal area ... which may be more meaninful anyway 
      # unique(floor(trees.clean[[2]]$TreeNum)) however this works
      # as does length(unique(floor(trees.clean[[2]]$TreeNum)))

      tree.cover.by.plot[[i]]$"Number"[j] <- length(trees.clean[[i]][trees.clean$Species==tree.species[j], (unique(floor("TreeNum")))])


# What I think might work

for (i in 1:length(tree.cover.by.plot)) # loops through each of the 50 data frames; could have also used length(trees.clean) here because they should both equal 50
{
	for (j in 1:dim(tree.cover.by.plot[[i]])[1]) # inner loop looks at whichever data.frame you happen to be on (starting with plot 1's data frame), and loops through as many rows as there are (which should coorespond to the number of unique species in that plot)
	{
		tree.cover.by.plot[[i]]$"Number"[j] <- length(unique(floor(trees.clean[[i]][trees.clean[[i]]$Species==tree.cover.by.plot[[i]]$Species[j], "TreeNum"]))) 	
		
		# creates a new column for the tree.cover.by.plot data frame cooresponding to the specific plot you are currently looking at. Assigns the jth row of that column (cooresponding to which unique species within that particular plot you are looking at) to be the length of the unique tree tag numbers (after rounding them all down) from trees.clean that match the following criteria: (1) you are only looking at the "TreeNum" column (2) you are looking at the rows in trees.clean whose "Species" column matches the jth unique species in the tree.cover.by.plot[[i]] data frame
		
		# Broken down: 
		# You want to pass a vector of tree tag numbers to the floor() function. It will round all of them down, leaving some duplicates where there was a main branch and then .1's, .2's, etc. That vector gets passed to unique(), which itself returns a vector that gets rid of all of the duplicates-- leaving only one representative tree tag number for all trees, regardless of how many sub-branches they have. That vector gets passed to length(), which then returns the number of tree tag numbers that are unique (after rounding down)
		# I think you understood all of this, based on how you talked about it on the phone and based on how you used the code in your comments section above.
		# What I think is confusing is what vector do you really want to pass to the length(unique(floor())) set of nested functions?
		# You want it to be a long vector of all tree tag numbers, so you know you'll have to specify which column of your data.frame to use to be "TreeNum". That's why your subset has to have a comma and then "TreeNum" -- to specify that that is the only column you want to deal with
		# The tricky part is which rows you want to deal with. You want to subset by species, so you want to include ALL rows that meet the criteria of their "Species" column being equal to the species that you are currently looking at during your looping
		# We can do that by using the conditional statement in the reference to the data frame. Because we know data frames are referenced as df[rows, columns], and we've already specified the columns (by putting "TreeNum" in that spot), we put our conditional statement where the "rows" placeholder is
		# It's messy here because the data frame is part of the list, so that's why you need the double brackets every time you reference the list elements-- trees.clean[[i]] is the data frame and then the [rows, columns] come after it
		# So overall it will have to be trees.clean[[i]][rows, columns]
		# And we know the columns, so we can fill that in -- trees.clean[[i]][rows, "TreeNum"]
		# And we know we want to pass this vector into the nested set of functions, so the almost complete form will look like length(unique(floor(trees.clean[[i]][rows, "TreeNum"])))
		# Now it is starting to take shape, but we still need a conditional statement in place of "rows" because we only want some of the rows at any given time
		
		# You have your loops set up to loop all the way through all 50 plots and each of the unique species in each of those 50 plots. The outside loop (going 1 to 50), is pretty straightforward, so let's ignore that
		# The inner loop looks at (for a given plot) each of the rows of the tree.cover.by.plot[[i]] data frame, one at a time. Each of these rows cooresponds to a unique species within the given plot, with its own basal area (and a number of individuals, after we finish with it)
		# So our conditional statement has two parts. We want to look at all of the rows of trees.clean[[i]] whose species is equal to the ONE species we are currently interested in in the tree.cover.by.plot[[i]] data frame
		# We reference the ONE species we are currently interested in by saying tree.cover.by.plot[[i]]$Species[j]
		# We reference that one particular species in trees.clean by saying trees.clean[[i]]$Species
		# We return ALL of the rows in trees.clean[[i]] that are equal to the ONE species we are currently interested in by saying trees.clean[[i]]$Species == tree.cover.by.plot[[i]]$Species[j]
		# This becomes the conditional statement that we then substitute back into the nested set of functions (line 31) as length(unique(floor(trees.clean[[i]][ trees.clean[[i]]$Species == tree.cover.by.plot[[i]]$Species[j] , "TreeNum" ])))
		# We then assign the "Number" column of the tree.cover.by.plot[[i]] data frame to be that particular length for that particular species
	}

}
