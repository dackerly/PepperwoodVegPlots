# A function to break up and label the plots by any size subplot
applySubplots <- function(treepoints, size, bbox=FALSE, plot_sub=FALSE) {
  if(!(size %in% c(10,20,25,50,100))) stop("Only subplot sizes 10, 20, 25, 50, and 100m can be calculated.\n  Try again with one of these sizes.")
  
  suppressPackageStartupMessages(library(raster))

  # Convert to a SpatialPointsDataFrame if necessary
  df=FALSE
  if(class(treepoints)!="SpatialPointsDataFrame") {
    df=TRUE
    coordinates(treepoints) <- 5:6
    crs(treepoints) <- '+proj=utm +zone=10S +datum=WGS84 +units=m +ellps=WGS84 +towgs84=0,0,0'
  }

  coords <- read.csv('Z:/Box/Desktop/Superplots/hectare_corners.csv',header=TRUE,sep=",")

  for(p in 1:nrow(coords)) {
    for(x in 0:(100/size-1)) {
      for(y in 0:(100/size-1)) {
        ext <- extent(coords[p,4]+x*size, coords[p,4]+x*size+size,
                      coords[p,5]+y*size, coords[p,5]+y*size+size)
        letter <- c("A","B","C","D","E","F","G","H","I","J")
        temp <- crop(treepoints,ext)

        if(length(temp)) {
          # Write the new subplot ID
          temp[[2]] <- paste(letter[x+1],y+1,sep="")

          # If bbox=TRUE save the bounding box coordinates
          # for each subplot to the data frame
          if(bbox) {
            temp$Min.UTM.E <- ext[1]
            temp$Max.UTM.E <- ext[2]
            temp$Min.UTM.N <- ext[3]
            temp$Max.UTM.N <- ext[4]
          }

          if(p==1 & x==0 & y==0) subtrees <- temp
            else subtrees <- rbind(subtrees,temp)
        }
      }
    }
  }

  # Clean up any trees that are right on a line!
  subtrees <- subtrees[!duplicated(subtrees$Num),]

  # Rename the "Subplot" column to reflect the size selection
  names(subtrees@data)[[2]] <- paste("Subplot_",size,"m",sep="")

  # If plot_sub=TRUE dd a column "Plot_sub" with plot and sub ID
  if(plot_sub) subtrees@data$Plot_sub <- paste(subtrees[[1]],subtrees[[2]],sep="_")

  # Return whichever object class was sent
  if(df) as.data.frame(subtrees)
    else subtrees
}


# Get the hectare survey data with trunks aggregated to basal area
getHectareTrees <- function(na.omit=TRUE, basal.area = TRUE, agg.trunks=TRUE, filter.size=TRUE, trim.data=TRUE) {
  if(agg.trunks & !basal.area) stop("Trunks can only be aggregated if basal area is also calculated.\n  If agg.trunks is set TRUE, basal.area must also be set TRUE.")
  if(filter.size & !agg.trunks) stop("Small trees can only be filtered after aggregating trunks.\n  If filter.size is set TRUE, agg.trunks must also be set TRUE.")

  suppressPackageStartupMessages(library(data.table))

  # Load the CSV of hectare surveys as a data frame
  get.trees <- read.csv('Z:/Box/Desktop/Superplots/hectare_trees_all.csv',header=TRUE,sep=",")

  # Dirty fix to remove data with missing coordinates, species, or DBH
  if(na.omit) get.trees <- get.trees[!(is.na(get.trees$DBH.cm) | is.na(get.trees$UTM.N) | is.na(get.trees$Species)),]

  # Calculate basal area
  if(basal.area) get.trees$Basal.Area <- (get.trees$DBH.cm/2)^2*pi

  # Aggregate multiple trunks to single individuals
  if(basal.area & agg.trunks) {
    get.trees$Num <- trunc(get.trees$Num)
    get.trees <- data.table(get.trees)
    get.trees[,Basal.Area:=sum(Basal.Area),by="Num"]
    get.trees <- data.frame(get.trees)
    # get rid of duplications created by truncating
    get.trees <- get.trees[!duplicated(get.trees$Num,incomparables=NA),]
  }

  # Get rid of trees less than 20cm DBH
  if(basal.area & agg.trunks & filter.size) get.trees <- get.trees[get.trees$Basal.Area>=(100*pi),]

  # Renumber data frame rows
  rownames(get.trees) <- NULL
  
  # Send the requested columns, trimmed or not
  if(!trim.data) get.trees
    else if(trim.data & basal.area) get.trees[,c("Plot","Subplot","Num","Species","UTM.E","UTM.N","Year","Basal.Area")]
      else if(trim.data & !basal.area) get.trees[,c("Plot","Subplot","Num","Species","UTM.E","UTM.N","Year","DBH.cm")]
}