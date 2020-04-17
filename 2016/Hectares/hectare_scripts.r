# A function to break up and label the plots by any size subplot 
applySubplots <- function(treepoints, size, centroid=FALSE, bbox=FALSE, fill=FALSE, plot_sub=FALSE) { 
  if(!(size %in% c(10,20,25,50,100))) stop("Only subplot sizes 10, 20, 25, 50, and 100m can be calculated.\n  Try again with one of these sizes.") 
   
  suppressPackageStartupMessages(library(raster)) 
 
  # Convert to a SpatialPointsDataFrame if necessary 
  df=FALSE 
  if(class(treepoints)!="SpatialPointsDataFrame") { 
    df=TRUE 
    coordinates(treepoints) <- 5:6 
    crs(treepoints) <- '+proj=utm +zone=10S +datum=WGS84 +units=m +ellps=WGS84 +towgs84=0,0,0' 
  } 
 
  coords <- read.csv('https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/2016/Hectares/hectare_corners.csv',header=TRUE,sep=",",stringsAsFactors=FALSE) 

  for(p in 1:nrow(coords)) { 
    for(x in 0:(100/size-1)) { 
      for(y in 0:(100/size-1)) { 
        ext <- extent(coords[p,4]+x*size, coords[p,4]+x*size+size, 
                      coords[p,5]+y*size, coords[p,5]+y*size+size) 
        letter <- c("Q","R","S","T","U","V","W","X","Y","Z") 
        temp <- crop(treepoints,ext) 

        #If fill=TRUE and there's not trees in the subplot, make a dummy
        if(fill & is.null(temp)) {
          temp <- treepoints@data[0,]
          temp[nrow(temp)+1,]$Plot <- coords$Plot[p]
          temp$UTM.E=mean(c(ext[1],ext[2]))
          temp$UTM.N=mean(c(ext[3],ext[4]))
          coordinates(temp) <- c("UTM.E","UTM.N")
          crs(temp) <- '+proj=utm +zone=10S +datum=WGS84 +units=m +ellps=WGS84 +towgs84=0,0,0' 
        }      
        if(length(temp)) { 
          # Write the new subplot ID 
          temp$Subplot <- paste(letter[10-(100/size-1)+x],y+1,sep="") 
 
          # If centroid=TRUE save the subplot's centroid
          if(centroid) { 
            temp$Center.UTM.E <- mean(c(ext[1],ext[2]))
            temp$Center.UTM.N <- mean(c(ext[3],ext[4]))
          } 

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
  subtrees <- subtrees[!duplicated(subtrees$Num,incomparables=NA),]
 
  # Rename the "Subplot" column to reflect the size selection 
  names(subtrees@data)[[2]] <- paste("Subplot_",size,"m",sep="") 
 
  # If plot_sub=TRUE add a column "Plot_sub" with plot and sub ID 
  if(plot_sub) subtrees@data$Plot_sub <- paste(subtrees[[1]],subtrees[[2]],sep="_") 
 
  # Return whichever object class was sent 
  if(df) as.data.frame(subtrees, stringsAsFactors=FALSE) 
    else subtrees 
} 
 
 
# Get the hectare survey data with trunks aggregated to basal area 
getHectareTrees <- function(na.omit=TRUE, basal.area = TRUE, agg.trunks=TRUE, filter.size=TRUE, trim.data=TRUE) { 
  if(agg.trunks & !basal.area) stop("Trunks can only be aggregated if basal area is also calculated.\n  If agg.trunks is set TRUE, basal.area must also be set TRUE.") 
  if(filter.size & !agg.trunks) stop("Small trees can only be filtered after aggregating trunks.\n  If filter.size is set TRUE, agg.trunks must also be set TRUE.") 
 
  suppressPackageStartupMessages(library(data.table)) 
 
  # Load the CSV of hectare surveys as a data frame 
  get.trees <- read.csv('https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/2016/Hectares/hectare_trees_all.csv',header=TRUE,sep=",",stringsAsFactors=FALSE)
 
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

# Correct the UTM coordinates of the original veg plot survey trees
# based on the angle of the plot and slope and also select trees >20cm DBH
# THESE CORRECTIONS ARE ALREADY MADE FOR THE VEGPLOT TREES INCLUDED IN
# THE hectare_trees_all.csv FILE, THE FUNCTION IS INCLUDED AS DOCUMENTATION.
correctTreeCoordinates <- function(indivs) { 
  # Note: PPW1317 is included here, even though it's not a superplot,
  # because it falls entirely within the hectare around superplot PPW1315
  superplots <- c("PPW1301","PPW1307","PPW1309","PPW1310","PPW1312","PPW1315","PPW1321","PPW1322","PPW1324","PPW1325","PPW1332","PPW1335","PPW1338","PPW1339","PPW1340","PPW1344","PPW1349","PPW1317")

  # Load the differentially corrected coordinates for the superplots
  corners <- read.csv('https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/2016/Hectares/vegplot_corners_corrected.csv',header=TRUE,sep=",",stringsAsFactors=FALSE)

  attach(corners)
  # individual angles for each side of the plot
  corners$S.angle <- atan((SW.UTM.N - SE.UTM.N)/(SE.UTM.E - SW.UTM.E)) #* (180/pi)
  corners$N.angle <- atan((NW.UTM.N - NE.UTM.N)/(NE.UTM.E - NW.UTM.E)) #* (180/pi)
  corners$W.angle <- atan((NW.UTM.E - SW.UTM.E)/(NW.UTM.N - SW.UTM.N)) #* (180/pi)
  corners$E.angle <- atan((NE.UTM.E - SE.UTM.E)/(NE.UTM.N - SE.UTM.N)) #* (180/pi)

  # calculate a mean angle of declination for each plot
  corners$plot.angle <- rowMeans(corners[,11:14])

  # rough correction to adjust for the difference between on-the-ground
  # and aerial measurements due to the slope of the plot
  corners$X.correction <- (SE.UTM.E - SW.UTM.E) / 20
  corners$Y.correction <- (NW.UTM.N - SW.UTM.N) / 20

  detach(corners)

  # Aggregate the stems from the vegplot data to make a size cut-off
  indv.data <- indivs
  indv.data$Num <- trunc(indv.data$Num)
  indv.data <- indv.data[indv.data$Type!="TS",]
  library(data.table)
  indv.data <- data.table(indv.data)
  indv.data[,Basal.Area:=sum(Basal.Area),by="Num"]
  indv.data <- indv.data[!duplicated(indv.data$Num,incomparables=NA),] 
  indv.data <- subset(indv.data, indv.data$Basal.Area > pi*100)
  # subset them based on basal area and superplot identity
  indivs <- subset(indivs, trunc(indivs$Num) %in% indv.data$Num & indivs$Plot %in% superplots)

  # Set up the quad offsets to convert measurements to coordinates
  quad <- (c('A1','A2','A3','A4','B1','B2','B3','B4','C1','C2','C3','C4','D1','D2','D3','D4'))
  Xoffset <- c(0,0,0,0,5,5,5,5,10,10,10,10,15,15,15,15)
  Yoffset <- c(0,5,10,15,0,5,10,15,0,5,10,15,0,5,10,15)
  off <- data.frame(Quad=quad, X.offset=Xoffset, Y.offset=Yoffset)
  indivs <- merge(indivs, off, by="Quad")

  # Change the measured Xs and Ys from cm to m
  indivs$X <- indivs$X_cm / 100
  indivs$Y <- indivs$Y_cm / 100

  # Add all the measurements up for an uncorrected individual UTM
  indivs <- merge(indivs, corners, by="Plot")
  indivs$UTM.E <- indivs$SW.UTM.E + (indivs$X.offset + indivs$X) #* indivs$X.correct
  indivs$UTM.N <- indivs$SW.UTM.N + (indivs$Y.offset + indivs$Y) #* indivs$Y.correct

  # Now calculate angle and distance from the SW corner for each individual
  indivs$angle <- atan(((indivs$Y.offset + indivs$Y) * indivs$Y.correct)/((indivs$X.offset + indivs$X) * indivs$X.correct))
  indivs$dist <- sqrt(((indivs$X.offset + indivs$X) * indivs$X.correct)^2 + ((indivs$Y.offset + indivs$Y) * indivs$Y.correct)^2)

  # Calculate corrected UTM coordinates
  indivs$UTM.E.corrected <- indivs$SW.UTM.E + (indivs$dist * cos(indivs$angle - indivs$plot.angle))
  indivs$UTM.N.corrected <- indivs$SW.UTM.N + (indivs$dist * sin(indivs$angle - indivs$plot.angle))

  indivs <- indivs[order(indivs$Num),c("Plot","Quad","Num","Species","UTM.E.corrected","UTM.N.corrected","DBH_cm","Type","Year","Notes")]
  #write.csv(indivs,"center_plot_trees.csv",row.names=FALSE,quote=FALSE)
  return(indivs)

  # Compare original and corrected coordinates for a plot
#  plot(indivs$UTM.E[indivs$Plot=="PPW1301"],indivs$UTM.N[indivs$Plot=="PPW1301"], pch=16)
#  points(indivs$UTM.E.corrected, indivs$UTM.N.corrected, col="red", pch=16)


  # Some test code to check algorithms on a grid
#  trees.all <- expand.grid(1:10,1:10)
#  angle <- atan(trees.all[,2]/trees.all[,1]) #* 180/pi
#  dist <- sqrt(trees.all[,1]^2 + trees.all[,2]^2)
#  plot(dist*cos((angle-mean(angle))),dist*sin((angle-mean(angle))))
}