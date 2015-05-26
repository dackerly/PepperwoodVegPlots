# June 11, 2013 DDA
# Script to reformat file created by script Plot.Info.Tree_Script.R
# Columns: 
# Plot.ID, UTM.E (SW corner), UTM.N(SW corner), Slope (field collected), Aspect (field collected) 
# Tree.Species, Tree.Number(per species, DBH > 1cm), Basal.Area_cm2(per species, DBH>1cm), 
#Total.Basal.Area_cm2 (per plot, all species combined, DBH > 1cm), Percent.Cover (per species) 

options(stringsAsFactors=FALSE) 
#Setting this option makes sure that character strings aren't treated as factors. 

if (Sys.getenv("USER")=='meaganoldfather') setwd("/Users/meaganoldfather/Dropbox/PepperwoodVegPlots/Database/2013/Woody2013/Outputs") else setwd("/Users/david/Documents/Projects/Dropbox/PepperwoodVegPlots_2013/Database/2013/Woody2013/Outputs")
# In order to use the same script with multiple users; Final folder is 'PlotInfo'

info <- read.csv("Plot.Info.Tree.csv")
head(info)

library(picante)
num.mat <- sample2matrix(info[,c(2,8,7)])
head(cover.mat)

ba.mat <- sample2matrix(info[,c(2,9,7)])
head(ba.mat)
class(ba.mat)

## ordination
ord <- metaMDS(ba.mat)
summary(ord)
plot(ord,type='t')

dis=vegdist(ba.mat)
clus=hclust(dis,'average')
plot(clus)
# end ordination

tba <- apply(ba.mat,2,sum)
sort(tba)

names(info)
plot <- data.frame(ID=row.names(ba.mat))
plot$UTM.E <- info$UTM.E[match(row.names(ba.mat),info$Plot.ID)]
plot$UTM.N <- info$UTM.N[match(row.names(ba.mat),info$Plot.ID)]
plot$Slope <- info$Slope[match(row.names(ba.mat),info$Plot.ID)]
plot$Aspect <- info$Aspect[match(row.names(ba.mat),info$Plot.ID)]
plot$Total.Basal.Area_cm2 <- info$Total.Basal.Area_cm2[match(row.names(ba.mat),info$Plot.ID)]

plot <- cbind(plot,ba.mat)
head(plot)

#hist(plot$Total.Basal.Area_cm2)

write.csv(plot,'PPW.plots.basal.area.csv')

# make spatial points dataframe
library(sp)
library(maptools)
plots.sp <- SpatialPoints(plot[,2:3])
plots.sp <- SpatialPointsDataFrame(plot[,2:3],data=plot,proj4string=CRS('+proj=utm +zone=10 +datum=WGS84'))
writeSpatialShape(plots.sp,fn='PPW2013/PPWplots_basalarea')

tmp=readShapeSpatial(fn='PPW2013/PPWplots_basalarea.shp')
tmp
#write species code file
names(ba.mat)
write.csv(names(ba.mat),file='species.codes.csv')