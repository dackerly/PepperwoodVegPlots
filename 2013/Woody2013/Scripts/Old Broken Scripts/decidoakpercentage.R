setwd("/Users/mattb/Dropbox/Berkeley Projects/PepperwoodVegPlots_2013/Veg Survey Data/Data Processing") # macbook
tree<-read.csv("/Users/mattb/Dropbox/Berkeley Projects/PepperwoodVegPlots_2013/Veg Survey Data/Data Processing/Tree_Area.csv", as.is=TRUE) # macbook
tree
tree[,c(2,3,4)]
attach(tree)

allspecies<-sort(unique(tree$Species))
allspecies

FT<-c(rep("ew",8),rep("do",4), "ew")
ft.table <- data.frame(FT, allspecies)

s2f <- match(tree$Species,ft.table$allspecies)
tree$FT <- ft.table$FT[s2f]
head(tree)

names(tree)[4] = "basalarea"
tree

FTbasalarea <- data.frame(tapply(tree$basalarea, list(tree$Plot, tree$FT),sum))

FTbasalarea[is.na(FTbasalarea)] = 0
FTbasalarea$total<-apply(FTbasalarea,1,sum)
FTbasalarea$doperc <- FTbasalarea[,1]/FTbasalarea[,3]

###write a match function to join the summed areas to the plots surveyed column.

write.csv(FTbasalarea, file="FTbasalarea.csv")



