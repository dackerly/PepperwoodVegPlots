# species<-c("QUEGAR", "PSEMEN","QUEAGR","QUELOB","UMBCAL", "QUEDOU", "TORCAL",
#            "AESCAL" , "HETARB" , "ARBMEN", "QUEKEL", "ARCMAN", "AMOCAL","FRACAL","QUEWIS", "BACPIL","QUEAGKE", 
#            "QUEBER" , "QUEDEC" ,"NOTDEN", "ADEFAS" ,"CEOCUN", "UNKN27", "UNK28", "UNKN30", "UNKN47", 
#            "QUEXXX", "UNK21","UNKN42")

plot.list<-c("PPW1301", "PPW1302", "PPW1303", "PPW1304", "PPW1305", "PPW1306", "PPW1307", "PPW1308" ,"PPW1309", "PPW1310" ,"PPW1311", "PPW1312",
             "PPW1313", "PPW1314" ,"PPW1315" ,"PPW1316" ,"PPW1317", "PPW1318" ,"PPW1319" ,"PPW1320" ,"PPW1321", "PPW1322", "PPW1323" ,"PPW1324",
             "PPW1325", "PPW1326" ,"PPW1327", "PPW1328" ,"PPW1329" ,"PPW1330" ,"PPW1331", "PPW1332", "PPW1333", "PPW1334", "PPW1335", "PPW1336",
             "PPW1337", "PPW1338", "PPW1339", "PPW1340", "PPW1341", "PPW1342", "PPW1343", "PPW1344", "PPW1345", "PPW1346" ,"PPW1347" ,"PPW1348",
             "PPW1349", "PPW1350")

x<-unique(TR$Species)
y<-unique(SA$Species)
z<-unique(SEJU.Plot$Species)
species<-unique(unlist(list(x,y,z)))
species

df.template <- data.frame(Plot=numeric(length(species)), Species=species, Adult=0, BA=0, Sapling=0, Juvenile=0, Seedling=0)
df.template
all <- vector("list", length=50)
all
# use plot.list, TR, SA, SEJU.Plot

for (i in 1:50)
{
	all[[i]] <- df.template
	all[[i]]$Plot <- plot.list[i]
	### This is the tricky part: match(TR[TR$Plot == i, "Species"], species) returns a vector of the indices of the species vector (which is in the same order as the species column in the data frame for the particular plot you are looking at in the all list) that match with the limited vector of species in the TR data frame. In this case, this bit returns this vector: c(1, 3, 11, 13). Meaning that, in the new data frame you are building, you want to go to those rows, and change the "Adult" column to be the entire column of "Number" in the TR data frame. Using match allows everything to line up such that you can just pick a few different rows to deal with in "all" and set those few rows to the entire column of TR (for a given plot)
	all[[i]]$Adult[match(TR[TR$Plot == plot.list[i], "Species"], species)] <- TR$Number[TR$Plot==plot.list[i]]
	all[[i]]$BA[match(TR[TR$Plot == plot.list[i], "Species"], species)] <-  TR$Basal.Area[TR$Plot==plot.list[i]]
	all[[i]]$Sapling[match(SA[SA$Plot == plot.list[i], "Species"], species)] <- SA$Number[SA$Plot==plot.list[i]]
	all[[i]]$Juvenile[match(SEJU.Plot[SEJU.Plot$Plot == plot.list[i], "Species"], species)] <- SEJU.Plot$Num.Juveniles[SEJU.Plot$Plot==plot.list[i]]
	all[[i]]$Seedling[match(SEJU.Plot[SEJU.Plot$Plot == plot.list[i], "Species"], species)] <- SEJU.Plot$Num.Seedlings[SEJU.Plot$Plot==plot.list[i]]
}

head(all)
tail(all)

total<-do.call(rbind, all)
head(total)
tail(total)
total$JS<-total$Juvenile+total$Seedling
head(total)
# write.csv(total, "~/Desktop/AllInfo.csv")
# write.csv(TR, "~/Desktop/TR.csv")
# write.csv(SA, "~/Desktop/SA.csv")
# write.csv(SEJU.Plot, "~/Desktop/SEJU.csv")

#correlation <- data.frame(Species=species, COR=0)
#correlation
cor<-data.frame(numeric(length(species)))
colnames(cor)<-("COR")
for (i in 1:length(species)){
 cor$COR[i]<-cor((total[total$Species==species[i], "Adult"]), (total[total$Species==species[i], "JS"]))
 cor$CORB[i]<-cor((total[total$Species==species[i], "BA"]), (total[total$Species==species[i], "JS"]))  
}
cor
cor1<-cbind(species, cor)
cor1

class(cor1)
colnames(cor1)<- c("Species", "Correlation", "CorrelationBA")
write.csv(cor1, file="~/Desktop/Adult.Juvenile.Correlations.csv")



for (i in 1:length(species)){
  cor2[i]<-cor((total[total$Species==species[i], "Adult"]), (total[total$Species==species[i], "Sapling"]))
}
cor2<-cbind(species, cor)


class(cor2)
cor2<-as.data.frame(cor1)
cor2
colnames(cor2)<- c("Species", "Correlation")


# par(ask=TRUE)
# for(i in length(species)){
# plot[i](total[total$Species==species[i], "Adult"]), (total[total$Species==species[i], "JS"])
# }

# The script below works , null correlation for QUEGAR bc no
cor(total[total$Species=="UMBCAL", "Adult"], total[total$Species=="UMBCAL", "JS"])
plot(total[total$Species=="HETARB", "Adult"], total[total$Species=="HETARB", "JS"])

length(species)
length(correlation)
correlation$Species<-species
class(cor1)
cor1<-as.data.frame(cor1)
cor1
colnames(cor1)<- c("Species", "Correlation")

