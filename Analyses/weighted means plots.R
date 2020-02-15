x=read.csv("/Users/melinakozanitas/Desktop/Pepperwood/woody/plot.means.csv")
head(x)
dim(x)
x$TR.score <- NA
x$SA.score <- NA

i=1
for (i in 1:54) {
  x$TR.score[i] <- weighted.mean(1:4,x[i,2:5])
  x$SA.score[i] <- weighted.mean(1:4,x[i,8:11])
}
head(x)
write.csv(x)
write.csv(x, "~/Desktop/weighted.means.csv", row.names=FALSE)
