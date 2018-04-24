matrix <- as.matrix (read.csv("museumbythedecadebar.csv",  head=TRUE, sep=","))
matrix2 <- data.frame(matrix)

prevalence <- matrix2[1,]
time <- matrix2[2,]
library(data.table)

setnames(matrix2, old=c("X1890", "X1900", "X1910", "X1920","X1930", "X1940","X1950", "X1960","X1970","X1980","X1990","X2000","X2010"), new=c("1890-99", "1900-09","1910-19", "1920-29", "1930-39","1940-49","1950-59","1960-69","1970-79","1980-89","1990-99","2000-09", "2010-present"))
df.int <- as.matrix(prevalence)
par(mar=c(5,4,4,6)+0.1)
taricha <- read.table("museumbydecade.csv", head=TRUE, sep=",")
df.bar <- barplot(df.int, axes=FALSE, ylim=c(0,20), xlim=c(0,15.5), ylab="Percent Prevalence", xlab="Decade", cex.names=0.6, col="turquoise2")
axis(2,ylim=c(0,20), col="black",las=1)
box()

library(Hmisc)
cred <- read.table ("datci_decade.csv",sep=",", fill=TRUE, head=TRUE)
lower <- cred$lower.cred
upper <- cred$upper.cred
heights <- cred$prev
errbar(df.bar,heights,upper,lower,add=T,xlab="",pch="",lwd="1.5")

# allow new plot on top of 1st
par(new=TRUE)

# plot line
plot(df.bar, taricha$avg.ZE, ylim=c(0,161), ylab="", xlim=c(0,15.5), xlab="", axes=FALSE, type="l", col="indianred2", lwd="1.5")
points(df.bar,taricha$avg.ZE, pch=1, col="indianred2")
mtext("Intensity (averageZE)",side=4,col="black",line=3)
axis(4,ylim=c(0,4),col="black",col.axis="black",las=1)

#add lines +/- 1 SD (?)
lower2 <- cred$minus.sd
upper2 <- cred$plus.sd
lines(df.bar,upper2,lty=2,col="deepskyblue", lwd=1.5)
lines(df.bar,lower2,lty=2,col="deepskyblue", lwd=1.5)

#add legend
legend("topleft", legend=c("% infected", "95% credible interval", "intensity (logZE)", "logZE+-1 SD"), bty="n", col=c("darkseagreen3","black","dodgerblue2","deepskyblue"), lty=c(0,1,1,2), lwd=c(0,1,1.5,1.5), pch=c(22,NA,NA,NA),pt.bg=c("darkseagreen3",NA,NA,NA),pt.cex=2)

