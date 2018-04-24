matrix <- as.matrix (read.csv("museumbythedecadebar2.csv",  head=TRUE, sep=","))
matrix2 <- data.frame(matrix)

Samplesize <- matrix2[1,]
time <- matrix2[2,]
library(data.table)

#change x-axis label
setnames(matrix2, old=c("X1890", "X1900", "X1910", "X1920","X1930", "X1940","X1950", "X1960","X1970","X1980","X1990","X2000","X2010"), new=c("1890-99", "1900-09","1910-19", "1920-29", "1930-39","1940-49","1950-59","1960-69","1970-79","1980-89","1990-99","2000-09", "2010-present"))
df.int <- as.matrix(Samplesize)
par(mar=c(5,4,4,6)+0.1)
taricha <- read.table("museumbydecade.csv", head=TRUE, sep=",")
df.bar <- barplot(df.int, axes=FALSE, ylim=c(0,300), xlim=c(0,16), ylab="Sample Size", xlab="Decade", main="Emergence Of Bd in Pacific Newts Over Time", cex.lab=1,  cex.main=1.75, cex.sub=3.5, cex.names=0.6, col="pink1")
axis(2,ylim=c(0,300), col="black",las=1)

box()

# allow new plot on top of 1st
par(new=TRUE)

prevl <- plot(df.bar, taricha$percent, ylim=c(0,20), ylab="", xlim=c(0,16), xlab="", axes=FALSE, type="l", col="darkblue", lwd="3")
prevpoint <- points(df.bar,taricha$percent, pch=1, col="darkblue")

axis(4,ylim=c(0,5),col="black", ylab="Percent Prevalence" , col.axis="black",las=1)

mtext("Prevalence", side = 4, line = 1.75, cex = par("cex.lab"))

abline(h=0,col=2,lty=2)
#add credible intervals


library(Hmisc)
cred <- read.table ("DecadeTable_SC.csv",sep=",", fill=TRUE, head=TRUE)
lower <- cred$lowercred
upper <- cred$uppercred
heights <- cred$prev
errbar(df.bar, heights,upper,lower,add=T,xlab="",pch="",lwd="1.5")


par(new=TRUE)
infection <-plot(df.bar, taricha$avg.logZE, ylim=c(0,5), ylab="", xlim=c(0,16), xlab="", axes=FALSE, type="o", pch=22, lty=2, col="darkgreen", lwd="3")
axis(4, lwd=2, line=3, ylim=c(0,5))
mtext("Average Log ZE", side = 4, line = 5, cex = par("cex.lab"))
abline(h=4,col="purple",lty=2)

legend("topleft", legend=c("Sample Size", "95% confidence interval", "Intensity (logZE)","Percent Prevalence", "Mortality Threshold"), bty="o", col=c("pink1","black","darkgreen","darkblue", "purple"), lty=c(0,1,2,1,2), lwd=c(0,1,2.5,2.5, 1.5), pch=c(22,NA,NA,NA, NA),pt.bg=c("pink1",NA,"darkgreen","darkblue", "purple"),pt.cex=2, cex =0.75)










box(bty="U")


legend("topleft", "a", bty="n")       