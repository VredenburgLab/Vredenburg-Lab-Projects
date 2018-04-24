
setwd("Dropbox/Chaukulkar Manuscript/Data files/")
taricha=read.csv("DecadeTable_SC.csv", header=TRUE)
# Taricha
par(mar=c(5, 4, 4, 8) + 0.1)
a=barplot(taricha$tot, width=0.775, space=0.275, xlim=c(0,13), ylim=c(0,400), axes=FALSE, xlab="Time Period", ylab="Sample Size", col="azure3")
mtext(1, at=c(0.5:12.5), line=1, text = c("1890-1899", "1900-1909", "1910-1919", 
                                         "1920-1929", "1930-1939", "1940-1949", 
                                         "1950-1959", "1960-1969", "1970-1979", 
                                         "1980-1989", "1990-1999", "2000-2009", 
                                         "2010-2015"), cex=0.7)
taricha$x.axis=c(1:13)
axis(1, at=c(0.5:12.5), labels=FALSE)
axis(2)
par(new=TRUE)
plot(x=c(0.5:12.5), taricha$prev, type="l", lwd=2, 
     ylab=" ", xlab=" ", ylim=c(0,30), xlim=c(0,13), axes=FALSE, col="darkblue")
axis(4, line=-.5, col="darkblue")
mtext("Prevalence", side = 4, line = 1.35, cex = par("cex.lab"), col="darkblue")
errbar(x=c(0.5:12.5), y=taricha$prev, yplus=taricha$upperc, yminus=taricha$lowerc, 
       add=T,xlab="",pch="",lwd="1",cap=0.01)
par(new=TRUE)
plot(x=c(0.5:12.5), taricha$log.avg.zscore, type="l", xlim=c(0,13), lwd=2,
     ylab=" ", xlab=" ", ylim=c(0,5), lty=2, axes=FALSE, col="darkred")
axis(4, lwd=1, line=3, ylim=c(0,5),col="darkred")
mtext("Log10 Bd Load (Zoospore Equivalents)", side = 4, line = 5, cex = par("cex.lab"), col="darkred")
abline(h=4, lty=3, lwd=0.75)
abline(h=0, lty=3, lwd=0.5)

taricha$avg.Zscore = c(0,0,0,0,0,0.0000976,2.956,23.21584,159.2576,8.208933,8.707522,52.25765,4.688)
taricha$log.avg.zscore = log10(taricha$avg.Zscore + 1)
