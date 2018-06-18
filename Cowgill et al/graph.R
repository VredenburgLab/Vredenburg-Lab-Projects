
# ++++++++++++++++++++++++
# Cowgill et al. Graphing
# ++++++++++++++++++++++++

# Library and data setup ----
## Required packages 
library(ggplot2)
library(reshape2)
library(Hmisc)
library(plotrix)

## Aneides lugubris data setup
AL = read.csv("ALhistorical.csv")
AL.summary = dcast(data=AL,decade~BdStatus,value.var='decade',fun.aggregate=length)
names(AL.summary)<-c("Decade","Negatives","Positives")          # Renaming columns
AL.summary$n = AL.summary$Negatives + AL.summary$Positives      # Getting n per decade
AL.summary$prev = AL.summary$Positives/AL.summary$n*100         # Getting prevalence
AL.summary[is.na(AL.summary)] = 0                               # Replace NA w/ 0
### Adding the missing decades to make it even
AL.summary[9,] = c(1920,0,0,0)
AL.summary[10,] = c(1930,0,0,0)
AL.summary = AL.summary[order(AL.summary$Decade),]
write.csv(AL.summary, "summaryAL.csv")

## Batrachoseps luciae data setup
BL = read.csv("BLhistorical.csv")
BL.summary = dcast(data=BL,decade~BdStatus,value.var='decade',fun.aggregate=length)
names(BL.summary)<-c("Decade","Negatives","Positives")          # Renaming columns
BL.summary$n = BL.summary$Negatives + BL.summary$Positives      # Getting n per decade
BL.summary$prev = BL.summary$Positives/BL.summary$n*100         # Getting prevalence
BL.summary[is.na(BL.summary)] = 0                               # Replace NA w/ 0
### Adding the missing decades to make it even
BL.summary[9,] = c(1950,0,0,0)
BL.summary[10,] = c(2010,0,0,0)
BL.summary = BL.summary[order(BL.summary$Decade),]
write.csv(BL.summary, "summaryBL.csv")

# Added log.zscore manually. Reading the data again ----
AL = read.csv("summaryAL.csv")
BL = read.csv("summaryBL.csv")

# Getting the CI and power based on binomial distribution ----
## Aneides lugubris CI 
AL = read.csv("graphAL.csv")
AL[is.na(AL)] = 0 
AL$lower = 0
AL$upper = 0
for (i in 3:nrow(AL)){                                        
  AL$lower[i]=binom.test(AL$Positives[i],AL$n[i])$conf.int[1]
  AL$upper[i]=binom.test(AL$Positives[i],AL$n[i])$conf.int[2]
}
### Probability of no positives, given an expected endemic prev of 11%
AL$pr =  dbinom(0, AL$n, .11)
write.csv(AL,'graphAL.csv')

## Batrachoseps luciae CI 
BL = read.csv("graphBL.csv")
BL[is.na(BL)] = 0 
BL$lower = 0
BL$upper = 0
for (i in 6:10){                                        
  BL$lower[i]=binom.test(BL$Positives[i],BL$n[i])$conf.int[1]
  BL$upper[i]=binom.test(BL$Positives[i],BL$n[i])$conf.int[2]
}
### Probability of no positives, given an expected endemic prev of 11%
BL$pr =  dbinom(0, BL$n, .11)
write.csv(BL,'graphBL.csv')

# B.luciae graph ----
par(mar=c(5, 4, 4, 8) + 0.1)
barplot(BL$n, width=0.770, space=0.275, xlim=c(0,10), ylim=c(0,500), 
          axes=FALSE, xlab="Time Period", ylab="", col="gray95")
mtext(1, at=c(0.5:9.5), line=1, text = c("1920-1929", "1930-1939", "1940-1949", 
                                          "1950-1959", "1960-1969", "1970-1979", 
                                          "1980-1989", "1990-1999", "2000-2009", 
                                          "2010-2015"), cex=0.7)
BL$x.axis=c(0.5:9.5)
axis(1, at=c(0.5:9.5), labels=FALSE)
axis(2, at=c(0,100,200,300,400,500), labels=c("0","100","200","300","400", "1300"), line = -1.75)
mtext("Sample Size", side = 2, line = 0.5, cex = par("cex.lab"))
par(new=TRUE)
plot(x=c(0.5:9.5), BL$prev, type="l", lwd=2, 
     ylab=" ", xlab=" ", ylim=c(0,70), xlim=c(0,10), axes=FALSE, col="darkblue")
axis(4, line=-.5, col="darkblue")
mtext("Prevalence", side = 4, line = 1.35, cex = par("cex.lab"), col="darkblue")
errbar(x=c(0.5:9.5), y=BL$prev, yplus=BL$upper*100, yminus=BL$lower*100, 
       add=T,xlab="",pch="",lwd="1",cap=0.01)
par(new=TRUE)
plot(x=c(0.5:9.5), BL$log.zscore, type="l", xlim=c(0,10), lwd=2,
     ylab=" ", xlab=" ", ylim=c(0,5), lty=2, axes=FALSE, col="darkred")
axis(4, lwd=1, line=3, ylim=c(0,5),col="darkred")
mtext("Log10 Bd Load (Zoospore Equivalents)", side = 4, line = 5, 
      cex = par("cex.lab"), col="darkred")
abline(h=4, lty=3, lwd=0.75)
abline(h=0, lty=3, lwd=0.5)

# A.lugubris graph ----
par(mar=c(5, 4, 4, 8) + 0.1)
barplot(AL$n, width=0.770, space=0.275, xlim=c(0,10), ylim=c(0,500), 
        axes=FALSE, xlab="Time Period", ylab="", col="gray95")
mtext(1, at=c(0.5:9.5), line=1, text = c("1920-1929", "1930-1939", "1940-1949", 
                                         "1950-1959", "1960-1969", "1970-1979", 
                                         "1980-1989", "1990-1999", "2000-2009", 
                                         "2010-2015"), cex=0.7)
AL$x.axis=c(0.5:9.5)
axis(1, at=c(0.5:9.5), labels=FALSE)
axis(2, line = -1.75)
mtext("Sample Size", side = 2, line = 0.5, cex = par("cex.lab"))
par(new=TRUE)
plot(x=c(0.5:9.5), AL$prev, type="l", lwd=2, 
     ylab=" ", xlab=" ", ylim=c(0,70), xlim=c(0,10), axes=FALSE, col="darkblue")
axis(4, line=-.5, col="darkblue")
mtext("Prevalence", side = 4, line = 1.35, cex = par("cex.lab"), col="darkblue")
errbar(x=c(0.5:9.5), y=AL$prev, yplus=AL$upper*100, yminus=AL$lower*100, 
       add=T,xlab="",pch="",lwd="1",cap=0.01)
par(new=TRUE)
plot(x=c(0.5:9.5), AL$log.zscore, type="l", xlim=c(0,10), lwd=2,
     ylab=" ", xlab=" ", ylim=c(0,5), lty=2, axes=FALSE, col="darkred")
axis(4, lwd=1, line=3, ylim=c(0,5),col="darkred")
mtext("Log10 Bd Load (Zoospore Equivalents)", side = 4, line = 5, 
      cex = par("cex.lab"), col="darkred")
abline(h=4, lty=3, lwd=0.75)
abline(h=0, lty=3, lwd=0.5)

