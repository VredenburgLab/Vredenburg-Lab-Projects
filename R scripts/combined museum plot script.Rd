matrix <- as.matrix(read.csv(file="museum by decade - luciae bar.csv", head=TRUE, sep=","))
matrix2 <- data.frame(matrix)

intensity <- matrix2[1,]
time <- matrix2[2,]
library(data.table)

setnames(matrix2, old=c("X1935","X1945", "X1955","X1965","X1975","X1985","X1995","X2005","X2010"), new=c("1930-39","1940-49","1950-59","1960-69","1970-79","1980-89","1990-99","2000-09","field"))

df.int <- as.matrix(intensity)
 df.int
  1930-39 1940-49 1950-59 1960-69 1970-79 1980-89 1990-99 2000-09 field
1       0       0       0       0    2.63    7.69   32.76   14.81 10.13
par(mar=c(5,4,4,6)+0.1)
luciae <- read.table("museum by decade - B. luciae.csv", head=TRUE, sep=",")
df.bar <- barplot(df.int, axes=FALSE, ylim=c(0,50), xlim=c(0,10.5), ylab="Percent Prevalence", xlab="Decade", cex.names=0.6, col="darkseagreen3")
axis(2,ylim=c(0,50), col="black",las=1)
box()

# Add credible intervals
library(Hmisc)
cred <- read.table("datci_decade.csv", sep=",", fill=TRUE, head=TRUE)
lower <- cred$lower.cred
upper <- cred$upper.cred
heights <- cred$prev
errbar(df.bar,heights,upper,lower,add=T,xlab="",pch="",lwd="1.5")

#add sample sizes
text(0.7,2,"0/30",font=2)
text(1.9,2,"0/53",font=2)
text(3.1,2,"0/0",font=2)
text(4.3,2,"0/40",font=2)
text(5.5,4.63,"34/1292",font=2)
text(6.7,9.14,"1/14",font=2)
text(7.9,34.76,"19/58",font=2)
text(9.1,16.81,"4/27",font=2)
text(10.3,11.87,"38/385",font=2)

# allow new plot on top of 1st
par(new=TRUE)

# plot line
plot(df.bar, luciae$avg.logZE, ylim=c(0,4), ylab="", xlim=c(0,10.5), xlab="", axes=FALSE, type="l", col="dodgerblue2", lwd="1.5")
points(df.bar,luciae$avg.logZE, pch=1, col="dodgerblue2")
mtext("Intensity (logZE)",side=4,col="black",line=3)
axis(4,ylim=c(0,4),col="black",col.axis="black",las=1)

#add lines +/- 1 SD (?)
lower2 <- cred$minus.sd
upper2 <- cred$plus.sd
lines(df.bar,upper2,lty=2,col="deepskyblue", lwd=1.5)
lines(df.bar,lower2,lty=2,col="deepskyblue", lwd=1.5)

#add legend
legend("topleft", legend=c("% infected", "95% credible interval", "intensity (logZE)", "logZE+-1 SD"), bty="n", col=c("darkseagreen3","black","dodgerblue2","deepskyblue"), lty=c(0,1,1,2), lwd=c(0,1,1.5,1.5), pch=c(22,NA,NA,NA),pt.bg=c("darkseagreen3",NA,NA,NA),pt.cex=2)


### Aneides
matrix <- as.matrix(read.csv(file="museum by decade - aneides bar.csv", head=TRUE, sep=","))
Warning message:
In read.table(file = file, header = header, sep = sep, quote = quote,  :
  incomplete final line found by readTableHeader on 'museum by decade - aneides bar.csv'
head(matrix)
     X1940 X1950 X1960 X1970 X1980 X1990 X2000 X2010 X2015
[1,]     0     0  1.29  2.31  6.84 16.92 10.53 16.67 19.35
matrix2 <- data.frame(matrix)
intensity <- matrix2[1,]
intensity
  X1940 X1950 X1960 X1970 X1980 X1990 X2000 X2010 X2015
1     0     0  1.29  2.31  6.84 16.92 10.53 16.67 19.35
library(data.table)
setnames(matrix2, old=c("X1940","X1950", "X1960","X1970","X1980","X1990","X2000","X2010","X2015"), new=c("1940-49","1950-59","1960-69","1970-79","1980-89","1990-99","2000-09","2010-15","field"))


df.int <- as.matrix(intensity)
df.int
  1940-49 1950-59 1960-69 1970-79 1980-89 1990-99 2000-09 2010-15 field
1       0       0    1.29    2.31    6.84   16.92   10.53   16.67 19.35
par(mar=c(5,4,4,6)+0.1)
df.bar <- barplot(df.int, axes=FALSE, ylim=c(0,50), xlim=c(0,10.5), yaxs="n", ylab="Percent Prevalence", xlab="Decade", cex.names=0.6, col="darkseagreen3")
axis(2,ylim=c(0,50), col="black",las=1)
box()
library(Hmisc)
cred <- read.table("datci_decade_al.csv", sep=",", fill=TRUE, head=TRUE)
head(cred)
  X decade positives   n  prev          V1         V2 lower.cred upper.cred
1 1   1940         0 157  0.00 0.000160226 0.02307690 0.01602264   2.307690
2 2   1950         0 152  0.00 0.000165462 0.02382199 0.01654622   2.382199
3 3   1960         2 155  1.29 0.003983542 0.04554302 0.39835416   4.554302
4 4   1970         4 173  2.31 0.009394870 0.05781117 0.93948696   5.781117
5 5   1980        13 190  6.84 0.040650359 0.11357734 4.06503594  11.357734
6 6   1990        11  65 16.92 0.097635202 0.27867345 9.76352020  27.867345
    minus.sd   plus.sd         sd mean.logZE
1 0.00000000 0.0000000 0.00000000  0.0000000
2 0.00000000 0.0000000 0.00000000  0.0000000
3 0.04100505 0.2389950 0.09899495  0.1400000
4 0.12091090 0.9890891 0.43408910  0.5550000
5 0.17947120 1.1312980 0.47591340  0.6553846
6 0.26828500 2.8244430 1.27807900  1.5463640
lower <- cred$lower.cred
upper <- cred$upper.cred
heights <- cred$prev
errbar(df.bar,heights,upper,lower,add=T,xlab="",pch="",lwd="1.5")
df.bar
[1]  0.7  1.9  3.1  4.3  5.5  6.7  7.9  9.1 10.3

text(0.7,3.5,"0/157",font=2)
text(1.9,3.5,"0/152",font=2)
text(3.1,3.29,"2/155",font=2)
text(4.3,4.31,"4/173",font=2)
text(5.5,8.84,"13/190",font=2)
text(6.7,18.92,"11/65",font=2)
text(7.9,12.53,"4/38",font=2)
text(9.1,18.67,"1/6",font=2)
text(10.3,21.35,"6/31",font=2)

# allow new plot on top of 1st
par(new=TRUE)

plot(df.bar, cred$mean.logZE, ylim=c(0,4), ylab="", xlim=c(0,10.5), xlab="", axes=FALSE, type="l", col="dodgerblue2", lwd="1.5")
points(df.bar,cred$mean.logZE,pch=1,col="dodgerblue2")
mtext("Intensity (logZE)",side=4,col="black",line=3)
axis(4,ylim=c(0,4),col="black",col.axis="black",las=1)

lower2 <- cred$minus.sd
upper2 <- cred$plus.sd
lines(df.bar,upper2,lty=2,col="deepskyblue", lwd=1.5)
lines(df.bar,lower2,lty=2,col="deepskyblue", lwd=1.5)

legend("topleft", legend=c("% infected", "95% credible interval", "intensity (logZE)", "logZE+-1 SD"), bty="n", col=c("darkseagreen3","black","dodgerblue2","deepskyblue"), lty=c(0,1,1,2), lwd=c(0,1,1.5,1.5), pch=c(22,NA,NA,NA),pt.bg=c("darkseagreen3",NA,NA,NA),pt.cex=2)


### luciae for pub

matrix <- as.matrix(read.csv(file="museum by decade - luciae bar.csv", head=TRUE, sep=","))
matrix2 <- data.frame(matrix)

intensity <- matrix2[1,]
time <- matrix2[2,]
library(data.table)
setnames(matrix2, old=c("X1935","X1945", "X1955","X1965","X1975","X1985","X1995","X2005","X2010"), new=c("1930s","1940s","1950s","1960s","1970s","1980s","1990s","2000s","field"))
df.int <- as.matrix(intensity)
df.int
  1930s 1940s 1950s 1960s 1970s 1980s 1990s 2000s field
1     0     0     0     0  2.63  7.14 32.76 14.81  9.87
par(mar=c(5,4,4,6)+0.1)

df.bar <- barplot(df.int, axes=FALSE, cex.names=0.8, ylim=c(0,50), xlim=c(0,11), ylab="Prevalence (%)", xlab="", col="darkseagreen3")
luciae <- read.table("museum by decade - B. luciae.csv", head=TRUE, sep=",")
axis(2,ylim=c(0,50), col="black",las=1)
library(Hmisc)
cred <- read.table("datci_decade.csv", sep=",", fill=TRUE, head=TRUE)
lower <- cred$lower.cred
upper <- cred$upper.cred
heights <- cred$prev
errbar(df.bar,heights,upper,lower,add=T,xlab="",pch="",lwd="1")
text(0.7,4.63,"0/30",font=1, cex=0.8)
text(1.9,4.63,"0/53",font=1, cex=0.8)
text(3.1,4.63,"0/0",font=1, cex=0.8)
text(4.3,4.63,"0/40",font=1, cex=0.8)
text(5.5,4.63,"34/1292",font=1, cex=0.8)
text(6.7,4.63,"1/14",font=1, cex=0.8)
text(7.9,4.63,"19/58",font=1, cex=0.8)
text(9.1,4.63,"4/27",font=1, cex=0.8)
text(10.3,4.63,"38/385",font=1, cex=0.8)

# allow new plot on top of 1st
par(new=TRUE)

plot(df.bar, cred$mean.logZE, ylim=c(0,4), ylab="", xlim=c(0,11), xlab="", axes=FALSE, type="l", col="dodgerblue2", lwd="1.5")

### mtext("Intensity (logZE)",side=4,col="black",line=3)
axis(4,ylim=c(0,4),col="black",col.axis="black",las=1,labels=FALSE)

lower2 <- cred$minus.sd
upper2 <- cred$plus.sd
lines(df.bar,upper2,lty=2,col="deepskyblue", lwd=1.5)
lines(df.bar,lower2,lty=2,col="deepskyblue", lwd=1.5)

legend("left", legend=c("% infected", "95% credible interval", "intensity (logZE)", "logZE±1 SD"), bty="n", col=c("darkseagreen3","black","dodgerblue2","deepskyblue"), lty=c(0,1,1,2), lwd=c(0,1,1.5,1.5), pch=c(22,NA,NA,NA),pt.bg=c("darkseagreen3",NA,NA,NA),pt.cex=2)
legend("topleft", "a", bty="n")

box(bty="U")

### no labels
df.bar <- barplot(df.int, axes=FALSE, xaxt="n", ylim=c(0,50), xlim=c(0,11), ylab="", xlab="", col="darkseagreen3")
luciae <- read.table("museum by decade - B. luciae.csv", head=TRUE, sep=",")
axis(2,ylim=c(0,50), col="black",las=1)
cred <- read.table("datci_decade.csv", sep=",", fill=TRUE, head=TRUE)
lower <- cred$lower.cred
upper <- cred$upper.cred
heights <- cred$prev
errbar(df.bar,heights,upper,lower,add=T,xlab="",pch="",lwd="1")
text(0.7,4.63,"0/30",font=1, cex=0.8)
text(1.9,4.63,"0/53",font=1, cex=0.8)
text(3.1,4.63,"0/0",font=1, cex=0.8)
text(4.3,4.63,"0/40",font=1, cex=0.8)
text(5.5,4.63,"34/1292",font=1, cex=0.8)
text(6.7,4.63,"1/14",font=1, cex=0.8)
text(7.9,4.63,"19/58",font=1, cex=0.8)
text(9.1,4.63,"4/27",font=1, cex=0.8)
text(10.3,4.63,"38/385",font=1, cex=0.8)
par(new=TRUE)
plot(df.bar, cred$mean.logZE, ylim=c(0,4), ylab="", xlim=c(0,11), xlab="", axes=FALSE, type="l", col="dodgerblue2", lwd="1.5")
axis(4,ylim=c(0,4),col="black",col.axis="black",las=1)
lines(df.bar,upper2,lty=2,col="deepskyblue", lwd=1.5)
lines(df.bar,lower2,lty=2,col="deepskyblue", lwd=1.5)
legend("left", legend=c("% infected", "95% credible interval", "intensity (logZE)", "logZE±1 SD"), bty="n", col=c("darkseagreen3","black","dodgerblue2","deepskyblue"), lty=c(0,1,1,2), lwd=c(0,1,1.5,1.5), pch=c(22,NA,NA,NA),pt.bg=c("darkseagreen3",NA,NA,NA),pt.cex=2)
box(bty="U")



#### aneides for pub

matrix <- as.matrix(read.csv(file="museum by decade - aneides bar.csv", head=TRUE, sep=","))
Warning message:
In read.table(file = file, header = header, sep = sep, quote = quote,  :
  incomplete final line found by readTableHeader on 'museum by decade - aneides bar.csv'
head(matrix)
     X1940 X1950 X1960 X1970 X1980 X1990 X2000 X2010 X2015
[1,]     0     0  1.29  2.31  6.84 16.92 10.53 16.67 19.35
matrix2 <- data.frame(matrix)
intensity <- matrix2[1,]
intensity
  X1940 X1950 X1960 X1970 X1980 X1990 X2000 X2010 X2015
1     0     0  1.29  2.31  6.84 16.92 10.53 16.67 19.35
library(data.table)
setnames(matrix2, old=c("X1940","X1950", "X1960","X1970","X1980","X1990","X2000","X2010","X2015"), new=c("1930s","1940s","1950s","1960s","1970s","1980s","1990s","2000s","field"))


df.int <- as.matrix(intensity)
df.int
  1940-49 1950-59 1960-69 1970-79 1980-89 1990-99 2000-09 2010-15 field
1       0       0    1.29    2.31    6.84   16.92   10.53   16.67 19.35
par(mar=c(5,4,4,6)+0.1)
df.bar <- barplot(df.int, axes=FALSE, ylim=c(0,50), xlim=c(0,11), ylab="", xlab="", cex.names=0.8, col="darkseagreen3")
axis(2,ylim=c(0,50), col="black",las=1, labels=FALSE)
box(bty="U")
library(Hmisc)
cred <- read.table("datci_decade_al.csv", sep=",", fill=TRUE, head=TRUE)
head(cred)
  X decade positives   n  prev          V1         V2 lower.cred upper.cred
1 1   1940         0 157  0.00 0.000160226 0.02307690 0.01602264   2.307690
2 2   1950         0 152  0.00 0.000165462 0.02382199 0.01654622   2.382199
3 3   1960         2 155  1.29 0.003983542 0.04554302 0.39835416   4.554302
4 4   1970         4 173  2.31 0.009394870 0.05781117 0.93948696   5.781117
5 5   1980        13 190  6.84 0.040650359 0.11357734 4.06503594  11.357734
6 6   1990        11  65 16.92 0.097635202 0.27867345 9.76352020  27.867345
    minus.sd   plus.sd         sd mean.logZE
1 0.00000000 0.0000000 0.00000000  0.0000000
2 0.00000000 0.0000000 0.00000000  0.0000000
3 0.04100505 0.2389950 0.09899495  0.1400000
4 0.12091090 0.9890891 0.43408910  0.5550000
5 0.17947120 1.1312980 0.47591340  0.6553846
6 0.26828500 2.8244430 1.27807900  1.5463640
lower <- cred$lower.cred
upper <- cred$upper.cred
heights <- cred$prev
errbar(df.bar,heights,upper,lower,add=T,xlab="",pch="",lwd="1")
df.bar
[1]  0.7  1.9  3.1  4.3  5.5  6.7  7.9  9.1 10.3

text(0.7,4.63,"0/157",font=1, cex=0.8)
text(1.9,4.63,"0/152",font=1, cex=0.8)
text(3.1,4.63,"2/155",font=1, cex=0.8)
text(4.3,4.63,"4/173",font=1, cex=0.8)
text(5.5,4.63,"13/190",font=1, cex=0.8)
text(6.7,4.63,"11/65",font=1, cex=0.8)
text(7.9,4.63,"4/38",font=1, cex=0.8)
text(9.1,4.63,"1/6",font=1, cex=0.8)
text(10.3,4.63,"6/31",font=1, cex=0.8)

# allow new plot on top of 1st
par(new=TRUE)

plot(df.bar, cred$mean.logZE, ylim=c(0,4), ylab="", xlim=c(0,11), xlab="", axes=FALSE, type="l", col="dodgerblue2", lwd="1.5")
### points(df.bar,cred$mean.logZE,pch=1,col="dodgerblue2")
#### mtext("Intensity (logZE)",side=4,col="black",line=3)
axis(4,ylim=c(0,4),col="black",col.axis="black",las=1)

lower2 <- cred$minus.sd
upper2 <- cred$plus.sd
lines(df.bar,upper2,lty=2,col="deepskyblue", lwd=1.5)
lines(df.bar,lower2,lty=2,col="deepskyblue", lwd=1.5)

legend("left", legend=c("% infected", "95% credible interval", "intensity (logZE)", "logZE±1 SD"), bty="n", col=c("darkseagreen3","black","dodgerblue2","deepskyblue"), lty=c(0,1,1,2), lwd=c(0,1,1.5,1.5), pch=c(22,NA,NA,NA),pt.bg=c("darkseagreen3",NA,NA,NA),pt.cex=2)
legend("topleft", "b", bty="n")

