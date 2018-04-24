library(emdbook)

dat<-read.csv ("byweekexp.csv")
dat$date <- as.factor(tolower(dat$week)); levels(dat$week)

datci<-matrix()
datci<-aggregate(dat$number,by=list(dat$week),FUN=sum,na.rm=T)
names(datci)<-c("week","number")

datn<-aggregate(dat$total,by=list(dat$week),FUN=sum,na.rm=T);datci$n<-datn$x



x=byweekexp$shape1
y=byweekexp$shape2

credint<-function(x,y)
{
  qbeta(p=c(0.025, 0.975), shape1=x, shape2=y)
}

CI_results=matrix(NA, nrow=22, ncol=2)

for(i in 1:length(x))
{
  CI_results[i,]=credint(x[i], y[i])
}

head(CI_results)

write.csv(CI_results, "datci_decadeEXP.csv")


x.axis=as.factor(byweekexp$week)
x=c(1:8)
plot(x,byweekexp$percent, type="l", ylim=c(0,100), col="blue", xlab="Weeks of Experiment", ylab="Percent Prevalence", lwd="3")
par(new=TRUE)
library(Hmisc)
cred <- read.table ("datci_decadeEXP.csv",sep=",", fill=TRUE, head=TRUE)
lower <- cred$lower
upper <- cred$upper
heights <- cred$prev
errbar(x,heights,upper,lower,add=T,xlab="",pch="",lwd="1.5")


 
