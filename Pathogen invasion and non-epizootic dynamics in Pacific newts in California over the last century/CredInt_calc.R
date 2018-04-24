library(emdbook)

#LOCALITY
#group your data by locality and find prevalence
dat<-read.csv("/Users/hasansulaeman/Dropbox/Costa Rica Bd Project/Data/S.table.1.csv")

#aggregate data by site and find the total number of samples (n) and number of positives (positives)
datci<-matrix()
datci<-aggregate(dat$bd_results,by=list(dat$site),FUN=sum,na.rm=T)
names(datci)<-c("site","positives")
datn<-aggregate(dat$bd_results,by=list(dat$site),FUN=length);datci$n<-datn$x
datci$prev<-round(datci$positives/datci$n*100, 2)
datci
write.csv(datci,"/Users/tinacheng/Dropbox/Taiwan/data for R/summaries/datci_site.csv")

#make new columns for shape1 (shape1=positives + 1) and shape2 (shape2=n-pos+1)
datci$shape1<-datci$positives + 1
datci$shape2<-datci$n-datci$positives + 1



#ncredint function in emdbook to calculate Bayesian confidence intervals
x=datci$shape1
y=datci$shape2

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

#Write to excel file
write.csv(CI_results, "/Users/tinacheng/Dropbox/Taiwan/data for R/summaries/credint_locality.csv")
#manually copy and paste upper and lower bayesian ci's to datci file, trim unnecessary columns and save in finaldata

#######################################################
#SPECIES

#group your data by species and find prevalence
dat<-read.csv("/Users/tinacheng/Dropbox/Taiwan/data for R/rawdata/wild_data.csv")

#qbeta(p=c(0.025, 0.975), shape1=S+1, shape2=N-S+1), where S=# positive animals and N=total number sampled
dat$species<-as.factor(tolower(dat$species)); levels(dat$species)

#aggregate data by site and find the total number of samples (n) and number of positives (positives)
datci<-matrix()
datci<-aggregate(dat$bd_results,by=list(dat$species),FUN=sum,na.rm=T)
names(datci)<-c("species","positives")
datn<-aggregate(dat$bd_results,by=list(dat$species),FUN=length);datci$n<-datn$x
datci$prev<-round(datci$positives/datci$n*100, 2)
datci
write.csv(datci,"/Users/tinacheng/Dropbox/Taiwan/data for R/summaries/datci_species.csv")

#make new columns for shape1 (shape1=positives + 1) and shape2 (shape2=n-pos+1)
datci$shape1<-datci$positives + 1
datci$shape2<-datci$n-datci$positives + 1



#ncredint function in emdbook to calculate Bayesian confidence intervals
x=datci$shape1
y=datci$shape2

credint<-function(x,y)
	{
		qbeta(p=c(0.025, 0.975), shape1=x, shape2=y)
		}
	
CI_results=matrix(NA, nrow=30, ncol=2)

for(i in 1:length(x))
	{
		CI_results[i,]=credint(x[i], y[i])
		}

head(CI_results)

#Write to excel file
write.csv(CI_results, "/Users/tinacheng/Dropbox/Taiwan/data for R/summaries/credint_species.csv")
#manually copy and paste upper and lower bayesian ci's to datci file, trim unnecessary columns and save in finaldata

