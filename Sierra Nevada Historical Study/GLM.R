# ++++++++++++++++++++++++++++++++++++++++++++++++
# Sierra Nevada Historical Bd Occurrence Modeling
# ++++++++++++++++++++++++++++++++++++++++++++++++

# Library and data setup ----
library(reshape2)
library(dplyr)
library(bootstrap)
library(MASS)
library(corrplot)
library(ggplot2)
library(ggmap) 
library(DAAG)
library(tidyr)
library(ggpubr)
library(Hmisc)
data = read.csv("/Users/hasansulaeman/Dropbox/Sam's Sierra Bd work/Data/samsara.csv")
pre = subset(data, data$Year > 1969)
post = subset(data, data$Year < 1969)

# Making a summary table ----
sum.table = dcast(data=data,Decade~BdStatus,value.var='Decade',fun.aggregate=length)
names(sum.table)[c(1,2,3)] = c('Decade','neg','pos')
sum.table$n = sum.table$neg + sum.table$pos

for (i in 1:nrow(sum.table)){
  sum.table$lower[i]=binom.test(sum.table$pos[i],sum.table$n[i])$conf.int[1]
  sum.table$upper[i]=binom.test(sum.table$pos[i],sum.table$n[i])$conf.int[2]
}

sum.table$lower=sum.table$lower*100
sum.table$upper=sum.table$upper*100

sum.table$prob =  dbinom(0, sum.table$n,.11)
write.csv(sum.table,'DecadeTable.csv')

# Fetching Anthropogenic Data from Venter et al. and NHD data ----
## Venter et al. Data ----
## Important: You first have to download the files at the repository specified in Venter et al.
## Change the directory below to the .tif file you downloaded from the repository
HFP=raster("/Users/hasansulaeman/Google Drive/HumFootprint/Maps/HFP2009.tif")
crop=raster("/Users/hasansulaeman/Google Drive/HumFootprint/Maps/Croplands2005.tif")
built=raster("/Users/hasansulaeman/Google Drive/HumFootprint/Maps/Built2009.tif")
navwat=raster("/Users/hasansulaeman/Google Drive/HumFootprint/Maps/Navwater2009.tif")
popden=raster("/Users/hasansulaeman/Google Drive/HumFootprint/Maps/Popdensity2010.tif")
road=raster("/Users/hasansulaeman/Google Drive/HumFootprint/Maps/Roads.tif")
rail=raster("/Users/hasansulaeman/Google Drive/HumFootprint/Maps/Railways.tif")
pasture=raster("/Users/hasansulaeman/Google Drive/HumFootprint/Maps/Pasture2009.tif")
## After you load all the files into R, we have to change our data frame into spatial points 
## We also need to stack the raster files from Venter et al. together
anthro=stack(HFP, crop, built, navwat, popden, road, rail, pasture)
x = data.frame(x=data$Longitude,y=data$Latitude)   # We're converting the coordinates into a spatial information
sp = SpatialPointsDataFrame(coords = x, data=x,
     proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
ay = raster::extract(anthro, sp)
data = cbind.data.frame(data, ay)

## NHD Closest water body ----
data$distwater = 0
data$catwater = 0
for(i in 1:nrow(data)){
  data[i,]$distwater = min(data[i,57:66])       
  for (j in 57:66){                           # Change column numbers here
    if(data[i,j] == data[i,]$distwater){
      data[i,]$catwater = names(data)[j]
    }
  }
}

write.csv(data, "samsara.csv")

# Multicollinearity Reduction and Covariate Scaling ----
cor = rcorr(as.matrix(data[,c(6,29:41)]) , type= "pearson")
cor.pre = rcorr(as.matrix(pre[,c(6,29:41)]) , type= "pearson")
## From cor$r we see that, for all three, only elev and temps are highly correlated
## Now let's scale the covariates
data[,c(29:36,38,40:41)] = scale(data[,c(29:36,38,40:41)])

# Run the models ----
## Logistic Regression for Overall
lm = glm(BdStatus ~ distwater + factor(catwater) + Roads + ppt +
         tmean + Popdensity2010 + croplands2005 + Built2009 + Species +
         Popdensity2010 + Railways + Pasture2009 + HFP2009, 
         family = "binomial", data = data)
step = stepAIC(lm, direction = "both")
model.comparison = step$anova
final.lm = glm(BdStatus ~ Roads + ppt + tmean + Popdensity2010 + croplands2005 + 
               Built2009 + Railways + HFP2009, family = "binomial", data = data)
summary(final.lm)
## 5-fold Cross-validation
kfold = cv.binary(final.lm, nfolds=100) # 5 fold cross-validation

## Logistic Regression for pre-1970
lm = glm(BdStatus ~ distwater + factor(catwater) + Roads + ppt +
         tmean + Popdensity2010 + croplands2005 + Built2009 + Species +
         Popdensity2010 + Railways + Pasture2009 + HFP2009, 
         family = "binomial", data = pre)
step1 = stepAIC(lm, direction = "both")
model.comparison = step1$anova
final.lm = glm(BdStatus ~ distwater + Built2009 + Species 
               + Railways + HFP2009,family = "binomial", data = pre)
summary(final.lm)

## 5-fold Cross-validation
kfold = cv.binary(final.lm, nfolds=100) # 5 fold cross-validation

## Logistic Regression for post-1970
lm = glm(BdStatus ~ distwater + factor(catwater) + ppt + tmean + HFP2009 +
           croplands2005 + Built2009 + Popdensity2010 + Roads + Railways +
           Pasture2009, family = "binomial", data = post)
step = stepAIC(lm, direction = "both")
model.comparison = step$anova
final.lm = glm(BdStatus ~ bio1 + bio3 + bio7 + bio12 + bio15 + Elevation.m. + HFP2009 + 
                 Built2009 + Popdensity2010, family = "binomial", data = )
summary(final.lm)
## 5-fold Cross-validation
kfold = cv.binary(step1, nfolds=100) # 5 fold cross-validation

# Table for Publication ----
library(sjPlot)
tab_model(step, step1, show.aic = T, show.ci = F, show.est = T, 
          show.fstat = T, show.obs = F, show.r2 = F)
