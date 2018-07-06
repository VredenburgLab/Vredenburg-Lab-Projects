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
data = read.csv("Samrawdata_final.csv")
pre = subset(data, data$Year > 1969)
post = subset(data, data$Year < 1969)

# Fetching Worlcclim and Anthropogenic Data from Venter et al. ----
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
## Worldclim Bioclimatic Variable Data ----

# Multicollinearity Reduction and Covariate Scaling ----
cor = rcorr(as.matrix(data[,29:55]) , type= "pearson")
cor.pre = rcorr(as.matrix(pre[,29:55]) , type= "pearson")
cor.post = rcorr(as.matrix(post[,29:55]) , type= "pearson")
## From cor$r we see that, for all three, Bio 1, 3, 7, 12, 15 
## Now let's scale the covariates
data[,c(29,31,35,40,43,48:55)] = scale(data[,c(29,31,35,40,43,48:55)])

# Run the models ----
## Logistic Regression for Overall
lm = glm(BdStatus ~ bio1 + bio3 + bio7 + bio12 + bio15 + Elevation.m. +
           HFP2009 + croplands2005 + Built2009 + Popdensity2010 + Roads + Railways +
           Pasture2009, family = "binomial", data = data)
step = stepAIC(lm, direction = "both")
model.comparison = step$anova
final.lm = glm(BdStatus ~ bio1 + bio3 + bio7 + bio12 + bio15 + Elevation.m. + HFP2009 + 
               Built2009 + Popdensity2010, family = "binomial", data = data)
summary(final.lm)
## 5-fold Cross-validation
kfold = cv.binary(final.lm, nfolds=100) # 5 fold cross-validation

## Logistic Regression for pre-1970
lm = glm(BdStatus ~ bio1 + bio3 + bio7 + bio12 + bio15 + Elevation.m. +
         HFP2009 + croplands2005 + Built2009 + Popdensity2010 + Roads + Railways +
         Pasture2009, family = "binomial", data = pre)
step = stepAIC(lm, direction = "both")
model.comparison = step$anova
final.lm = glm(BdStatus ~ bio7 + bio12 + bio15 + Elevation.m. + HFP2009,
               family = "binomial", data = pre)
summary(final.lm)
## 5-fold Cross-validation
kfold = cv.binary(final.lm, nfolds=100) # 5 fold cross-validation

## Logistic Regression for post-1970
lm = glm(BdStatus ~ bio1 + bio3 + bio7 + bio12 + bio15 + Elevation.m. +
           HFP2009 + croplands2005 + Built2009 + Popdensity2010 + Roads + Railways +
           Pasture2009, family = "binomial", data = post)
step = stepAIC(lm, direction = "both")
model.comparison = step$anova
final.lm = glm(BdStatus ~ bio1 + bio3 + bio7 + bio12 + bio15 + Elevation.m. + HFP2009 + 
                 Built2009 + Popdensity2010, family = "binomial", data = )
summary(final.lm)
## 5-fold Cross-validation
kfold = cv.binary(final.lm, nfolds=100) # 5 fold cross-validation

