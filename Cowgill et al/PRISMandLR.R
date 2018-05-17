
# ++++++++++++++++++++++++++
# Cowgill et al. Regression
# ++++++++++++++++++++++++++

# Library and data setup ----
data = read.csv("rawdata.csv")
prism = read.csv("PRISMraw.csv")
library(reshape2)
library(dplyr)
library(raster)
library(sp)
library(corrplot)
library(ggplot2)
library(ggmap) 
library(magrittr)
library(tidyr)
options(prism.path = "~/prismtmp")

# Getting the PRISM monthly data ----
## Setting the dates up so they match up
data = separate(data, "Year", c("Year", "Day"), sep = c(7))
## Creating empty columns 
data$ppt = 0
data$tmin = 0
data$tmax = 0
data$tmean = 0 
data$elev = 0
## Match it
for (i in 1:nrow(data)) {
  a = which(data[i,]$X == prism$ID & data[i,]$Year == prism$Date)
  data[i,11:14] = prism[a,6:9]
  data[i,15] = prism[a,4]
}
## Ship it off
write.csv(data, "datawprism.csv")

# Exploring correlations in covariates ----
cor = rcorr(as.matrix(data[,11:15]) , type= "pearson")
## From cor$r we see that tmax and tmin are highly correlated

# Scaling and Regression ----
## Scaling the continuous variables
data = read.csv("datawprism.csv")
data[,c(7,11,13,14,15,16)] = scale(data[,c(7,11,13,14,15,16)])

## Linear Regression
lm = lm(BdStatus ~ SVL + factor(Species) + ppt + elev + tmax +
        tmean + Group.size, data = data)
step(lm)
final.lm = lm(formula = BdStatus ~ factor(Species) + ppt + elev + 
              tmax + tmean + Group.size, data = data)
summary(final.lm)
