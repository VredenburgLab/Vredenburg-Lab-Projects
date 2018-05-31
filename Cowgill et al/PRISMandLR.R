
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
library(ggpubr)
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
data[,c(7,11,13,14,15,16,17)] = scale(data[,c(7,11,13,14,15,16,17)])

## Linear Regression
### Both species
lm = lm(BdStatus ~ SVL + factor(Species) + ppt + elev + tmax +
        tmean + Group.size + other.species, data = data)
step(lm, direction = c("both", "backward", "forward"))
final.lm = lm(BdStatus ~ ppt + elev + tmean + Group.size + 
              other.species, data = data)
summary(final.lm)

### Aneides lugubris only
data.al = subset(data, data$Species == "AL")
lm = lm(BdStatus ~ SVL + ppt + elev + tmax + tmean + 
        Group.size + other.species, data = data.al)
step(lm, direction = c("both", "backward", "forward"))
final.lm = lm(BdStatus ~ SVL + ppt + elev + tmean + 
              other.species, data = data)
summary(final.lm)

### Batrachoseps luciae only
data.bl = subset(data, data$Species == "BL")
lm = lm(BdStatus ~ SVL + ppt + elev + tmax + tmean + 
        Group.size + other.species, data = data.bl)
step(lm, direction = c("both", "backward", "forward"))
final.lm = lm(BdStatus ~ ppt + elev + tmean + Group.size + 
              other.species, data = data)
summary(final.lm)

ggplot(zscore, aes(zscore$Zscore)) +
  geom_density(aes(fill="red")) + xlab("Log10 Zscore")

# Kruskal-Wallis on Zscore and Group size and Cohabitant
## For both species. Not evry informative
table(data$Group.size)
table(data$other.species)

## Subsetting A.lugubris only 
data.al = subset(data, data$Species == "AL" & data$Zscore > 0)
table(data.al$Group.size)
table(data.al$other.species)

## Subsetting B. luciae only
data.bl = subset(data, data$Species == "BL" & data$Zscore > 0)
table(data.bl$Group.size)
table(data.bl$other.species)

## Visualizing the data frames
### A. lugubris
ggline(data.al, x = "Group.size", y = "Zscore", 
       add = c("mean_se", "jitter"), 
       order = c("1", "3", "4"),
       ylab = "Log10 Zscore", xlab = "Group Size", 
       main = "A. lugubris Group Size Effect on Zscore")
ggline(data.al, x = "other.species", y = "Zscore", 
       add = c("mean_se", "jitter"), 
       order = c("0", "5", "13"),
       ylab = "Log10 Zscore", xlab = "Group Size", 
       main = "A. lugubris, B. luciae's presence Effect on Zscore")

### B. luciae
ggline(data.bl, x = "Group.size", y = "Zscore", 
       add = c("mean_se", "jitter"), 
       order = c("1", "2", "3", "4", "6", "20", "30"),
       ylab = "Log10 Zscore", xlab = "Group Size", 
       main = "B. luciae Group Size Effect on Zscore")
ggline(data.bl, x = "other.species", y = "Zscore", 
       add = c("mean_se", "jitter"), 
       order = c("0", "1", "2", "4"),
       ylab = "Log10 Zscore", xlab = "Group Size", 
       main = "B. luciae, A. lugubris' presence Effect on Zscore")

### K-W on A.lugubris
kruskal.test(Zscore ~ Group.size, data = data.al)
# Kruskal-Wallis chi-squared = 2.1333, df = 2, p-value = 0.3442
kruskal.test(Zscore ~ other.species, data = data.al)
# Kruskal-Wallis chi-squared = 2.1333, df = 2, p-value = 0.3442

### K-W on B. luciae
kruskal.test(Zscore ~ Group.size, data = data.bl)
# Kruskal-Wallis chi-squared = 7.9843, df = 6, p-value = 0.2393
kruskal.test(Zscore ~ other.species, data = data.bl)
# Kruskal-Wallis chi-squared = 1.3287, df = 3, p-value = 0.7223