setwd("/Users/hasansulaeman/Dropbox/Chaukulkar Manuscript/Data files")
mue <- read.csv('museumfinal.csv')
ctest = mue[c(19:37)]
factor.cor = rcorr(as.matrix(ctest) , type= "pearson")
factor.cor$r

# bio 12 = 13, 16, 19
# bio 17 = 14, 18

ctest2 =
facotr.cor2 = rcorr(as.matrix(ctest2) , type= "pearson")
facotr.cor2

attach(mue)

lm <- lm(Disease_St ~ SVL + Elevation + Species + Lifestage +
           bio_1 + bio_3 + bio_5 + bio_6 + bio_7 + bio_12 + bio_15 + bio_17 + 
           built_2 + navwater_2 + pasture_2 + popdens_2 + rail_2 + 
           roads_2 + amph_rich + aet_yr + dist.closest.wb + cat.closest.wb, data = mue)
a=step(lm, scale=0, direction="both", steps=1000, k=2)
a$call
write.csv(a$anova, "LRranking.csv")

#1st place
first=lm(Disease_St ~ SVL + Elevation + bio_5 + bio_12 + 
     bio_17 + aet_yr, data = mue)
summary(first)

lm <- glm(Disease_St ~ Lifestage + SVL + Elevation + amph_rich + bio_1 + bio_3 + bio_5 + bio_6 + bio_7 + bio_12 + bio_15 + bio_17 + built_2 + crops_2 + navwater_2 + pasture_2 + popdens_2 + rail_2 + roads_2 + lakepond + swampmarsh + IceMass + Playa + streaminte + streamperr + coastline + artificial + pipeline + connector + aet_yr , data = mue, family = binomial)

lm2 <- glm(Disease_St ~ Lifestage + SVL + Elevation + amph_rich + bio_1 + bio_3 + bio_5 + bio_6 + bio_7 + bio_12 + bio_15 + bio_17 + built_2 + crops_2 + navwater_2 + pasture_2 + popdens_2 + rail_2 + roads_2 + lakepond + swampmarsh + IceMass + Playa + streaminte + streamperr + coastline + artificial + pipeline + connector + aet_yr , data = mue, family = binomial)
step(lm, scale=0, direction="both", steps=1000, k=2)

lmfinal=glm(Disease_St ~ Elevation + bio_1 + bio_12 + bio_17 + 
              crops_2 + navwater_2 + lakepond + swampmarsh + Playa + streamperr + 
              canalditch + artificial + aet_yr + bio_7, family = binomial, 
            data = mue)
summary(lmfinal)

lm.no.canal=glm(Disease_St ~ SVL + Elevation + bio_1 + bio_3 + 
                  bio_12 + bio_17 + crops_2 + rail_2 + lakepond + swampmarsh + 
                  Playa + streamperr + aet_yr, family = binomial, data = mue)
summary(lm.no.canal)

hist(mue$streamperr)
lines(density(mue$streamperr, adjust=2), lty="dotted", col="blue")
plot(density(mue$bio_17), main="Precipitation of Driest Quarter Spread")
lines(density(mue$canalditch), lty="dotted", col="blue")

hist(mue$streaminte)

# Here are the top 5
uno=glm(Disease_St ~ Elevation + bio_1 + bio_12 + bio_17 + 
          Built2009 + NavWat2009 + Railways + lakepond + swampmarsh + 
          Playa + streamperr + artificial + canalditch + aet_yr + crops_2, family = binomial, data = mue)
dos=glm()

attach(mue)

library(ggplot2) #For plotting the distribution of data
library(pscl) #For the zero-inflation model
library(MASS)
library(boot)

zeroinf = zeroinfl(Disease_St ~ Elevation + bio_1 + bio_12 + bio_17 + 
                     Built2009 + NavWat2009 + Railways + lakepond + swampmarsh + 
                     Playa + streamperr + artificial + canalditch + aet_yr + crops_2, data = mue, dist = "negbin", link= "logit")
summary(zeroinf)

null.zeroinf = update(zeroinf, . ~ 1)

pchisq(2 * (logLik(zeroinf) - logLik(null.zeroinf)), df = 3, lower.tail=FALSE)

prev.lm=glm(Disease_St~ Elevation+ Decade + bio_3 , data=mue, family=binomial)

summary(prev.lm)

vuong(prev.lm, zeroinf)[]

lm29 <- glm(Disease_St ~ Decade, Elevation + bio_3 + bio_12 + bio_15 + bio_17 
            + Built2009 + crop2005 + Railways + Roads + lakepond + streamperr + coastline + pipeline 
            + aet_yr, data = mue, family = binomial)










lm <- glm(Disease_St ~ crop2005 + Railways + Roads + lakepond + streamperr + coastline + pipeline + aet_yr, data = mue, family = binomial)
summary (lm29)

lm29 <- glm(Disease_St ~ Railways + Roads + lakepond + streamperr + coastline + pipeline + aet_yr, data = mue, family = binomial)
summary (lm29)

lm29 <- glm(Disease_St ~  Roads + lakepond + streamperr + coastline + pipeline + aet_yr, data = mue, family = binomial)
summary (lm29)

lm29 <- glm(Disease_St ~  lakepond + streamperr + coastline + pipeline + aet_yr, data = mue, family = binomial)
summary (lm29)

lm29 <- glm(Disease_St ~ streamperr + coastline + pipeline + aet_yr, data = mue, family = binomial)
summary (lm29)

lm29 <- glm(Disease_St ~ coastline + pipeline + aet_yr, data = mue, family = binomial)
summary (lm29)

lm29 <- glm(Disease_St ~  pipeline + aet_yr, data = mue, family = binomial)
summary (lm29)

lm29 <- glm(Disease_St ~   aet_yr, data = mue, family = binomial)
summary (lm29)

lm29 <- glm(Disease_St ~ Elevation + bio_3 + bio_12 + bio_15 + bio_17 + Built2009 + crop2005 + Railways + Roads + lakepond + streamperr + coastline + pipeline + aet_yr, data = mue, family = binomial)
summary (lm29)

lm5 <- glm(Disease_St ~ Decade + SVL + Elevation  + lakepond + streaminte +streamperr + aet_yr, data = museumeverything, family = binomial)
summary (lm5)

lm6 <- glm(Disease_St ~ Decade + SVL + Elevation + bio_1 + bio_3 + bio_5 + bio_6 + bio_7 + bio_12 + bio_17 + bio_15 +  lakepond + streaminte +streamperr  +  aet_yr, data = museumeverything, family = binomial)
summary (lm6)

lm7 <- glm(Disease_St ~ Decade + SVL + Elevation +  bio_6 + bio_7 + bio_12 + bio_17 + bio_15 +  lakepond + streaminte +streamperr  +  aet_yr, data = museumeverything, family = binomial)
summary (lm7)





lm29 <- glm(Disease_St ~ Decade + Elevation + bio_3 + bio_12 + bio_15 + bio_17 + Built2009 + crop2005 + Railways + Roads + lakepond + streamperr + coastline + pipeline + aet_yr, data = museumeverything, family = binomial)
summary (lm29)
lm30 <- glm(Disease_St ~ Decade + Elevation  + bio_12 +  bio_17 + Built2009 + crop2005  + Roads + Railways + lakepond + streamperr + coastline + pipeline + aet_yr, data = museumeverything, family = binomial)
summary (lm30)


Call:
  glm(formula = Disease_St ~ Decade + Elevation + bio_12 + bio_17 + 
        Built2009 + crop2005 + Roads + Railways + lakepond + streamperr + 
        coastline + pipeline + aet_yr, family = binomial, data = museumeverything)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-1.7903  -0.2352  -0.1349  -0.0611   3.3263  

Coefficients:
               Estimate Std. Error z value Pr(>|z|)    
(Intercept) -1.086e+02  1.559e+01  -6.965 3.28e-12 ***
Decade       5.456e-02  7.793e-03   7.002 2.53e-12 ***
Elevation    1.786e-03  4.150e-04   4.303 1.69e-05 ***
Annualp       2.256e-03  1.198e-03   1.884  0.05959 .  
PoDQ     -4.464e-02  2.134e-02  -2.092  0.03642 *  
Built2009    1.216e-01  4.785e-02   2.542  0.01103 *  
crop2005     5.741e-01  1.409e-01   4.075 4.60e-05 ***
Roads        8.856e-02  5.276e-02   1.679  0.09321 .  
Railways    -1.792e+00  8.672e+01  -0.021  0.98351    
lakepond    -8.308e-05  3.651e-05  -2.276  0.02287 *  
streamperr  -2.926e-04  1.325e-04  -2.208  0.02723 *  
coastline   -1.350e-06  2.705e-06  -0.499  0.61758    
pipeline     8.273e-06  7.235e-06   1.143  0.25289    
aet_yr      -8.470e-03  2.978e-03  -2.845  0.00445 ** 
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 518.58  on 1893  degrees of freedom
Residual deviance: 399.44  on 1880  degrees of freedom
AIC: 427.44

Number of Fisher Scoring iterations: 17


plot(lm30)

library(MASS)


glm.diag.plots(glmfit, glmdiag=glm.diag(glmfit),subset=NULL, iden=FALSE, labels=NULL, ret=FALSE)





attach(museumeverything)
bestglm(Disease_St, family = binomial, IC = "AIC", t = "default", CVArgs = "default", qLevel = 0.99, TopModels = 10, method = "exhaustive", intercept = TRUE, weights = NULL, nvmax = "default", RequireFullEnumerationQ = FALSE)


lbw.for.bestglm <-lbw.for.bestglm[, c("Decade", "SVL", "Elevation", "Species", "Bioregion", "Lifestage","bio_1" , "bio_3" , "bio_5" , "bio_6" , "bio_7" ,"bio_12" , "bio_15" , "bio_17" , "Built2009" , "crop2005" , "NavWat2009" , "Pastur2009" , "Popden2009" , "Railways" + "Roads" , "amph_rich", "lakepond" , "swampmarsh" , "IceMass" , "Playa streaminte" , "streamperr" , "coastline" , "artificial" , "pipeline" , "canalditch" , "connector" , "aet_yr" ,"crops_2")]


bestglm(museumeverything, IC="AIC")

attach(taricha)

zscore.glm17=glm(logZE ~ bio_1 + bio_2 + bio_4 + bio_6 + bio_14 + bio_15 + crop2005 + HFP2009 + amph_rich + lakepond + swampmarsh + Playa + streaminte + canalditch + connector + Elevation +  ai_yr + pet_yr + aet_yr + SVL , data=taricha, family=gaussian)
summary(zscore.glm17)

