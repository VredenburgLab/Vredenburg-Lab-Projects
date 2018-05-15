
# MAKING A 3D GRAPH FOR LATITUDE, ZSCORE, TIME, AND PREVALENCE

# Setting up the library and data ----
library(plot3D)
library(plot3Drgl)
library(magick)
data = read.csv("/Users/hasansulaeman/Documents/GitHub/Publications/Sierra Nevada Historical Study/3dgraphyoseseki.csv")

# Set up the variables ----
x = data$longitude
y = data$latitude
z = data$year
a = log10(data$Zscore+1)

# Plotting the positives with points in the background ----
scatter3D(x, y, z, pch = 20, col.var = as.factor(data$group), 
          col = c("red", "green"), ticktype = "detailed", 
          theta = -60, phi=20, d = 2, bty = "u", cex = 2, 
          col.panel ="steelblue", expand =0.5, col.grid = "darkblue", 
          xlab="Longitude", ylab="Latitude", 
          zlab="Time (Year)", xlim=c(-121,-117))
# To look at it interactively, run the code below after plotting
plotrgl()
movie3d(spin3d(), duration = 10, fps = 100)

neg = subset(data, data$group=="negative")
pos = subset(data, data$group=="positive")
a = log10(pos$Zscore+1)

par(c(2,4,4,2))
x = neg$longitude
y = neg$latitude
z = neg$year
scatter3D(x, y, z, pch = 20, ticktype = "detailed", col = "azure3",
          theta = -60, phi=20, d = 2, bty = "u", cex = 2, 
          col.panel ="steelblue", expand =0.5, col.grid = "darkblue", 
          xlab="Longitude", ylab="Latitude", 
          zlab="Time (Year)", xlim=c(-121,-117))
x = pos$longitude
y = pos$latitude
z = pos$year
scatter3D(x, y, z, pch = 20, ticktype = "detailed", theta = -60, colvar = a,
          phi=20, d = 2, bty = "u", cex = 2, col.panel ="steelblue", expand =0.5,
          col.grid = "darkblue", xlab="Longitude", ylab="Latitude", 
          col=ramp.col(c("yellow","orange","red")),zlab="Time (Year)", 
          xlim=c(-121,-117), add=TRUE, colkey = list(side = 4, length = 0.5, 
          dist=0.01), clab="Zscore")
