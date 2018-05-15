
# MAKING A 3D GRAPH FOR LATITUDE, ZSCORE, TIME, AND PREVALENCE

# Setting up the library and data ----
library(plot3D)
library(plot3Drgl)
pos = read.csv("/Users/hasansulaeman/Dropbox/Sam's Sierra Bd work/Data/positives.csv")

# Set up the variables ----
x = pos$Longitude
y = pos$Latitude
z = pos$Year
a = log10(pos$ZEScore+1)

# A function to also have points on X and Y axes ----
scatter3D_improved <- function(x, y, z,..., colvar = a)
{
  panelfirst <- function(pmat) {
    XY <- trans3D(x, y, z = rep(min(z), length(z)), pmat = pmat)
    scatter2D(XY$x, XY$y, colvar = a, pch = 20, col=ramp.col(c("yellow","orange","red","red1")),
              cex = 1, add = TRUE, colkey = FALSE)
    
    XY <- trans3D(x = rep(min(x), length(x)), y, z, pmat = pmat)
    scatter2D(XY$x, XY$y, colvar = a, pch = 20, col=ramp.col(c("yellow","orange","red","red1")), 
              cex = 1, add = TRUE, colkey = FALSE)
  }
  scatter3D(x, y, z, ..., colvar = colvar, panel.first=panelfirst,
            colkey = list(length = 0.5, width = 0.5, cex.clab = 0.75)) 
}

# Plotting the positives with points in the background ----
scatter3D_improved(x, y, z, pch = 16, col=ramp.col(c("yellow","orange","red","red1")),
          ticktype = "detailed", theta = 15, phi=20, d = 2, bty ="g", cex = 2, expand=0.5,
          clab ="Zscore", xlab="Longitude", ylab="Latitude", zlab="Time (Year)")
# To look at it interactively, run the code below after plotting
plotrgl()