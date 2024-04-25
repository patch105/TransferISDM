################################################################################
# Simulate data (random coordinates) within a bounded domain from a non-homogeneous 
# Poisson process (NHPP) based on a log Gaussian Cox process (LGCP)
################################################################################

rm(list = ls())

library(spatstat)

set.seed(42)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
outpath <- getwd()

#-------------------------------------------------------------------------------
# create a bounded domain on [0, 300] x [0, 300]
#------------------------------------------------------------------------------

dim <- c(300, 300)

beta0 <- -2 # Intercept
beta1 <- 2 # Coefficient
sigma2x <- 0.5 # Variance of the Gaussian field     
range <- 20 # Scale parameter
nu <- 1 # Smoothness parameter


win <- owin(c(0,dim[1]), c(0,dim[2])) #

# set number of pixels
spatstat.options(npixel=c(dim[1],dim[2]))

# Create an artificial covariate
# create y0 and x0 separately as rectangle
y0 <- seq(win$yrange[1], win$yrange[2],
          length=spatstat.options()$npixel[2])
x0 <- seq(win$xrange[1], win$xrange[2],
          length=spatstat.options()$npixel[1])

# Covariate is a continuous gradient from the bottom to the top with values from 0 to 1
multiplier <- 1/dim[2]

gridcov <- outer(y0,x0, function (x,y) multiplier*x + 0*y)

# Mean of process is dependent on intercept + 1 covariate
mu <- im(beta0 + beta1*gridcov, xcol=x0, yrow=y0)

# Create LGCP with environmental covariate
# Has a warning message but still runs
lg.s <- rLGCP('matern', mu = mu,
              var=sigma2x, scale=range, nu=nu)

# takes the coordinates of the randomly generated points
xy <- cbind(lg.s$x, lg.s$y)

# access attribute (Lambda) of lg.s object and create Lam 
Lam <- attr(lg.s, 'Lambda') 
summary(as.vector(rf.s <- log(Lam$v)))


# plot the intensity and the points (don't think we do need to plot the points here - only sampled points)
par(mfrow=c(1,1)) 
library(fields) 
image.plot(list(x=Lam$xcol, y=Lam$yrow, z=t(rf.s)), main='log-Lambda', asp=1) 

# Plot the covariate
image.plot(list(x=x0, y=y0, z=t(gridcov)), main='Covariate', asp=1) 

#-------------------------------------------------------------------------------
# output
#-------------------------------------------------------------------------------

save(lg.s, file = paste0(outpath, "/output/nhpp.rdata"))



## Save the covariate as a raster (have to flip rows first)
cov <- apply(gridcov, 2, rev)
cov <- rast(cov)

writeRaster(cov, paste0(outpath, "/output/covariate.tif"), overwrite = T)


