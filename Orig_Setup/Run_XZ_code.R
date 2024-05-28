
# RUN XZ VERSION WITH SOME UPDATES ----------------------------------

rm(list = ls())

library(terra)
library(spatstat)
library(RandomFields)
library(RandomFieldsUtils)

set.seed(42)

outpath <- getwd()

# Create a bounded domain on [0, 1] x [0, 1]

east_min <- 0
east_max <- 1
north_min <- 0
north_max <- 1
win <- spatstat.geom::owin(c(east_min, east_max), c(north_min, north_max))

# We generate the grid resolution from min, max dimensions and the number of pixels

# Set number of pixels (100 x 100)
n_bau_east <- 100
n_bau_north <- 100
# so now we have n_bau_est x n_bau_north grid cells

# Obtain the cell resolution
bau_east_step <- (east_max - east_min) / n_bau_east
bau_north_step <- (north_max - north_min) / n_bau_north 

# Generate grid centroid coordinates
# We do this so that our centroid begins in the centre of a cell (hence, bau_east_step/2))

eastings <- seq(east_min + bau_east_step/2, east_max - bau_east_step/2, by = bau_east_step)
northings <- seq(north_min + bau_north_step/2, north_max - bau_north_step/2, by = bau_north_step)

coords <- as.matrix(expand.grid(eastings, northings))
colnames(coords) <- c("eastings", "northings")

#-------------------------------------------------------------------------------
# simulate a covariate and obtain a pixel image of the fixed effect
#-------------------------------------------------------------------------------
cov <- cbind(x = coords[,1],  y = coords[, 2], rnorm(n_bau_east * n_bau_north))
colnames(cov) <- c("x", "y", "cov")

true_betas <- c(5, 0.5)
beta0 <- 5
beta1 <- 0.5

# Fixed effect (intercept + covariate effect)
# fe <- cbind(1, cov[,"cov"]) %*% true_betas
fe <- beta0 + beta1*cov[,"cov"]
# fe <- rep(6, n_bau_east * n_bau_north) # here we consider only an intercept, so no covariates

mu <- data.frame(x = coords[,1],  y = coords[, 2], z = fe)
mu <- spatstat.geom::as.im(mu, W = win)

plot(mu)

#-------------------------------------------------------------------------------
# simulate from a LGCP
#-------------------------------------------------------------------------------
# GP parameter
scal <- 0.2
var <- 1

# simulate a point pattern
lgcp <- rLGCP(model = "exp", mu = mu, var = var, scal = scal, saveLambda = TRUE)
plot(lgcp$x, lgcp$y)


# SAVE DATA ----------------------------------------------------------

spp_process <- cbind(x = lgcp$x, y = lgcp$y)
colnames(spp_process) <- c("x", "y")

mypath <- getwd()

# po (we may sub-sample the data here)
readr::write_csv(as.data.frame(spp_process), file = paste0(mypath, "/output/po_all_no_thin.csv"))


# RUN MODELS------------------------------------------------------------------------

packages <- c("sf", "terra", "ggpubr", "RISDM", "DescTools", "spatstat", "Metrics", "scoringutils")

walk(packages, require, character.only = T)

wd <- getwd()
mypath <- getwd()

# Load occurrence data ----------------------------------------------------

PO <- read.csv(paste0(wd,"/output/po_all_no_thin.csv"))
names(PO) <- c("x", "y")

# # Load presence-absence data for model fitting
# PA_fit <- read.csv(paste0(wd,"/output/pa_a.csv"))
# PA_fit <- PA_fit %>% mutate(area = 0.02*0.02)
# 
# # Load presence-absence data for model validation
# PA_val <- read.csv(paste0(wd,"/output/pa_b.csv"))
# PA_val <- PA_val %>% mutate(area = 0.02*0.02)

# Create a raster of the study domain
PO_vect <- vect(as.data.frame(PO), geom = c("x", "y"))
# PO_vect_buffered <- buffer(PO_vect, width = 0.3)


# Load enviro. covs -------------------------------------------------------

# Covariates
cov <- rast(cov, type = "xyz")
crs(cov) <- "epsg:3031"

# Mesh default ------------------------------------------------------------

mesh.default <- makeMesh(cov,
                         max.n = c(5000, 2500), # Default c(500,200)
                         dep.range = NULL, # In raster projection units, default is 1/3 diagonal length of raster extent
                         expans.mult = 1.5, # Default, 1.5 x dep.range
                         max.edge = NULL, # Default c(0.2, 0.5)*dep.range
                         cutoff = NULL, # Default 0.2*max.edge1
                         offset = NULL, # Default is dep.range
                         doPlot = TRUE
)


# Presence-Only Model fitting ---------------------------------------------
my.control <- list(coord.names = c("x", "y"),
                   prior.mean = 0,
                   int.sd = 1000, # Intercept standard deviation
                   other.sd = 10, # Covariate effect standard deviation
                   prior.range = c(1, 0.1), # Prior chance 10% that parameter falls below range of 1km
                   prior.space.sigma = c(5, 0.1)) # Prior chance 10% that parameter falls above SD of 5




#PO data only
fm.PO <- isdm(observationList = list(POdat = PO), 
              covars = cov,
              mesh = mesh.default,
              responseNames = NULL,
              sampleAreaNames = NULL,
              distributionFormula = ~0 + cov, #Linear w one cov
              biasFormula = ~1, #Intercept only
              artefactFormulas = NULL,
              control = my.control)

summary(fm.PO)





# NOW RUN SAME TRYING TO USE MY ORIG CODE ----------------------------------
library(terra)

rm(list = ls())

library(spatstat)
library(RandomFields)
library(RandomFieldsUtils)

set.seed(42)

outpath <- getwd()

# Create a bounded domain on [0, 1] x [0, 1]

east_min <- 0
east_max <- 1
north_min <- 0
north_max <- 1
win <- spatstat.geom::owin(c(east_min, east_max), c(north_min, north_max))

# We generate the grid resolution from min, max dimensions and the number of pixels

# Set number of pixels (100 x 100)
n_bau_east <- 100
n_bau_north <- 100
# so now we have n_bau_est x n_bau_north grid cells

# Obtain the cell resolution
bau_east_step <- (east_max - east_min) / n_bau_east
bau_north_step <- (north_max - north_min) / n_bau_north 

# Generate grid centroid coordinates
# We do this so that our centroid begins in the centre of a cell (hence, bau_east_step/2))
y0 <- seq(north_min + bau_north_step/2, north_max - bau_north_step/2, by = bau_north_step)

x0 <- seq(east_min + bau_east_step/2, east_max - bau_east_step/2, by = bau_east_step)

# Covariate is a continuous gradient from the bottom to the top with values from 0 to 1

y0.cov <- seq(win$yrange[1], win$yrange[2],
              length=n_bau_north)
x0.cov <- seq(win$xrange[1], win$xrange[2],
              length=n_bau_east)

# Generate random Gaussian numbers
rand.norm <- rnorm(n_bau_east * n_bau_north)

gridcov.rand <- matrix(rand.norm, nrow = n_bau_east, ncol = n_bau_north)


beta0 <- 5 # Intercept
beta1 <- 0.5 # Coefficient for cov 1

mu_gcov.rand <- im(beta0 + beta1*gridcov.rand, xcol=x0, yrow=y0)

plot(mu_gcov.rand)
mu <- mu_gcov.rand

#-------------------------------------------------------------------------------
# simulate from a LGCP
#-------------------------------------------------------------------------------
# GP parameter
scal <- 0.2
var <- 1

# simulate a point pattern
lgcp <- rLGCP(model = "exp", mu = mu, var = var, scal = scal, saveLambda = TRUE)
plot(lgcp$x, lgcp$y)


# SAVE DATA ----------------------------------------------------------

spp_process <- cbind(x = lgcp$x, y = lgcp$y)
colnames(spp_process) <- c("x", "y")

# po (we may sub-sample the data here)
readr::write_csv(as.data.frame(spp_process), file = paste0(mypath, "/output/po_all_no_thin.csv"))


# RUN MODELS------------------------------------------------------------------------

packages <- c("sf", "terra", "ggpubr", "RISDM", "DescTools", "spatstat", "Metrics", "scoringutils")

walk(packages, require, character.only = T)

wd <- getwd()
mypath <- getwd()

# Load occurrence data ----------------------------------------------------

PO <- read.csv(paste0(wd,"/output/po_all_no_thin.csv"))
names(PO) <- c("x", "y")

# # Load presence-absence data for model fitting
# PA_fit <- read.csv(paste0(wd,"/output/pa_a.csv"))
# PA_fit <- PA_fit %>% mutate(area = 0.02*0.02)
# 
# # Load presence-absence data for model validation
# PA_val <- read.csv(paste0(wd,"/output/pa_b.csv"))
# PA_val <- PA_val %>% mutate(area = 0.02*0.02)

# Create a raster of the study domain
PO_vect <- vect(as.data.frame(PO), geom = c("x", "y"))
# PO_vect_buffered <- buffer(PO_vect, width = 0.3)


# Load enviro. covs -------------------------------------------------------

cov <- gridcov.rand %>% 
  reshape2::melt(c("y", "x"), value.name = "cov") 

cov <- rast(cov) 
crs(cov) <- "epsg:3031"
plot(cov)

# Mesh default ------------------------------------------------------------

mesh.default <- makeMesh(cov,
                         max.n = c(5000, 2500), # Default c(500,200)
                         dep.range = NULL, # In raster projection units, default is 1/3 diagonal length of raster extent
                         expans.mult = 1.5, # Default, 1.5 x dep.range
                         max.edge = NULL, # Default c(0.2, 0.5)*dep.range
                         cutoff = NULL, # Default 0.2*max.edge1
                         offset = NULL, # Default is dep.range
                         doPlot = TRUE
)


# Presence-Only Model fitting ---------------------------------------------
my.control <- list(coord.names = c("x", "y"),
                   prior.mean = 0,
                   int.sd = 1000, # Intercept standard deviation
                   other.sd = 10, # Covariate effect standard deviation
                   prior.range = c(1, 0.1), # Prior chance 10% that parameter falls below range of 1km
                   prior.space.sigma = c(5, 0.1)) # Prior chance 10% that parameter falls above SD of 5




#PO data only
fm.PO <- isdm(observationList = list(POdat = PO), 
              covars = cov,
              mesh = mesh.default,
              responseNames = NULL,
              sampleAreaNames = NULL,
              distributionFormula = ~0 + cov, #Linear w one cov
              biasFormula = ~1, #Intercept only
              artefactFormulas = NULL,
              control = my.control)

summary(fm.PO)

cov

#################################################
####### Now RUN WITH A HABITAT COV  ------------------------------
#################################################

library(terra)

rm(list = ls())

library(spatstat)
library(RandomFields)
library(RandomFieldsUtils)
library(NLMR)
library(landscapetools)


set.seed(42)

outpath <- getwd()

cov1 <- nlm_gaussianfield(ncol = 100,
                          nrow = 100,
                          resolution = 0.01,
                          autocorr_range = 50, # Maximum range (raster units) of spatial autocorrelation
                          mag_var = 1, # Magnitude of variation over the landscape
                          nug = 0.01, # Magnitude of variation in the scale of the autocorr_range (smaller values = more homogenous)
                          mean = 0.5, # Mean value over the field
                          user_seed = 2L, # Set random seed for the simulation
                          rescale = T # If T, the values are rescaled between 0 and 1
) %>% 
  rast()




# Create a bounded domain on [0, 1] x [0, 1]

east_min <- 0
east_max <- 1
north_min <- 0
north_max <- 1
win <- spatstat.geom::owin(c(east_min, east_max), c(north_min, north_max))

# We generate the grid resolution from min, max dimensions and the number of pixels

# Set number of pixels (100 x 100)
n_bau_east <- 100
n_bau_north <- 100
# so now we have n_bau_est x n_bau_north grid cells

# Obtain the cell resolution
bau_east_step <- (east_max - east_min) / n_bau_east
bau_north_step <- (north_max - north_min) / n_bau_north 

# Generate grid centroid coordinates
# We do this so that our centroid begins in the centre of a cell (hence, bau_east_step/2))

x0 <- seq(east_min + bau_east_step/2, east_max - bau_east_step/2, by = bau_east_step)
y0 <- seq(north_min + bau_north_step/2, north_max - bau_north_step/2, by = bau_north_step)

# Also save coords for calculating 0 to 1 covariate
y0.cov <- seq(win$yrange[1], win$yrange[2],
              length=n_bau_north)
x0.cov <- seq(win$xrange[1], win$xrange[2],
              length=n_bau_east)

coords <- as.matrix(expand.grid(x0, y0))
colnames(coords) <- c("eastings", "northings")

#-------------------------------------------------------------------------------
# simulate a covariate and obtain a pixel image of the fixed effect
#-------------------------------------------------------------------------------
cov <- cbind(x = coords[,1],  y = coords[, 2], rnorm(n_bau_east * n_bau_north))
cov2 <- cbind(x = coords[,1],  y = coords[, 2], multiplier*x0.cov + 0*y0.cov)

multiplier <- 1/n_bau_north

vals <- multiplier*x0.cov + 0*y0.cov

coords$cov <- multiplier*coords$x0  + 0*coords$y0
  
cov <- cbind(x = coords[,1],  y = coords[, 2], )
colnames(cov) <- c("x", "y", "cov")

true_betas <- c(5, 0.5)

# Fixed effect (intercept + covariate effect)
fe <- cbind(1, cov[,"cov"]) %*% true_betas

# fe <- rep(6, n_bau_east * n_bau_north) # here we consider only an intercept, so no covariates

mu <- data.frame(x = coords[,1],  y = coords[, 2], z = fe)
mu <- spatstat.geom::as.im(mu, W = dom)

plot(mu)

#-------------------------------------------------------------------------------
# simulate from a LGCP
#-------------------------------------------------------------------------------
# GP parameter
scal <- 0.2
var <- 1

# simulate a point pattern
lgcp <- rLGCP(model = "exp", mu = mu, var = var, scal = scal, saveLambda = TRUE)
plot(lgcp$x, lgcp$y)


# SAVE DATA ----------------------------------------------------------

spp_process <- cbind(x = lgcp$x, y = lgcp$y)
colnames(spp_process) <- c("x", "y")

mypath <- getwd()

# po (we may sub-sample the data here)
readr::write_csv(as.data.frame(spp_process), file = paste0(mypath, "/output/po_all_no_thin.csv"))


# RUN MODELS------------------------------------------------------------------------

packages <- c("sf", "terra", "ggpubr", "RISDM", "DescTools", "spatstat", "Metrics", "scoringutils")

walk(packages, require, character.only = T)

wd <- getwd()
mypath <- getwd()

# Load occurrence data ----------------------------------------------------

PO <- read.csv(paste0(wd,"/output/po_all_no_thin.csv"))
names(PO) <- c("x", "y")

# # Load presence-absence data for model fitting
# PA_fit <- read.csv(paste0(wd,"/output/pa_a.csv"))
# PA_fit <- PA_fit %>% mutate(area = 0.02*0.02)
# 
# # Load presence-absence data for model validation
# PA_val <- read.csv(paste0(wd,"/output/pa_b.csv"))
# PA_val <- PA_val %>% mutate(area = 0.02*0.02)

# Create a raster of the study domain
PO_vect <- vect(as.data.frame(PO), geom = c("x", "y"))
# PO_vect_buffered <- buffer(PO_vect, width = 0.3)


# Load enviro. covs -------------------------------------------------------

# Covariates
cov <- rast(cov, type = "xyz")
crs(cov) <- "epsg:3031"

# Mesh default ------------------------------------------------------------

mesh.default <- makeMesh(cov,
                         max.n = c(5000, 2500), # Default c(500,200)
                         dep.range = NULL, # In raster projection units, default is 1/3 diagonal length of raster extent
                         expans.mult = 1.5, # Default, 1.5 x dep.range
                         max.edge = NULL, # Default c(0.2, 0.5)*dep.range
                         cutoff = NULL, # Default 0.2*max.edge1
                         offset = NULL, # Default is dep.range
                         doPlot = TRUE
)


# Presence-Only Model fitting ---------------------------------------------
my.control <- list(coord.names = c("x", "y"),
                   prior.mean = 0,
                   int.sd = 1000, # Intercept standard deviation
                   other.sd = 10, # Covariate effect standard deviation
                   prior.range = c(1, 0.1), # Prior chance 10% that parameter falls below range of 1km
                   prior.space.sigma = c(5, 0.1)) # Prior chance 10% that parameter falls above SD of 5




#PO data only
fm.PO <- isdm(observationList = list(POdat = PO), 
              covars = cov,
              mesh = mesh.default,
              responseNames = NULL,
              sampleAreaNames = NULL,
              distributionFormula = ~0 + cov, #Linear w one cov
              biasFormula = ~1, #Intercept only
              artefactFormulas = NULL,
              control = my.control)

summary(fm.PO)




# as.im test code ---------------------------------------------------------

# Create a dataframe with x, y, and z values
data <- data.frame(
  x = c(0, 1, 0, 1),
  y = c(0, 0, 1, 1),
  z = c(1, 2, 3, 4)
)

ggplot(data)+
  geom_tile(aes(x = x, y = y, fill = z)) +
  theme_classic()

# Print the dataframe
print("Dataframe:")
print(data)

# Convert the ppp object to an im object using the z values
im_obj <- as.im(data, window=owin(c(0, 1), c(0, 1)))

im_obj$v


