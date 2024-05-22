library(tidyverse)

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

cov <- rast(paste0(wd,"/output/covariate.tif"))
crs(cov) <- "epsg:3031"
names(cov) <- "cov"
plot(cov)

cov <- gridcov.rand %>% 
  reshape2::melt(c("y", "x"), value.name = "cov") 

cov <- rast(cov) 
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




# Simulating a Gaussian covariate -----------------------------------------

#-------------------------------------------------------------------------------
# create a bounded domain on [0, 1] x [0, 1]
#-------------------------------------------------------------------------------
east_min <- 0
east_max <- 300
north_min <- 0
north_max <- 300
dom <- spatstat.geom::owin(c(east_min, east_max), c(north_min, north_max))

#-------------------------------------------------------------------------------
# set the number of basic areal units (BAUs) which define the finest resolution 
# note that we will generate data from a log-Gaussian process over the corresponding grid
# also sampling from an LGCP is done based on the grid that discretizes the domain
#-------------------------------------------------------------------------------
# set grid dimension
n_bau_east <- 300 
n_bau_north <- 300
# so now we have n_bau_est x n_bau_north grid cells

# obtain the BAU resolution
bau_east_step <- (east_max - east_min) / 300 
bau_north_step <- (north_max - north_min) / 300 

# generate grid centroid coordinates
eastings <- seq(east_min + bau_east_step/2, east_max - bau_east_step/2, by = bau_east_step)
northings <- seq(north_min + bau_north_step/2, north_max - bau_north_step/2, by = bau_north_step)
coords <- as.matrix(expand.grid(eastings, northings))
colnames(coords) <- c("eastings", "northings")

#-------------------------------------------------------------------------------
# simulate a covariate and obtain a pixel image of the fixed effect
#-------------------------------------------------------------------------------
cov <- cbind(x = coords[,1],  y = coords[, 2], rnorm(n_bau_east * n_bau_north))
colnames(cov) <- c("x", "y", "cov")
true_betas <- c(-2, 2)
fe <- cbind(1, cov[,"cov"]) %*% true_betas
# fe <- rep(6, n_bau_east * n_bau_north) # here we consider only an intercept, so no covariates
mu <- data.frame(x = coords[,1],  y = coords[, 2], z = fe)
mu <- spatstat.geom::as.im(mu, W = dom)
plot(mu)




