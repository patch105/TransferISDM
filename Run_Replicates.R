
library(spatstat)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(viridis)
library(terra)


# PARAMETERS --------------------------------------------------------------

# Set the seed for all
seed <- 50
# set.seed(50)

# DOMAIN SETUP ------------------------------------------------------------

# START with set up of resolution and north/east step length for later Site A and B grid creation.

# Set ncol
ncol <- 1000
nrow <- 1000
res <- 0.01

# Create a bounded domain on [0, 10] x [0, 10]

east_min <- 0
east_max <- 10
north_min <- 0
north_max <- 10


# We generate the grid resolution from min, max dimensions and the number of pixels

# Set number of pixels (100 x 100)
n_bau_east <- ncol
n_bau_north <- nrow
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


# Run setup for replicates ------------------------------------------------

# Specify number of replicates per extrapolation type
nreps <- 10

# Set up a list to save covariates, latent dist, and extrapolation results
reps.setup.list <- list(Low = list(), Moderate = list(), High = list())

# Set up the function that runs the covariates, latent distribution, and extrapolation 
# LATER TO DO: ADD REQUIRED INPUTS TO THIS SETUP FUNCTION

run_setup_func <- function(){

  
  # 1.Simulate Covariates ---------------------------------------------------
  
  source("1.Simulate_Covariates.R")
  
  # Set autocorrelation range Cov1
  range_cov1 <- 10 # Maximum range (raster units) of spatial autocorrelation
  
  # Set autocorrelation range Cov2
  range_cov2 <- 100 # Maximum range (raster units) of spatial autocorrelation
  
  print("Simulating new cov")
  
  cov.list <- sim_covariates_func(plot = FALSE, 
                                  ncol = ncol, 
                                  nrow = nrow,
                                  res = res,
                                  seed = seed,
                                  range_cov1 = range_cov1,
                                  range_cov2 = range_cov2)
  
  
  # 2. Simulate Latent Distribution -----------------------------------------
  
  source("2.Simulate_Latent_Distribution.R")
  
  beta0 <- 6 # Intercept
  beta1 <- 0.5 # Coefficient for cov 1
  beta2 <- 0.1 # Coefficient for cov 2
  
  scal <- 0.2 # Scale parameter (range of spatial effect)
  
  print("Simulating new latent distribution")
  
  latent.list <- sim_latent_dist_func(beta0 = beta0,
                                      beta1 = beta1,
                                      beta2 = beta2,
                                      scal = scal,
                                      cov1 = cov.list$cov1,
                                      cov1.mat = cov.list$cov1.mat,
                                      cov2.mat = cov.list$cov2.mat,
                                      cov1.df = cov.list$cov1.df,
                                      response = "linear",
                                      plot.mu = FALSE,
                                      plot.lg.s = FALSE)
  
  
  # Simulate Environmental Extrapolation ------------------------------------
  
  source("3.Simulate_Enviro_Extrapolation.R")
  
  # Set size of grid (number of cells) for Site A (Reference) and Site B (Target)
  # NOTE - must be smaller than total cell number in x y directions
  rast_cellsA <- c(50, 50)
  rast_cellsB <- c(50, 50)
  
  print("Simulating environmental extrapolation")
  
  run.extrap.list <- run_extrap_func(nreps = nreps,
                                     rast_cellsA = rast_cellsA,
                                     rast_cellsB = rast_cellsB,
                                     bau_east_step = bau_east_step,
                                     bau_north_step = bau_north_step,
                                     eastings = eastings,
                                     northings = northings,
                                     cov1 = cov.list$cov1,
                                     cov2 = cov.list$cov2,
                                     covs = cov.list$covs,
                                     reps.setup.list = reps.setup.list)
  
  
  # Return the list of replicates
  reps.setup.list[[run.extrap.list$extrap.type]] <- c(reps.setup.list[[run.extrap.list$extrap.type]], list(list(cov.list = cov.list, latent.list = latent.list, extrap.reps.out = run.extrap.list$extrap.reps.out)))
  
  return(reps.setup.list)
  
}



# Iterate over the setup function until you get to the desired nreps for Low, Moderate, High

while(length(reps.setup.list$Low) < nreps | length(reps.setup.list$Moderate) < nreps | length(reps.setup.list$High) < nreps) {
  
  reps.setup.list <- run_setup_func()
  
  
}


# 4. PO sampling ----------------------------------------------------------


                                   



