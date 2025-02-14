
## HPC version

# # extract the arguments provided in the command line
# args <- commandArgs(trailingOnly = TRUE)
# # The first argument is now the job index
# job_index <- as.integer(args[1])

# Set the library for packages
# lib_loc <- paste(getwd(),"/r_lib",sep="")
lib_loc = .libPaths() # Do this to maintain consistency across HPC and non-HPC script

# For non-hpc version
# job_index <- 1


library(spatstat)
library(ggplot2)
library(dplyr)
library(ggpubr, lib.loc = lib_loc)
library(viridis)
library(terra)
library(purrr)
library(readr)


# Scenario choices --------------------------------------------------------

scenario_name = "6"

# "Enviro.Extrap" or "Spatial.Auto"
scenario.type =  "Enviro.Extrap"

nreps <- 1 # Replicates per extrapolation type

# Spatial autocorrelation?
latent.type = "lgcp" 

# Bias in PO sampling?
bias <- TRUE

# Model choices -----------------------------------------------------------

# Model types to run
# Options are "no-GRF" "spatial" "no-GRF.bias" "spatial.bias"
mod.type = c("no-GRF", "spatial", "no-GRF.bias" ,"spatial.bias")


# If doing a spatial model, choose whether to predict the GRF and the Fixed effect
pred.GRF <- TRUE
pred.fixed <- TRUE


# Parameters --------------------------------------------------------------

# Autocorrelation range & variance for covs 
#(Maximum range (raster units) of spatial autocorrelation)
range_cov1 <- 10 
range_cov2 <- 100 
var_cov1 <- 0.5
var_cov2 <- 10

beta0 <- -1 # Intercept
beta1 <- 0.01 # Coefficient for cov 1
beta2 <- 0.2 # Coefficient for cov 2

if(scenario.type == "Spatial.Auto") {
  
  scal <- list(20, 100, 200) # List of range parameters for spatial autocorr scenario
  
} else {
  
  scal <- 20 # Scale parameter (range of spatial effect)
  
}

# Relative variance of GRF to fixed effect (can be 0.2, 1, or 5)
GRF.var.multiplier <- 1

#Maximum probability for bias field (0.2 or 0.05)
maxprob <- 0.2
# PO sampling values (0.08 or 0.02)
detect.prob <- 0.08


# Implementation choices --------------------------------------------------

n_cores <- 1

# Number of posterior samples to take * note that it slows things down
posterior_nsamps <- 5000


# START SETUP -------------------------------------------------------------


# Output folder name ------------------------------------------------------

outpath <- file.path(getwd(), "output")

# Make dir if not already there
if(!dir.exists(file.path(outpath, scenario_name))) {
  
  dir.create(file.path(outpath, scenario_name), recursive = TRUE)
  
}


# DOMAIN SETUP ------------------------------------------------------------

# START with set up of resolution and north/east step length for later Site A and B grid creation.

# Set ncol
ncol <- 1000
nrow <- 1000
res <- 1

# Create a bounded domain on [0, 10] x [0, 10]

east_min <- 0
east_max <- 1000
north_min <- 0
north_max <- 1000


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

# Depending on your scenario type, source 1 of 2 R scripts for running replicates

if(scenario.type == "Enviro.Extrap") {
  
  source("0b.Run_Replicate.R")
  
}

if(scenario.type == "Spatial.Auto") {
  
  source("0b.Run_Replicate_SA.R")
  
}

# Calculate the variance of the GRF so can save for plotting: --------------
# Calculate 'variance' of fixed effect
fe.var <- beta1^2*var_cov1 + beta2^2*var_cov2 

# Multiply variance by multiplier to get target variance for GRF
variance <- GRF.var.multiplier * fe.var


# Save the input parameters for this job ----------------------------------
save(scenario.type, pred.GRF, pred.fixed, mod.type, beta0, beta1, beta2, scal, variance, file = paste0(file.path(outpath, scenario_name), "/Scenario_", scenario_name, "_Input_Params.RData"))


Run_Replicate_Func(n_cores = n_cores,
                   outpath = outpath,
                   scenario_name = scenario_name,
                   scenario.type = scenario.type,
                   ncol = ncol,
                   nrow =nrow,
                   res = res,
                   east_min = east_min,
                   east_max = east_max,
                   north_min = north_min,
                   north_max = north_max,
                   n_bau_east = n_bau_east,
                   n_bau_north = n_bau_north,
                   bau_east_step = bau_east_step,
                   bau_north_step = bau_north_step,
                   eastings = eastings,
                   northings = northings,
                   coords = coords,
                   nreps = nreps,
                   mod.type = mod.type,
                   range_cov1 = range_cov1,
                   range_cov2 = range_cov2,
                   var_cov1 = var_cov1,
                   var_cov2 = var_cov2,
                   beta0 = beta0,
                   beta1 = beta1,
                   beta2 = beta2,
                   scal = scal,
                   GRF.var.multiplier = GRF.var.multiplier,
                   bias = bias,
                   detect.prob = detect.prob,
                   maxprob = maxprob,
                   latent.type = latent.type,
                   pred.GRF = pred.GRF,
                   pred.fixed = pred.fixed,
                   posterior_nsamps = posterior_nsamps,
                   job_index = job_index)



