
# This script was run on a HPC so to keep consistency, there's a job_index variable.
# When running on a local machine, this can be set to 1.

job_index <- 1

# Load packages 
library(ggplot2)
library(dplyr)
library(ggpubr)
library(viridis)
library(terra)
library(purrr)
library(readr)
library(sf)
library(Metrics)
library(scoringutils)
library(spatstat)

# RISDM requires INLA to be installed which is done with the following code:

# library( devtools)
# install.packages("INLA",repos=c(getOption("repos"),  
#                                 INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

# devtools::install_github( repo="Scott-Foster/RISDM", build_vignettes=FALSE) # Had to do force = T when I wanted to re-load
library(RISDM)


# Scenario choices --------------------------------------------------------

scenario_name = "1C"

# "Enviro.Extrap" or "Spatial.Auto"
scenario.type =  "Enviro.Extrap"

nreps <- 1 # Replicates per extrapolation type

# Spatial autocorrelation?
latent.type = "ipp" 

# Bias in PO sampling?
bias <- TRUE

# Model choices -----------------------------------------------------------

# Model types to run
# Options are "no-GRF" "spatial" "no-GRF.bias" "spatial.bias"
mod.type = c("no-GRF", "no-GRF.bias" )


# If doing a spatial model, choose whether to predict the GRF and the Fixed effect
pred.GRF <- FALSE
pred.fixed <- FALSE


# Parameters --------------------------------------------------------------

beta0 <- -2 # Intercept
beta1 <- 0.01 # Coefficient for cov 1
beta2 <- 0.2 # Coefficient for cov 2

if(scenario.type == "Spatial.Auto") {
  
  scal <- list(20, 100, 200) # List of range parameters for spatial autocorr scenario
  
} else {
  
  scal <- 20 # Scale parameter (range of spatial effect)
  
}

variance <- 2 # Variance of the Gaussian field at distance zero (changed  from 0.5)

# PO sampling values
detect.prob <- 0.05
maxprob <- 0.05


# Implementation choices --------------------------------------------------

n_cores <- 1

# Number of posterior samples to take * note that it slows things down
posterior_nsamps <- 5000



# START SETUP -------------------------------------------------------------


# Output folder name ------------------------------------------------------

outpath <- file.path(dirname(getwd()), "output")

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
                   beta0 = beta0,
                   beta1 = beta1,
                   beta2 = beta2,
                   scal = scal,
                   variance = variance,
                   bias = bias,
                   detect.prob = detect.prob,
                   maxprob = maxprob,
                   latent.type = latent.type,
                   pred.GRF = pred.GRF,
                   pred.fixed = pred.fixed,
                   posterior_nsamps = posterior_nsamps,
                   job_index = job_index)


