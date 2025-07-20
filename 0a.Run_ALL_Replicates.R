
########################################################################
############### Run all replicates for a given scenario ################
########################################################################

# The script is the baseline for running a scenario.

# Name the scenario and set the scenario specifications and parameters.

# Set up the domain and grid resolution for the scenario.

# Key choices include the scenario type: environmental dissimilarity or spatial autocorrelation and whether bias is present.

########################################################################


# Set the library for packages to maintain consistency with HPC and non-HPC script
lib_loc = .libPaths() 

# Set job index for non-HPC script
job_index <- 1

library(spatstat)
library(ggplot2)
library(dplyr)
library(ggpubr, lib.loc = lib_loc)
library(viridis)
library(terra)
library(purrr)
library(readr)
# # Install flexsdm
# remotes::install_github("sjevelazco/flexsdm")
library(flexsdm, lib.loc = lib_loc)

# Scenario choices --------------------------------------------------------

# Name the scenario
scenario_name = "Scenario_X"

# Select scenario type: "Enviro.Extrap" or "Spatial.Auto"
scenario.type = "Enviro.Extrap"

# Replicates per scenario level
nreps <- 1 

# Spatial autocorrelation? 
#"lgcp" for spatial autocorrelation or "ipp" for no spatial-autocorrelation
#NOTE - the scenario can be scenario.type == "Enviro.Extrap" and latent.type == "lgcp" - the scenario will be environmental dissimilarity with spatial autocorrelation 

latent.type = "ipp" 

# Bias in PO sampling?
bias <- FALSE


# Model choices -----------------------------------------------------------

# Model types to run
# Options are "no-GRF", "spatial", "no-GRF.bias", "spatial.bias"
# "no-GRF" means no Gaussian random field
# "spatial means with Gaussian random field
# .bias means model with a bias covariate 

mod.type = c("no-GRF")


# If doing a spatial model, choose whether to make a separate prediction of the GRF and the Fixed effect for evaluation 

pred.GRF <- FALSE
pred.fixed <- FALSE


# Parameters --------------------------------------------------------------

# Autocorrelation range & variance for 2 covariates 
#(Maximum range (raster units) of spatial autocorrelation)

range_cov1 <- 10 
range_cov2 <- 100 
var_cov1 <- 0.5
var_cov2 <- 10

beta0 <- -1 # Species distribution intercept
beta1 <- 0.01 # Coefficient for cov 1
beta2 <- 0.2 # Coefficient for cov 2

# If the scenario is spatial autocorrelation, set the range parameter for spatial autocorrelation of the species distribution to vary across 3 range values

if(scenario.type == "Spatial.Auto") {
  
  scal <- list(20, 300, 700) # List of range parameters for spatial autocorr scenario
  
} else {
  
  scal <- 20 # Otherwise set to this value
  
}

# Relative variance of GRF to fixed effect (multiplied)
# Controls how influential the random effect is in the species distribution relative to the fixed effect 
GRF.var.multiplier <- 1


# Maximum probability for bias field 
maxprob <- 0.2

# PO sampling detection probability - chosen to be the mean of the bias field to keep bias and non-bias scenarios consistent.
detect.prob <- 0.08


# Implementation choices --------------------------------------------------

n_cores <- 3 # Number of cores to use for parallel processing. The flexsdm::extra_eval function can utilise parallel processing. If available on your system, this can speed up the calculation of the Shape metric for environmental dissimilarit in the script 3.Simulate_Enviro_Extrapolation.R but it's only used there. 

# Number of posterior samples to take * note that it slows things down when more
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

ncol <- 1000
nrow <- 1000
res <- 1

# Create a bounded domain on [0, 1000] x [0, 1000]

east_min <- 0
east_max <- 1000
north_min <- 0
north_max <- 1000


# We generate the grid resolution from min, max dimensions and the number of pixels

# Set number of pixels (1000 x 1000)
n_bau_east <- ncol
n_bau_north <- nrow

# Calculate the cell resolution
bau_east_step <- (east_max - east_min) / n_bau_east
bau_north_step <- (north_max - north_min) / n_bau_north 

# Generate grid centroid coordinates
# We do this so that our centroid begins in the centre of a cell (hence, bau_east_step/2))

eastings <- seq(east_min + bau_east_step/2, east_max - bau_east_step/2, by = bau_east_step)
northings <- seq(north_min + bau_north_step/2, north_max - bau_north_step/2, by = bau_north_step)

coords <- as.matrix(expand.grid(eastings, northings))
colnames(coords) <- c("eastings", "northings")


# Run setup for replicates ------------------------------------------------

# Depending on your scenario type (spatial autocorrelation or not), source one of two R scripts for running replicates

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

# Run the scenario --------------------------------------------------------

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




