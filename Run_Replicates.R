
## TO DO: ADD PLOTTING COV AND SITES AND AND B BACK INTO THE EXTRAP FUNCTION


library(spatstat)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(viridis)
library(terra)
library(purrr)


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
nreps <- 1

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
                                  seed = NA,
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

# PO_min
# thin 

source("4.PO_Sampling.R")

reps.setup.list <- po_sampling_func(reps.setup.list = reps.setup.list)

# If there are any reps with no PO data, re-run the parts 1,2,3 for those reps

po <- po_checking_func(reps.setup.list)

removed_counts <- po$removed_counts
reps.setup.list <- po$reps.setup.list

# Iterate over the setup function until you get to the desired nreps for Low, Moderate, High AND none of them have no PO data anymore

while(length(reps.setup.list$Low) < nreps | length(reps.setup.list$Moderate) < nreps | length(reps.setup.list$High) < nreps | sum(removed_counts) != 0) {
  
  reps.setup.list <- run_setup_func()
  
  reps.setup.list <- po_sampling_func(reps.setup.list = reps.setup.list)
  
  # If there are any reps with no PO data, re-run the parts 1,2,3 for those reps
  
  po <- po_checking_func(reps.setup.list)
  
  removed_counts <- po$removed_counts
  reps.setup.list <- po$reps.setup.list
  
  
}



# 5. PA sampling ----------------------------------------------------------

source("5.PA_Sampling.R")

reps.setup.list <- pa_sampling_func(reps.setup.list = reps.setup.list)



# 6. Run Models -----------------------------------------------------------

source("6.Run_Model.R")

# Set model control parameters
prior.mean <- 0
int.sd <- 1000 # Intercept standard deviation
other.sd <- 10 # Covariate effect standard deviation
prior.range <- c(1, 0.1) # Prior chance 10% that parameter falls below range of 1km
prior.space.sigma <- c(5, 0.1) # Prior chance 10% that parameter falls above SD of 5

# Set mesh parameters
max.n <- c(5000, 2500) # Default c(500,200)
dep.range <- NULL # In raster projection units, default is 1/3 diagonal length of raster extent
expans.mult <- 1.5 # Default, 1.5 x dep.range                         
max.edge <- NULL # Default c(0.2, 0.5)*dep.range                         
cutoff <- NULL # Default 0.2*max.edge1                         
offset <- NULL # Default is dep.range                       
doPlot <- FALSE                       

# Set the distribution formula for the model
distributionFormula <- ~0 + cov1 + cov2 # Linear w two covs

reps.setup.list <- run_model_func(prior.mean = prior.mean,
                                  int.sd = int.sd, 
                                  other.sd = other.sd, 
                                  prior.range = prior.range, 
                                  prior.space.sigma = prior.space.sigma, 
                                  reps.setup.list = reps.setup.list,
                                  max.n = max.n,
                                  dep.range = dep.range,
                                  expans.mult = expans.mult,
                                  max.edge = max.edge,
                                  cutoff = cutoff,
                                  offset = offset,
                                  doPlot = doPlot,
                                  distributionFormula = distributionFormula)


# 7. Extract Model Results ------------------------------------------------

source("7.Extract_Model_Results.R")

extrap.scenario.df <- extract_model_results_func(reps.setup.list = reps.setup.list)

write_csv(extrap.scenario.df, paste0(outpath, "/output/Summary_Extrap_PO_INT_PA.no.bias.no.GRF.csv"))



# 8. Make Truth -----------------------------------------------------------

source("8.Make_Truth.R")

make_truth_func(reps.setup.list = reps.setup.list)


  

