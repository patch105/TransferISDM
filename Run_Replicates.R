
library(spatstat)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(viridis)
library(terra)
library(purrr)
library(readr)


# OUTPUT FOLDER & SCENARIO NAME -------------------------------------------

outpath <- file.path(getwd(), "output")

scenario_name = "Test_Sep_5_IPP"

# Make dir if not already there
if(!dir.exists(file.path(outpath, scenario_name))) {
  
  dir.create(file.path(outpath, scenario_name), recursive = TRUE)
  
}

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
  range_cov1 <<- 10 # Maximum range (raster units) of spatial autocorrelation
  
  # Set autocorrelation range Cov2
  range_cov2 <<- 100 # Maximum range (raster units) of spatial autocorrelation
  
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
  
  beta0 <<- 6 # Intercept
  beta1 <<- 0.5 # Coefficient for cov 1
  beta2 <<- 0.1 # Coefficient for cov 2
  
  scal <<- 0.2 # Scale parameter (range of spatial effect)
  
  response.type <<- "linear"
  
  print("Simulating new latent distribution")
  
  latent.list <- sim_latent_dist_func(beta0 = beta0,
                                      beta1 = beta1,
                                      beta2 = beta2,
                                      scal = scal,
                                      cov1 = cov.list$cov1,
                                      cov1.mat = cov.list$cov1.mat,
                                      cov2.mat = cov.list$cov2.mat,
                                      cov1.df = cov.list$cov1.df,
                                      response.type = response.type,
                                      plot.mu = FALSE,
                                      plot.lg.s = FALSE,
                                      latent.type = "ipp")
  
  
  # Simulate Environmental Extrapolation ------------------------------------
  
  source("3.Simulate_Enviro_Extrapolation.R")
  
  # Set size of grid (number of cells) for Site A (Reference) and Site B (Target)
  # NOTE - must be smaller than total cell number in x y directions
  rast_cellsA <<- c(50, 50)
  rast_cellsB <<- c(50, 50)
  
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
  
  extrap_type <- run.extrap.list$extrap.type
  
  # Create the directory structure
  if(!dir.exists(file.path(outpath, scenario_name, extrap_type))) {
    
    dir.create(file.path(outpath, scenario_name, extrap_type), recursive = TRUE)
    
  }
  
  # Create a subfolder for each replicate
  rep_id <- length(reps.setup.list[[extrap_type]]) + 1
  rep_path <- file.path(outpath, scenario_name, extrap_type, paste0("Rep_", rep_id))
  
  if(!dir.exists(rep_path)) {
    
    dir.create(rep_path, recursive = T)
  }
  
  
  # If it says to plot the covariates
  if(length(cov.list$covs.plot) != 0) {
    
    # Save the covariates plot
    ggsave(file.path(rep_path, "covariates_plot.png"), cov.list$covs.plot)
    
  }
  
  # If it says to plot the mu distribution
  if(length(latent.list$mu.plot) != 0) {
    
    # Save the covariates plot
    ggsave(file.path(rep_path, "mu_plot.png"), latent.list$mu.plot)
    
  }
  
  # If it says to plot the latent distribution
  if(length(latent.list$lg.s.plot) != 0) {
    
    # Save the covariates plot
    ggsave(file.path(rep_path, "mu_plot.png"), latent.list$lg.s.plot)
    
  }
  
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

reps.setup.list <- pa_sampling_func(reps.setup.list = reps.setup.list,
                                    new.latent = FALSE) # If you want to make a separate realisation of the latent state for the PA data set to true



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

reps.setup.list <- run_model_func(reps.setup.list = reps.setup.list,
                                  prior.mean = prior.mean,
                                  int.sd = int.sd, 
                                  other.sd = other.sd, 
                                  prior.range = prior.range, 
                                  prior.space.sigma = prior.space.sigma, 
                                  max.n = max.n,
                                  dep.range = dep.range,
                                  expans.mult = expans.mult,
                                  max.edge = max.edge,
                                  cutoff = cutoff,
                                  offset = offset,
                                  doPlot = doPlot,
                                  distributionFormula = distributionFormula)



# 6b. Save Scenario Information -------------------------------------------

scenario_info.df <- tibble(scenario_name = scenario_name,
                               nreps = nreps,
                               range_cov1 = range_cov1,
                               range_cov2 = range_cov2,
                               beta0 = beta0,
                               beta1 = beta1,
                               beta2 = beta2,
                               scal = scal,
                               response.type = response.type,
                               rast_cellsA = rast_cellsA[1],
                               rast_cellsB = rast_cellsB[1],
                               prior.mean = prior.mean,
                               int.sd = int.sd,
                               other.sd = other.sd,
                               prior.range = paste0(as.character(prior.range[1]), "_", as.character(prior.range[2])),
                               prior.space.sigma = paste0(as.character(prior.space.sigma[1]), "_", as.character(prior.space.sigma[2])),
                               max.n = paste0(as.character(max.n[1]), "_", as.character(max.n[2])),
                               dep.range = NA,
                               expans.mult = expans.mult,
                               max.edge = NA,
                               cutoff = NA,
                               offset = NA)

write_csv(scenario_info.df, paste0(file.path(outpath, scenario_name), "/Scenario_", scenario_name ,"_Info.csv"))
                               

# 6C. Save Replicate Information ------------------------------------------

# Get the names of the extrap types for indexing
extrap_names <- names(reps.setup.list)

for(extrap.type in seq_along(reps.setup.list)) {
  
  # Extract the name (e.g., "Low") for indexing from the names list
  name <- extrap_names[extrap.type]
  
  for(rep in seq_along(reps.setup.list[[name]])) {
    
    # Get path for saving
    rep_path <- file.path(outpath, scenario_name, name, paste0("Rep_", rep))
    
    replicate_info.df <- data.frame(
      scenario_name = scenario_name,
      extrap.type = name,
      rep = rep,
      n_po_gridA = reps.setup.list[[name]][[rep]]$n_po_gridA,
      n_presence_gridA = reps.setup.list[[name]][[rep]]$n_presence_gridA,
      n_absence_gridA = reps.setup.list[[name]][[rep]]$n_absence_gridA)
    
    write_csv(replicate_info.df, paste0(rep_path, "/Replicate_Info_Scenario_", scenario_name, "_Rep_", name, "_", rep, ".csv"))
    
  }
  
}


# 7. Extract Model Results ------------------------------------------------

source("7.Extract_Model_Results.R")

extrap.scenario.df <- extract_model_results_func(reps.setup.list = reps.setup.list)

write_csv(extrap.scenario.df, paste0(file.path(outpath, scenario_name), "/Scenario_", scenario_name, "_Results_Summary.csv"))


# 8. Make Truth -----------------------------------------------------------

source("8.Make_Truth.R")

reps.setup.list <- make_truth_func(reps.setup.list = reps.setup.list)


# 9. Predict from fitted --------------------------------------------------

source("9.Predict_from_fitted.R")

reps.setup.list <- predict_from_fitted_SiteB_func(reps.setup.list = reps.setup.list)


# 10. Validation true intensity -------------------------------------------

source("10.Validation_True_Intensity.R")

true.validation.df <- validation_SiteB_func(reps.setup.list = reps.setup.list)


# 11. Plot validation true intensity --------------------------------------

source("11.Plot_Validation_True_Intensity.R")

plot_validation_SiteB_func(true.validation.df = true.validation.df,
                           save = TRUE,
                           outpath = outpath,
                           scenario_name = scenario_name)  


# 12. Plot Model Outputs --------------------------------------------------

source("12.Plot_Model_Outputs.R")
# 
# plot_residuals_func(reps.setup.list = reps.setup.list,
#                     outpath = outpath,
#                     scenario_name = scenario_name)

plot_parameter_recovery_func(reps.setup.list = reps.setup.list,
                             outpath = outpath,
                             scenario_name = scenario_name,
                             extrap.scenario.df = extrap.scenario.df,
                             save = TRUE,
                             beta1 = beta1,
                             beta2 = beta2,
                             beta0 = beta0)

# 12B. Plot Data  ---------------------------------------------------------
# If I want to plot the Presence-Absence and Presence-Only data

source("12B.Plot_Data.R")

plot_data_func(reps.setup.list = reps.setup.list,
               outpath = outpath,
               scenario_name = scenario_name)


# 12C. Plot Predictions ---------------------------------------------------

source("12C.Plot_Predictions.R")

plot_predictions_SiteB_func(reps.setup.list = reps.setup.list,
                            pred.type = c("link"),
                            outpath = outpath,
                            scenario_name = scenario_name)



# OPTIONAL - predict to and validate Site A -------------------------------

# *Optional* - predict to Site A

reps.setup.list <- predict_from_fitted_SiteA_func(reps.setup.list = reps.setup.list)

# *Optional* - run validation for Site A
true.validation.SiteA.df <- validation_SiteA_func(reps.setup.list = reps.setup.list)

plot_validation_SiteA_func(true.validation.df = true.validation.SiteA.df,
                           save = TRUE,
                           outpath = outpath,
                           scenario_name = scenario_name)

plot_predictions_SiteA_func(reps.setup.list = reps.setup.list,
                            pred.type = c("link"),
                            outpath = outpath,
                            scenario_name = scenario_name)

