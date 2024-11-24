
Run_Replicate_Func <- function(n_cores,
                               outpath,
                               scenario_name,
                               scenario.type,
                               ncol,
                               nrow,
                               res,
                               east_min,
                               east_max,
                               north_min,
                               north_max,
                               n_bau_east,
                               n_bau_north,
                               bau_east_step,
                               bau_north_step,
                               eastings,
                               northings,
                               coords,
                               nreps,
                               mod.type,
                               beta0,
                               beta1,
                               beta2,
                               scal,
                               variance,
                               bias,
                               add.bias.cov,
                               detect.prob,
                               maxprob,
                               latent.type,
                               pred.GRF,
                               pred.fixed,
                               posterior_nsamps,
                               job_index
) {
  
  
  
  # Set up a list to save covariates, latent dist, and extrapolation results
  reps.setup.list <- list(Low = list(), Moderate = list(), High = list())
  
  scal.list <- scal
  
  run_setup_func <- function(extrap.type, scal){
    
    # 1.Simulate Covariates ---------------------------------------------------
    
    source("1.Simulate_Covariates.R")
    # source("1B.Simulate_Covariates_Simple_Cov.R")
    
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
                                    range_cov2 = range_cov2,
                                    east_min = east_min,
                                    east_max = east_max,
                                    north_min = north_min,
                                    north_max = north_max)
    
    
    # 2. Simulate Latent Distribution -----------------------------------------
    
    source("2.Simulate_Latent_Distribution.R")
    
    response.type <<- "linear"
    latent.type <<- latent.type 
    
    print("Simulating new latent distribution")
    
    latent.list <- sim_latent_dist_func(beta0 = beta0,
                                        beta1 = beta1,
                                        beta2 = beta2,
                                        scal = scal, # pass current range
                                        variance = variance,
                                        cov1 = cov.list$cov1,
                                        cov2 = cov.list$cov2,
                                        cov1.mat = cov.list$cov1.mat,
                                        cov2.mat = cov.list$cov2.mat,
                                        cov1.df = cov.list$cov1.df,
                                        response.type = response.type,
                                        plot.mu = FALSE,
                                        plot.lg.s = FALSE,
                                        latent.type = latent.type)
    
    
    # 3.Simulate Environmental Extrapolation ------------------------------------
    
    source("3.Simulate_Enviro_Extrapolation_BA.R")
    
    # Set size of grid (number of cells) for Site A (Reference) and Site B (Target)
    # NOTE - must be smaller than total cell number in x y directions
    rast_cellsA <<- c(100, 100)
    rast_cellsB <<- c(100, 100)
    
    print("Simulating environmental extrapolation")
    
    run.extrap.list <- run_extrap_func_Spat_Auto(n_cores = n_cores,
                                                 nreps = nreps, 
                                                 rast_cellsA = rast_cellsA,
                                                 rast_cellsB = rast_cellsB,
                                                 bau_east_step = bau_east_step,
                                                 bau_north_step = bau_north_step,
                                                 eastings = eastings,
                                                 northings = northings,
                                                 cov1 = cov.list$cov1,
                                                 cov2 = cov.list$cov2,
                                                 covs = cov.list$covs,
                                                 reps.setup.list = reps.setup.list,
                                                 extrap.type = extrap.type)
    
    extrap_type <- run.extrap.list$extrap.type
    
    # Create the directory structure
    if(!dir.exists(file.path(outpath, scenario_name, extrap_type))) {
      
      dir.create(file.path(outpath, scenario_name, extrap_type), recursive = TRUE)
      
    }
    
    # Create a subfolder for each replicate
    rep_id <- length(reps.setup.list[[extrap_type]]) + 1
    rep_path <- file.path(outpath, scenario_name, extrap_type, paste0("Rep_", rep_id, "Job_", job_index))
    
    if(!dir.exists(rep_path)) {
      
      dir.create(rep_path, recursive = T)
    }
    
    
    # If it says to plot the covariates
    if(length(cov.list$covs.plot) != 0) {
      
      # Save the covariates plot
      ggsave(file.path(rep_path, "covariates_plot.png"), cov.list$covs.plot)
      
    }
    
    # Return the list of replicates
    reps.setup.list[[run.extrap.list$extrap.type]] <- c(reps.setup.list[[run.extrap.list$extrap.type]], list(list(cov.list = cov.list, latent.list = latent.list, extrap.reps.out = run.extrap.list$extrap.reps.out)))
    
    return(reps.setup.list)
    
  }
  
  
  
  #### Iterate over the setup function until you get enough replicates for each extrap type
  
  # Get the names of the extrap types for indexing
  extrap_names <- names(reps.setup.list)                            
  
  # For extrap type (low range, mod range, high range)
  for(extrap.type in seq_along(reps.setup.list)) { 
    
    # Extract the name ("Low") for indexing from the names list
    name <- extrap_names[extrap.type]
    
    scl <- scal.list[[extrap.type]]
    
    for(rep in 1:nreps) {
      
      # Run the setup function
      reps.setup.list <- run_setup_func(extrap.type = name,
                                        scal = scl)
      
      
    }
    
  }
  
  
  # 4. PO sampling ----------------------------------------------------------
  
  # PO_min
  # thin 
  
  source("4.PO_Sampling.R")
  
  reps.setup.list <- po_sampling_func(reps.setup.list = reps.setup.list,
                                      bias = bias,
                                      detect.prob = detect.prob,
                                      maxprob = maxprob,
                                      rast_cellsA = rast_cellsA)
  
  # If there are any reps with no PO data, re-run the parts 1,2,3 for those reps
  
  po <- po_checking_func(reps.setup.list)
  
  removed_counts <- po$removed_counts # List with Low = nremoved, Mod = nremoved, High = nremoved
  reps.setup.list <- po$reps.setup.list # List with any reps removed that had no PO 
  
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
  
  # Set the type of models to run
  mod.type = mod.type
  
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
                                    distributionFormula = distributionFormula,
                                    mod.type = mod.type,
                                    bias = bias)
  
  
  # 6b. Save Scenario Information -------------------------------------------
  
  scenario_info.df <- tibble(scenario_name = scenario_name,
                             scenario.type = scenario.type,
                             nreps = nreps,
                             range_cov1 = range_cov1,
                             range_cov2 = range_cov2,
                             beta0 = beta0,
                             beta1 = beta1,
                             beta2 = beta2,
                             scal = unlist(scal.list),
                             variance = variance,
                             latent.type = latent.type,
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
  
  write_csv(scenario_info.df, paste0(file.path(outpath, scenario_name), "/Scenario_", scenario_name, "Job_", job_index,"_Info.csv"))
  
  
  # 6C. Save Replicate Information ------------------------------------------
  
  # Get the names of the extrap types for indexing
  extrap_names <- names(reps.setup.list)
  
  replicate_info_ALL.df <- data.frame(scenario_name = character(),
                                      extrap.type = character(),
                                      rep = numeric(),
                                      n_po_gridA = numeric(),
                                      n_presence_gridA = numeric(),
                                      n_absence_gridA = numeric(),
                                      cor.covs = numeric())
  
  for(extrap.type in seq_along(reps.setup.list)) {
    
    # Extract the name (e.g., "Low") for indexing from the names list
    name <- extrap_names[extrap.type]
    
    for(rep in seq_along(reps.setup.list[[name]])) {
      
      # Get path for saving
      rep_path <- file.path(outpath, scenario_name, name, paste0("Rep_", rep, "Job_", job_index))
      
      replicate_info.df <- data.frame(
        scenario_name = scenario_name,
        extrap.type = name,
        rep = rep,
        n_po_gridA = reps.setup.list[[name]][[rep]]$n_po_gridA,
        n_presence_gridA = reps.setup.list[[name]][[rep]]$n_presence_gridA,
        n_absence_gridA = reps.setup.list[[name]][[rep]]$n_absence_gridA,
        cor.covs = reps.setup.list[[name]][[rep]]$cov.list$cor.covs)
      
      if(latent.type == "lgcp") {
        
        replicate_info.df <- replicate_info.df %>% 
          mutate(cor.GRF.cov1 = reps.setup.list[[name]][[rep]]$latent.list$cor.GRF.cov1,
                 cor.GRF.cov2 = reps.setup.list[[name]][[rep]]$latent.list$cor.GRF.cov2)
        
      }
      
      write_csv(replicate_info.df, paste0(rep_path, "/Replicate_Info_Scenario_", scenario_name, "_Rep_", name, "_", rep, "_Job_", job_index, ".csv"))
      
      replicate_info_ALL.df <- rbind(replicate_info_ALL.df, replicate_info.df)
    }
    
  }
  
  
  write_csv(replicate_info_ALL.df, paste0(file.path(outpath, scenario_name),  "/Scenario_", scenario_name, "_N_Presences_EACH_rep_Job", job_index,".csv"))
  
  # 7. Extract Model Results ------------------------------------------------
  
  source("7.Extract_Model_Results.R")
  
  extrap.scenario.df <- extract_model_results_func(reps.setup.list = reps.setup.list,
                                                   mod.type = mod.type)
  
  write_csv(extrap.scenario.df, paste0(file.path(outpath, scenario_name), "/Scenario_", scenario_name, "_Results_Summary_Job_", job_index, ".csv"))
  
  
  # 8. Make Truth -----------------------------------------------------------
  
  source("8.Make_Truth.R")
  
  reps.setup.list <- make_truth_func(reps.setup.list = reps.setup.list)
  
  
  # 9. Predict to Site B from fitted --------------------------------------------------
  
  source("9.Predict_from_fitted.R")
  
  reps.setup.list <- predict_from_fitted_SiteB_func(reps.setup.list = reps.setup.list,
                                                    posterior_nsamps = posterior_nsamps)
  
  
  
  # 10. Validation true intensity -------------------------------------------
  
  source("10.Validation_True_Intensity.R")
  
  true.validation.df <- validation_SiteB_func(reps.setup.list = reps.setup.list)
  
  write_csv(true.validation.df, paste0(file.path(outpath, scenario_name), "/Scenario_", scenario_name, "_True_Validation_Job_", job_index, ".csv"))
  
  
  # 12B. Plot Data  ---------------------------------------------------------
  # If I want to plot the Presence-Absence and Presence-Only data
  
  source("12B.Plot_Data.R")
  
  plot_data_func(reps.setup.list = reps.setup.list,
                 outpath = outpath,
                 scenario_name = scenario_name,
                 job_index = job_index)
  
  
  # 12C. Plot Predictions ---------------------------------------------------
  
  source("12C.Plot_Predictions.R")
  
  plot_predictions_SiteB_func(reps.setup.list = reps.setup.list,
                              outpath = outpath,
                              scenario_name = scenario_name,
                              mod.type = mod.type,
                              job_index = job_index)
  
  
  # OPTIONAL - predict to and validate Site A -------------------------------
  
  # *Optional* - predict to Site A
  ## You can choose to predict just the random effect here
  
  reps.setup.list <- predict_from_fitted_SiteA_func(reps.setup.list = reps.setup.list,
                                                    pred.GRF = pred.GRF,
                                                    pred.fixed = pred.fixed,
                                                    mod.type = mod.type,
                                                    posterior_nsamps = posterior_nsamps)
  
  plot_predictions_SiteA_func(reps.setup.list = reps.setup.list,
                              outpath = outpath,
                              scenario_name = scenario_name,
                              pred.GRF = pred.GRF,
                              pred.fixed = pred.fixed,
                              mod.type = mod.type,
                              job_index = job_index)
  
  # *Optional* - run validation for Site A
  true.validation.SiteA.df <- validation_SiteA_func(reps.setup.list = reps.setup.list,
                                                    pred.GRF = pred.GRF,
                                                    pred.fixed = pred.fixed)
  
  
  write_csv(true.validation.SiteA.df, paste0(file.path(outpath, scenario_name), "/Scenario_", scenario_name, "_True_Validation_SiteA_Job_", job_index, ".csv"))
  
}




# ARCHIVE -----------------------------------------------------------------

# # Checking number of PO and PA and rerunning as necessary -----------------
# 
# # If there are any reps with no PO data OR no PO data in PA sampling Grid A, re-run the parts 1,2,3, 4 for those reps
# 
# po_pa <- po_pa_checking_func(reps.setup.list)
# 
# removed_counts_pa <- po_pa$removed_counts_pa # List with Low = nremoved, Mod = nremoved, High = nremoved
# reps.setup.list <- po_pa$reps.setup.list # List with any reps removed that had no PO at PA sampling Grid A
# 
# 
# # Iterate over the setup function until you get to the desired nreps for Low, Moderate, High AND none of them have no PO data anymore
# 
# while(length(reps.setup.list$Low) < nreps | length(reps.setup.list$Moderate) < nreps | length(reps.setup.list$High) < nreps | sum(removed_counts_pa) != 0) {
#   
#   reps.setup.list <- run_setup_func()
#   
#   reps.setup.list <- po_sampling_func(reps.setup.list = reps.setup.list)
#   
#   reps.setup.list <- pa_sampling_func(reps.setup.list = reps.setup.list,
#                                       new.latent = FALSE) # If you want to make a separate realisation of the latent state for the PA data set to true
#   
#   # If there are any reps with no PO data, re-run the parts 1,2,3 for those reps
#   
#   po_pa <- po_pa_checking_func(reps.setup.list)
#   
#   removed_counts_pa <- po_pa$removed_counts_pa
#   reps.setup.list <- po_pa$reps.setup.list
#   
#   
# }


