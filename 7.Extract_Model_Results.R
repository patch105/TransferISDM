
# 7. Extract Model Results ------------------------------------------------

extract_model_results_func <- function(reps.setup.list,
                                       mod.type,
                                       job_index) {
  
  # Extract and save summary results ----------------------------------------
  
  # Create an empty list to store results
  results_list <- list()
  
  # Get the names of the extrap types for indexing
  extrap_names <- names(reps.setup.list)
  
  # For every level of extrap type (Low, Mod, High)
  for(extrap.type in seq_along(reps.setup.list)) {
    
    # Extract the name (e.g., "Low") for indexing from the names list
    name <- extrap_names[extrap.type] 
    
    # For every replicate
    for(rep in seq_along(reps.setup.list[[name]])) {
      
      # Extract the models dataframe [[name]] double brackets for list extract
      models_df <- reps.setup.list[[name]][[rep]]$models
      
      Model <- reps.setup.list[[name]][[rep]]$models$Model
      
      # Extract the median extrapolation amount
      # BA <- reps.setup.list[[name]][[rep]]$extrap.reps.out$summary.extrap$BA
      # BD <- reps.setup.list[[name]][[rep]]$extrap.reps.out$summary.extrap$BD
      mean <- reps.setup.list[[name]][[rep]]$extrap.reps.out$summary.extrap$mean
      median <- reps.setup.list[[name]][[rep]]$extrap.reps.out$summary.extrap$median
      
      meanPA <- reps.setup.list[[name]][[rep]]$extrap.reps.out$summary.realised.extrap$meanPA
      meanPO <- reps.setup.list[[name]][[rep]]$extrap.reps.out$summary.realised.extrap$meanPO
      meanPAPO <- reps.setup.list[[name]][[rep]]$extrap.reps.out$summary.realised.extrap$meanPAPO
      
      medianPA <- reps.setup.list[[name]][[rep]]$extrap.reps.out$summary.realised.extrap$medianPA
      medianPO <- reps.setup.list[[name]][[rep]]$extrap.reps.out$summary.realised.extrap$medianPO
      medianPAPO <- reps.setup.list[[name]][[rep]]$extrap.reps.out$summary.realised.extrap$medianPAPO
      
      # Extract the distance between the sites
      Site.distance <- reps.setup.list[[name]][[rep]]$extrap.reps.out$Site.distance
      
      for (i in seq_along(Model)) { # Until get PA again
        
        mod.summary <- models_df[[i, "Summary"]]
        
          results_list[[length(results_list) + 1]] <- data.frame(
          extrap.type = name,
          mean.extrap = mean,
          median.extrap = median,
          meanPA.extrap = meanPA,
          medianPA.extrap = medianPA,
          meanPO.extrap = meanPO,
          medianPO.extrap = medianPO,
          meanPAPO.extrap = meanPAPO,
          medianPAPO.extrap = medianPAPO,
          Site.distance = Site.distance,
          rep = rep,
          job_index = job_index,
          mod.type = as.character(models_df[i, "Mod.type"]),
          beta1.mean = mod.summary[[1]]$DISTRIBUTION["cov1", "mean"],
          beta2.mean = mod.summary[[1]]$DISTRIBUTION["cov2", "mean"],
          beta1.median = mod.summary[[1]]$DISTRIBUTION[[4]][1],
          beta2.median = mod.summary[[1]]$DISTRIBUTION[[4]][2],
          beta1_25 = mod.summary[[1]]$DISTRIBUTION["cov1", "0.025quant"],
          beta1_975 = mod.summary[[1]]$DISTRIBUTION["cov1", "0.975quant"],
          beta2_25 = mod.summary[[1]]$DISTRIBUTION["cov2", "0.025quant"],
          beta2_975 = mod.summary[[1]]$DISTRIBUTION["cov2", "0.975quant"],
          beta1.sd = mod.summary[[1]]$DISTRIBUTION["cov1", "sd"],
          beta2.sd = mod.summary[[1]]$DISTRIBUTION["cov2", "sd"],
          PO_intercept = NA,
          PO_intercept_25 = NA,
          PO_intercept_975 = NA,
          PO_intercept.sd = NA,
          PA_intercept = NA,
          PA_intercept_25 = NA,
          PA_intercept_975 = NA,
          PA_intercept.sd = NA,
           marg_lik = mod.summary[[1]]$marg.lik,
          GRF.range.mean = NA,
          GRF.sd.mean = NA,
          GRF.range_25 = NA,
          GRF.range_975 = NA,
          GRF.sd_25 = NA,
          GRF.sd_975 = NA,
          GRF.range.sd = NA,
          GRF.sd.sd = NA,
          bias.coef.mean = NA,
          bias.coef_25 = NA,
          bias.coef_95 = NA,
          bias.coef.sd = NA
        )
        
          # If the model name contains PO or Integrated, save the PO intercept
        if(grepl("PO", models_df[i, "Mod.type"], fixed = T) | grepl("int", models_df[i, "Mod.type"], fixed = T)) {

          results_list[[length(results_list)]]$PO_intercept <- mod.summary[[1]]$PO_BIAS["PO_Intercept", "mean"]
          results_list[[length(results_list)]]$PO_intercept_25 <- mod.summary[[1]]$PO_BIAS["PO_Intercept", "0.025quant"]
          results_list[[length(results_list)]]$PO_intercept_975 <- mod.summary[[1]]$PO_BIAS["PO_Intercept", "0.975quant"]
          results_list[[length(results_list)]]$PO_intercept.sd <- mod.summary[[1]]$PO_BIAS["PO_Intercept", "sd"]
          
        }  
          
          if(grepl("PA", models_df[i, "Mod.type"], fixed = T) | grepl("int", models_df[i, "Mod.type"], fixed = T)) {
            
            results_list[[length(results_list)]]$PA_intercept <- mod.summary[[1]]$PA_ARTEFACT["PA_Intercept", "mean"]
            results_list[[length(results_list)]]$PA_intercept_25 <- mod.summary[[1]]$PA_ARTEFACT["PA_Intercept", "0.025quant"]
            results_list[[length(results_list)]]$PA_intercept_975 <- mod.summary[[1]]$PA_ARTEFACT["PA_Intercept", "0.975quant"]
            results_list[[length(results_list)]]$PA_intercept.sd <- mod.summary[[1]]$PA_ARTEFACT["PA_Intercept", "sd"]
            
          }
       
          # If the model is spatial, save the spatial parameters
          if(grepl("GRF", models_df[i, "Mod.type"], fixed = T)) {
            
            results_list[[length(results_list)]]$GRF.range.mean <- mod.summary[[1]]$SPATIAL$mean[1]
            results_list[[length(results_list)]]$GRF.sd.mean <- mod.summary[[1]]$SPATIAL$mean[2]
            results_list[[length(results_list)]]$GRF.range_25 <- mod.summary[[1]]$SPATIAL[[3]][1]
            results_list[[length(results_list)]]$GRF.range_975 <- mod.summary[[1]]$SPATIAL[[5]][1]
            results_list[[length(results_list)]]$GRF.sd_25 <- mod.summary[[1]]$SPATIAL[[3]][2]
            results_list[[length(results_list)]]$GRF.sd_975 <- mod.summary[[1]]$SPATIAL[[5]][2]
            results_list[[length(results_list)]]$GRF.range.sd <- mod.summary[[1]]$SPATIAL[[2]][1]
            results_list[[length(results_list)]]$GRF.sd.sd <- mod.summary[[1]]$SPATIAL[[2]][2]
          
          }
          
          # If the model has a bias covariate, save the bias coefficient
          if(grepl("bias", models_df[i, "Mod.type"], fixed = T)) {
            
            results_list[[length(results_list)]]$bias.coef.mean <- mod.summary[[1]]$PO_BIAS["PO_bias", "mean"]
            results_list[[length(results_list)]]$bias.coef_25 <- mod.summary[[1]]$PO_BIAS["PO_bias", "0.025quant"]
            results_list[[length(results_list)]]$bias.coef_95 <- mod.summary[[1]]$PO_BIAS["PO_bias", "0.975quant"]
            results_list[[length(results_list)]]$bias.coef.sd <- mod.summary[[1]]$PO_BIAS["PO_bias", "sd"]

          }
          
      }
      
      
    }}
  
  # Combine all the results into a single dataframe
  extrap.scenario.df <- do.call(rbind, results_list)
  
  return(extrap.scenario.df)
  
}
  
  
  
