
# 7. Extract Model Results ------------------------------------------------

extract_model_results_func <- function(reps.setup.list,
                                       mod.type) {
  
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
      extrap.median <- reps.setup.list[[name]][[rep]]$extrap.reps.out$summary.extrap$median
      
      for (i in seq_along(Model)) { # Until get PA again
        
        mod.summary <- models_df[[i, "Summary"]]
        
          results_list[[length(results_list) + 1]] <- data.frame(
          extrap.type = name,
          extrap.median = extrap.median,
          rep = rep,
          mod.type = as.character(models_df[i, "Mod.type"]),
          beta1.mean = mod.summary[[1]]$DISTRIBUTION["cov1", "mean"],
          beta2.mean = mod.summary[[1]]$DISTRIBUTION["cov2", "mean"],
          beta1.median = mod.summary[[1]]$DISTRIBUTION[[4]][1],
          beta2.median = mod.summary[[1]]$DISTRIBUTION[[4]][2],
          beta1_25 = mod.summary[[1]]$DISTRIBUTION["cov1", "0.025quant"],
          beta1_975 = mod.summary[[1]]$DISTRIBUTION["cov1", "0.975quant"],
          beta2_25 = mod.summary[[1]]$DISTRIBUTION["cov2", "0.025quant"],
          beta2_975 = mod.summary[[1]]$DISTRIBUTION["cov2", "0.975quant"],
          PO_intercept = NA,
          PO_intercept_25 = NA,
          PO_intercept_975 = NA,
          PA_intercept = NA,
          PA_intercept_25 = NA,
          PA_intercept_975 = NA,
           marg_lik = mod.summary[[1]]$marg.lik,
          GRF.range.mean = NA,
          GRF.sd.mean = NA,
          GRF.range_25 = NA,
          GRF.range_975 = NA,
          GRF.sd_25 = NA,
          GRF.sd_975 = NA,
          bias.coef.mean = NA,
          bias.coef_25 = NA,
          bias.coef_95 = NA
        )
        
          # If the model name contains PO or Integrated, save the PO intercept
        if(grepl("PO", models_df[i, "Mod.type"], fixed = T) | grepl("int", models_df[i, "Mod.type"], fixed = T)) {

          results_list[[length(results_list)]]$PO_intercept <- mod.summary[[1]]$PO_BIAS["PO_Intercept", "mean"]
          results_list[[length(results_list)]]$PO_intercept_25 <- mod.summary[[1]]$PO_BIAS["PO_Intercept", "0.025quant"]
          results_list[[length(results_list)]]$PO_intercept_975 <- mod.summary[[1]]$PO_BIAS["PO_Intercept", "0.975quant"]
          
        }  
          
          if(grepl("PA", models_df[i, "Mod.type"], fixed = T) | grepl("int", models_df[i, "Mod.type"], fixed = T)) {
            
            results_list[[length(results_list)]]$PA_intercept <- mod.summary[[1]]$PA_ARTEFACT["PA_Intercept", "mean"]
            results_list[[length(results_list)]]$PA_intercept_25 <- mod.summary[[1]]$PA_ARTEFACT["PA_Intercept", "0.025quant"]
            results_list[[length(results_list)]]$PA_intercept_975 <- mod.summary[[1]]$PA_ARTEFACT["PA_Intercept", "0.975quant"]
            
          }
       
          # If the model is spatial, save the spatial parameters
          if(grepl("GRF", models_df[i, "Mod.type"], fixed = T)) {
            
            results_list[[length(results_list)]]$GRF.range.mean <- mod.summary[[1]]$SPATIAL$mean[1]
            results_list[[length(results_list)]]$GRF.sd.mean <- mod.summary[[1]]$SPATIAL$mean[2]
            results_list[[length(results_list)]]$GRF.range_25 <- mod.summary[[1]]$SPATIAL[[3]][1]
            results_list[[length(results_list)]]$GRF.range_975 <- mod.summary[[1]]$SPATIAL[[5]][1]
            results_list[[length(results_list)]]$GRF.sd_25 <- mod.summary[[1]]$SPATIAL[[3]][2]
            results_list[[length(results_list)]]$GRF.sd_975 <- mod.summary[[1]]$SPATIAL[[5]][2]
          
          }
          
          # If the model has a bias covariate, save the bias coefficient
          if(grepl("bias", models_df[i, "Mod.type"], fixed = T)) {
            
            results_list[[length(results_list)]]$bias.coef.mean <- mod.summary[[1]]$PO_BIAS["PO_bias", "mean"]
            results_list[[length(results_list)]]$bias.coef_25 <- mod.summary[[1]]$PO_BIAS["PO_bias", "0.025quant"]
            results_list[[length(results_list)]]$bias.coef_95 <- mod.summary[[1]]$PO_BIAS["PO_bias", "0.975quant"]

          }
          
      }
      
      
    }}
  
  # Combine all the results into a single dataframe
  extrap.scenario.df <- do.call(rbind, results_list)
  
  return(extrap.scenario.df)
  
}
  
  
  
