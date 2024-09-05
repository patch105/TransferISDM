
# 7. Extract Model Results ------------------------------------------------

extract_model_results_func <- function(reps.setup.list) {
  
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
      
      for (i in seq_along(models_df)) { # Until get PA again
        
        mod.summary <- models_df[[i, "Summary"]]
        
          results_list[[length(results_list) + 1]] <- data.frame(
          extrap.type = name,
          rep = rep,
          mod.type = as.character(models_df[i, "Mod.type"]),
          beta1.mean = mod.summary[[1]]$DISTRIBUTION$mean[1],
          beta2.mean = mod.summary[[1]]$DISTRIBUTION$mean[2],
          beta1.median = mod.summary[[1]]$DISTRIBUTION[[4]][1],
          beta2.median = mod.summary[[1]]$DISTRIBUTION[[4]][2],
          beta1_25 = mod.summary[[1]]$DISTRIBUTION[[3]][1],
          beta1_975 = mod.summary[[1]]$DISTRIBUTION[[5]][1],
          beta2_25 = mod.summary[[1]]$DISTRIBUTION[[3]][2],
          beta2_975 = mod.summary[[1]]$DISTRIBUTION[[5]][2],
          PO_intercept = NA,
          PO_intercept_25 = NA,
          PO_intercept_975 = NA,
          PA_intercept = NA,
          PA_intercept_25 = NA,
          PA_intercept_975 = NA,
           marg_lik = mod.summary[[1]]$marg.lik
        )
        
          # If the model name contains PO or Integrated, save the PO intercept
        if(grepl("PO", models_df[i, "Mod.type"], fixed = T) | grepl("Integrated", models_df[i, "Mod.type"], fixed = T)) {

          results_list[[length(results_list)]]$PO_intercept <- mod.summary[[1]]$PO_BIAS$mean
          results_list[[length(results_list)]]$PO_intercept_25 <- mod.summary[[1]]$PO_BIAS[[3]]
          results_list[[length(results_list)]]$PO_intercept_975 <- mod.summary[[1]]$PO_BIAS[[5]]
          
        }  
          
          if(grepl("PA", models_df[i, "Mod.type"], fixed = T) | grepl("Integrated", models_df[i, "Mod.type"], fixed = T)) {
            
            results_list[[length(results_list)]]$PA_intercept <- mod.summary[[1]]$PA_ARTEFACT$mean
            results_list[[length(results_list)]]$PA_intercept_25 <- mod.summary[[1]]$PA_ARTEFACT[[3]]
            results_list[[length(results_list)]]$PA_intercept_975 <- mod.summary[[1]]$PA_ARTEFACT[[5]]
            
          }
          
      }
      
      
    }}
  
  # Combine all the results into a single dataframe
  extrap.scenario.df <- do.call(rbind, results_list)
  
  return(extrap.scenario.df)
  
}
  
  
  
