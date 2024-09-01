## TO DO - NEED TO CALL MODELS BY NAME NOT BY 1:3, WHAT IF I HAD PO AND PA AND NO INTEGRATED? NAMES WOULD BE WRONG?

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
      
      for (i in 1:3) { # Until get PA again
        
        mod.summary <- models_df[[i, "Summary"]]
        
        # Had to add the [[1]] here because the summary is always list of length 1
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
          marg_lik = mod.summary[[1]]$marg.lik
        )
        
      }
      
      
    }}
  
  # Combine all the results into a single dataframe
  extrap.scenario.df <- do.call(rbind, results_list)
  
  return(extrap.scenario.df)
  
}
  
  
  
