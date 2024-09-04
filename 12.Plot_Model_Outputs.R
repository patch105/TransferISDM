
plot_residuals_func <- function(reps.setup.list, 
                                outpath,
                                scenario_name) {
  
  # Get the names of the extrap types for indexing
  extrap_names <- names(reps.setup.list)
  
  for(extrap.type in seq_along(reps.setup.list)) {
    
    # Extract the name ("Low") for indexing from the names list
    name <- extrap_names[extrap.type]
    
    # For every replicate
    for(rep in seq_along(reps.setup.list[[name]])) {
      
      # Get path for saving
      rep_path <- file.path(outpath, scenario_name, name, paste0("Rep_", rep))
      
      # Extract the models dataframe [[name]] double brackets for list extract
      models_df <- reps.setup.list[[name]][[rep]]$models
      
      for (i in seq_along(models_df)) {
        
        png(paste0(rep_path, "/plot.isdm_EXTRAP_", extrap_names[extrap.type], "_", as.character(models_df[i, "Mod.type"]),  ".png"), width = 10, height = 10, units = "in", res = 300)
        
        plot(models_df[[i, 'Model']], nFigRow = 2, ask = FALSE)
        
        dev.off()
        
      }
      
    }
    
  }
  
}

