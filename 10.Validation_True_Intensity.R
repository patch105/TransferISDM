
library(Metrics)
library(scoringutils)

# 10. Validation true intensity -------------------------------------------

validation_func <- function(reps.setup.list) {
  
  # Create an empty list to store results
  results_list <- list()
  
  # Get the names of the extrap types for indexing
  extrap_names <- names(reps.setup.list)
  
  # For every level of extrap type (Low, Mod, High)
  for(extrap.type in seq_along(reps.setup.list)) {
    
    # Extract the name ("Low") for indexing from the names list
    name <- extrap_names[extrap.type] 
    
    # For every replicate
    for(rep in seq_along(reps.setup.list[[name]])) {
      
      # Extract the models dataframe [[name]] double brackets for list extract
      models_df <- reps.setup.list[[name]][[rep]]$models
      
      # Crop out the true log intensity from the Site B
      rand.gridB <- reps.setup.list[[name]][[rep]]$extrap.reps.out$rand.gridB
      true_log_int.rast <- reps.setup.list[[name]][[rep]]$true_log_int.rast
      true_log_int.rast.SiteB <- crop(true_log_int.rast, ext(rand.gridB))
      
      for (i in seq_along(models_df)) { # NEED TO ADD BACK IN ONCE HAVE PA WORKING
        
        mod <- models_df[[i, "Model"]]
        
        # Pull out the mean intensity prediction for each cell
        mean.int.pred <- mod[[1]]$preds$field$Mean
        
        # Pull out the lower and upper bounds of the prediction
        lower.int.pred <- mod[[1]]$preds$field$Lower
        
        upper.int.pred <- mod[[1]]$preds$field$Upper
        
        # Metrics from Simmonds et al. 
        # Compare the predicted intensity to the true intensity 
        cor <- cor(as.vector(mean.int.pred), as.vector(true_log_int.rast.SiteB))
        
        MAE <- mean(abs(as.vector(mean.int.pred - true_log_int.rast.SiteB)))
        
        RMSE <- Metrics::rmse(actual = as.vector(true_log_int.rast.SiteB), 
                              predicted = as.vector(mean.int.pred))
        
        ### Calculating the Interval Score ###
        
        interval_score <- interval_score(true_values = as.vector(true_log_int.rast.SiteB),
                                         lower = as.vector(lower.int.pred), 
                                         upper = as.vector(upper.int.pred),
                                         interval_range = 95,
                                         weigh = TRUE)
        
        Sum.Int.Score <- sum(interval_score)
        
        Mean.Int.Score <- mean(interval_score)
        
        # Save results to list
        
        results_list[[length(results_list) + 1]] <- data.frame(
          extrap.type = name,
          rep = rep,
          mod.type = as.character(models_df[i, "Mod.type"]),
          correlation = cor,
          MAE = MAE,
          RMSE = RMSE,
          Sum.Int.Score = Sum.Int.Score,
          Mean.Int.Score = Mean.Int.Score
        )
        
      }}
    
  }
  
  # Combine all the results into a single dataframe
  true.validation.df <- do.call(rbind, results_list)
  
  return(true.validation.df)
  
}
