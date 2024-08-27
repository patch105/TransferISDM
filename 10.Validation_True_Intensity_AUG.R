library(Metrics)


# Make a dataframe of the format I want to save the results in
true.validation.df <- data.frame(extrap.type = character(),
                                 rep = numeric(),
                                 mod.type = character(),
                                 correlation = numeric(),
                                 MAE = numeric(),
                                 RMSE = numeric(),
                                 Sum.Int.Score = numeric(),
                                 Mean.Int.Score = numeric())


# Get the names of the extrap types for indexing
extrap_names <- names(extrap.reps.out.mods)

# For every level of extrap type (Low, Mod, High)
for(extrap.type in seq_along(extrap.reps.out.mods)) {
  
  # Extract the name ("Low") for indexing from the names list
  name <- extrap_names[extrap.type] 
  
  # For every replicate
  for(rep in seq_along(extrap.reps.out.mods[[name]])) {
    
    # Extract the models dataframe [[name]] double brackets for list extract
    models_df <- extrap.reps.out.mods[[name]][[rep]]$models
    
    # Crop out the true log intensity from the Site B
    rand.gridB <- extrap.reps.out.mods[[name]][[rep]]$rand.gridB
    true_log_int.rast.SiteB <- crop(true_log_int.rast, ext(rand.gridB))
    
    for (i in 1:2) { # NEED TO ADD BACK IN nrow(models_df) ONCE HAVE PA WORKING
      
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
    
      # Save results to dataframe
      true.validation.df <<- true.validation.df %>% 
        add_row(extrap.type = name,
                rep = rep,
                mod.type = as.character(models_df[i, "Mod.type"]),
                correlation = cor,
                MAE = MAE,
                RMSE = RMSE,
                Sum.Int.Score = Sum.Int.Score,
                Mean.Int.Score = Mean.Int.Score)
                
      
    }
    
  }
  
}


# write_csv(true.validation.df, filename = paste0("/output/Validation_Extrap_Low_PO_INT.no.bias.2.GRF.cov.csv"))


