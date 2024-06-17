
#### PREDICT for every extrap type, for every rep, for every model type
# Save prediction to the model list
####

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
    
    for (i in 1:2) { # NEED TO ADD BACK IN nrow(models_df) ONCE HAVE PA WORKING
      
      mod <- models_df[[i, "Model"]]
      
      # Had to add the [[1]] here because the summary is always list of length 1
      mod$preds <- predict(mod[[1]],
                           covars = cov,
                           S = 1, 
                           intercept.terms = "PO_Intercept",
                           type = "link")
      
      mod <- mod[-2]
      
      # Save the updated model back to the dataframe
      models_df[[i, "Model"]] <- mod[1]
      
    }
    
    # Save the updated models dataframe back to the original list
    extrap.reps.out.mods[[name]][[rep]]$models <<- models_df
    
    
  }
  
  }


####### TO DO: 
# Get code working for running through all the reps
# Check that the saving mods$preds back into the list works
# CHECK TYPE = "link" function

