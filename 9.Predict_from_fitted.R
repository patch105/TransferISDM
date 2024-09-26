

# 9. Predict from fitted --------------------------------------------------

predict_from_fitted_SiteB_func <- function(reps.setup.list,
                                           posterior_nsamps) {
                                                    
  #### PREDICT for every extrap type, for every rep, for every model type
  # Save prediction to the model list
  ####
  
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
      
      # Load covariate
      cov.rep <- reps.setup.list[[name]][[rep]]$extrap.reps.out$covs.SiteB.rast
      
      for (i in seq_along(models_df)) { # NEED TO ADD BACK IN nrow(models_df) ONCE HAVE PA WORKING
        
        mod <- models_df[[i, "Model"]]
        
        type <- models_df[[i, "Mod.type"]]
        
        if(grepl("PO", type, fixed = T)) { # If models are PO, use PO intercept
          
          # Had to add the [[1]] here because the summary is always list of length 1
          mod[[1]]$preds.link.siteB <- predict(mod[[1]],
                                               covars = cov.rep,
                                               S = posterior_nsamps, 
                                               intercept.terms = "PO_Intercept",
                                               type = "link",
                                               includeRandom = F)
          
          # Save the updated model back to the dataframe
          models_df[[i, "Model"]] <- mod
          
        } else { # If models are PA or Integrated, use PA intercept
          
          # Had to add the [[1]] here because the summary is always list of length 1
          mod[[1]]$preds.link.siteB <- predict(mod[[1]],
                                               covars = cov.rep,
                                               S = posterior_nsamps, 
                                               intercept.terms = "PA_Intercept",
                                               type = "link",
                                               includeRandom = F)
          
          # Save the updated model back to the dataframe
          models_df[[i, "Model"]] <- mod
          
        }
      }
      
      # Save the updated models dataframe back to the original list
      reps.setup.list[[name]][[rep]]$models <- models_df
      
    }
  }
  
  return(reps.setup.list)
  
}

# OPTIONAL - RUN THE PREDICTION FOR SITE A --------------------------------

predict_from_fitted_SiteA_func <- function(reps.setup.list,
                                           pred.GRF = FALSE,
                                           pred.fixed = FALSE,
                                           mod.type = "no-GRF",
                                           posterior_nsamps) {
  
  #### PREDICT for every extrap type, for every rep, for every model type
  # Save prediction to the model list
  ####
  
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
      
      # Load covariate
      cov.rep <- reps.setup.list[[name]][[rep]]$extrap.reps.out$covs.SiteA.rast
      
      for (i in seq_along(models_df)) { # NEED TO ADD BACK IN nrow(models_df) ONCE HAVE PA WORKING
        
        mod <- models_df[[i, "Model"]]
        
        type <- models_df[[i, "Mod.type"]]
        
        ######################
        ### SPATIAL MODEL W GRF
        ######################
        
        if(grepl("GRF", type, fixed = T)) { # If model is spatial 
          
          if(grepl("PO", type, fixed = T)) { # If models are PO, use PO intercept
            
            # Had to add the [[1]] here because the summary is always list of length 1
            mod[[1]]$preds.link.siteA <- predict(mod[[1]],
                                                 covars = cov.rep,
                                                 S = posterior_nsamps, 
                                                 intercept.terms = "PO_Intercept",
                                                 type = "link",
                                                 includeRandom = T)
            
            # Save the updated model back to the dataframe
            models_df[[i, "Model"]] <- mod
            
          } else { # If models are PA or Integrated, use PA intercept
            
            # Had to add the [[1]] here because the summary is always list of length 1
            mod[[1]]$preds.link.siteA <- predict(mod[[1]],
                                                 covars = cov.rep,
                                                 S = posterior_nsamps, 
                                                 intercept.terms = "PA_Intercept",
                                                 type = "link",
                                                 includeRandom = T)
            
            # Save the updated model back to the dataframe
            models_df[[i, "Model"]] <- mod
            
          }
        }
        
          ######################
          ### NON-SPATIAL MODEL
          ######################
          
        if(!grepl("GRF",  type, fixed = T)) { # If there's NO GRF
          
          if(grepl("PO", type, fixed = T)) { # If models are PO, use PO intercept
            
            # Had to add the [[1]] here because the summary is always list of length 1
            mod[[1]]$preds.link.siteA <- predict(mod[[1]],
                                                 covars = cov.rep,
                                                 S = posterior_nsamps, 
                                                 intercept.terms = "PO_Intercept",
                                                 type = "link",
                                                 includeRandom = F)
            
            # Save the updated model back to the dataframe
            models_df[[i, "Model"]] <- mod
            
          } else { # If models are PA or Integrated, use PA intercept
            
            # Had to add the [[1]] here because the summary is always list of length 1
            mod[[1]]$preds.link.siteA <- predict(mod[[1]],
                                                 covars = cov.rep,
                                                 S = posterior_nsamps, 
                                                 intercept.terms = "PA_Intercept",
                                                 type = "link",
                                                 includeRandom = F)
            
            # Save the updated model back to the dataframe
            models_df[[i, "Model"]] <- mod
            
          }
          
        }
        

        ###########
        ### If also plotting random effect at Site A
        ###########
        
        if(pred.GRF == TRUE & grepl("GRF", type, fixed = T)) {
          
          if(grepl("PO", type, fixed = T)) { # If models are PO, use PO intercept
            
            # Had to add the [[1]] here because the summary is always list of length 1
            mod[[1]]$preds.GRF.siteA <- predict(mod[[1]],
                                                 covars = cov.rep,
                                                 S = posterior_nsamps, 
                                                 intercept.terms = "PO_Intercept",
                                                 type = "link",
                                                 includeRandom = T,
                                                includeFixed = F)
            
            # Save the updated model back to the dataframe
            models_df[[i, "Model"]] <- mod
            
          } else { # If models are PA or Integrated, use PA intercept
            
            # Had to add the [[1]] here because the summary is always list of length 1
            mod[[1]]$preds.GRF.siteA <- predict(mod[[1]],
                                                 covars = cov.rep,
                                                 S = posterior_nsamps, 
                                                 intercept.terms = "PA_Intercept",
                                                 type = "link",
                                                 includeRandom = T,
                                                includeFixed = F)
            
            # Save the updated model back to the dataframe
            models_df[[i, "Model"]] <- mod
            
          }
          
          
        }

        ###########
        ### If also plotting fixed effect at Site A
        ###########
        
        if(pred.fixed == TRUE & grepl("GRF", type, fixed = T)) {
          
          if(grepl("PO", type, fixed = T)) { # If models are PO, use PO intercept
            
            # Had to add the [[1]] here because the summary is always list of length 1
            mod[[1]]$preds.FIXED.siteA <- predict(mod[[1]],
                                                covars = cov.rep,
                                                S = posterior_nsamps, 
                                                intercept.terms = "PO_Intercept",
                                                type = "link",
                                                includeRandom = F,
                                                includeFixed = T)
            
            # Save the updated model back to the dataframe
            models_df[[i, "Model"]] <- mod
            
          } else { # If models are PA or Integrated, use PA intercept
            
            # Had to add the [[1]] here because the summary is always list of length 1
            mod[[1]]$preds.FIXED.siteA <- predict(mod[[1]],
                                                covars = cov.rep,
                                                S = posterior_nsamps, 
                                                intercept.terms = "PA_Intercept",
                                                type = "link",
                                                includeRandom = F,
                                                includeFixed = T)
            
            # Save the updated model back to the dataframe
            models_df[[i, "Model"]] <- mod
            
          }
          
          
        }
        
      
      # Save the updated models dataframe back to the original list
      reps.setup.list[[name]][[rep]]$models <- models_df
      
    }
  } }
  
  return(reps.setup.list)
  
}



