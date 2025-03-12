
########################################################################
########################## 9. Predict from fitted #####################
########################################################################

# This script takes all fitted models and predicts the species intensity at the projection site (Site B) and the training site (Site A)

# Prediction are made with the RISDM predict() function by taking draws from the posterior distribution of the model parameters and any spatial random effect

# If the model includes a Gaussian random field, this will be included in the prediction at the training site, but not the projection site

# If the model includes a bias covariate, this will not be included in any predictions

# The model must include either a PO or PA intercept. In the ISDM the model uses the PA intercept

# For spatial models, the prediction to the training site will also be made with just the random or fixed component of the model 

# Inputs:

# Output from step 8

# posterior_nsamps which is the number of posterior samples to take

# Output:

# A list with the same structure as the input, but with the predictions added to the models dataframe


########################################################################


predict_from_fitted_SiteB_func <- function(reps.setup.list,
                                           posterior_nsamps) {
  
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
      
      Model <- reps.setup.list[[name]][[rep]]$models$Model
      
      # Load covariates at projection site (Site B)
      cov.rep <- reps.setup.list[[name]][[rep]]$extrap.reps.out$covs.SiteB.rast
      
      # Add a dummy bias variable to keep RISDM happy
      cov.rep$bias <- 1
      
      for (i in seq_along(Model)) { 
        
        mod <- models_df[[i, "Model"]]
        
        type <- models_df[[i, "Mod.type"]]
        
        if(grepl("PO", type, fixed = T)) { # If models are PO, use PO intercept
          
          # Had to add the [[1]] here because the summary is always list of length 1
          mod[[1]]$preds.link.siteB <- predict(mod[[1]],
                                               covars = cov.rep,
                                               S = posterior_nsamps, 
                                               intercept.terms = "PO_Intercept",
                                               type = "link",
                                               includeRandom = F) # No GRF
          
          # Save the updated model back to the dataframe
          models_df[[i, "Model"]] <- mod
          
        } else { # If models are PA or Integrated, use PA intercept
          
          # Had to add the [[1]] here because the summary is always list of length 1
          mod[[1]]$preds.link.siteB <- predict(mod[[1]],
                                               covars = cov.rep,
                                               S = posterior_nsamps, 
                                               intercept.terms = "PA_Intercept",
                                               type = "link",
                                               includeRandom = F) # No GRF
          
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

# RUN THE PREDICTION FOR Training SITE A --------------------------------

predict_from_fitted_SiteA_func <- function(reps.setup.list,
                                           pred.GRF = FALSE,
                                           pred.fixed = FALSE,
                                           mod.type = "no-GRF",
                                           posterior_nsamps) {

  
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
      
      Model <- reps.setup.list[[name]][[rep]]$models$Model
      
      # Load covariates at training Site B
      cov.rep <- reps.setup.list[[name]][[rep]]$extrap.reps.out$covs.SiteA.rast
      
      # Add a dummy bias variable to keep RISDM happy
      cov.rep$bias <- 1
      
      for (i in seq_along(Model)) { 
        
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
                                                 includeRandom = T) # Add GRF
            
            # Save the updated model back to the dataframe
            models_df[[i, "Model"]] <- mod
            
          } else { # If models are PA or Integrated, use PA intercept
            
            # Had to add the [[1]] here because the summary is always list of length 1
            mod[[1]]$preds.link.siteA <- predict(mod[[1]],
                                                 covars = cov.rep,
                                                 S = posterior_nsamps, 
                                                 intercept.terms = "PA_Intercept",
                                                 type = "link",
                                                 includeRandom = T) # Add GRF
            
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
                                                 includeRandom = F) # No GRF
            
            # Save the updated model back to the dataframe
            models_df[[i, "Model"]] <- mod
            
          } else { # If models are PA or Integrated, use PA intercept
            
            # Had to add the [[1]] here because the summary is always list of length 1
            mod[[1]]$preds.link.siteA <- predict(mod[[1]],
                                                 covars = cov.rep,
                                                 S = posterior_nsamps, 
                                                 intercept.terms = "PA_Intercept",
                                                 type = "link",
                                                 includeRandom = F) # No GRF
            
            # Save the updated model back to the dataframe
            models_df[[i, "Model"]] <- mod
            
          }
          
        }
        
        
        ###########
        ### If also plotting random effect at Site A
        ###########
        
        if(pred.GRF == TRUE & grepl("GRF", type, fixed = T)) {
          
          # Had to add the [[1]] here because the summary is always list of length 1
          # Prediction doesn't include an intercept
          mod[[1]]$preds.GRF.siteA <- predict(mod[[1]],
                                              covars = cov.rep,
                                              S = posterior_nsamps, 
                                              intercept.terms = NULL,
                                              type = "link",
                                              includeRandom = T,
                                              includeFixed = F) # No fixed effect
          
          # Save the updated model back to the dataframe
          models_df[[i, "Model"]] <- mod
          
        }
        
        ###########
        ### If also plotting fixed effect at Site A
        ###########
        
        if(pred.fixed == TRUE & grepl("GRF", type, fixed = T)) {
          
          # Had to add the [[1]] here because the summary is always list of length 1
          # Prediction doesn't include an intercept
          mod[[1]]$preds.FIXED.siteA <- predict(mod[[1]],
                                                covars = cov.rep,
                                                S = posterior_nsamps, 
                                                intercept.terms = NULL,
                                                type = "link",
                                                includeRandom = F,
                                                includeFixed = T) # Only fixed effect
          
          # Save the updated model back to the dataframe
          models_df[[i, "Model"]] <- mod
          
        }
        
        
        # Save the updated models dataframe back to the original list
        reps.setup.list[[name]][[rep]]$models <- models_df
        
      }
    } }
  
  return(reps.setup.list)
  
}



