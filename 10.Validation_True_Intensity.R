
library(Metrics)
library(scoringutils, lib.loc=lib_loc)

# 10. Validation true intensity -------------------------------------------

validation_SiteB_func <- function(reps.setup.list,
                                  job_index) {
  
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
      
      # And site distance
      Site.distance <- reps.setup.list[[name]][[rep]]$extrap.reps.out$Site.distance
      
      # Crop out the true log intensity from the Site B
      rand.gridB <- reps.setup.list[[name]][[rep]]$extrap.reps.out$rand.gridB
      true_log_int.rast <- reps.setup.list[[name]][[rep]]$true_log_int.rast
      true_log_int.rast.SiteB <- crop(true_log_int.rast, ext(rand.gridB))
      
      for (i in seq_along(Model)) { # NEED TO ADD BACK IN ONCE HAVE PA WORKING
        
        mod <- models_df[[i, "Model"]]
        
        # Pull out the median posterior intensity prediction for each cell
        median.int.pred <- mod[[1]]$preds.link.siteB$field$Median
        
        # Pull out the lower and upper bounds of the prediction
        lower.int.pred <- mod[[1]]$preds.link.siteB$field$Lower
        
        upper.int.pred <- mod[[1]]$preds.link.siteB$field$Upper
        
        # Metrics from Simmonds et al. 
        # Compare the predicted intensity to the true intensity 
        cor <- cor(as.vector(median.int.pred), as.vector(true_log_int.rast.SiteB),
                   method = "spearman")
        
        MAE <- mean(abs(as.vector(median.int.pred - true_log_int.rast.SiteB)))
        
        RMSE <- Metrics::rmse(actual = as.vector(true_log_int.rast.SiteB), 
                              predicted = as.vector(median.int.pred))
        
        ### Calculating the Interval Score ###
        
        interval_score <- scoringutils:::interval_score(observed = as.vector(true_log_int.rast.SiteB),
                                                        lower = as.vector(lower.int.pred), 
                                                        upper = as.vector(upper.int.pred),
                                                        interval_range = 95,
                                                        weigh = TRUE)
        
        Sum.Int.Score <- sum(interval_score)
        
        Mean.Int.Score <- mean(interval_score)
        
        # Save results to list
        
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


# OPTIONAL - RUN THE VALIDATION FOR SITE A --------------------------------


validation_SiteA_func <- function(reps.setup.list,
                                  pred.GRF = FALSE,
                                  pred.fixed = FALSE,
                                  job_index) {
  
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
      
      Model <- reps.setup.list[[name]][[rep]]$models$Model
      
      # Extract the median extrapolation amount
      # BA <- reps.setup.list[[name]][[rep]]$extrap.reps.out$summary.extrap$BA
      # BD <- reps.setup.list[[name]][[rep]]$extrap.reps.out$summary.extrap$BD
      mean <- reps.setup.list[[name]][[rep]]$extrap.reps.out$summary.extrap$mean
      median <- reps.setup.list[[name]][[rep]]$extrap.reps.out$summary.extrap$median
      
      # And site distance
      Site.distance <- reps.setup.list[[name]][[rep]]$extrap.reps.out$Site.distance
      
      # Crop out the true log intensity from the Site A
      rand.gridA <- reps.setup.list[[name]][[rep]]$extrap.reps.out$rand.gridA
      true_log_int.rast <- reps.setup.list[[name]][[rep]]$true_log_int.rast
      true_log_int.rast.SiteA <- crop(true_log_int.rast, ext(rand.gridA))
      
      ### IF GRF has been plotted, look at correlation with TRUTH
      # First get true one from rep
      if(pred.GRF == TRUE) {
        
        # First extract the TRUE random effect for comparison
        # Crop out the TRUE random effect from the Site A
        GRF.rast <- reps.setup.list[[name]][[rep]]$latent.list$GRF.rast
        GRF.rast.SiteA <- crop(GRF.rast, ext(rand.gridA))
        
        # Also extract grid B GRF for correlation comparison
        rand.gridB <- reps.setup.list[[name]][[rep]]$extrap.reps.out$rand.gridB
        GRF.rast.SiteB <- crop(GRF.rast, ext(rand.gridB))
        
      }
      
      ### IF Fixed effect has been plotted, look at correlation with TRUTH
      # First get true one from rep
      if(pred.fixed == TRUE) {
        
        # First extract the TRUE fixed effect for comparison
        # Crop out the TRUE fixed effect from the Site A
        fixed.rast <- reps.setup.list[[name]][[rep]]$latent.list$fixed.rast
        fixed.rast.SiteA <- crop(fixed.rast, ext(rand.gridA))
        
      }
      
      for (i in seq_along(Model)) { # NEED TO ADD BACK IN ONCE HAVE PA WORKING
        
        mod <- models_df[[i, "Model"]]
        type <- as.character(models_df[i, "Mod.type"])
        
        # Pull out the median intensity prediction for each cell
        median.int.pred <- mod[[1]]$preds.link.siteA$field$Median
        
        # Pull out the lower and upper bounds of the prediction
        lower.int.pred <- mod[[1]]$preds.link.siteA$field$Lower
        
        upper.int.pred <- mod[[1]]$preds.link.siteA$field$Upper
        
        # Metrics from Simmonds et al. 
        # Compare the predicted intensity to the true intensity 
        cor <- cor(as.vector(median.int.pred), as.vector(true_log_int.rast.SiteA),
                   method = "spearman")
        
        MAE <- mean(abs(as.vector(median.int.pred - true_log_int.rast.SiteA)))
        
        RMSE <- Metrics::rmse(actual = as.vector(true_log_int.rast.SiteA), 
                              predicted = as.vector(median.int.pred))
        
        ### Calculating the Interval Score ###
        
        interval_score <- scoringutils:::interval_score(observed = as.vector(true_log_int.rast.SiteA),
                                                        lower = as.vector(lower.int.pred), 
                                                        upper = as.vector(upper.int.pred),
                                                        interval_range = 95,
                                                        weigh = TRUE)
        
        Sum.Int.Score <- sum(interval_score)
        
        Mean.Int.Score <- mean(interval_score)
        
        if(pred.GRF == TRUE & grepl("GRF", type, fixed = T)) {
          
          median.GRF.pred <- mod[[1]]$preds.GRF.siteA$field$Median
          
          cor.GRF <- cor(as.vector(median.GRF.pred), as.vector(GRF.rast.SiteA), 
                         method = "spearman")
          
          cor.GRFA.GRFB <- cor(as.vector(GRF.rast.SiteB), as.vector(GRF.rast.SiteA), 
                               method = "spearman")
          
        } else { 
          cor.GRF = NA 
        cor.GRFA.GRFB = NA
        }
        
        
        if(pred.fixed == TRUE & grepl("GRF", type, fixed = T)) {
          
          median.FIXED.pred <- mod[[1]]$preds.FIXED.siteA$field$Median
          
          cor.FIXED <- cor(as.vector(median.FIXED.pred), as.vector(fixed.rast.SiteA),
                           method = "spearman")
          
        } else { cor.FIXED = NA }
        
        # Save results to list
        
        results_list[[length(results_list) + 1]] <- data.frame(
          extrap.type = name,
          mean.extrap = mean,
          median.extrap = median,
          Site.distance = Site.distance,
          rep = rep,
          job_index = job_index,
          mod.type = as.character(models_df[i, "Mod.type"]),
          correlation = cor,
          cor.GRF = cor.GRF,
          cor.FIXED = cor.FIXED,
          cor.GRFA.GRFB = cor.GRFA.GRFB,
          MAE = MAE,
          RMSE = RMSE,
          Sum.Int.Score = Sum.Int.Score,
          Mean.Int.Score = Mean.Int.Score
        )
        
      }
      }
    
  }
  
  # Combine all the results into a single dataframe
  true.validation.df <- do.call(rbind, results_list)
  
  return(true.validation.df)
  
}

