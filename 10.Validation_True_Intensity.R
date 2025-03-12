
########################################################################
########################## 10. Validation true intensity ###################
########################################################################

# This script loads the model predictions at the training and projection sites and compares them to the true species intensity with several metrics. The results are fed back to the main script where they are saved as a .csv filed.

# Input:

# Output from step 9

# Input variables

# Whether the true species distribution is spatial or non-spatial

# Output: 

# A dataframe with predictive performance results per model type per replicate


########################################################################

validation_SiteB_func <- function(reps.setup.list,
                                  job_index,
                                  GRF.var.multiplier,
                                  latent.type) {
  
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
      mean <- reps.setup.list[[name]][[rep]]$extrap.reps.out$summary.extrap$mean
      median <- reps.setup.list[[name]][[rep]]$extrap.reps.out$summary.extrap$median
      
      meanPA <- reps.setup.list[[name]][[rep]]$summary.realised.extrap$meanPA
      meanPO <- reps.setup.list[[name]][[rep]]$summary.realised.extrap$meanPO
      meanPAPO <- reps.setup.list[[name]][[rep]]$summary.realised.extrap$meanPAPO
      
      medianPA <- reps.setup.list[[name]][[rep]]$summary.realised.extrap$medianPA
      medianPO <- reps.setup.list[[name]][[rep]]$summary.realised.extrap$medianPO
      medianPAPO <- reps.setup.list[[name]][[rep]]$summary.realised.extrap$medianPAPO
      
      # And Euclidean site distance
      Site.distance <- reps.setup.list[[name]][[rep]]$extrap.reps.out$Site.distance
      
      # Fixed and GRF variance
      if(latent.type == "lgcp") {
        
        fixed.variance <- reps.setup.list[[name]][[rep]]$latent.list$fixed.variance
        GRF.variance <- reps.setup.list[[name]][[rep]]$latent.list$GRF.variance
        
      } else {
        
        fixed.variance <- NA
        GRF.variance <- NA
        
      }
      
      # Crop out the true log intensity from the Site B (projection site)
      rand.gridB <- reps.setup.list[[name]][[rep]]$extrap.reps.out$rand.gridB
      true_log_int.rast <- reps.setup.list[[name]][[rep]]$true_log_int.rast
      true_log_int.rast.SiteB <- crop(true_log_int.rast, ext(rand.gridB))
      
      for (i in seq_along(Model)) {
        
        mod <- models_df[[i, "Model"]]
        
        # Pull out the median posterior intensity prediction for each cell
        median.int.pred <- mod[[1]]$preds.link.siteB$field$Median
        
        # Pull out the lower and upper bounds of the prediction
        lower.int.pred <- mod[[1]]$preds.link.siteB$field$Lower
        
        upper.int.pred <- mod[[1]]$preds.link.siteB$field$Upper
        
        # Compare the predicted intensity to the true intensity 
        correlation <- cor(as.vector(median.int.pred), as.vector(true_log_int.rast.SiteB),
                           method = "spearman")
        
        MAE <- mean(abs(as.vector(median.int.pred - true_log_int.rast.SiteB)))
        
        RMSE.global <- Metrics::rmse(actual = as.vector(true_log_int.rast.SiteB), 
                              predicted = as.vector(median.int.pred))
        
        
        ### Calculating the Interval Score ###
        
        interval_score <- scoringutils:::interval_score(observed = as.vector(true_log_int.rast.SiteB),
                                                        lower = as.vector(lower.int.pred), 
                                                        upper = as.vector(upper.int.pred),
                                                        interval_range = 95,
                                                        weigh = TRUE)
        
        Sum.Int.Score <- sum(interval_score)
        
        Mean.Int.Score <- mean(interval_score)
        
        Mean.CI.width <- global(upper.int.pred - lower.int.pred, "mean")[1,1]
        
        # Coverage rate
        
        coverage.true <- ifelse(as.vector(true_log_int.rast.SiteB) >= as.vector(lower.int.pred) 
                                & as.vector(true_log_int.rast.SiteB) <= as.vector(upper.int.pred), 1, 0)
        
        coverage.rate <- sum(coverage.true) / ncell(true_log_int.rast.SiteB)
        
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
          GRF.var.multiplier = GRF.var.multiplier,
          fixed.variance = fixed.variance,
          GRF.variance = GRF.variance,
          rep = rep,
          job_index = job_index,
          mod.type = as.character(models_df[i, "Mod.type"]),
          correlation = correlation,
          MAE = MAE,
          RMSE.global = RMSE.global,
          Sum.Int.Score = Sum.Int.Score,
          Mean.Int.Score = Mean.Int.Score,
          Mean.CI.width = Mean.CI.width,
          coverage.rate = coverage.rate
          
        )
        
      }}
    
  }
  
  # Combine all the results into a single dataframe
  true.validation.df <- do.call(rbind, results_list)
  
  return(true.validation.df)
  
}


# RUN THE VALIDATION FOR SITE A (training site) --------------------------------


validation_SiteA_func <- function(reps.setup.list,
                                  pred.GRF = FALSE,
                                  pred.fixed = FALSE,
                                  job_index,
                                  GRF.var.multiplier,
                                  latent.type) {
  
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
      mean <- reps.setup.list[[name]][[rep]]$extrap.reps.out$summary.extrap$mean
      median <- reps.setup.list[[name]][[rep]]$extrap.reps.out$summary.extrap$median
      
      # And the Euclidean site distance
      Site.distance <- reps.setup.list[[name]][[rep]]$extrap.reps.out$Site.distance
      
      # Fixed and GRF variance
      if(latent.type == "lgcp") {
        
        fixed.variance <- reps.setup.list[[name]][[rep]]$latent.list$fixed.variance
        GRF.variance <- reps.setup.list[[name]][[rep]]$latent.list$GRF.variance
        
      } else {
        
        fixed.variance <- NA
        GRF.variance <- NA
        
      }
      
      # Crop out the true log intensity from the Site A (training site)
      rand.gridA <- reps.setup.list[[name]][[rep]]$extrap.reps.out$rand.gridA
      true_log_int.rast <- reps.setup.list[[name]][[rep]]$true_log_int.rast
      true_log_int.rast.SiteA <- crop(true_log_int.rast, ext(rand.gridA))
      
      ### IF GRF has been plotted, look at correlation with truth
      if(pred.GRF == TRUE) {
        
        # Crop out the true  random effect from the Site A
        GRF.rast <- reps.setup.list[[name]][[rep]]$latent.list$GRF.rast
        GRF.rast.SiteA <- crop(GRF.rast, ext(rand.gridA))
        
        # Also extract grid B GRF for correlation comparison
        rand.gridB <- reps.setup.list[[name]][[rep]]$extrap.reps.out$rand.gridB
        GRF.rast.SiteB <- crop(GRF.rast, ext(rand.gridB))
        
      }
      
      ### If Fixed effect has been plotted, look at correlation with truth
      if(pred.fixed == TRUE) {
        
        # Crop out the TRUE fixed effect from the Site A
        fixed.rast <- reps.setup.list[[name]][[rep]]$latent.list$fixed.rast
        fixed.rast.SiteA <- crop(fixed.rast, ext(rand.gridA))
        
      }
      
      for (i in seq_along(Model)) { 
        
        mod <- models_df[[i, "Model"]]
        type <- as.character(models_df[i, "Mod.type"])
        
        # Pull out the median intensity prediction for each cell
        median.int.pred <- mod[[1]]$preds.link.siteA$field$Median
        
        # Pull out the lower and upper bounds of the prediction
        lower.int.pred <- mod[[1]]$preds.link.siteA$field$Lower
        
        upper.int.pred <- mod[[1]]$preds.link.siteA$field$Upper
        
        # Compare the predicted intensity to the true intensity 
        correlation <- cor(as.vector(median.int.pred), as.vector(true_log_int.rast.SiteA),
                           method = "spearman")
        
        MAE <- mean(abs(as.vector(median.int.pred - true_log_int.rast.SiteA)))
        
        RMSE.global <- Metrics::rmse(actual = as.vector(true_log_int.rast.SiteA), 
                              predicted = as.vector(median.int.pred))
        
        ### Calculating the Interval Score ###
        
        interval_score <- scoringutils:::interval_score(observed = as.vector(true_log_int.rast.SiteA),
                                                        lower = as.vector(lower.int.pred), 
                                                        upper = as.vector(upper.int.pred),
                                                        interval_range = 95,
                                                        weigh = TRUE)
        
        Sum.Int.Score <- sum(interval_score)
        
        Mean.Int.Score <- mean(interval_score)
        
        Mean.CI.width <- global(upper.int.pred - lower.int.pred, "mean")[1,1]
        
        coverage.true <- ifelse(as.vector(true_log_int.rast.SiteA) >= as.vector(lower.int.pred) 
                                & as.vector(true_log_int.rast.SiteA) <= as.vector(upper.int.pred), 1, 0)
        
        coverage.rate <- sum(coverage.true) / ncell(true_log_int.rast.SiteA)
        
        # If the model included a random effect, calculate the recovery of the true random and fixed effects with correlation, RMSE and int score
        
        if(pred.GRF == TRUE & grepl("GRF", type, fixed = T)) {
          
          median.GRF.pred <- mod[[1]]$preds.GRF.siteA$field$Median
          lower.GRF.pred <- mod[[1]]$preds.GRF.siteA$field$Lower
          upper.GRF.pred <- mod[[1]]$preds.GRF.siteA$field$Upper
          
          cor.GRF <- cor(as.vector(median.GRF.pred), as.vector(GRF.rast.SiteA), 
                         method = "spearman")
          
          cor.GRFA.GRFB <- cor(as.vector(GRF.rast.SiteB), as.vector(GRF.rast.SiteA), 
                               method = "spearman")
          
          RMSE.global.GRF <- Metrics::rmse(actual = as.vector(GRF.rast.SiteA), 
                                    predicted = as.vector(median.GRF.pred))
          
          interval_score.GRF <- scoringutils:::interval_score(observed = as.vector(GRF.rast.SiteA),
                                                             lower = as.vector(lower.GRF.pred), 
                                                             upper = as.vector(upper.GRF.pred),
                                                             interval_range = 95,
                                                             weigh = TRUE)
          
          Mean.Int.Score.GRF <- mean(interval_score.GRF)
          
          coverage.true.GRF <- ifelse(as.vector(GRF.rast.SiteA) >= as.vector(lower.GRF.pred) 
                                      & as.vector(GRF.rast.SiteA) <= as.vector(upper.GRF.pred), 1, 0)
          
          coverage.rate.GRF <- sum(coverage.true.GRF) / ncell(GRF.rast.SiteA)
          
        } else { 
          
        cor.GRF = NA 
        cor.GRFA.GRFB = NA
        RMSE.global.GRF = NA
        Mean.Int.Score.GRF = NA
        coverage.rate.GRF = NA
        
        }
        
        
        if(pred.fixed == TRUE & grepl("GRF", type, fixed = T)) {
          
          median.FIXED.pred <- mod[[1]]$preds.FIXED.siteA$field$Median
          lower.FIXED.pred <- mod[[1]]$preds.FIXED.siteA$field$Lower
          upper.FIXED.pred <- mod[[1]]$preds.FIXED.siteA$field$Upper
          
          cor.FIXED <- cor(as.vector(median.FIXED.pred), as.vector(fixed.rast.SiteA),
                           method = "spearman")
          
          RMSE.global.FIXED <- Metrics::rmse(actual = as.vector(fixed.rast.SiteA), 
                                      predicted = as.vector(median.FIXED.pred))
          
          interval_score.FIXED <- scoringutils:::interval_score(observed = as.vector(fixed.rast.SiteA),
                                                              lower = as.vector(lower.FIXED.pred), 
                                                              upper = as.vector(upper.FIXED.pred),
                                                              interval_range = 95,
                                                              weigh = TRUE)
          
          Mean.Int.Score.FIXED <- mean(interval_score.FIXED)
          
          coverage.true.FIXED <- ifelse(as.vector(fixed.rast.SiteA) >= as.vector(lower.FIXED.pred) 
                                      & as.vector(fixed.rast.SiteA) <= as.vector(upper.FIXED.pred), 1, 0)
          
          coverage.rate.FIXED <- sum(coverage.true.FIXED) / ncell(fixed.rast.SiteA)
          
          
          
        } else { 
          
          cor.FIXED = NA 
          RMSE.global.FIXED = NA
          Mean.Int.Score.FIXED = NA
          coverage.rate.FIXED = NA
          
          }
        
        # Save results to list
        
        results_list[[length(results_list) + 1]] <- data.frame(
          extrap.type = name,
          mean.extrap = mean,
          median.extrap = median,
          Site.distance = Site.distance,
          GRF.var.multiplier = GRF.var.multiplier,
          fixed.variance = fixed.variance,
          GRF.variance = GRF.variance,
          rep = rep,
          job_index = job_index,
          mod.type = as.character(models_df[i, "Mod.type"]),
          correlation = correlation,
          cor.GRF = cor.GRF,
          RMSE.global.GRF = RMSE.global.GRF,
          Mean.Int.Score.GRF = Mean.Int.Score.GRF,
          coverage.rate.GRF = coverage.rate.GRF,
          cor.FIXED = cor.FIXED,
          RMSE.global.FIXED = RMSE.global.FIXED,
          Mean.Int.Score.FIXED = Mean.Int.Score.FIXED,
          coverage.rate.FIXED = coverage.rate.FIXED,
          cor.GRFA.GRFB = cor.GRFA.GRFB,
          MAE = MAE,
          RMSE.global = RMSE.global,
          Sum.Int.Score = Sum.Int.Score,
          Mean.Int.Score = Mean.Int.Score,
          Mean.CI.width = Mean.CI.width,
          coverage.rate = coverage.rate
        )
        
      }
      }
    
  }
  
  # Combine all the results into a single dataframe
  true.validation.df <- do.call(rbind, results_list)
  
  return(true.validation.df)
  
}

