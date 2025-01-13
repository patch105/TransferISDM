
# 7. Extract Model Results ------------------------------------------------

extract_model_results_func <- function(reps.setup.list,
                                       mod.type,
                                       job_index,
                                       res,
                                       scal) {
  
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
      
      Model <- reps.setup.list[[name]][[rep]]$models$Model
      
      # Extract the median extrapolation amount
      # BA <- reps.setup.list[[name]][[rep]]$extrap.reps.out$summary.extrap$BA
      # BD <- reps.setup.list[[name]][[rep]]$extrap.reps.out$summary.extrap$BD
      mean <- reps.setup.list[[name]][[rep]]$extrap.reps.out$summary.extrap$mean
      median <- reps.setup.list[[name]][[rep]]$extrap.reps.out$summary.extrap$median
      
      meanPA <- reps.setup.list[[name]][[rep]]$summary.realised.extrap$meanPA
      meanPO <- reps.setup.list[[name]][[rep]]$summary.realised.extrap$meanPO
      meanPAPO <- reps.setup.list[[name]][[rep]]$summary.realised.extrap$meanPAPO
      
      medianPA <- reps.setup.list[[name]][[rep]]$summary.realised.extrap$medianPA
      medianPO <- reps.setup.list[[name]][[rep]]$summary.realised.extrap$medianPO
      medianPAPO <- reps.setup.list[[name]][[rep]]$summary.realised.extrap$medianPAPO
      
      # Extract the distance between the sites
      Site.distance <- reps.setup.list[[name]][[rep]]$extrap.reps.out$Site.distance
      
    
      for (i in seq_along(Model)) { 
        
        
        if(latent.type == "lgcp") { # if you have a spatial model
          
          if(scenario.type == "Spatial.Auto") {
            
            scal.var <- scal[extrap.type] # Pull out spatial auto range
            
          } else {scal.var <- scal}
          
          # Calculating Moran's I  ----------------------------------------------
          
          #### PO residuals ####
          if (grepl("PO", models_df[i, "Mod.type"], fixed = TRUE) | grepl("int", models_df[i, "Mod.type"], fixed = TRUE)) { 
            
            # PO Residuals ------------------------------------------------------------
            
            # MORAN'S I: Residual spatial autocorrelation --------------------------
            
            # Calculate the neighbourhood distance as scal (true spat auto range)
            neighbour_dist <- res*scal.var
            
            # Load covariates
            cov.rep <- rep$extrap.reps.out$covs.SiteA.rast
            
            update_rast_xy <- cov.rep %>% 
              as.data.frame(xy=TRUE) %>% 
              .[, -c(3,4)] 
            
            # dnearneigh is in metres, finds to index of all values within the specified distance
            nb1 <- dnearneigh(as.matrix(update_rast_xy), 0, neighbour_dist)
            
            # Generate a weights object
            weights <- nb2listw(nb1, 
                                glist = NULL,
                                style = "B",
                                zero.policy = TRUE) 
            
            # Print list of distances
            #dists <- nbdists(nb1, update_rast_xy)
            
            # Then extract just the values per cell
            POresid <- residuals(i)$PO$POresids$residual
            
            # Normalize the data so it is not affected by high values
            normalize <- function(i, na.rm = TRUE) {
              return((i- min(i)) /(max(i)-min(i)))
            }
            
            norm_POresid <- normalize(POresid)
            
            # Moran's I value for normalized data
            PO_MoransI <- moran(norm_POresid, 
                                weights, 
                                zero.policy = T, 
                                length(nb1), 
                                Szero(weights)) %>% 
              .[[1]]
            
            # Hypothesis test  ----------------------------------------------------
            
            # Run a hypothesis test via permutation
            moran_bootstrap <- moran.mc(norm_POresid,
                                        weights,
                                        nsim = 10000,
                                        zero.policy = TRUE)
            
            # Print the output of the hypothesis test
            PO_MoransI_pvalue <- summary(moran_bootstrap)
            
            
          }
          
          #### PA residuals ####
          if (grepl("PA", models_df[i, "Mod.type"], fixed = TRUE) | grepl("int", models_df[i, "Mod.type"], fixed = TRUE)) { 
            
            # PA Residuals ------------------------------------------------------------
            
            # Calculate the neighbourhood distance as scal (true spat auto range)
            neighbour_dist <- res*scal.var
            
            # Load covariates
            cov.rep <- rep$extrap.reps.out$covs.SiteA.rast
            
            PA_fit <- rep$pa_a_df
            
            update_rast_xy <- PA_fit %>% 
              select(x, y)
            
            # dnearneigh is in metres, finds to index of all values within the specified distance
            nb1 <- dnearneigh(as.matrix(update_rast_xy), 0, neighbour_dist)
            
            # Generate a weights object
            weights <- nb2listw(nb1, 
                                glist = NULL,
                                style = "B",
                                zero.policy = TRUE) 
            
            # Print list of distances
            #dists <- nbdists(nb1, update_rast_xy)
            
            # Then extract just the values per cell
            PAresid <- residuals(x)$PA$residual
            
            # Normalize the data so it is not affected by high values
            normalize <- function(x, na.rm = TRUE) {
              return((x- min(x)) /(max(x)-min(x)))
            }
            
            norm_PAresid <- normalize(PAresid)
            
            # Moran's I value for normalized data
            PA_MoransI <- moran(norm_PAresid, 
                                weights, 
                                zero.policy = T, 
                                length(nb1), 
                                Szero(weights)) %>% 
              .[[1]]
            
            # Hypothesis test  ----------------------------------------------------
            
            # Run a hypothesis test via permutation
            moran_bootstrap <- moran.mc(norm_PAresid,
                                        weights,
                                        nsim = 10000,
                                        zero.policy = TRUE)
            
            # Print the output of the hypothesis test
            PA_MoransI_pvalue <- moran_bootstrap$p.value
            
          }
          
        } else { # If you don't have a lgcp model, don't do Moran's I
          
          PO_MoransI <- NA
          PO_MoransI_pvalue <- NA
          PA_MoransI <- NA
          PA_MoransI_pvalue <- NA
          
        }
        
        
        
        mod.summary <- models_df[[i, "Summary"]]
        
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
          beta1.mean = mod.summary[[1]]$DISTRIBUTION["cov1", "mean"],
          beta2.mean = mod.summary[[1]]$DISTRIBUTION["cov2", "mean"],
          beta1.median = mod.summary[[1]]$DISTRIBUTION[[4]][1],
          beta2.median = mod.summary[[1]]$DISTRIBUTION[[4]][2],
          beta1_25 = mod.summary[[1]]$DISTRIBUTION["cov1", "0.025quant"],
          beta1_975 = mod.summary[[1]]$DISTRIBUTION["cov1", "0.975quant"],
          beta2_25 = mod.summary[[1]]$DISTRIBUTION["cov2", "0.025quant"],
          beta2_975 = mod.summary[[1]]$DISTRIBUTION["cov2", "0.975quant"],
          beta1.sd = mod.summary[[1]]$DISTRIBUTION["cov1", "sd"],
          beta2.sd = mod.summary[[1]]$DISTRIBUTION["cov2", "sd"],
          PO_intercept = NA,
          PO_intercept_25 = NA,
          PO_intercept_975 = NA,
          PO_intercept.sd = NA,
          PA_intercept = NA,
          PA_intercept_25 = NA,
          PA_intercept_975 = NA,
          PA_intercept.sd = NA,
           marg_lik = mod.summary[[1]]$marg.lik,
          GRF.range.mean = NA,
          GRF.sd.mean = NA,
          GRF.range_25 = NA,
          GRF.range_975 = NA,
          GRF.sd_25 = NA,
          GRF.sd_975 = NA,
          GRF.range.sd = NA,
          GRF.sd.sd = NA,
          bias.coef.mean = NA,
          bias.coef_25 = NA,
          bias.coef_95 = NA,
          bias.coef.sd = NA,
          PO_MoransI = NA,
          PO_MoransI_pvalue = NA,
          PA_MoransI = NA,
          PA_MoransI_pvalue = NA
        )
        
          # If the model name contains PO or Integrated, save the PO intercept
        if(grepl("PO", models_df[i, "Mod.type"], fixed = T) | grepl("int", models_df[i, "Mod.type"], fixed = T)) {

          results_list[[length(results_list)]]$PO_intercept <- mod.summary[[1]]$PO_BIAS["PO_Intercept", "mean"]
          results_list[[length(results_list)]]$PO_intercept_25 <- mod.summary[[1]]$PO_BIAS["PO_Intercept", "0.025quant"]
          results_list[[length(results_list)]]$PO_intercept_975 <- mod.summary[[1]]$PO_BIAS["PO_Intercept", "0.975quant"]
          results_list[[length(results_list)]]$PO_intercept.sd <- mod.summary[[1]]$PO_BIAS["PO_Intercept", "sd"]
          results_list[[length(results_list)]]$PO_MoransI <- PO_MoransI
          results_list[[length(results_list)]]$PO_MoransI_pvalue <- PO_MoransI_pvalue
          
        }  
          
          if(grepl("PA", models_df[i, "Mod.type"], fixed = T) | grepl("int", models_df[i, "Mod.type"], fixed = T)) {
            
            results_list[[length(results_list)]]$PA_intercept <- mod.summary[[1]]$PA_ARTEFACT["PA_Intercept", "mean"]
            results_list[[length(results_list)]]$PA_intercept_25 <- mod.summary[[1]]$PA_ARTEFACT["PA_Intercept", "0.025quant"]
            results_list[[length(results_list)]]$PA_intercept_975 <- mod.summary[[1]]$PA_ARTEFACT["PA_Intercept", "0.975quant"]
            results_list[[length(results_list)]]$PA_intercept.sd <- mod.summary[[1]]$PA_ARTEFACT["PA_Intercept", "sd"]
            results_list[[length(results_list)]]$PA_MoransI <- PA_MoransI
            results_list[[length(results_list)]]$PA_MoransI_pvalue <- PA_MoransI_pvalue
            
          }
       
          # If the model is spatial, save the spatial parameters
          if(grepl("GRF", models_df[i, "Mod.type"], fixed = T)) {
            
            results_list[[length(results_list)]]$GRF.range.mean <- mod.summary[[1]]$SPATIAL$mean[1]
            results_list[[length(results_list)]]$GRF.sd.mean <- mod.summary[[1]]$SPATIAL$mean[2]
            results_list[[length(results_list)]]$GRF.range_25 <- mod.summary[[1]]$SPATIAL[[3]][1]
            results_list[[length(results_list)]]$GRF.range_975 <- mod.summary[[1]]$SPATIAL[[5]][1]
            results_list[[length(results_list)]]$GRF.sd_25 <- mod.summary[[1]]$SPATIAL[[3]][2]
            results_list[[length(results_list)]]$GRF.sd_975 <- mod.summary[[1]]$SPATIAL[[5]][2]
            results_list[[length(results_list)]]$GRF.range.sd <- mod.summary[[1]]$SPATIAL[[2]][1]
            results_list[[length(results_list)]]$GRF.sd.sd <- mod.summary[[1]]$SPATIAL[[2]][2]
          
          }
          
          # If the model has a bias covariate, save the bias coefficient
          if(grepl("bias", models_df[i, "Mod.type"], fixed = T)) {
            
            results_list[[length(results_list)]]$bias.coef.mean <- mod.summary[[1]]$PO_BIAS["PO_bias", "mean"]
            results_list[[length(results_list)]]$bias.coef_25 <- mod.summary[[1]]$PO_BIAS["PO_bias", "0.025quant"]
            results_list[[length(results_list)]]$bias.coef_95 <- mod.summary[[1]]$PO_BIAS["PO_bias", "0.975quant"]
            results_list[[length(results_list)]]$bias.coef.sd <- mod.summary[[1]]$PO_BIAS["PO_bias", "sd"]

          }
          
      }
      
      
    }}
  
  # Combine all the results into a single dataframe
  extrap.scenario.df <- do.call(rbind, results_list)
  
  return(extrap.scenario.df)
  
}
  
  
  
