### TO DO - FIX PLOTTING MEAN PREDICTIONS AS IT'S BROKEN HAVE JUST COMMENTED OUT FOR NOW


# 12C. Plot Predictions ---------------------------------------------------

plot_predictions_SiteB_func <- function(reps.setup.list,
                                  outpath,
                                  scenario_name,
                                  mod.type,
                                  job_index) {
  
  # Get the names of the extrap types for indexing
  extrap_names <- names(reps.setup.list)
  
  # For every level of extrap type (Low, Mod, High)
  for(extrap.type in seq_along(reps.setup.list)) {
    
    # Extract the name (e.g., "Low") for indexing from the names list
    name <- extrap_names[extrap.type] 
    
    # For every replicate
    for(rep in seq_along(reps.setup.list[[name]])) {
      
      # First extract the log intensity of latent true distribution for comparison
      # Crop out the true log intensity from the Site B
      rand.gridB <- reps.setup.list[[name]][[rep]]$extrap.reps.out$rand.gridB
      true_log_int.rast <- reps.setup.list[[name]][[rep]]$true_log_int.rast
      true_log_int.rast.SiteB <- crop(true_log_int.rast, ext(rand.gridB))
      range_true_log_int.SiteB <- range(values(true_log_int.rast.SiteB))
      
      # Extract the models dataframe [[name]] double brackets for list extract
      models_df <- reps.setup.list[[name]][[rep]]$models
      
      Model <- reps.setup.list[[name]][[rep]]$models$Model
      
      for (i in seq_along(Model)) {
        
        mod <- models_df[[i, "Model"]]
        
        type <- models_df[[i, "Mod.type"]]
        
        # Pull out the median intensity prediction for each cell
        median.int.pred <- mod[[1]]$preds.link.siteB$field$Median
        median.int.pred.name <- paste0("median.int.pred.", type)
        
        plot.name <- paste0("pred.plot.", type)
        
          p <- median.int.pred %>% 
            as.data.frame(xy = T) %>%  
            ggplot() + 
            geom_tile(aes(x = x, y = y, fill = Median)) + 
            scale_fill_viridis(guide = guide_colorbar(barwidth = 0.5), limits = range_true_log_int.SiteB) +
            coord_fixed() + 
            theme_bw() + 
            theme(axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  legend.ticks = element_blank(),
                  legend.title = element_blank()) 
        
          # Save back to main list
          reps.setup.list[[name]][[rep]][[plot.name]] <- p 
          reps.setup.list[[name]][[rep]][[median.int.pred.name]] <- median.int.pred

      }
      
      p1 <- true_log_int.rast.SiteB %>% 
        as.data.frame(xy = T) %>% 
        ggplot() +
        geom_tile(aes(x = x, y = y, fill = int)) +
        scale_fill_viridis(guide = guide_colorbar(barwidth = 0.5)) +
        coord_fixed() +
        theme_bw() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.ticks = element_blank(),
              legend.title = element_blank(),
              plot.margin = unit(c(0.5, 0.1, 0.5, 0.1), "lines"),
              plot.title = element_text(hjust = 0.5)) +  # Reduce margins
        ggtitle('True log intensity')
      
      pred.plot.list <- list()
      
      if(sum(grepl("no-GRF", mod.type, fixed = T) & !grepl("bias", mod.type, fixed = T)) > 0) {
        
        p2a <- reps.setup.list[[name]][[rep]]$pred.plot.m.int +
          ggtitle('Median predicted log intensity - Integrated no GRF')
        p3a <- reps.setup.list[[name]][[rep]]$pred.plot.m.PO +
          ggtitle('Median predicted log intensity - PO no GRF')
        p4a <- reps.setup.list[[name]][[rep]]$pred.plot.m.PA +
          ggtitle('Median predicted log intensity - PA no GRF')
       
        pred.plot.list$prediction.plot.no.GRF <- ggarrange(p1, p2a, p3a, p4a, ncol = 2, nrow = 2)
         
      }
      
      if(sum(grepl("spatial", mod.type, fixed = T) & !grepl("bias", mod.type, fixed = T)) > 0) {
        
        p2b <- reps.setup.list[[name]][[rep]]$pred.plot.m.int.GRF +
          ggtitle('Median predicted log intensity - Integrated GRF')
        p3b <- reps.setup.list[[name]][[rep]]$pred.plot.m.PO.GRF +
          ggtitle('Median predicted log intensity - PO GRF')
        p4b <- reps.setup.list[[name]][[rep]]$pred.plot.m.PA.GRF +
          ggtitle('Median predicted log intensity - PA GRF')
        
        pred.plot.list$prediction.plot.GRF <- ggarrange(p1, p2b, p3b, p4b, ncol = 2, nrow = 2)
        
      }
      
      if(sum(grepl("no-GRF", mod.type, fixed = T) & grepl("bias", mod.type, fixed = T)) > 0) {
        
        p2c <- reps.setup.list[[name]][[rep]]$pred.plot.m.int.bias +
          ggtitle('Median predicted log intensity - Integrated no GRF w bias')
        p3c <- reps.setup.list[[name]][[rep]]$pred.plot.m.PO.bias +
          ggtitle('Median predicted log intensity - PO no GRF w bias')
        p4c <- reps.setup.list[[name]][[rep]]$pred.plot.m.PA.bias +
          ggtitle('Median predicted log intensity - PA no GRF w bias')
        
        pred.plot.list$prediction.plot.no.GRF.bias <- ggarrange(p1, p2c, p3c, p4c, ncol = 2, nrow = 2)
        
      }
      
      if(sum(grepl("spatial", mod.type, fixed = T) & grepl("bias", mod.type, fixed = T)) > 0) {
        
        p2d <- reps.setup.list[[name]][[rep]]$pred.plot.m.int.GRF.bias +
          ggtitle('Median predicted log intensity - Integrated GRF w bias')
        p3d <- reps.setup.list[[name]][[rep]]$pred.plot.m.PO.GRF.bias +
          ggtitle('Median predicted log intensity - PO GRF w bias')
        p4d <- reps.setup.list[[name]][[rep]]$pred.plot.m.PA.GRF.bias +
          ggtitle('Median predicted log intensity - PA GRF w bias')
        
        pred.plot.list$prediction.plot.GRF.bias <- ggarrange(p1, p2d, p3d, p4d, ncol = 2, nrow = 2)
        
      }
      
      
      
      # Save plot
      rep_path <- file.path(outpath, scenario_name, name,  paste0("Rep_", rep, "Job_", job_index))
      
      plot.names <- names(pred.plot.list)
      
      for(x in seq_along(pred.plot.list)) {
        
        plot.name <- plot.names[x]
       
        ggsave(paste0(rep_path, "/", plot.name, ".png"), x, width = 21, height = 25, units = "cm", dpi = 400, device = "png")
         
      }
 
      }}
  }
  



# ################
# # Now summarising median rasters across extrap types ----------------------
# ################
# 
# # Get the names of the extrap types for indexing
# extrap_names <- names(reps.setup.list)
# 
# # # Initialise a list to store mean predictions across replicates
# pred_mean_across_reps <- list()
# 
# Integrated.rasts <- list()
# PA.rasts <- list()
# PO.rasts <- list()
# 
# # For every level of extrap type (Low, Mod, High)
# for(extrap.type in seq_along(reps.setup.list)) {
#   
#   # Extract the name (e.g., "Low") for indexing from the names list
#   name <- extrap_names[extrap.type] 
#   
#   # # List to store rasters for each repetition
#   Integrated.rasts[[name]] <- list()
#   PO.rasts[[name]] <- list()
#   PA.rasts[[name]] <- list()
#   
#   # For every replicate extract elements that match "median.int.pred.Integrated"
#   for(rep in seq_along(reps.setup.list[[name]])) {
#     
#     # Get the current replicate
#     current_rep <- reps.setup.list[[name]][[rep]]
#     
#     # Go through names of elements of current_rep and find one that matches
#     Integrated_matches <- sapply(names(current_rep), function(x) {
#       grepl("median.int.pred.Integrated", x, fixed = T)
#     })
#     
#     # Extract elements that match "median.int.pred.PO"
#     PA_matches <- sapply(names(current_rep), function(x) {
#       grepl("median.int.pred.PA", x)
#     })
#     
#     # Extract elements that match "median.int.pred.PO"
#     PO_matches <- sapply(names(current_rep), function(x) {
#       grepl("median.int.pred.PO", x)
#     })
#     
#     Integrated.rasts[[name]][rep] <- current_rep[Integrated_matches]
#     PA.rasts[[name]][rep] <- current_rep[PA_matches]
#     PO.rasts[[name]][rep] <- current_rep[PO_matches]
#     
#   }
#   
#   pred_mean_across_reps[[name]] <- list()
#   
#   stack <- rast(Integrated.rasts[[name]])
#   
#   pred_mean_across_reps[[name]] <- global(rast(Integrated.rasts[[name]]), fun = "mean") 
#   
# }
# 


# OPTIONAL - Plot predictions Site A --------------------------------------


plot_predictions_SiteA_func <- function(reps.setup.list,
                                        pred.type = c("link", "intensity", "probability"),
                                        outpath,
                                        scenario_name,
                                        pred.GRF = FALSE,
                                        pred.fixed = FALSE,
                                        mod.type,
                                        job_index) {
  
  # Get the names of the extrap types for indexing
  extrap_names <- names(reps.setup.list)
  
  # For every level of extrap type (Low, Mod, High)
  for(extrap.type in seq_along(reps.setup.list)) {
    
    # Extract the name (e.g., "Low") for indexing from the names list
    name <- extrap_names[extrap.type] 
    
    # For every replicate
    for(rep in seq_along(reps.setup.list[[name]])) {
      
      # First extract the log intensity of latent true distribution for comparison
      # Crop out the true log intensity from the Site A
      rand.gridA <- reps.setup.list[[name]][[rep]]$extrap.reps.out$rand.gridA
      true_log_int.rast <- reps.setup.list[[name]][[rep]]$true_log_int.rast
      true_log_int.rast.SiteA <- crop(true_log_int.rast, ext(rand.gridA))
      
      range_true_log_int.SiteA <- range(values(true_log_int.rast.SiteA))
      
      # Extract the models dataframe [[name]] double brackets for list extract
      models_df <- reps.setup.list[[name]][[rep]]$models
      
      Model <- reps.setup.list[[name]][[rep]]$models$Model
      
      for (i in seq_along(Model)) {
        
        mod <- models_df[[i, "Model"]]
        
        type <- models_df[[i, "Mod.type"]]
        
        # Pull out the median intensity prediction for each cell
        median.int.pred <- mod[[1]]$preds.link.siteA$field$Median
        
        plot.name <- paste0("pred.plot.", type)
        
        p <- median.int.pred %>% 
          as.data.frame(xy = T) %>%  
          ggplot() + 
          geom_tile(aes(x = x, y = y, fill = Median)) + 
          scale_fill_viridis(guide = guide_colorbar(barwidth = 0.5), limits = range_true_log_int.SiteA) +
          coord_fixed() + 
          theme_bw() + 
          theme(axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                legend.ticks = element_blank(),
                legend.title = element_blank()) 
        
        # Save back to main list
        reps.setup.list[[name]][[rep]][[plot.name]] <- p     
        
        

# If the model contains a GRF ---------------------------------------------

        if(grepl("GRF", type, fixed = T)) {
          
          ## IF ALSO PLOTTING THE GRF PREDICTION
          if(pred.GRF == TRUE) {
            
            # First extract the TRUE random effect for comparison
            # Crop out the TRUE random effect from the Site A
            GRF.rast <- reps.setup.list[[name]][[rep]]$latent.list$GRF.rast
            GRF.rast.SiteA <- crop(GRF.rast, ext(rand.gridA))
            range.GRF.rast <- range(values(GRF.rast.SiteA))
            
            median.GRF.pred <- mod[[1]]$preds.GRF.siteA$field$Median
            
            plot.name.GRF <- paste0("pred.GRF.plot.", type)
            
            p <- median.GRF.pred %>% 
              as.data.frame(xy = T) %>%  
              ggplot() + 
              geom_tile(aes(x = x, y = y, fill = Median)) + 
              scale_fill_viridis(guide = guide_colorbar(barwidth = 0.5), limits = range.GRF.rast) +
              coord_fixed() + 
              theme_bw() + 
              theme(axis.title.x = element_blank(),
                    axis.title.y = element_blank(),
                    legend.ticks = element_blank(),
                    legend.title = element_blank()) 
            
            # Save back to main list
            reps.setup.list[[name]][[rep]][[plot.name.GRF]] <- p 
            
            
          }
          
          ## IF ALSO PLOTTING THE FIXED EFFECT
          if(pred.fixed == TRUE) {
            
            # First extract the TRUE fixed effect for comparison
            # Crop out the TRUE fixed effect from the Site A
            fixed.rast <- reps.setup.list[[name]][[rep]]$latent.list$fixed.rast
            fixed.rast.SiteA <- crop(fixed.rast, ext(rand.gridA))
            range.fixed.rast <- range(values(fixed.rast.SiteA))
            
            median.FIXED.pred <- mod[[1]]$preds.FIXED.siteA$field$Median
            
            plot.name.FIXED <- paste0("pred.FIXED.plot.", type)
            
            p <- median.FIXED.pred %>% 
              as.data.frame(xy = T) %>%  
              ggplot() + 
              geom_tile(aes(x = x, y = y, fill = Median)) + 
              scale_fill_viridis(guide = guide_colorbar(barwidth = 0.5), limits = range.fixed.rast) +
              coord_fixed() + 
              theme_bw() + 
              theme(axis.title.x = element_blank(),
                    axis.title.y = element_blank(),
                    legend.ticks = element_blank(),
                    legend.title = element_blank()) 
            
            # Save back to main list
            reps.setup.list[[name]][[rep]][[plot.name.FIXED]] <- p 
            
            
          }
          
        }
 
      }
      
      
      p1 <- true_log_int.rast.SiteA %>% 
        as.data.frame(xy = T) %>% 
        ggplot() +
        geom_tile(aes(x = x, y = y, fill = int)) +
        scale_fill_viridis(guide = guide_colorbar(barwidth = 0.5)) +
        coord_fixed() +
        theme_bw() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.ticks = element_blank(),
              legend.title = element_blank(),
              plot.margin = unit(c(0.5, 0.1, 0.5, 0.1), "lines"),
              plot.title = element_text(hjust = 0.5)) +  # Reduce margins
        ggtitle('True log intensity')
      
      pred.plot.list <- list()
      
      if(sum(grepl("no-GRF", mod.type, fixed = T) & !grepl("bias", mod.type, fixed = T)) >0 ) {
        
        p2a <- reps.setup.list[[name]][[rep]]$pred.plot.m.int +
          ggtitle('Median predicted log intensity - Integrated no GRF')
        p3a <- reps.setup.list[[name]][[rep]]$pred.plot.m.PO +
          ggtitle('Median predicted log intensity - PO no GRF')
        p4a <- reps.setup.list[[name]][[rep]]$pred.plot.m.PA +
          ggtitle('Median predicted log intensity - PA no GRF')
        
        pred.plot.list$SITEA_prediction.plot.no.GRF <- ggarrange(p1, p2a, p3a, p4a, ncol = 2, nrow = 2)
        
      }
      
      if(sum(grepl("spatial", mod.type, fixed = T) & !grepl("bias", mod.type, fixed = T)) > 0) {
        
        p2b <- reps.setup.list[[name]][[rep]]$pred.plot.m.int.GRF +
          ggtitle('Median predicted log intensity - Integrated GRF')
        p3b <- reps.setup.list[[name]][[rep]]$pred.plot.m.PO.GRF +
          ggtitle('Median predicted log intensity - PO GRF')
        p4b <- reps.setup.list[[name]][[rep]]$pred.plot.m.PA.GRF +
          ggtitle('Median predicted log intensity - PA GRF')
        
        pred.plot.list$SITEA_prediction.plot.GRF <- ggarrange(p1, p2b, p3b, p4b, ncol = 2, nrow = 2)
        
      }
      
      if(sum(grepl("no-GRF", mod.type, fixed = T) & grepl("bias", mod.type, fixed = T)) > 0) {
        
        p2c <- reps.setup.list[[name]][[rep]]$pred.plot.m.int.bias +
          ggtitle('Median predicted log intensity - Integrated no GRF w bias')
        p3c <- reps.setup.list[[name]][[rep]]$pred.plot.m.PO.bias +
          ggtitle('Median predicted log intensity - PO no GRF w bias')
        p4c <- reps.setup.list[[name]][[rep]]$pred.plot.m.PA.bias +
          ggtitle('Median predicted log intensity - PA no GRF w bias')
        
        pred.plot.list$SITEA_prediction.plot.no.GRF.bias <- ggarrange(p1, p2c, p3c, p4c, ncol = 2, nrow = 2)
        
      }
      
      if(sum(grepl("spatial", mod.type, fixed = T) & grepl("bias", mod.type, fixed = T)) > 0) {
        
        p2d <- reps.setup.list[[name]][[rep]]$pred.plot.m.int.GRF.bias +
          ggtitle('Median predicted log intensity - Integrated GRF w bias')
        p3d <- reps.setup.list[[name]][[rep]]$pred.plot.m.PO.GRF.bias +
          ggtitle('Median predicted log intensity - PO GRF w bias')
        p4d <- reps.setup.list[[name]][[rep]]$pred.plot.m.PA.GRF.bias +
          ggtitle('Median predicted log intensity - PA GRF w bias')
        
        pred.plot.list$SITEA_prediction.plot.GRF.bias <- ggarrange(p1, p2d, p3d, p4d, ncol = 2, nrow = 2)
        
      }
      
      
      # Save plot
      rep_path <- file.path(outpath, scenario_name, name,  paste0("Rep_", rep, "Job_", job_index))
      
      plot.names <- names(pred.plot.list)
      
      for(x in seq_along(pred.plot.list)) {
        
        plot.name <- plot.names[x]
        
        ggsave(paste0(rep_path, "/", plot.name, ".png"), x, width = 21, height = 25, units = "cm", dpi = 400, device = "png")
        
      }
      
      
      # Plot the random effect with true random effect
      if(sum(pred.GRF == TRUE & grepl("spatial", mod.type, fixed = T)) > 0) {
        
        # First extract the TRUE random effect for comparison
        # Crop out the TRUE random effect from the Site A
        GRF.rast <- reps.setup.list[[name]][[rep]]$latent.list$GRF.rast
        GRF.rast.SiteA <- crop(GRF.rast, ext(rand.gridA))
        
        # Make a plot for the TRUE random effect
        p5 <- GRF.rast.SiteA %>% 
          as.data.frame(xy = T) %>% 
          ggplot() +
          geom_tile(aes(x = x, y = y, fill = GRF)) +
          scale_fill_viridis(guide = guide_colorbar(barwidth = 0.5)) +
          coord_fixed() +
          theme_bw() +
          theme(axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                legend.ticks = element_blank(),
                legend.title = element_blank(),
                plot.margin = unit(c(0.5, 0.1, 0.5, 0.1), "lines"),
                plot.title = element_text(hjust = 0.5)) +  # Reduce margins
          ggtitle('True random effect')
        
        GRF.pred.plot.list <- list()
        
        if(sum(!grepl("bias", mod.type, fixed = T)) > 0) {
          
          p6a <- reps.setup.list[[name]][[rep]]$pred.GRF.plot.m.int.GRF +
            ggtitle('Median predicted GRF - Integrated GRF')
          p7a <- reps.setup.list[[name]][[rep]]$pred.GRF.plot.m.PO.GRF +
            ggtitle('Median predicted GRF - PO GRF')
          p8a <- reps.setup.list[[name]][[rep]]$pred.GRF.plot.m.PA.GRF +
            ggtitle('Median predicted GRF - PA GRF')
          
          GRF.pred.plot.list$SITEA_GRF.prediction.plot <- ggarrange(p5, p6a, p7a, p8a, ncol = 2, nrow = 2)
          
        }
        
        if(sum(grepl("bias", mod.type, fixed = T)) > 0) {
          
          p6b <- reps.setup.list[[name]][[rep]]$pred.GRF.plot.m.int.GRF.bias +
            ggtitle('Median predicted GRF - Integrated GRF w bias')
          p7b <- reps.setup.list[[name]][[rep]]$pred.GRF.plot.m.PO.GRF.bias +
            ggtitle('Median predicted GRF - PO GRF w bias')
          p8b <- reps.setup.list[[name]][[rep]]$pred.GRF.plot.m.PA.GRF.bias +
            ggtitle('Median predicted GRF - PA GRF w bias')
          
          GRF.pred.plot.list$SITEA_GRF.prediction.plot.bias <- ggarrange(p5, p6b, p7b, p8b, ncol = 2, nrow = 2)
          
        }
        
        
        
        # Save plot
        rep_path <- file.path(outpath, scenario_name, name,  paste0("Rep_", rep, "Job_", job_index))
        
        plot.names <- names(GRF.pred.plot.list)
        
        for(x in seq_along(GRF.pred.plot.list)) {
          
          plot.name <- plot.names[x]
          
          ggsave(paste0(rep_path, "/", plot.name, ".png"), x, width = 21, height = 25, units = "cm", dpi = 400, device = "png")
          
        }
        
      }
      
      # Plot the fixed effect with true fixed effect
      if(sum(pred.fixed == TRUE & grepl("spatial", mod.type, fixed = T)) > 0) {
        
        # First extract the TRUE fixed effect for comparison
        # Crop out the TRUE fixed effect from the Site A
        fixed.rast <- reps.setup.list[[name]][[rep]]$latent.list$fixed.rast
        fixed.rast.SiteA <- crop(fixed.rast, ext(rand.gridA))
        
        # Make a plot for the TRUE random effect
        p5 <- fixed.rast.SiteA %>% 
          as.data.frame(xy = T) %>% 
          ggplot() +
          geom_tile(aes(x = x, y = y, fill = Fixed)) +
          scale_fill_viridis(guide = guide_colorbar(barwidth = 0.5)) +
          coord_fixed() +
          theme_bw() +
          theme(axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                legend.ticks = element_blank(),
                legend.title = element_blank(),
                plot.margin = unit(c(0.5, 0.1, 0.5, 0.1), "lines"),
                plot.title = element_text(hjust = 0.5)) +  # Reduce margins
          ggtitle('True fixed effect')
        
        FIXED.pred.plot.list <- list()
        
        if(sum(!grepl("bias", mod.type, fixed = T)) > 0) { 
          
          p6a <- reps.setup.list[[name]][[rep]]$pred.FIXED.plot.m.int.GRF +
            ggtitle('Median predicted FIXED - Integrated GRF')
          p7a <- reps.setup.list[[name]][[rep]]$pred.FIXED.plot.m.PO.GRF +
            ggtitle('Median predicted FIXED - PO GRF')
          p8a <- reps.setup.list[[name]][[rep]]$pred.FIXED.plot.m.PA.GRF +
            ggtitle('Median predicted FIXED - PA GRF')
          
          FIXED.pred.plot.list$SITEA_FIXED.prediction.plot <- ggarrange(p5, p6a, p7a, p8a, ncol = 2, nrow = 2)
          
          }
        
        if(sum(grepl("bias", mod.type, fixed = T)) > 0) { 
          
          p6b <- reps.setup.list[[name]][[rep]]$pred.FIXED.plot.m.int.GRF.bias +
            ggtitle('Median predicted FIXED - Integrated GRF w bias')
          p7b <- reps.setup.list[[name]][[rep]]$pred.FIXED.plot.m.PO.GRF.bias +
            ggtitle('Median predicted FIXED - PO GRF w bias')
          p8b <- reps.setup.list[[name]][[rep]]$pred.FIXED.plot.m.PA.GRF.bias +
            ggtitle('Median predicted FIXED - PA GRF w bias')
          
          FIXED.pred.plot.list$SITEA_FIXED.prediction.plot.bias <- ggarrange(p5, p6b, p7c, p8d, ncol = 2, nrow = 2)
          
        }
        
        # Save plot
        rep_path <- file.path(outpath, scenario_name, name,  paste0("Rep_", rep, "Job_", job_index))
        
        plot.names <- names(FIXED.pred.plot.list)
        
        for(x in seq_along(FIXED.pred.plot.list)) {
          
          plot.name <- plot.names[x]
          
          ggsave(paste0(rep_path, "/", plot.name, ".png"), x, width = 21, height = 25, units = "cm", dpi = 400, device = "png")
          
        }
        
      }
      
    }
    
  }
  
}
