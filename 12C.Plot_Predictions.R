

# 12C. Plot Predictions ---------------------------------------------------

plot_predictions_func <- function(reps.setup.list,
                                  pred.type = c("link", "intensity", "probability"),
                                  outpath,
                                  scenario_name) {
  
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
      
      # Extract the models dataframe [[name]] double brackets for list extract
      models_df <- reps.setup.list[[name]][[rep]]$models
      
      for (i in seq_along(models_df)) {
        
        mod <- models_df[[i, "Model"]]
        
        type <- models_df[[i, "Mod.type"]]
        
        # Pull out the mean intensity prediction for each cell
        mean.int.pred <- mod[[1]]$preds.link.siteB$field$Mean
        
        plot.name <- paste0("pred.plot.", type)
        
          p <- mean.int.pred %>% 
            as.data.frame(xy = T) %>%  
            ggplot() + 
            geom_tile(aes(x = x, y = y, fill = Mean)) + 
            scale_fill_viridis() +
            coord_fixed() + 
            theme_bw() + 
            theme(axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  legend.ticks = element_blank(),
                  legend.title = element_blank()) 
        
          # Save back to main list
          reps.setup.list[[name]][[rep]][[plot.name]] <- p     

        
        
        
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
      
      p2 <- reps.setup.list[[name]][[rep]]$pred.plot.Integrated.no.GRF +
        ggtitle('Mean predicted log intensity - Integrated no GRF')
      p3 <- reps.setup.list[[name]][[rep]]$pred.plot.PO.no.GRF +
        ggtitle('Mean predicted log intensity - PO no GRF')
      p4 <- reps.setup.list[[name]][[rep]]$pred.plot.PA.no.GRF +
        ggtitle('Mean predicted log intensity - PA no GRF')
      
      prediction.plot <- ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
      
      # Save plot
      rep_path <- file.path(outpath, scenario_name, name, paste0("Rep_", rep))
      
      ggsave(paste0(rep_path, "/Prediction_Plot.png"), prediction.plot, width = 21, height = 25, units = "cm", dpi = 400, device = "png")
      
    }
    
  }
  
}
