

# 12B. Plot_Data -----------------------------------------------------------

plot_data_func <- function(reps.setup.list,
                           outpath,
                           scenario_name,
                           job_index) {
  
  # Get the names of the extrap types for indexing
  extrap_names <- names(reps.setup.list)
  
  # For every level of extrap type (Low, Mod, High)
  for(extrap.type in seq_along(reps.setup.list)) {
    
    # Extract the name (e.g., "Low") for indexing from the names list
    name <- extrap_names[extrap.type] 
    
    # For every replicate
    for(rep in seq_along(reps.setup.list[[name]])) {
      
      # Crop out the true log intensity from the Site A and Site B
      rand.gridA <- reps.setup.list[[name]][[rep]]$extrap.reps.out$rand.gridA
      true_log_int.rast <- reps.setup.list[[name]][[rep]]$true_log_int.rast
      true_log_int.rast.SiteA <- crop(true_log_int.rast, ext(rand.gridA))
      
      rand.gridB <- reps.setup.list[[name]][[rep]]$extrap.reps.out$rand.gridB
      true_log_int.rast.SiteB <- crop(true_log_int.rast, ext(rand.gridB))
      
      # Get extent for plotting
      extPA <- ext(reps.setup.list[[name]][[rep]]$PA.rand.gridA) 
      
      df_extPA <- data.frame(
        xmin = extPA[1],
        xmax = extPA[2],
        ymin = extPA[3],
        ymax = extPA[4],
        color = "black",
        label = "PA Grid",
        label_x = (extPA[1] + extPA[2]) / 2,  # Center of the extent for text placement
        label_y = extPA[4] + (extPA[4] - extPA[3]) * 0.1  # Slightly above the top of the extent
      )
      
      # Plot PO data on grid A and B
      A <- true_log_int.rast.SiteA %>% 
        as.data.frame(xy = T) %>% 
        ggplot() +
        geom_tile(aes(x = x, y = y, fill = int)) +
        scale_fill_viridis(guide = guide_colorbar(barwidth = 0.5)) +
        coord_fixed() +
        geom_rect(data = df_extPA, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, color = color), 
                  fill = NA, linetype = "solid", linewidth = 1) +
        geom_point(data = reps.setup.list[[name]][[rep]]$poforPA.rand.gridA, aes(x = x, y = y), color = "red", size = 1.5) +
        geom_point(data = reps.setup.list[[name]][[rep]]$pa_a_df, aes(x=x, y=y, shape = factor(presence, levels = c(1,0))), size = 1.5) +
        scale_shape_manual(values = c("0" = 4, "1" = 16),
                           label = c("0" = "Absence", "1" = "Presence")) +
        scale_color_identity() +
        theme_bw() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.ticks = element_blank(),
              legend.title = element_blank(),
              plot.margin = unit(c(0.5, 0.1, 0.5, 0.1), "lines"),
              plot.title = element_text(hjust = 0.5)) +  # Reduce margins
        ggtitle('Site A')
      
      B <- true_log_int.rast.SiteB %>% 
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
        ggtitle('Site B')
      
      # Save plots
      rep_path <- file.path(outpath, scenario_name, name, paste0("Rep_", rep, "Job_", job_index))
      
      
      # Make dir if not already there
      if(!dir.exists(rep_path)) {
        
        dir.create(rep_path, recursive = TRUE, warning = FALSE)
        
      }
      
      ggsave(paste0(rep_path, "/SiteA_PO_PA_Plot.png"), A, width = 21, height = 25, units = "cm", dpi = 400, device = "png")
      ggsave(paste0(rep_path, "/SiteB_Plot.png"), B, width = 21, height = 25, units = "cm", dpi = 400, device = "png")
      
      
    }
    
  }
  
}




