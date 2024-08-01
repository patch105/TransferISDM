
# Final Plots -------------------------------------------------------------

library(tidyterra)

Bunger.landsat.sf <- st_read(here("Data/Bunger_Landsat_Polygon.shp")) %>% 
  st_transform(3031)
  
  
  p <- ggplot() +
    geom_sf(data = Bunger.landsat.sf) +
    geom_point(data = PA_val, aes(x = x, y = y, color = as.factor(presence)), size = 2) +
    scale_color_manual(values = c("1" = "red3", "0" = "purple4"), labels = c("1" = "Present", "0" = "Absent")) +
    guides(color = guide_legend(title = NULL)) +

    # coord_sf(
    #   xlim = c(st_bbox(Vestfold.landsat.sf)$xmin, st_bbox(Vestfold.landsat.sf)$xmax), 
    #   ylim = c(st_bbox(Vestfold.landsat.sf)$ymin, st_bbox(Vestfold.landsat.sf)$ymax)) +
    theme_classic() +
    theme(legend.text = element_text(size = 16),
          panel.border = element_rect(colour = "black", fill = NA),  # Add a border around the plot
          axis.line = element_blank())  # Remove axis lines
  
  ggsave(plot = p, filename = here("Case_Study/Figures/Plot_Bunger_PA_Dataset.png"), width = 10, height = 10, units = "in", dpi = 300)
  
  
  
  p <- ggplot() +
    geom_sf(data = Vestfold.landsat.sf) +
    geom_point(data = PA_fit, aes(x = x, y = y, color = as.factor(presence)), size = 2) +
    scale_color_manual(values = c("1" = "red3", "0" = "purple4"), labels = c("1" = "Present", "0" = "Absent")) +
    guides(color = guide_legend(title = NULL)) +
    theme_classic() +
    theme(legend.text = element_text(size = 16),
          panel.border = element_rect(colour = "black", fill = NA),  # Add a border around the plot
          axis.line = element_blank())  # Remove axis lines
  
  ggsave(plot = p, filename = here("Case_Study/Figures/Plot_Vestfold_PA_Dataset.png"), width = 10, height = 10, units = "in", dpi = 300)
  
  
 p <- ggplot() +
    geom_spatvector(data = ice_freeSPVE.NorthEastAnt) +
    geom_point(data = PO, aes(x = x, y = y), shape = 21, color = "black", fill = "red3", size = 2) +
    theme_classic() +
    theme(legend.text = element_text(size = 16),
          panel.border = element_rect(colour = "black", fill = NA),  # Add a border around the plot
          axis.line = element_blank())  # Remove axis lines

  ggsave(plot = p, filename = here("Case_Study/Figures/Plot_Vestfold_PO_Dataset.png"), width = 10, height = 10, units = "in", dpi = 300)
