
library(ggpubr)

# 11. Plot Validation True Intensity --------------------------------------

plot_validation_SiteB_func <- function(true.validation.df,
                                       save = FALSE,
                                       outpath,
                                       scenario_name,
                                       scenario.type,
                                       mod.type) { 
  
  # Set the label for the x axis based on the scenario type 
  # "Spatial autocorrelation range" or "Environmental dissimilarity"
  
  if(scenario.type == "Enviro.Extrap") {
    
    x.label <- "Environmental dissimilarity"
    
    x.discrete.label <- c("Low", "Mod", "High")
    
  }
  
  if(scenario.type == "Spatial.Auto") {
    
    x.label <- "Spatial autocorrelation range"
    
    scal.list <- scal
    
    x.discrete.label <- c(as.character(scal))
    
  }
  
  
  # Set values for fill for each model 
  fill.colours = c("m.int" = "purple", 
                   "m.int.GRF" = "purple4",
                   "m.int.bias" = "pink",
                   "m.int.GRF.bias" = "pink4",
                   "m.PO" = "skyblue",
                   "m.PO.GRF" = "skyblue4",
                   "m.PO.bias" = "green3",
                   "m.PO.GRF.bias" = "green4",
                   "m.PA" = "orange",
                   "m.PA.GRF" = "orange3")
  
  
  # Plot the validation
  
  cor <- true.validation.df %>% 
    ggplot(aes(x = extrap.type, y = correlation, fill = mod.type)) +
    geom_boxplot() +
    labs(x = x.label, y = "Correlation", fill = "Model Type") +
    scale_x_discrete(labels = x.discrete.label) +
    scale_fill_manual(values = fill.colours) +
    theme_bw()
  
  MAE <- true.validation.df %>% 
    ggplot(aes(x = extrap.type, y = MAE, fill = mod.type)) +
    geom_boxplot() +
    labs(x = x.label, y = "MAE", fill = "Model Type") +
    scale_x_discrete(labels = x.discrete.label) +
    scale_fill_manual(values = fill.colours) +
    theme_bw()
  
  RMSE <- true.validation.df %>% 
    ggplot(aes(x = extrap.type, y = RMSE, fill = mod.type)) +
    geom_boxplot() +
    labs(x = x.label, y = "RMSE", fill = "Model Type") +
    scale_x_discrete(labels = x.discrete.label) +
    scale_fill_manual(values = fill.colours) +
    theme_bw()
  
  ## Smaller values of interval score are better
  
  Int.score.mean <- true.validation.df %>% 
    ggplot(aes(x = extrap.type, y = Mean.Int.Score, fill = mod.type)) +
    geom_boxplot() +
    labs(x = x.label, y = "Mean Interval Score", fill = "Model Type") +
    scale_x_discrete(labels = x.discrete.label) +
    scale_fill_manual(values = fill.colours) +
    theme_bw()
  
  Int.score.sum <- true.validation.df %>% 
    ggplot(aes(x = extrap.type, y = Sum.Int.Score, fill = mod.type)) +
    geom_boxplot() +
    labs(x = x.label, y = "Sum Interval Score", fill = "Model Type") +
    scale_x_discrete(labels = x.discrete.label) +
    scale_fill_manual(values = fill.colours) +
    theme_bw()
  
  p1 <- ggarrange(RMSE, MAE, common.legend = T,  ncol = 2, nrow = 1)
  
  p2 <- ggarrange(Int.score.mean, Int.score.sum, common.legend = T,  ncol = 2, nrow = 1)
  
  if(save == TRUE) {
    
    ggsave(plot = p1, filename = paste0(file.path(outpath, scenario_name),"/Scenario_", scenario_name, "_RMSE_MAE_plot.png"), w = 21.5, h = 15, units = "cm", dpi = 400, device = "png")
    
    ggsave(plot = p2, filename = paste0(file.path(outpath, scenario_name),"/Scenario_", scenario_name, "_Int_Score_plot.png"), w = 21.5, h = 15, units = "cm", dpi = 400, device = "png")
    
    ggsave(plot = cor, filename = paste0(file.path(outpath, scenario_name),"/Scenario_", scenario_name, "_COR_plot.png"), w = 21.5, h = 15, units = "cm", dpi = 400, device = "png")
    
    
  } else{print(p1) 
    
    print(p2) 
    
    print(cor)}

}

####################################
# OPTIONAL - PLOT VALIDATION SITE A ---------------------------------------
###################################

plot_validation_SiteA_func <- function(true.validation.df,
                                       save = FALSE,
                                       outpath,
                                       scenario_name,
                                       scenario.type,
                                       mod.type,
                                       pred.GRF = FALSE,
                                       pred.fixed = FALSE) {
  
  # Set the label for the x axis based on the scenario type 
  # "Spatial autocorrelation range" or "Environmental dissimilarity"
  
  if(scenario.type == "Enviro.Extrap") {
    
    x.label <- "Environmental dissimilarity"
    
    x.discrete.label <- c("Low", "Mod", "High")
    
  }
  
  if(scenario.type == "Spatial.Auto") {
    
    x.label <- "Spatial autocorrelation range"
    
    scal.list <- scal
    
    x.discrete.label <- c(as.character(scal))
    
  }
  
  
  
  # Set values for fill for each model 
  fill.colours = c("m.int" = "purple", 
                   "m.int.GRF" = "purple4",
                   "m.int.bias" = "pink",
                   "m.int.GRF.bias" = "pink4",
                   "m.PO" = "skyblue",
                   "m.PO.GRF" = "skyblue4",
                   "m.PO.bias" = "green3",
                   "m.PO.GRF.bias" = "green4",
                   "m.PA" = "orange",
                   "m.PA.GRF" = "orange3")
  
 
  # Make SiteA folder:
  dir.create(file.path(outpath, scenario_name, "SiteA"), showWarnings = FALSE)
  
  # Plot the validation
  
  cor <- true.validation.df %>% 
    ggplot(aes(x = extrap.type, y = correlation, fill = mod.type)) +
    geom_boxplot() +
    labs(x = x.label, y = "Correlation", fill = "Model Type") +
    scale_x_discrete(labels = x.discrete.label) +
    scale_fill_manual(values = fill.colours) +
    theme_bw()
  
  MAE <- true.validation.df %>% 
    ggplot(aes(x = extrap.type, y = MAE, fill = mod.type)) +
    geom_boxplot() +
    labs(x = x.label, y = "MAE", fill = "Model Type") +
    scale_x_discrete(labels = x.discrete.label) +
    scale_fill_manual(values = fill.colours) +
    theme_bw()
  
  RMSE <- true.validation.df %>% 
    ggplot(aes(x = extrap.type, y = RMSE, fill = mod.type)) +
    geom_boxplot() +
    labs(x = x.label, y = "RMSE", fill = "Model Type") +
    scale_x_discrete(labels = x.discrete.label) +
    scale_fill_manual(values = fill.colours) +
    theme_bw()
  
  ## Smaller values of interval score are better
  
  Int.score.mean <- true.validation.df %>% 
    ggplot(aes(x = extrap.type, y = Mean.Int.Score, fill = mod.type)) +
    geom_boxplot() +
    labs(x = x.label, y = "Mean Interval Score", fill = "Model Type") +
    scale_x_discrete(labels = x.discrete.label) +
    scale_fill_manual(values = fill.colours) +
    theme_bw()
  
  Int.score.sum <- true.validation.df %>% 
    ggplot(aes(x = extrap.type, y = Sum.Int.Score, fill = mod.type)) +
    geom_boxplot() +
    labs(x = x.label, y = "Sum Interval Score", fill = "Model Type") +
    scale_x_discrete(labels = x.discrete.label) +
    scale_fill_manual(values = fill.colours) +
    theme_bw()
  
  p1 <- ggarrange(RMSE, MAE, common.legend = T,  ncol = 2, nrow = 1)
  
  p2 <- ggarrange(Int.score.mean, Int.score.sum, common.legend = T,  ncol = 2, nrow = 1)
  
  if(save == TRUE) {
    
    ggsave(plot = p1, filename = paste0(file.path(outpath, scenario_name, "SiteA"),"/Scenario_", scenario_name, "_RMSE_MAE_plot.png"), w = 21.5, h = 15, units = "cm", dpi = 400, device = "png")
    
    ggsave(plot = p2, filename = paste0(file.path(outpath, scenario_name, "SiteA"),"/Scenario_", scenario_name, "_Int_Score_plot.png"), w = 21.5, h = 15, units = "cm", dpi = 400, device = "png")
    
    ggsave(plot = cor, filename = paste0(file.path(outpath, scenario_name, "SiteA"),"/Scenario_", scenario_name, "_COR_plot.png"), w = 21.5, h = 15, units = "cm", dpi = 400, device = "png")
    
    
  } else{print(p1) 
    
    print(p2) 
    
    print(cor)}
  
  
  # If there are spatial models ------------------------------------------
  
  if(sum(grepl("spatial", mod.type, fixed = T)) > 0) {
    
    ### IF Predicted GRF, plot the Correlation
    if(pred.GRF == TRUE) {
      
      cor.GRF <- true.validation.df %>% 
        filter(!is.na(cor.GRF)) %>% 
        ggplot(aes(x = extrap.type, y = cor.GRF, fill = mod.type)) +
        geom_boxplot() +
        labs(x = x.label, y = "Correlation GRF", fill = "Model Type") +
        scale_x_discrete(labels = x.discrete.label) +
        scale_fill_manual(values = fill.colours) +
        theme_bw()
      
      if(save == TRUE) {
        
        ggsave(plot = cor.GRF, filename = paste0(file.path(outpath, scenario_name, "SiteA"),"/Scenario_", scenario_name, "_COR_GRF_plot.png"), w = 21.5, h = 15, units = "cm", dpi = 400, device = "png")
        
      } else {print(cor.GRF)}
      
    }
    
    ### IF Predicted Fixed, plot the Correlation
    if(pred.fixed == TRUE) {
      
      cor.FIXED <- true.validation.df %>% 
        filter(!is.na(cor.FIXED)) %>%
        ggplot(aes(x = extrap.type, y = cor.FIXED, fill = mod.type)) +
        geom_boxplot() +
        labs(x = x.label, y = "Correlation FIXED", fill = "Model Type") +
        scale_x_discrete(labels = x.discrete.label) +
        scale_fill_manual(values = fill.colours) +
        theme_bw()
      
      if(save == TRUE) {
        
        ggsave(plot = cor.FIXED, filename = paste0(file.path(outpath, scenario_name, "SiteA"),"/Scenario_", scenario_name, "_COR_FIXED_plot.png"), w = 21.5, h = 15, units = "cm", dpi = 400, device = "png")
        
      } else {print(cor.FIXED)}
    
  }
  }
  }


# PLOT VALIDATION CONTINUOUS EXTRAPOLATION --------------------------------

plot_validation_SiteB_continuous_func <- function(true.validation.df,
                                                  save = FALSE,
                                                  outpath,
                                                  scenario_name,
                                                  scenario.type,
                                                  mod.type) {

  if(scenario.type == "Enviro.Extrap") {
    
    x.label <- "Environmental dissimilarity"
    
  }
  
  if(scenario.type == "Spatial.Auto") {
    
    x.label <- "Spatial autocorrelation range"
    
    scal.list <- scal
    
  }
  
  fill.colours = c("m.int" = "purple",
                   "m.int.GRF" = "purple4",
                   "m.int.bias" = "pink",
                   "m.int.GRF.bias" = "pink4",
                   "m.PO" = "skyblue",
                   "m.PO.GRF" = "skyblue4",
                   "m.PO.bias" = "green3",
                   "m.PO.GRF.bias" = "green4",
                   "m.PA" = "orange",
                   "m.PA.GRF" = "orange3")

  # Plot the validation

  cor <- true.validation.df %>%
    ggplot(aes(x = extrap.median, y = correlation, color = mod.type)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "loess", se = T, aes(fill = mod.type, color = mod.type), alpha = 0.2) +
    labs(x = x.label, y = "Correlation", fill = "Model Type") +
    scale_color_manual(values = fill.colours) +
    scale_fill_manual(values = fill.colours) +
    # coord_cartesian(xlim = c(NA, 50)) +
    theme_bw()

  true.validation.df %>%
    ggplot(aes(x = extrap.median, y = MAE, color = mod.type)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "loess", se = T, aes(fill = mod.type, color = mod.type), alpha = 0.2) +
        labs(x = x.label, y = "MAE", fill = "Model Type") +
    scale_color_manual(values = fill.colours) +
    scale_fill_manual(values = fill.colours) +
    coord_cartesian(xlim = c(NA, 50)) +
    theme_bw()


  }

  

