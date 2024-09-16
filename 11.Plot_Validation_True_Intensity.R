
library(ggpubr)

# 11. Plot Validation True Intensity --------------------------------------

plot_validation_SiteB_func <- function(true.validation.df,
                                       save = FALSE,
                                       outpath,
                                       scenario_name,
                                       mod.type) { 
  
  # If model is NON-SPATIAL  -------------------------------------------------
  
  if(mod.type == "non-spatial") {
    
    # Plot the validation
    
    cor <- true.validation.df %>% 
      ggplot(aes(x = extrap.type, y = correlation, fill = mod.type)) +
      geom_boxplot() +
      labs(x = "Extrapolation", y = "Correlation", fill = "Model Type") +
      scale_x_discrete(labels = c("Low", "Mod", "High")) +
      scale_fill_manual(values = c("Integrated.no.GRF" = "purple", "PO.no.GRF" = "skyblue", "PA.no.GRF" = "orange")) +
      theme_bw()
    
    MAE <- true.validation.df %>% 
      ggplot(aes(x = extrap.type, y = MAE, fill = mod.type)) +
      geom_boxplot() +
      labs(x = "Extrapolation", y = "MAE", fill = "Model Type") +
      scale_x_discrete(labels = c("Low", "Mod", "High")) +
      scale_fill_manual(values = c("Integrated.no.GRF" = "purple", "PO.no.GRF" = "skyblue", "PA.no.GRF" = "orange")) +
      theme_bw()
    
    RMSE <- true.validation.df %>% 
      ggplot(aes(x = extrap.type, y = RMSE, fill = mod.type)) +
      geom_boxplot() +
      labs(x = "Extrapolation", y = "RMSE", fill = "Model Type") +
      scale_x_discrete(labels = c("Low", "Mod", "High")) +
      scale_fill_manual(values = c("Integrated.no.GRF" = "purple", "PO.no.GRF" = "skyblue", "PA.no.GRF" = "orange")) +
      theme_bw()
    
    ## Smaller values of interval score are better
    
    Int.score.mean <- true.validation.df %>% 
      ggplot(aes(x = extrap.type, y = Mean.Int.Score, fill = mod.type)) +
      geom_boxplot() +
      labs(x = "Extrapolation", y = "Mean Interval Score", fill = "Model Type") +
      scale_x_discrete(labels = c("Low", "Mod", "High")) +
      scale_fill_manual(values = c("Integrated.no.GRF" = "purple", "PO.no.GRF" = "skyblue", "PA.no.GRF" = "orange")) +
      theme_bw()
    
    Int.score.sum <- true.validation.df %>% 
      ggplot(aes(x = extrap.type, y = Sum.Int.Score, fill = mod.type)) +
      geom_boxplot() +
      labs(x = "Extrapolation", y = "Sum Interval Score", fill = "Model Type") +
      scale_x_discrete(labels = c("Low", "Mod", "High")) +
      scale_fill_manual(values = c("Integrated.no.GRF" = "purple", "PO.no.GRF" = "skyblue", "PA.no.GRF" = "orange")) +
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
  
  
  # If model is SPATIAL  -------------------------------------------------
  
  if(mod.type == "spatial") {
    
    # Plot the validation
    
    cor <- true.validation.df %>% 
      ggplot(aes(x = extrap.type, y = correlation, fill = mod.type)) +
      geom_boxplot() +
      labs(x = "Extrapolation", y = "Correlation", fill = "Model Type") +
      scale_x_discrete(labels = c("Low", "Mod", "High")) +
      scale_fill_manual(values = c("Integrated.GRF" = "purple", "PO.GRF" = "skyblue", "PA.GRF" = "orange")) +
      theme_bw()
    
    MAE <- true.validation.df %>% 
      ggplot(aes(x = extrap.type, y = MAE, fill = mod.type)) +
      geom_boxplot() +
      labs(x = "Extrapolation", y = "MAE", fill = "Model Type") +
      scale_x_discrete(labels = c("Low", "Mod", "High")) +
      scale_fill_manual(values = c("Integrated.GRF" = "purple", "PO.GRF" = "skyblue", "PA.GRF" = "orange")) +
      theme_bw()
    
    RMSE <- true.validation.df %>% 
      ggplot(aes(x = extrap.type, y = RMSE, fill = mod.type)) +
      geom_boxplot() +
      labs(x = "Extrapolation", y = "RMSE", fill = "Model Type") +
      scale_x_discrete(labels = c("Low", "Mod", "High")) +
      scale_fill_manual(values = c("Integrated.GRF" = "purple", "PO.GRF" = "skyblue", "PA.GRF" = "orange")) +
      theme_bw()
    
    ## Smaller values of interval score are better
    
    Int.score.mean <- true.validation.df %>% 
      ggplot(aes(x = extrap.type, y = Mean.Int.Score, fill = mod.type)) +
      geom_boxplot() +
      labs(x = "Extrapolation", y = "Mean Interval Score", fill = "Model Type") +
      scale_x_discrete(labels = c("Low", "Mod", "High")) +
      scale_fill_manual(values = c("Integrated.GRF" = "purple", "PO.GRF" = "skyblue", "PA.GRF" = "orange")) +
      theme_bw()
    
    Int.score.sum <- true.validation.df %>% 
      ggplot(aes(x = extrap.type, y = Sum.Int.Score, fill = mod.type)) +
      geom_boxplot() +
      labs(x = "Extrapolation", y = "Sum Interval Score", fill = "Model Type") +
      scale_x_discrete(labels = c("Low", "Mod", "High")) +
      scale_fill_manual(values = c("Integrated.GRF" = "purple", "PO.GRF" = "skyblue", "PA.GRF" = "orange")) +
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
  
  
  }
  



####################################
# OPTIONAL - PLOT VALIDATION SITE A ---------------------------------------
###################################

plot_validation_SiteA_func <- function(true.validation.df,
                                       save = FALSE,
                                       outpath,
                                       scenario_name,
                                       mod.type) {
  
  
  if(mod.type == "non-spatial") {
    
    # Plot the validation
    
    cor <- true.validation.df %>% 
      ggplot(aes(x = extrap.type, y = correlation, fill = mod.type)) +
      geom_boxplot() +
      labs(x = "Extrapolation", y = "Correlation", fill = "Model Type") +
      scale_x_discrete(labels = c("Low", "Mod", "High")) +
      scale_fill_manual(values = c("Integrated.no.GRF" = "purple", "PO.no.GRF" = "skyblue", "PA.no.GRF" = "orange")) +
      theme_bw()
    
    MAE <- true.validation.df %>% 
      ggplot(aes(x = extrap.type, y = MAE, fill = mod.type)) +
      geom_boxplot() +
      labs(x = "Extrapolation", y = "MAE", fill = "Model Type") +
      scale_x_discrete(labels = c("Low", "Mod", "High")) +
      scale_fill_manual(values = c("Integrated.no.GRF" = "purple", "PO.no.GRF" = "skyblue", "PA.no.GRF" = "orange")) +
      theme_bw()
    
    RMSE <- true.validation.df %>% 
      ggplot(aes(x = extrap.type, y = RMSE, fill = mod.type)) +
      geom_boxplot() +
      labs(x = "Extrapolation", y = "RMSE", fill = "Model Type") +
      scale_x_discrete(labels = c("Low", "Mod", "High")) +
      scale_fill_manual(values = c("Integrated.no.GRF" = "purple", "PO.no.GRF" = "skyblue", "PA.no.GRF" = "orange")) +
      theme_bw()
    
    ## Smaller values of interval score are better
    
    Int.score.mean <- true.validation.df %>% 
      ggplot(aes(x = extrap.type, y = Mean.Int.Score, fill = mod.type)) +
      geom_boxplot() +
      labs(x = "Extrapolation", y = "Mean Interval Score", fill = "Model Type") +
      scale_x_discrete(labels = c("Low", "Mod", "High")) +
      scale_fill_manual(values = c("Integrated.no.GRF" = "purple", "PO.no.GRF" = "skyblue", "PA.no.GRF" = "orange")) +
      theme_bw()
    
    Int.score.sum <- true.validation.df %>% 
      ggplot(aes(x = extrap.type, y = Sum.Int.Score, fill = mod.type)) +
      geom_boxplot() +
      labs(x = "Extrapolation", y = "Sum Interval Score", fill = "Model Type") +
      scale_x_discrete(labels = c("Low", "Mod", "High")) +
      scale_fill_manual(values = c("Integrated.no.GRF" = "purple", "PO.no.GRF" = "skyblue", "PA.no.GRF" = "orange")) +
      theme_bw()
    
    p1 <- ggarrange(RMSE, MAE, common.legend = T,  ncol = 2, nrow = 1)
    
    p2 <- ggarrange(Int.score.mean, Int.score.sum, common.legend = T,  ncol = 2, nrow = 1)
    
    if(save == TRUE) {
      
      # Make directory for Site A
      dir.create(file.path(outpath, scenario_name, "SiteA"), showWarnings = FALSE)
      
      ggsave(plot = p1, filename = paste0(file.path(outpath, scenario_name, "SiteA"),"/Scenario_", scenario_name, "_RMSE_MAE_plot.png"), w = 21.5, h = 15, units = "cm", dpi = 400, device = "png")
      
      ggsave(plot = p2, filename = paste0(file.path(outpath, scenario_name, "SiteA"),"/Scenario_", scenario_name, "_Int_Score_plot.png"), w = 21.5, h = 15, units = "cm", dpi = 400, device = "png")
      
      ggsave(plot = cor, filename = paste0(file.path(outpath, scenario_name, "SiteA"),"/Scenario_", scenario_name, "_COR_plot.png"), w = 21.5, h = 15, units = "cm", dpi = 400, device = "png")
      
      
    } else{print(p1) 
      
      print(p2) 
      
      print(cor)}
    
    
  }
  
  

# If model is SPATIAL -----------------------------------------------------

  if(mod.type == "spatial") {
    
    
    # Plot the validation
    
    cor <- true.validation.df %>% 
      ggplot(aes(x = extrap.type, y = correlation, fill = mod.type)) +
      geom_boxplot() +
      labs(x = "Extrapolation", y = "Correlation", fill = "Model Type") +
      scale_x_discrete(labels = c("Low", "Mod", "High")) +
      scale_fill_manual(values = c("Integrated.GRF" = "purple", "PO.GRF" = "skyblue", "PA.GRF" = "orange")) +
      theme_bw()
    
    MAE <- true.validation.df %>% 
      ggplot(aes(x = extrap.type, y = MAE, fill = mod.type)) +
      geom_boxplot() +
      labs(x = "Extrapolation", y = "MAE", fill = "Model Type") +
      scale_x_discrete(labels = c("Low", "Mod", "High")) +
      scale_fill_manual(values = c("Integrated.GRF" = "purple", "PO.GRF" = "skyblue", "PA.GRF" = "orange")) +
      theme_bw()
    
    RMSE <- true.validation.df %>% 
      ggplot(aes(x = extrap.type, y = RMSE, fill = mod.type)) +
      geom_boxplot() +
      labs(x = "Extrapolation", y = "RMSE", fill = "Model Type") +
      scale_x_discrete(labels = c("Low", "Mod", "High")) +
      scale_fill_manual(values = c("Integrated.GRF" = "purple", "PO.GRF" = "skyblue", "PA.GRF" = "orange")) +
      theme_bw()
    
    ## Smaller values of interval score are better
    
    Int.score.mean <- true.validation.df %>% 
      ggplot(aes(x = extrap.type, y = Mean.Int.Score, fill = mod.type)) +
      geom_boxplot() +
      labs(x = "Extrapolation", y = "Mean Interval Score", fill = "Model Type") +
      scale_x_discrete(labels = c("Low", "Mod", "High")) +
      scale_fill_manual(values = c("Integrated.GRF" = "purple", "PO.GRF" = "skyblue", "PA.GRF" = "orange")) +
      theme_bw()
    
    Int.score.sum <- true.validation.df %>% 
      ggplot(aes(x = extrap.type, y = Sum.Int.Score, fill = mod.type)) +
      geom_boxplot() +
      labs(x = "Extrapolation", y = "Sum Interval Score", fill = "Model Type") +
      scale_x_discrete(labels = c("Low", "Mod", "High")) +
      scale_fill_manual(values = c("Integrated.GRF" = "purple", "PO.GRF" = "skyblue", "PA.GRF" = "orange")) +
      theme_bw()
    
    p1 <- ggarrange(RMSE, MAE, common.legend = T,  ncol = 2, nrow = 1)
    
    p2 <- ggarrange(Int.score.mean, Int.score.sum, common.legend = T,  ncol = 2, nrow = 1)
    
    if(save == TRUE) {
      
      # Make directory for Site A
      dir.create(file.path(outpath, scenario_name, "SiteA"), showWarnings = FALSE)
      
      ggsave(plot = p1, filename = paste0(file.path(outpath, scenario_name, "SiteA"),"/Scenario_", scenario_name, "_RMSE_MAE_plot.png"), w = 21.5, h = 15, units = "cm", dpi = 400, device = "png")
      
      ggsave(plot = p2, filename = paste0(file.path(outpath, scenario_name, "SiteA"),"/Scenario_", scenario_name, "_Int_Score_plot.png"), w = 21.5, h = 15, units = "cm", dpi = 400, device = "png")
      
      ggsave(plot = cor, filename = paste0(file.path(outpath, scenario_name, "SiteA"),"/Scenario_", scenario_name, "_COR_plot.png"), w = 21.5, h = 15, units = "cm", dpi = 400, device = "png")
      
      
    } else{print(p1) 
      
      print(p2) 
      
      print(cor)}
    
    
  }
  
  
}




# # PLOT VALIDATION CONTINUOUS EXTRAPOLATION --------------------------------
# 
# plot_validation_SiteB_continuous_func <- function(true.validation.df,
#                                                   save = FALSE,
#                                                   outpath,
#                                                   scenario_name,
#                                                   mod.type) {
#   # Plot the validation
# 
#   cor <- true.validation.df %>%
#     ggplot(aes(x = extrap.median, y = correlation, color = mod.type)) +
#     geom_point(alpha = 0.1) +
#     geom_smooth(method = "loess", se = T, aes(fill = mod.type, color = mod.type), alpha = 0.2) +
#     labs(x = "Extrapolation", y = "Correlation", fill = "Model Type") +
#     scale_color_manual(values = c("Integrated.GRF" = "purple", "PO.GRF" = "skyblue", "PA.GRF" = "orange")) +
#     scale_fill_manual(values = c("Integrated.GRF" = "purple", "PO.GRF" = "skyblue", "PA.GRF" = "orange")) +
#     # coord_cartesian(xlim = c(NA, 50)) +
#     theme_bw()
# 
#   true.validation.df %>% 
#     ggplot(aes(x = extrap.median, y = MAE, color = mod.type)) +
#     geom_point(alpha = 0.5) +
#     geom_smooth(method = "loess", se = T, aes(fill = mod.type, color = mod.type), alpha = 0.2) +
#         labs(x = "Extrapolation", y = "MAE", fill = "Model Type") +
#     scale_color_manual(values = c("Integrated.GRF" = "purple", "PO.GRF" = "skyblue", "PA.GRF" = "orange")) +
#     scale_fill_manual(values = c("Integrated.GRF" = "purple", "PO.GRF" = "skyblue", "PA.GRF" = "orange")) +
#     coord_cartesian(xlim = c(NA, 50)) +
#     theme_bw()
#   
# 
#   }
  
  

