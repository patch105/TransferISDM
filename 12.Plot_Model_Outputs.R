

# 12. Plot Model Outputs --------------------------------------------------


# Plot residuals ----------------------------------------------------------


plot_residuals_func <- function(reps.setup.list, 
                                outpath,
                                scenario_name) {
  
  # Get the names of the extrap types for indexing
  extrap_names <- names(reps.setup.list)
  
  for(extrap.type in seq_along(reps.setup.list)) {
    
    # Extract the name ("Low") for indexing from the names list
    name <- extrap_names[extrap.type]
    
    # For every replicate
    for(rep in seq_along(reps.setup.list[[name]])) {
      
      # Get path for saving
      rep_path <- file.path(outpath, scenario_name, name, paste0("Rep_", rep))
      
      # Extract the models dataframe [[name]] double brackets for list extract
      models_df <- reps.setup.list[[name]][[rep]]$models
      
      for (i in seq_along(models_df)) {
        
        png(paste0(rep_path, "/plot.isdm_EXTRAP_", extrap_names[extrap.type], "_", as.character(models_df[i, "Mod.type"]),  ".png"), width = 10, height = 10, units = "in", res = 300)
        
        plot(models_df[[i, 'Model']], nFigRow = 2, ask = FALSE)
        
        dev.off()
        
      }
      
    }
    
  }
  
}


# Plot parameter recovery -----------------------------------------------

plot_parameter_recovery_func <- function(outpath,
                                    scenario_name,
                                    extrap.scenario.df,
                                    save,
                                    beta1,
                                    beta2,
                                    beta0,
                                    scal,
                                    variance,
                                    mod.type) {

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
  

  ##### Plot the mean of coefficients #####
  
  b1 <- extrap.scenario.df %>% 
    ggplot(aes(x = extrap.type, y = beta1.mean, fill = mod.type)) +
    geom_boxplot() +
    geom_hline(yintercept = beta1, linetype = "dashed", color = "red") +
    labs(x = "Extrapolation", y = expression(beta[1]), fill = "Model Type") +
    scale_x_discrete(labels = c("Low", "Mod", "High")) +
    scale_fill_manual(values = fill.colours) +
    theme_bw()
  
  b2 <- extrap.scenario.df %>% 
    ggplot(aes(x = extrap.type, y = beta2.mean, fill = mod.type)) +
    geom_boxplot() +
    geom_hline(yintercept = beta2, linetype = "dashed", color = "red") +
    labs(x = "Extrapolation", y = expression(beta[2]), fill = "Model Type") +
    scale_x_discrete(labels = c("Low", "Mod", "High")) +
    scale_fill_manual(values = fill.colours) +
    theme_bw()
  
  beta_plot <- ggarrange(b1 , b2, common.legend = T,  ncol = 2, nrow = 1)
  
  if(save == TRUE) {
    
    ggsave(plot = beta_plot, filename = paste0(file.path(outpath, scenario_name),"/Scenario_", scenario_name, "_Coef_Recovery_plot.png"), w = 21.5, h = 15, units = "cm", dpi = 400, device = "png")
    
    
  } else {print(beta_plot)}
  
  ##### Plot the mean width of the credible interval #####
  
  extrap.scenario.df.CI <- extrap.scenario.df %>% 
    mutate(beta1.cred.int = beta1_975 - beta1_25,
           beta2.cred.int = beta2_975 - beta2_25,
           beta1.cred.int.true = ifelse(beta1 >= beta1_25 &  beta1 <= beta1_975, 1, 0),
           beta2.cred.int.true = ifelse(beta2 >= beta2_25 & beta2 <= beta2_975, 1, 0))
  
  b1.CI.width <- extrap.scenario.df.CI %>% 
    ggplot(aes(x = extrap.type, y = beta1.cred.int, fill = mod.type)) +
    geom_boxplot() +
    labs(x = "Extrapolation", y = bquote(beta[1] ~ " Credible Interval Width"), fill = "Model Type") +
    scale_x_discrete(labels = c("Low", "Mod", "High")) +
    scale_fill_manual(values = fill.colours) +
    theme_bw()
  
  b2.CI.width <- extrap.scenario.df.CI %>% 
    ggplot(aes(x = extrap.type, y = beta2.cred.int, fill = mod.type)) +
    geom_boxplot() +
    labs(x = "Extrapolation", y = bquote(beta[2] ~ " Credible Interval Width"), fill = "Model Type") +
    scale_x_discrete(labels = c("Low", "Mod", "High")) +
    scale_fill_manual(values = fill.colours) +
    theme_bw()
  
  beta_CI_width_plot <- ggarrange(b1.CI.width , b2.CI.width, common.legend = T,  ncol = 2, nrow = 1)
  
  if(save == TRUE) {
    
    ggsave(plot = beta_CI_width_plot, filename = paste0(file.path(outpath, scenario_name),"/Scenario_", scenario_name, "_Beta_CI_Width_plot.png"), w = 21.5, h = 15, units = "cm", dpi = 400, device = "png")
    
    
  } else {print(beta_CI_width_plot)}
  
  
  ##### Plot the intercepts #####
  
  po.int <- extrap.scenario.df %>%
    filter(!is.na(PO_intercept)) %>% 
    ggplot(aes(x = extrap.type, y = PO_intercept, fill = mod.type)) +
    geom_boxplot() +
    geom_hline(yintercept = beta0, linetype = "dashed", color = "red") +
    labs(x = "Extrapolation", y = expression(beta[0]), fill = "Model Type") +
    scale_x_discrete(labels = c("Low", "Mod", "High")) +
    scale_fill_manual(values = fill.colours) +
    theme_bw() +
    ggtitle('PO Intercept')
  
  pa.int <- extrap.scenario.df %>%
    filter(!is.na(PA_intercept)) %>%
    ggplot(aes(x = extrap.type, y = PA_intercept, fill = mod.type)) +
    geom_boxplot() +
    geom_hline(yintercept = beta0, linetype = "dashed", color = "red") +
    labs(x = "Extrapolation", y = expression(beta[0]), fill = "Model Type") +
    scale_x_discrete(labels = c("Low", "Mod", "High")) +
    scale_fill_manual(values = fill.colours) +
    theme_bw() +
    ggtitle('PA Intercept')
  
  intercepts_plot <- ggarrange(po.int , pa.int,  ncol = 2, nrow = 1)
  
  if(save == TRUE) {
    
    ggsave(plot = intercepts_plot, filename = paste0(file.path(outpath, scenario_name),"/Scenario_", scenario_name, "_Intercepts_Plot.png"), w = 21.5, h = 15, units = "cm", dpi = 400, device = "png")
    
    
  } else {print(intercepts_plot)}
  
  ##### Plot the marginal likelihood #####
  
  m.lik <- extrap.scenario.df %>% 
    ggplot(aes(x = extrap.type, y = marg_lik, fill = mod.type)) +
    geom_boxplot() +
    labs(x = "Extrapolation", y = "Marginal Likelihood", fill = "Model Type") +
    scale_x_discrete(labels = c("Low", "Mod", "High")) +
    scale_fill_manual(values = fill.colours) +
    theme_bw()
  
  if(save == TRUE) {
    
    ggsave(plot = m.lik, filename = paste0(file.path(outpath, scenario_name),"/Scenario_", scenario_name, "_Marginal_Likelihood_plot.png"), w = 21.5, h = 15, units = "cm", dpi = 400, device = "png")
    
    
  } else {print(m.lik)}
  
  ##### Table for whether credible interval contains true betas #####
  
  beta1_cred_int_true <- extrap.scenario.df.CI %>% 
    group_by(mod.type) %>% 
    summarise(prop.beta1.cred.int.true = sum(beta1.cred.int.true) / n()) 
  
  beta2_cred_int_true <- extrap.scenario.df.CI %>%
    group_by(mod.type) %>% 
    summarise(prop.beta2.cred.int.true = sum(beta2.cred.int.true) / n())
  
  beta_cred_int_true <- merge(beta1_cred_int_true, beta2_cred_int_true, by = "mod.type")
  
  write.csv(beta_cred_int_true, file = paste0(file.path(outpath, scenario_name),"/Scenario_", scenario_name, "_Prop_Cred_Int_Contains_True_Beta.csv"))
  


  # If there are spatial models ------------------------------------------
  
  if(sum(grepl("spatial", mod.type, fixed = T)) > 0) {
    
    g1 <- extrap.scenario.df %>% 
      filter(!is.na(GRF.range.mean)) %>%
      ggplot(aes(x = extrap.type, y = GRF.range.mean, fill = mod.type)) +
      geom_boxplot() +
      geom_hline(yintercept = scal, linetype = "dashed", color = "red") +
      labs(x = "Extrapolation", y = "GRF range", fill = "Model Type") +
      scale_x_discrete(labels = c("Low", "Mod", "High")) +
      scale_fill_manual(values = fill.colours) +
      theme_bw()
    
    g2 <- extrap.scenario.df %>% 
      filter(!is.na(GRF.sd.mean)) %>% 
      ggplot(aes(x = extrap.type, y = GRF.sd.mean^2, fill = mod.type)) +
      geom_boxplot() +
      geom_hline(yintercept = variance, linetype = "dashed", color = "red") +
      labs(x = "Extrapolation", y = "GRF svar", fill = "Model Type") +
      scale_x_discrete(labels = c("Low", "Mod", "High")) +
      scale_fill_manual(values = fill.colours) +
      theme_bw()
    
    GRF_plot <- ggarrange(g1, g2, common.legend = T, ncol = 2, nrow = 1)
    
    if(save == TRUE) {
      
      ggsave(plot = GRF_plot, filename = paste0(file.path(outpath, scenario_name),"/Scenario_", scenario_name, "_GRF_Coef_Recovery_plot.png"), w = 21.5, h = 15, units = "cm", dpi = 400, device = "png")
      
    } else {
      
      print(GRF_plot)
    }
   
    
    ##### Plot the mean width of the credible interval #####
    
    extrap.scenario.df.CI <- extrap.scenario.df %>% 
      mutate(GRF.range.cred.int = GRF.range_975 - GRF.range_25,
             GRF.var.cred.int = (GRF.sd_975)^2 - (GRF.sd_25)^2,
             GRF.range.cred.int.true = ifelse(scal >= GRF.range_25 &  scal <= GRF.range_975, 1, 0),
             GRF.var.cred.int.true = ifelse(variance >= (GRF.sd_25)^2 &  variance <= (GRF.sd_975)^2, 1, 0))
    
    g1.CI.width <- extrap.scenario.df.CI %>% 
      filter(!is.na(GRF.range.cred.int)) %>% 
      ggplot(aes(x = extrap.type, y = GRF.range.cred.int, fill = mod.type)) +
      geom_boxplot() +
      labs(x = "Extrapolation", y = "GRF range Credible Interval Width", fill = "Model Type") +
      scale_x_discrete(labels = c("Low", "Mod", "High")) +
      scale_fill_manual(values = fill.colours) +
      theme_bw()
    
    g2.CI.width <- extrap.scenario.df.CI %>%
      filter(!is.na(GRF.var.cred.int)) %>% 
      ggplot(aes(x = extrap.type, y = GRF.var.cred.int, fill = mod.type)) +
      geom_boxplot() +
      labs(x = "Extrapolation", y = "GRF var Credible Interval Width", fill = "Model Type") +
      scale_x_discrete(labels = c("Low", "Mod", "High")) +
      scale_fill_manual(values = fill.colours) +
      theme_bw()
    
    GRF_CI_width_plot <- ggarrange(g1.CI.width, g2.CI.width, common.legend = T, ncol = 2, nrow = 1)
    
    if(save == TRUE) {

       ggsave(plot = GRF_CI_width_plot, filename = paste0(file.path(outpath, scenario_name),"/Scenario_", scenario_name, "_GRF_CI_Width_plot.png"), w = 21.5, h = 15, units = "cm", dpi = 400, device = "png")
      
      
    } else {
      
      print(GRF_CI_width_plot)
    }
    
    
    ##### Table for whether credible interval contains true GRF #####
    
    GRF.range_cred_int_true <- extrap.scenario.df.CI %>% 
      filter(!is.na(GRF.range.cred.int)) %>%
      group_by(mod.type) %>% 
      summarise(prop.GRF.range.cred.int.true = sum(GRF.range.cred.int.true) / n()) 
    
    GRF.var_cred_int_true <- extrap.scenario.df.CI %>%
      filter(!is.na(GRF.var.cred.int)) %>%
      group_by(mod.type) %>% 
      summarise(prop.GRF.var.cred.int.true = sum(GRF.var.cred.int.true) / n())
    
    GRF_cred_int_true <- merge(GRF.range_cred_int_true, GRF.var_cred_int_true, by = "mod.type")
    
    write.csv(GRF_cred_int_true, file = paste0(file.path(outpath, scenario_name),"/Scenario_", scenario_name, "_Prop_Cred_Int_Contains_True_GRF.csv"))
      
  }
  
  
  # If there are bias models  ------------------------------------------
  
  if(sum(grepl("bias", mod.type, fixed = T)) > 0) {
    
    bias_plot <- extrap.scenario.df %>% 
      filter(!is.na(bias.coef.mean)) %>%
      ggplot(aes(x = extrap.type, y = bias.coef.mean, fill = mod.type)) +
      geom_boxplot() +
      labs(x = "Extrapolation", y = "Bias coef", fill = "Model Type") +
      scale_x_discrete(labels = c("Low", "Mod", "High")) +
      scale_fill_manual(values = fill.colours) +
      theme_bw()
    

    if(save == TRUE) {
      
      ggsave(plot = bias_plot, filename = paste0(file.path(outpath, scenario_name),"/Scenario_", scenario_name, "_Bias_Coef_Recovery_plot.png"), w = 21.5, h = 15, units = "cm", dpi = 400, device = "png")
      
    } else {
      
      print(bias_plot)
    }
    
    
    ##### Plot the mean width of the credible interval #####
    
    extrap.scenario.df.CI <- extrap.scenario.df %>% 
      mutate(bias.coef.cred.int = bias.coef_975 - bias.coef_25)
    
    bias.CI.width.plot <- extrap.scenario.df.CI %>% 
      filter(!is.na(bias.coef.cred.int)) %>% 
      ggplot(aes(x = extrap.type, y = bias.coef.cred.int, fill = mod.type)) +
      geom_boxplot() +
      labs(x = "Extrapolation", y = "Bias Credible Interval Width", fill = "Model Type") +
      scale_x_discrete(labels = c("Low", "Mod", "High")) +
      scale_fill_manual(values = fill.colours) +
      theme_bw()
  
    
    if(save == TRUE) {
      
      ggsave(plot = bias.CI.width.plot, filename = paste0(file.path(outpath, scenario_name),"/Scenario_", scenario_name, "_Bias_CI_Width_plot.png"), w = 21.5, h = 15, units = "cm", dpi = 400, device = "png")
      
      
    } else {
      
      print(bias.CI.width.plot)
    }
    
  }
  
}
  
  
   

# PLOT VALIDATION CONTINUOUS EXTRAPOLATION --------------------------------

plot_parameter_recovery_continuous_EXTRAP_func <- function(extrap.scenario.df,
                                                  save,
                                                  outpath,
                                                  scenario_name,
                                                  mod.type) {
  # Plot the validation

  b1 <- extrap.scenario.df %>%
    ggplot(aes(x = extrap.median, y = beta1.mean, color = mod.type)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "loess", se = T, aes(fill = mod.type, color = mod.type), alpha = 0.2) +
    geom_hline(yintercept = beta1, linetype = "dashed", color = "red") +
    labs(x = "Extrapolation", y = expression(beta[1]), fill = "Model Type") +
    scale_color_manual(values = c("Integrated.GRF" = "purple", "PO.GRF" = "skyblue", "PA.GRF" = "orange")) +
    scale_fill_manual(values = c("Integrated.GRF" = "purple", "PO.GRF" = "skyblue", "PA.GRF" = "orange")) +
    coord_cartesian(xlim = c(NA, 50)) +
    theme_bw()

  extrap.scenario.df %>%
    ggplot(aes(x = extrap.median, y = beta2.mean, color = mod.type)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "loess", se = T, aes(fill = mod.type, color = mod.type), alpha = 0.2) +
    geom_hline(yintercept = beta2, linetype = "dashed", color = "red") +
    labs(x = "Extrapolation", y = expression(beta[1]), fill = "Model Type") +
    scale_color_manual(values = c("Integrated.GRF" = "purple", "PO.GRF" = "skyblue", "PA.GRF" = "orange")) +
    scale_fill_manual(values = c("Integrated.GRF" = "purple", "PO.GRF" = "skyblue", "PA.GRF" = "orange")) +
    # coord_cartesian(xlim = c(NA, 50)) +
    theme_bw()

  }

