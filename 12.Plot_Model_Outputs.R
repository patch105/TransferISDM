

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

plot_parameter_recovery_func <- function(reps.setup.list,
                                    outpath,
                                    scenario_name,
                                    extrap.scenario.df,
                                    save = FALSE,
                                    beta1,
                                    beta2,
                                    beta0,
                                    mod.type) {

  

# Non-spatial version -----------------------------------------------------
  
  if(mod.type == "non-spatial") {
    
    ##### Plot the mean of coefficients #####
    
    b1 <- extrap.scenario.df %>% 
      ggplot(aes(x = extrap.type, y = beta1.mean, fill = mod.type)) +
      geom_boxplot() +
      geom_hline(yintercept = beta1, linetype = "dashed", color = "red") +
      labs(x = "Extrapolation", y = expression(beta[1]), fill = "Model Type") +
      scale_x_discrete(labels = c("Low", "Mod", "High")) +
      scale_fill_manual(values = c("Integrated.no.GRF" = "purple", "PO.no.GRF" = "skyblue", "PA.no.GRF" = "orange")) +
      theme_bw()
    
    b2 <- extrap.scenario.df %>% 
      ggplot(aes(x = extrap.type, y = beta2.mean, fill = mod.type)) +
      geom_boxplot() +
      geom_hline(yintercept = beta2, linetype = "dashed", color = "red") +
      labs(x = "Extrapolation", y = expression(beta[2]), fill = "Model Type") +
      scale_x_discrete(labels = c("Low", "Mod", "High")) +
      scale_fill_manual(values = c("Integrated.no.GRF" = "purple", "PO.no.GRF" = "skyblue", "PA.no.GRF" = "orange")) +
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
      scale_fill_manual(values = c("Integrated.no.GRF" = "purple", "PO.no.GRF" = "skyblue", "PA.no.GRF" = "orange")) +
      theme_bw()
    
    b2.CI.width <- extrap.scenario.df.CI %>% 
      ggplot(aes(x = extrap.type, y = beta2.cred.int, fill = mod.type)) +
      geom_boxplot() +
      labs(x = "Extrapolation", y = bquote(beta[2] ~ " Credible Interval Width"), fill = "Model Type") +
      scale_x_discrete(labels = c("Low", "Mod", "High")) +
      scale_fill_manual(values = c("Integrated.no.GRF" = "purple", "PO.no.GRF" = "skyblue", "PA.no.GRF" = "orange")) +
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
      scale_fill_manual(values = c("Integrated.no.GRF" = "purple", "PO.no.GRF" = "skyblue", "PA.no.GRF" = "orange")) +
      theme_bw() +
      ggtitle('PO Intercept')
    
    pa.int <- extrap.scenario.df %>%
      filter(!is.na(PA_intercept)) %>%
      ggplot(aes(x = extrap.type, y = PA_intercept, fill = mod.type)) +
      geom_boxplot() +
      geom_hline(yintercept = beta0, linetype = "dashed", color = "red") +
      labs(x = "Extrapolation", y = expression(beta[0]), fill = "Model Type") +
      scale_x_discrete(labels = c("Low", "Mod", "High")) +
      scale_fill_manual(values = c("Integrated.no.GRF" = "purple", "PO.no.GRF" = "skyblue", "PA.no.GRF" = "orange")) +
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
      scale_fill_manual(values = c("Integrated.no.GRF" = "purple", "PO.no.GRF" = "skyblue", "PA.no.GRF" = "orange")) +
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
    
  }
    
    




# Spatial version -----------------------------------------------------

if(mod.type == "spatial") {
  
  ##### Plot the mean of coefficients #####
  
  b1 <- extrap.scenario.df %>% 
    ggplot(aes(x = extrap.type, y = beta1.mean, fill = mod.type)) +
    geom_boxplot() +
    geom_hline(yintercept = beta1, linetype = "dashed", color = "red") +
    labs(x = "Extrapolation", y = expression(beta[1]), fill = "Model Type") +
    scale_x_discrete(labels = c("Low", "Mod", "High")) +
    scale_fill_manual(values = c("Integrated.GRF" = "purple", "PO.GRF" = "skyblue", "PA.GRF" = "orange")) +
    theme_bw()
  
  b2 <- extrap.scenario.df %>% 
    ggplot(aes(x = extrap.type, y = beta2.mean, fill = mod.type)) +
    geom_boxplot() +
    geom_hline(yintercept = beta2, linetype = "dashed", color = "red") +
    labs(x = "Extrapolation", y = expression(beta[2]), fill = "Model Type") +
    scale_x_discrete(labels = c("Low", "Mod", "High")) +
    scale_fill_manual(values = c("Integrated.GRF" = "purple", "PO.GRF" = "skyblue", "PA.GRF" = "orange")) +
    theme_bw()
  
  beta_plot <- ggarrange(b1 , b2, common.legend = T,  ncol = 2, nrow = 1)
  
  g1 <- extrap.scenario.df %>% 
    ggplot(aes(x = extrap.type, y = GRF.range.mean, fill = mod.type)) +
    geom_boxplot() +
    geom_hline(yintercept = scal, linetype = "dashed", color = "red") +
    labs(x = "Extrapolation", y = "GRF range", fill = "Model Type") +
    scale_x_discrete(labels = c("Low", "Mod", "High")) +
    scale_fill_manual(values = c("Integrated.GRF" = "purple", "PO.GRF" = "skyblue", "PA.GRF" = "orange")) +
    theme_bw()
    
  g2 <- extrap.scenario.df %>% 
    ggplot(aes(x = extrap.type, y = GRF.range.sd.mean, fill = mod.type)) +
    geom_boxplot() +
    geom_hline(yintercept = var, linetype = "dashed", color = "red") +
    labs(x = "Extrapolation", y = "GRF range sd", fill = "Model Type") +
    scale_x_discrete(labels = c("Low", "Mod", "High")) +
    scale_fill_manual(values = c("Integrated.GRF" = "purple", "PO.GRF" = "skyblue", "PA.GRF" = "orange")) +
    theme_bw()
  
  GRF.plot <- ggarrange(g1, g2, common.legend = T, ncol = 2, nrow = 1)
  
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
    scale_fill_manual(values = c("Integrated.GRF" = "purple", "PO.GRF" = "skyblue", "PA.GRF" = "orange")) +
    theme_bw()
  
  b2.CI.width <- extrap.scenario.df.CI %>% 
    ggplot(aes(x = extrap.type, y = beta2.cred.int, fill = mod.type)) +
    geom_boxplot() +
    labs(x = "Extrapolation", y = bquote(beta[2] ~ " Credible Interval Width"), fill = "Model Type") +
    scale_x_discrete(labels = c("Low", "Mod", "High")) +
    scale_fill_manual(values = c("Integrated.GRF" = "purple", "PO.GRF" = "skyblue", "PA.GRF" = "orange")) +
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
    scale_fill_manual(values = c("Integrated.GRF" = "purple", "PO.GRF" = "skyblue", "PA.GRF" = "orange")) +
    theme_bw() +
    ggtitle('PO Intercept')
  
  pa.int <- extrap.scenario.df %>%
    filter(!is.na(PA_intercept)) %>%
    ggplot(aes(x = extrap.type, y = PA_intercept, fill = mod.type)) +
    geom_boxplot() +
    geom_hline(yintercept = beta0, linetype = "dashed", color = "red") +
    labs(x = "Extrapolation", y = expression(beta[0]), fill = "Model Type") +
    scale_x_discrete(labels = c("Low", "Mod", "High")) +
    scale_fill_manual(values = c("Integrated.GRF" = "purple", "PO.GRF" = "skyblue", "PA.GRF" = "orange")) +
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
    scale_fill_manual(values = c("Integrated.GRF" = "purple", "PO.GRF" = "skyblue", "PA.GRF" = "orange")) +
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
  

  }


}
  
 

