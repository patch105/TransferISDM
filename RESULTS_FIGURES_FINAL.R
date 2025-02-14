

#lib_loc <- paste(getwd(),"/r_lib",sep="")
lib_loc = .libPaths() 

library(spatstat)
library(ggplot2)
library(dplyr)
library(ggpubr, lib.loc = lib_loc)
library(viridis)
library(terra)
library(purrr)
library(readr)

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

#outpath <- file.path("Z:/ISDM", "output/ARCHIVE/NOV24")
outpath <- file.path("Z:/ISDM", "output")
# outpath <- file.path(getwd(), "output")
result_path <- file.path(getwd(), "output/RESULTS")

############################################################################
# Figure 1. Scenario 1. ---------------------------------------------------
# Environmental extrap with bias
############################################################################



scenario_name = "1"

load(file = paste0(file.path(outpath, scenario_name), "/Scenario_", scenario_name, "_Input_Params.RData"))

# Load and join replicates together ---------------------------------------


# List all the files with validation dataframes (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_Job", full.names = TRUE, recursive = TRUE) 

true.validation.df.list <- lapply(file_list, read.csv)

true.validation.df <- do.call(rbind, true.validation.df.list)


# THEN list all the files with validation dataframes for Site A (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_SiteA_Job", full.names = TRUE, recursive = TRUE) 

true.validation.SiteA.df.list <- lapply(file_list, read.csv)

true.validation.SiteA.df <- do.call(rbind, true.validation.SiteA.df.list)


# FINALLY list all the files with model outputs (for each job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "Results_Summary", full.names = TRUE, recursive = TRUE) 

# Read all the CSV files and save them to a list
extrap.scenario.df.list <- lapply(file_list, read.csv)

# Combine all dataframes in the list into one dataframe using rbind
extrap.scenario.df <- do.call(rbind, extrap.scenario.df.list)


if(scenario.type == "Enviro.Extrap") {
  
  x.label <- "Environmental dissimilarity"
  
  x.discrete.label <- c("Low", "Mod", "High")
  
}

if(scenario.type == "Spatial.Auto") {
  
  x.label <- "Spatial autocorrelation range"
  
  scal.list <- scal
  
  x.discrete.label <- c(as.character(scal))
  
}


# Make the figure

true.validation.df <- true.validation.df %>%
  mutate(bias.type = ifelse(grepl("bias", mod.type, fixed = T), "With bias cov", "Without bias cov")) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PA", "m.PO", "m.int")))

RMSE <- true.validation.df %>% 
  ggplot(aes(x = mean.extrap, y = RMSE, color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.4, size= 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  labs(x = x.label, y = "RMSE", fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_shape_manual(values = c("Without bias cov" = 16, "With bias cov" = 17)) +  # Different shapes for bias types
  scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
  coord_cartesian(ylim = c(NA, 3)) +
  theme_bw() +
  facet_grid(cols = vars(mod.type2), labeller = as_labeller(c(m.int = "Integrated", m.PA = "PA", m.PO = "PO"))) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 13),  # Increase legend text size
        axis.title = element_text(size = 14),   # Increase axis titles
        axis.text = element_text(size = 11),    # Increase axis text
        strip.text = element_text(size = 14),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"))


#ggsave(plot = RMSE, filename = paste0(file.path(result_path),"/Figure_1a_Scenario_", scenario_name, ".png"), w = 23.5, h = 14, units = "cm", dpi = 400, device = "png")

# Now version for joint plot with 1C
RMSE_1 <- true.validation.df %>% 
  ggplot(aes(x = mean.extrap, y = RMSE, color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.4, size= 0.6) +
  geom_smooth(method = "gam", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  labs(x = x.label, y = "RMSE", fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_shape_manual(values = c("Without bias cov" = 16, "With bias cov" = 17)) +  # Different shapes for bias types
  scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
  coord_cartesian(ylim = c(NA, 4.5)) +
  theme_bw() +
  facet_grid(cols = vars(mod.type2), labeller = as_labeller(c(m.int = "Integrated", m.PA = "PA", m.PO = "PO"))) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  # Increase legend text size
        axis.title = element_text(size = 15),   # Increase axis titles
        axis.text = element_text(size = 12),    # Increase axis text
        strip.text = element_text(size = 15),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +  # Move title to the right)
  ggtitle('PO max. detection probability 0.2') 
 
RMSE_2 <- true.validation.df %>% 
  ggplot(aes(x = mean.extrap, y = RMSE, color = bias.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = bias.type, color = bias.type), alpha = 0.3) +
  labs(x = x.label, y = "RMSE") +
  scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) +
  scale_color_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) +
  coord_cartesian(ylim = c(NA, 4.5)) +
  theme_bw() +
  facet_grid(cols = vars(mod.type2), labeller = as_labeller(c(m.int = "Integrated", m.PA = "PA", m.PO = "PO"))) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  # Increase legend text size
        axis.title = element_text(size = 15),   # Increase axis titles
        axis.text = element_text(size = 12),    # Increase axis text
        strip.text = element_text(size = 15),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +  # Move title to the right
  ggtitle('PO max. detection probability 0.2')

#ggsave(plot = RMSE_1, filename = paste0(file.path(result_path),"/Figure_1_ESA_PLOT_", scenario_name, ".png"), w = 23.5, h = 14, units = "cm", dpi = 400, device = "png")


######## SITE A #################

# RMSE<- true.validation.SiteA.df %>% 
#   ggplot(aes(x = mean.extrap, y = RMSE, color = mod.type2, shape = bias.type, linetype = bias.type)) +
#   geom_point(alpha = 0.4, size= 0.6) +
#   geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
#   labs(x = x.label, y = "RMSE", fill = "Model Type", color = "Model Type") +
#   scale_color_manual(values = fill.colours, guide = "none") +
#   scale_fill_manual(values = fill.colours, guide = "none") +
#   scale_shape_manual(values = c("Without bias cov" = 16, "With bias cov" = 17)) +  # Different shapes for bias types
#   scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
#   coord_cartesian(ylim = c(NA, 2.75)) +
#   theme_bw() +
#   facet_grid(cols = vars(mod.type2), labeller = as_labeller(c(m.int = "Integrated", m.PA = "PA", m.PO = "PO"))) +
#   theme(legend.position = "bottom",
#         legend.key.size = unit(1.5, "line"),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 13),  # Increase legend text size
#         axis.title = element_text(size = 14),   # Increase axis titles
#         axis.text = element_text(size = 11),    # Increase axis text
#         strip.text = element_text(size = 14),   # Increase facet title size
#         strip.background = element_rect(fill = "gray96"))
# 
# 
# ggsave(plot = RMSE, filename = paste0(file.path(result_path),"/Figure_1a_SITEA_Scenario_", scenario_name, ".png"), w = 23.5, h = 14, units = "cm", dpi = 400, device = "png")

true.validation.SiteA.df <- true.validation.SiteA.df %>%
  mutate(bias.type = ifelse(grepl("bias", mod.type, fixed = T), "With bias cov", "Without bias cov")) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PA", "m.PO", "m.int")))

# RMSE_A_1 <- true.validation.SiteA.df %>% 
#   ggplot(aes(x = bias.type, y = RMSE, fill = bias.type)) +  # Change x to bias.type for one boxplot per facet
#   geom_boxplot() +
#   labs(x = NULL, y = "RMSE", color = NULL) +
#   scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) +  # Define custom colors
#   coord_cartesian(ylim = c(NA, 4.5)) +
#   theme_bw() +
#   facet_wrap(~mod.type2, labeller = as_labeller(c(m.int = "Integrated", m.PA = "PA", m.PO = "PO")), scales = "free_x") +  # Wrap facets and use free_x to adjust x-axis
#   theme(legend.position = "bottom",
#         legend.key.size = unit(1.5, "line"),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 14),  # Increase legend text size
#         axis.title.x = element_blank(),         # Remove x-axis title
#         axis.text.x = element_blank(),          # Remove x-axis text
#         axis.ticks.x = element_blank(),         # Remove x-axis ticks
#         panel.grid.major.x = element_blank(),   # Remove major grid lines
#         panel.grid.minor.x = element_blank(),   # Remove minor grid lines
#         axis.title.y = element_text(size = 15), # Keep y-axis title
#         axis.text = element_text(size = 12),    # Increase axis text
#         strip.text = element_text(size = 15),   # Increase facet title size
#         strip.background = element_rect(fill = "gray96"),
#         plot.title = element_text(hjust = 1, size = 15, face = "italic")) +  # Move title to the right)
#   ggtitle('PO max. detection probability 0.2')


RMSE_A_1 <- true.validation.SiteA.df %>% 
  ggplot(aes(x = bias.type, y = RMSE, fill = bias.type)) +  
  geom_violin(alpha = 0.2, trim = TRUE, color = NA) +  # Add a transparent violin plot with no outline
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +      # Add a boxplot without outliers
  geom_point(position = position_jitter(width = 0.15, height = 0), alpha = 0.1, size = 0.1, aes(color = bias.type), show.legend = F) +      # Add a boxplot without outliers
  labs(x = NULL, y = "RMSE training", color = NULL) +
  scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  scale_color_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  coord_cartesian(ylim = c(NA, 4.5)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "PA", 
                                      m.PO = "PO")), 
             scales = "free_x") +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  
        axis.title.x = element_blank(),         
        axis.text.x = element_blank(),          
        axis.ticks.x = element_blank(),         
        panel.grid.major.x = element_blank(),   
        panel.grid.minor.x = element_blank(),   
        axis.title.y = element_text(size = 15), 
        axis.text = element_text(size = 12),    
        strip.text = element_text(size = 15),   
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +
  ggtitle('PO max. detection probability 0.2')

COR_A_1 <- true.validation.SiteA.df %>% 
  ggplot(aes(x = bias.type, y = correlation, fill = bias.type)) +  
  geom_violin(alpha = 0.2, trim = TRUE, color = NA) +  # Add a transparent violin plot with no outline
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +      # Add a boxplot without outliers
  geom_point(position = position_jitter(width = 0.15, height = 0), alpha = 0.1, size = 0.1, aes(color = bias.type), show.legend = F) +      # Add a boxplot without outliers
  labs(x = NULL, y = "Correlation training site", color = NULL) +
  scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  scale_color_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  coord_cartesian(ylim = c(0, 1)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "PA", 
                                      m.PO = "PO")), 
             scales = "free_x") +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  
        axis.title.x = element_blank(),         
        axis.text.x = element_blank(),          
        axis.ticks.x = element_blank(),         
        panel.grid.major.x = element_blank(),   
        panel.grid.minor.x = element_blank(),   
        axis.title.y = element_text(size = 15), 
        axis.text = element_text(size = 12),    
        strip.text = element_text(size = 15),   
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +
  ggtitle('PO max. detection probability 0.2')


###### COEFFICIENTS #######

extrap.scenario.df  <- extrap.scenario.df %>% 
  mutate(bias.type = ifelse(grepl("bias", mod.type, fixed = T), "With bias cov", "Without bias cov")) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PA", "m.PO", "m.int"))) 

extrap.scenario.df <- extrap.scenario.df[!is.na(extrap.scenario.df$mod.type2), ]

b1_1 <- extrap.scenario.df %>% 
  ggplot(aes(x = bias.type, y = beta1.mean, fill = bias.type)) +  
  geom_violin(alpha = 0.2, trim = TRUE, color = NA) +  # Add a transparent violin plot with no outline
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +  # Add a boxplot without outliers
  geom_point(position = position_jitter(width = 0.15, height = 0), 
             alpha = 0.1, 
             size = 0.1, 
             aes(color = bias.type), 
             show.legend = FALSE) +  # Add jittered points and remove legend for color
  geom_hline(yintercept = beta1, linetype = "dashed", color = "red") +  # Add the horizontal dashed line
  labs(x = NULL, y = expression(beta[1]), color = NULL) +
  scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  scale_color_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  coord_cartesian(ylim = c(-1.5, 1.5)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "PA", 
                                      m.PO = "PO")), 
             scales = "free_x") +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  
        axis.title.x = element_blank(),         
        axis.text.x = element_blank(),          
        axis.ticks.x = element_blank(),         
        panel.grid.major.x = element_blank(),   
        panel.grid.minor.x = element_blank(),   
        axis.title.y = element_text(size = 15), 
        axis.text = element_text(size = 12),    
        strip.text = element_text(size = 15),   
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +
  ggtitle('PO max. detection probability 0.2')

b2_1 <- extrap.scenario.df %>% 
  ggplot(aes(x = bias.type, y = beta2.mean, fill = bias.type)) +  
  geom_violin(alpha = 0.2, trim = TRUE, color = NA) +  # Add a transparent violin plot with no outline
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +  # Add a boxplot without outliers
  geom_point(position = position_jitter(width = 0.15, height = 0), 
             alpha = 0.1, 
             size = 0.1, 
             aes(color = bias.type), 
             show.legend = FALSE) +  # Add jittered points and remove legend for color
  geom_hline(yintercept = beta2, linetype = "dashed", color = "red") +  # Add the horizontal dashed line
  labs(x = NULL, y = expression(beta[2]), color = NULL) +
  scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  scale_color_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  coord_cartesian(ylim = c(-1.5, 1.5)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "PA", 
                                      m.PO = "PO")), 
             scales = "free_x") +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  
        axis.title.x = element_blank(),         
        axis.text.x = element_blank(),          
        axis.ticks.x = element_blank(),         
        panel.grid.major.x = element_blank(),   
        panel.grid.minor.x = element_blank(),   
        axis.title.y = element_text(size = 15), 
        axis.text = element_text(size = 12),    
        strip.text = element_text(size = 15),   
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +
  ggtitle('PO max. detection probability 0.2')

beta_1 <- ggarrange(b1_1 + rremove("xlab"), b2_1, common.legend = T,  ncol = 1, nrow = 2, legend = "none")

##########################################
##### COEFFICIENT STANDARD DEVIATION #####
##########################################

extrap.scenario.df <- extrap.scenario.df[!is.na(extrap.scenario.df$mod.type2), ]

b1SD_1 <- extrap.scenario.df %>% 
  ggplot(aes(x = bias.type, y = beta1.sd, fill = bias.type)) +  
  geom_violin(alpha = 0.2, trim = TRUE, color = NA) +  # Add a transparent violin plot with no outline
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +  # Add a boxplot without outliers
  geom_point(position = position_jitter(width = 0.15, height = 0), 
             alpha = 0.1, 
             size = 0.1, 
             aes(color = bias.type), 
             show.legend = FALSE) +  # Add jittered points and remove legend for color
  labs(x = NULL, y = expression(beta[1]~"SD"), color = NULL) +
  scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  scale_color_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  coord_cartesian(ylim = c(0, 0.6)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "PA", 
                                      m.PO = "PO")), 
             scales = "free_x") +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  
        axis.title.x = element_blank(),         
        axis.text.x = element_blank(),          
        axis.ticks.x = element_blank(),         
        panel.grid.major.x = element_blank(),   
        panel.grid.minor.x = element_blank(),   
        axis.title.y = element_text(size = 15), 
        axis.text = element_text(size = 12),    
        strip.text = element_text(size = 15),   
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +
  ggtitle('PO max. detection probability 0.2')

b2SD_1 <- extrap.scenario.df %>% 
  ggplot(aes(x = bias.type, y = beta2.sd, fill = bias.type)) +  
  geom_violin(alpha = 0.2, trim = TRUE, color = NA) +  # Add a transparent violin plot with no outline
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +  # Add a boxplot without outliers
  geom_point(position = position_jitter(width = 0.15, height = 0), 
             alpha = 0.1, 
             size = 0.1, 
             aes(color = bias.type), 
             show.legend = FALSE) +  # Add jittered points and remove legend for color
  labs(x = NULL, y = expression(beta[2]~"SD"), color = NULL) +
  scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  scale_color_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  coord_cartesian(ylim = c(0, 0.6)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "PA", 
                                      m.PO = "PO")), 
             scales = "free_x") +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  
        axis.title.x = element_blank(),         
        axis.text.x = element_blank(),          
        axis.ticks.x = element_blank(),         
        panel.grid.major.x = element_blank(),   
        panel.grid.minor.x = element_blank(),   
        axis.title.y = element_text(size = 15), 
        axis.text = element_text(size = 12),    
        strip.text = element_text(size = 15),   
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +
  ggtitle('PO max. detection probability 0.2')

betaSD_1 <- ggarrange(b1SD_1 + rremove("xlab"), b2SD_1, common.legend = T,  ncol = 1, nrow = 2, legend = "none")


#### BETA CREDIBLE INTERVAL WIDTH ####
##### Plot the mean width of the credible interval #####

extrap.scenario.df.CI <- extrap.scenario.df %>% 
  mutate(beta1.cred.int = beta1_975 - beta1_25,
         beta2.cred.int = beta2_975 - beta2_25,
         beta1.cred.int.true = ifelse(beta1 >= beta1_25 &  beta1 <= beta1_975, 1, 0),
         beta2.cred.int.true = ifelse(beta2 >= beta2_25 & beta2 <= beta2_975, 1, 0))

b1.CI.width  <- extrap.scenario.df.CI %>% 
  ggplot(aes(x = bias.type, y = beta1.cred.int, fill = bias.type)) +  
  geom_violin(alpha = 0.2, trim = TRUE, color = NA) +  # Add a transparent violin plot with no outline
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +  # Add a boxplot without outliers
  geom_point(position = position_jitter(width = 0.15, height = 0), 
             alpha = 0.1, 
             size = 0.1, 
             aes(color = bias.type), 
             show.legend = FALSE) +  # Add jittered points and remove legend for color
  labs(x = NULL, y = bquote(beta[1] ~ " CI Width"), color = NULL) +
  scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  scale_color_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  coord_cartesian(ylim = c(NA, 3)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "PA", 
                                      m.PO = "PO")), 
             scales = "free_x") +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  
        axis.title.x = element_blank(),         
        axis.text.x = element_blank(),          
        axis.ticks.x = element_blank(),         
        panel.grid.major.x = element_blank(),   
        panel.grid.minor.x = element_blank(),   
        axis.title.y = element_text(size = 15), 
        axis.text = element_text(size = 12),    
        strip.text = element_text(size = 15),   
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +
  ggtitle('PO max. detection probability 0.2')


b2.CI.width <- extrap.scenario.df.CI %>% 
  ggplot(aes(x = bias.type, y = beta2.cred.int, fill = bias.type)) +  
  geom_violin(alpha = 0.2, trim = TRUE, color = NA) +  # Add a transparent violin plot with no outline
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +  # Add a boxplot without outliers
  geom_point(position = position_jitter(width = 0.15, height = 0), 
             alpha = 0.1, 
             size = 0.1, 
             aes(color = bias.type), 
             show.legend = FALSE) +  # Add jittered points and remove legend for color
  labs(x = NULL, y = bquote(beta[2] ~ " CI Width"), color = NULL) +
  scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  scale_color_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  coord_cartesian(ylim = c(NA, 3)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "PA", 
                                      m.PO = "PO")), 
             scales = "free_x") +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  
        axis.title.x = element_blank(),         
        axis.text.x = element_blank(),          
        axis.ticks.x = element_blank(),         
        panel.grid.major.x = element_blank(),   
        panel.grid.minor.x = element_blank(),   
        axis.title.y = element_text(size = 15), 
        axis.text = element_text(size = 12),    
        strip.text = element_text(size = 15),   
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +
  ggtitle('PO max. detection probability 0.2')

beta_CI_width_plot  <- ggarrange(b1.CI.width + rremove("xlab"), b2.CI.width, common.legend = T,  ncol = 1, nrow = 2, legend = "none")

#### INTERCEPTS ####
PO_INT <- extrap.scenario.df %>% 
  filter(!is.na(PO_intercept)) %>% 
  ggplot(aes(x = bias.type, y = PO_intercept, fill = bias.type)) +  
  geom_violin(alpha = 0.2, trim = TRUE, color = NA) +  # Transparent violin plot without outline
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +  # Boxplot with no outliers
  geom_point(position = position_jitter(width = 0.15, height = 0), 
             alpha = 0.1, 
             size = 0.1, 
             aes(color = bias.type), 
             show.legend = FALSE) +  # Jittered points, no color legend
  geom_hline(yintercept = beta0, linetype = "dashed", color = "red") +  # Dashed horizontal line
  labs(x = NULL, y = expression(beta[0]), color = NULL) +
  scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  scale_color_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  coord_cartesian(ylim = c(-7, 1)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "PA", 
                                      m.PO = "PO")), 
             scales = "free_x") +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  
        axis.title.x = element_blank(),         
        axis.text.x = element_blank(),          
        axis.ticks.x = element_blank(),         
        panel.grid.major.x = element_blank(),   
        panel.grid.minor.x = element_blank(),   
        axis.title.y = element_text(size = 15), 
        axis.text = element_text(size = 12),    
        strip.text = element_text(size = 15),   
        strip.background = element_rect(fill = "gray96")) +
  ggtitle('PO Intercept')


PA_INT <- extrap.scenario.df %>% 
  filter(!is.na(PA_intercept)) %>% 
  ggplot(aes(x = bias.type, y = PA_intercept, fill = bias.type)) +  
  geom_violin(alpha = 0.2, trim = TRUE, color = NA) +  # Transparent violin plot without outline
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +  # Boxplot with no outliers
  geom_point(position = position_jitter(width = 0.15, height = 0), 
             alpha = 0.1, 
             size = 0.1, 
             aes(color = bias.type), 
             show.legend = FALSE) +  # Jittered points, no color legend
  geom_hline(yintercept = beta0, linetype = "dashed", color = "red") +  # Dashed horizontal line
  labs(x = NULL, y = expression(beta[0]), color = NULL) +
  scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  scale_color_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  coord_cartesian(ylim = c(-7, 1)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "PA", 
                                      m.PO = "PO")), 
             scales = "free_x") +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  
        axis.title.x = element_blank(),         
        axis.text.x = element_blank(),          
        axis.ticks.x = element_blank(),         
        panel.grid.major.x = element_blank(),   
        panel.grid.minor.x = element_blank(),   
        axis.title.y = element_text(size = 15), 
        axis.text = element_text(size = 12),    
        strip.text = element_text(size = 15),   
        strip.background = element_rect(fill = "gray96")) +
  ggtitle('PA Intercept')

intercepts <- ggarrange(PO_INT, PA_INT, common.legend = T,  ncol = 2, nrow = 1, legend = "none")

#ggsave(plot = intercepts, filename = paste0(file.path(result_path),"/Figure_1_INTERCEPTS_Scenario_", scenario_name, "_test.png"), w = 22, h = 11, units = "cm", dpi = 400, device = "png")


############################################################################
# Figure 6. Scenario 1C. ---------------------------------------------------
# Environmental extrap with bias & PO detection prob 0.005
############################################################################

scenario_name = "1C"

load(file = paste0(file.path(outpath, scenario_name), "/Scenario_", scenario_name, "_Input_Params.RData"))

# Load and join replicates together ---------------------------------------


# List all the files with validation dataframes (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_Job", full.names = TRUE, recursive = TRUE) 

true.validation.df.list <- lapply(file_list, read.csv)

true.validation.df_1C <- do.call(rbind, true.validation.df.list)


# THEN list all the files with validation dataframes for Site A (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_SiteA_Job", full.names = TRUE, recursive = TRUE) 

true.validation.SiteA.df.list <- lapply(file_list, read.csv)

true.validation.SiteA.df_1C <- do.call(rbind, true.validation.SiteA.df.list)


# FINALLY list all the files with model outputs (for each job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "Results_Summary", full.names = TRUE, recursive = TRUE) 

# Read all the CSV files and save them to a list
extrap.scenario.df.list <- lapply(file_list, read.csv)

# Combine all dataframes in the list into one dataframe using rbind
extrap.scenario.df_1C <- do.call(rbind, extrap.scenario.df.list)


if(scenario.type == "Enviro.Extrap") {
  
  x.label <- "Environmental dissimilarity"
  
  x.discrete.label <- c("Low", "Mod", "High")
  
}

if(scenario.type == "Spatial.Auto") {
  
  x.label <- "Spatial autocorrelation range"
  
  scal.list <- scal
  
  x.discrete.label <- c(as.character(scal))
  
}


# Make the figure

true.validation.df_1C <- true.validation.df_1C %>%
  mutate(bias.type = ifelse(grepl("bias", mod.type, fixed = T), "With bias cov", "Without bias cov")) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PA", "m.PO", "m.int")))


RMSE <- true.validation.df_1C %>% 
  ggplot(aes(x = mean.extrap, y = RMSE, color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.4, size= 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  labs(x = x.label, y = "RMSE", fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_shape_manual(values = c("Without bias cov" = 16, "With bias cov" = 17)) +  # Different shapes for bias types
  scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
  coord_cartesian(ylim = c(NA, 4.5)) +
  theme_bw() +
  facet_grid(cols = vars(mod.type2), labeller = as_labeller(c(m.int = "Integrated", m.PA = "PA", m.PO = "PO"))) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 13),  # Increase legend text size
        axis.title = element_text(size = 14),   # Increase axis titles
        axis.text = element_text(size = 11),    # Increase axis text
        strip.text = element_text(size = 14),   # Increase facet title size
        strip.background = element_rect(fill = "gray96")) 

#ggsave(plot = RMSE, filename = paste0(file.path(result_path),"/Figure_6_Scenario_1B", scenario_name, ".png"), w = 23.5, h = 14, units = "cm", dpi = 400, device = "png")


RMSE_1C <- true.validation.df_1C %>% 
  ggplot(aes(x = mean.extrap, y = RMSE, color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  labs(x = x.label, y = "RMSE", fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_shape_manual(values = c("Without bias cov" = 16, "With bias cov" = 17)) +  # Different shapes for bias types
  scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
  coord_cartesian(ylim = c(NA, 4.5)) +
  theme_bw() +
  facet_grid(cols = vars(mod.type2), labeller = as_labeller(c(m.int = "Integrated", m.PA = "PA", m.PO = "PO"))) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  # Increase legend text size
        axis.title = element_text(size = 15),   # Increase axis titles
        axis.text = element_text(size = 12),    # Increase axis text
        strip.text = element_text(size = 15),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +  # Move title to the right
  ggtitle('PO max. detection probability 0.05')


RMSE_2C <- true.validation.df_1C %>% 
  ggplot(aes(x = mean.extrap, y = RMSE, color = bias.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = bias.type, color = bias.type), alpha = 0.3) +
  labs(x = x.label, y = "RMSE", fill = "Model Type", color = "Model Type") +
  scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) +
  scale_color_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) +
  coord_cartesian(ylim = c(NA, 4.5)) +
  theme_bw() +
  facet_grid(cols = vars(mod.type2), labeller = as_labeller(c(m.int = "Integrated", m.PA = "PA", m.PO = "PO"))) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  # Increase legend text size
        axis.title = element_text(size = 15),   # Increase axis titles
        axis.text = element_text(size = 12),    # Increase axis text
        strip.text = element_text(size = 15),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +  # Move title to the right
  ggtitle('PO max. detection probability 0.05')


#ggsave(plot = RMSE_1C, filename = paste0(file.path(result_path),"/Figure_6_ESA_PLOT_", scenario_name, ".png"), w = 23.5, h = 14, units = "cm", dpi = 400, device = "png")

############## SITE A ##################

# RMSE<- true.validation.SiteA.df_1C %>% 
#   ggplot(aes(x = mean.extrap, y = RMSE, color = mod.type2, shape = bias.type, linetype = bias.type)) +
#   geom_point(alpha = 0.4, size= 0.6) +
#   geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
#   labs(x = x.label, y = "RMSE", fill = "Model Type", color = "Model Type") +
#   scale_color_manual(values = fill.colours, guide = "none") +
#   scale_fill_manual(values = fill.colours, guide = "none") +
#   scale_shape_manual(values = c("Without bias cov" = 16, "With bias cov" = 17)) +  # Different shapes for bias types
#   scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
#   coord_cartesian(ylim = c(NA, 4.5)) +
#   theme_bw() +
#   facet_grid(cols = vars(mod.type2), labeller = as_labeller(c(m.int = "Integrated", m.PA = "PA", m.PO = "PO"))) +
#   theme(legend.position = "bottom",
#         legend.key.size = unit(1.5, "line"),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 13),  # Increase legend text size
#         axis.title = element_text(size = 14),   # Increase axis titles
#         axis.text = element_text(size = 11),    # Increase axis text
#         strip.text = element_text(size = 14),   # Increase facet title size
#         strip.background = element_rect(fill = "gray96"))
# 
# 
# ggsave(plot = RMSE, filename = paste0(file.path(result_path),"/Figure_1a_SITEA_Scenario_", scenario_name, ".png"), w = 23.5, h = 14, units = "cm", dpi = 400, device = "png")

true.validation.SiteA.df_1C <- true.validation.SiteA.df_1C %>%
  mutate(bias.type = ifelse(grepl("bias", mod.type, fixed = T), "With bias cov", "Without bias cov")) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PA", "m.PO", "m.int")))

RMSE_A_1C <- true.validation.SiteA.df_1C %>% 
  ggplot(aes(x = bias.type, y = RMSE, fill = bias.type)) +  
  geom_violin(alpha = 0.2, trim = TRUE, color = NA) +  # Add a transparent violin plot with no outline
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +      # Add a boxplot without outliers
  geom_point(position = position_jitter(width = 0.15, height = 0), alpha = 0.1, size = 0.1, aes(color = bias.type), show.legend = F) +      # Add a boxplot without outliers
  labs(x = NULL, y = "RMSE training", color = NULL) +
  scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  scale_color_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  coord_cartesian(ylim = c(NA, 4.5)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "PA", 
                                      m.PO = "PO")), 
             scales = "free_x") +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  
        axis.title.x = element_blank(),         
        axis.text.x = element_blank(),          
        axis.ticks.x = element_blank(),         
        panel.grid.major.x = element_blank(),   
        panel.grid.minor.x = element_blank(),   
        axis.title.y = element_text(size = 15), 
        axis.text = element_text(size = 12),    
        strip.text = element_text(size = 15),   
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +
  ggtitle('PO max. detection probability 0.05')


COR_A_1C <- true.validation.SiteA.df_1C %>% 
  ggplot(aes(x = bias.type, y = correlation, fill = bias.type)) +  
  geom_violin(alpha = 0.2, trim = TRUE, color = NA) +  # Add a transparent violin plot with no outline
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +      # Add a boxplot without outliers
  geom_point(position = position_jitter(width = 0.15, height = 0), alpha = 0.1, size = 0.1, aes(color = bias.type), show.legend = F) +      # Add a boxplot without outliers
  labs(x = NULL, y = "Correlation training site", color = NULL) +
  scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  scale_color_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  coord_cartesian(ylim = c(0, 1)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "PA", 
                                      m.PO = "PO")), 
             scales = "free_x") +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  
        axis.title.x = element_blank(),         
        axis.text.x = element_blank(),          
        axis.ticks.x = element_blank(),         
        panel.grid.major.x = element_blank(),   
        panel.grid.minor.x = element_blank(),   
        axis.title.y = element_text(size = 15), 
        axis.text = element_text(size = 12),    
        strip.text = element_text(size = 15),   
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +
  ggtitle('PO max. detection probability 0.05')


###### COEFFICIENTS #######

extrap.scenario.df_1C  <- extrap.scenario.df_1C %>% 
  mutate(bias.type = ifelse(grepl("bias", mod.type, fixed = T), "With bias cov", "Without bias cov")) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PA", "m.PO", "m.int"))) 

b1_1C <- extrap.scenario.df_1C %>% 
  ggplot(aes(x = bias.type, y = beta1.mean, fill = bias.type)) +  
  geom_violin(alpha = 0.2, trim = TRUE, color = NA) +  # Add a transparent violin plot with no outline
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +  # Add a boxplot without outliers
  geom_point(position = position_jitter(width = 0.15, height = 0), 
             alpha = 0.1, 
             size = 0.1, 
             aes(color = bias.type), 
             show.legend = FALSE) +  # Add jittered points and remove legend for color
  geom_hline(yintercept = beta1, linetype = "dashed", color = "red") +  # Add the horizontal dashed line
  labs(x = NULL, y = expression(beta[1]), color = NULL) +
  scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  scale_color_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  coord_cartesian(ylim = c(-1.5, 1.5)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "PA", 
                                      m.PO = "PO")), 
             scales = "free_x") +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  
        axis.title.x = element_blank(),         
        axis.text.x = element_blank(),          
        axis.ticks.x = element_blank(),         
        panel.grid.major.x = element_blank(),   
        panel.grid.minor.x = element_blank(),   
        axis.title.y = element_text(size = 15), 
        axis.text = element_text(size = 12),    
        strip.text = element_text(size = 15),   
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +
  ggtitle('PO max. detection probability 0.05')


b2_1C <- extrap.scenario.df_1C %>% 
  ggplot(aes(x = bias.type, y = beta2.mean, fill = bias.type)) +  
  geom_violin(alpha = 0.2, trim = TRUE, color = NA) +  # Add a transparent violin plot with no outline
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +  # Add a boxplot without outliers
  geom_point(position = position_jitter(width = 0.15, height = 0), 
             alpha = 0.1, 
             size = 0.1, 
             aes(color = bias.type), 
             show.legend = FALSE) +  # Add jittered points and remove legend for color
  geom_hline(yintercept = beta2, linetype = "dashed", color = "red") +  # Add the horizontal dashed line
  labs(x = NULL, y = expression(beta[2]), color = NULL) +
  scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  scale_color_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  coord_cartesian(ylim = c(-1.5, 1.5)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "PA", 
                                      m.PO = "PO")), 
             scales = "free_x") +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  
        axis.title.x = element_blank(),         
        axis.text.x = element_blank(),          
        axis.ticks.x = element_blank(),         
        panel.grid.major.x = element_blank(),   
        panel.grid.minor.x = element_blank(),   
        axis.title.y = element_text(size = 15), 
        axis.text = element_text(size = 12),    
        strip.text = element_text(size = 15),   
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic"))

beta_1C <- ggarrange(b1_1C + rremove("xlab"), b2_1C, common.legend = T,  ncol = 1, nrow = 2, legend = "bottom")


##########################################
##### COEFFICIENT STANDARD DEVIATION #####
##########################################

b1SD_1C <- extrap.scenario.df_1C %>% 
  ggplot(aes(x = bias.type, y = beta1.sd, fill = bias.type)) +  
  geom_violin(alpha = 0.2, trim = TRUE, color = NA) +  # Add a transparent violin plot with no outline
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +  # Add a boxplot without outliers
  geom_point(position = position_jitter(width = 0.15, height = 0), 
             alpha = 0.1, 
             size = 0.1, 
             aes(color = bias.type), 
             show.legend = FALSE) +  # Add jittered points and remove legend for color
  labs(x = NULL, y = expression(beta[1]~"SD"), color = NULL) +
  scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  scale_color_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  coord_cartesian(ylim = c(0, 0.6)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "PA", 
                                      m.PO = "PO")), 
             scales = "free_x") +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  
        axis.title.x = element_blank(),         
        axis.text.x = element_blank(),          
        axis.ticks.x = element_blank(),         
        panel.grid.major.x = element_blank(),   
        panel.grid.minor.x = element_blank(),   
        axis.title.y = element_text(size = 15), 
        axis.text = element_text(size = 12),    
        strip.text = element_text(size = 15),   
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +
  ggtitle('PO max. detection probability 0.05')

b2SD_1C <- extrap.scenario.df_1C %>% 
  ggplot(aes(x = bias.type, y = beta2.sd, fill = bias.type)) +  
  geom_violin(alpha = 0.2, trim = TRUE, color = NA) +  # Add a transparent violin plot with no outline
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +  # Add a boxplot without outliers
  geom_point(position = position_jitter(width = 0.15, height = 0), 
             alpha = 0.1, 
             size = 0.1, 
             aes(color = bias.type), 
             show.legend = FALSE) +  # Add jittered points and remove legend for color
  labs(x = NULL, y = expression(beta[2]~"SD"), color = NULL) +
  scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  scale_color_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  coord_cartesian(ylim = c(0, 0.6)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "PA", 
                                      m.PO = "PO")), 
             scales = "free_x") +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  
        axis.title.x = element_blank(),         
        axis.text.x = element_blank(),          
        axis.ticks.x = element_blank(),         
        panel.grid.major.x = element_blank(),   
        panel.grid.minor.x = element_blank(),   
        axis.title.y = element_text(size = 15), 
        axis.text = element_text(size = 12),    
        strip.text = element_text(size = 15),   
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +
  ggtitle('PO max. detection probability 0.05')

betaSD_1C <- ggarrange(b1SD_1C + rremove("xlab"), b2SD_1C, common.legend = T,  ncol = 1, nrow = 2, legend = "none")

#### BETA CREDIBLE INTERVAL WIDTH ####
##### Plot the mean width of the credible interval #####

extrap.scenario.df.CI_1C <- extrap.scenario.df_1C %>% 
  mutate(beta1.cred.int = beta1_975 - beta1_25,
         beta2.cred.int = beta2_975 - beta2_25,
         beta1.cred.int.true = ifelse(beta1 >= beta1_25 &  beta1 <= beta1_975, 1, 0),
         beta2.cred.int.true = ifelse(beta2 >= beta2_25 & beta2 <= beta2_975, 1, 0))

b1.CI.width_1C  <- extrap.scenario.df.CI_1C  %>% 
  ggplot(aes(x = bias.type, y = beta1.cred.int, fill = bias.type)) +  
  geom_violin(alpha = 0.2, trim = TRUE, color = NA) +  # Add a transparent violin plot with no outline
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +  # Add a boxplot without outliers
  geom_point(position = position_jitter(width = 0.15, height = 0), 
             alpha = 0.1, 
             size = 0.1, 
             aes(color = bias.type), 
             show.legend = FALSE) +  # Add jittered points and remove legend for color
  labs(x = NULL, y = bquote(beta[1] ~ " CI Width"), color = NULL) +
  scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  scale_color_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  coord_cartesian(ylim = c(NA, 3)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "PA", 
                                      m.PO = "PO")), 
             scales = "free_x") +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  
        axis.title.x = element_blank(),         
        axis.text.x = element_blank(),          
        axis.ticks.x = element_blank(),         
        panel.grid.major.x = element_blank(),   
        panel.grid.minor.x = element_blank(),   
        axis.title.y = element_text(size = 15), 
        axis.text = element_text(size = 12),    
        strip.text = element_text(size = 15),   
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +
  ggtitle('PO max. detection probability 0.05')


b2.CI.width_1C <- extrap.scenario.df.CI_1C %>% 
  ggplot(aes(x = bias.type, y = beta2.cred.int, fill = bias.type)) +  
  geom_violin(alpha = 0.2, trim = TRUE, color = NA) +  # Add a transparent violin plot with no outline
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +  # Add a boxplot without outliers
  geom_point(position = position_jitter(width = 0.15, height = 0), 
             alpha = 0.1, 
             size = 0.1, 
             aes(color = bias.type), 
             show.legend = FALSE) +  # Add jittered points and remove legend for color
  labs(x = NULL, y = bquote(beta[2] ~ " CI Width"), color = NULL) +
  scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  scale_color_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  coord_cartesian(ylim = c(NA, 3)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "PA", 
                                      m.PO = "PO")), 
             scales = "free_x") +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  
        axis.title.x = element_blank(),         
        axis.text.x = element_blank(),          
        axis.ticks.x = element_blank(),         
        panel.grid.major.x = element_blank(),   
        panel.grid.minor.x = element_blank(),   
        axis.title.y = element_text(size = 15), 
        axis.text = element_text(size = 12),    
        strip.text = element_text(size = 15),   
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic"))

beta_CI_width_plot_1C  <- ggarrange(b1.CI.width_1C + rremove("xlab"), b2.CI.width_1C, common.legend = T,  ncol = 1, nrow = 2, legend = "bottom")


#### INTERCEPTS ####
PO_INT_1C <- extrap.scenario.df_1C  %>% 
  filter(!is.na(PO_intercept)) %>% 
  ggplot(aes(x = bias.type, y = PO_intercept, fill = bias.type)) +  
  geom_violin(alpha = 0.2, trim = TRUE, color = NA) +  # Transparent violin plot without outline
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +  # Boxplot with no outliers
  geom_point(position = position_jitter(width = 0.15, height = 0), 
             alpha = 0.1, 
             size = 0.1, 
             aes(color = bias.type), 
             show.legend = FALSE) +  # Jittered points, no color legend
  geom_hline(yintercept = beta0, linetype = "dashed", color = "red") +  # Dashed horizontal line
  labs(x = NULL, y = expression(beta[0]), color = NULL) +
  scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  scale_color_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  coord_cartesian(ylim = c(-7, 1)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "PA", 
                                      m.PO = "PO")), 
             scales = "free_x") +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  
        axis.title.x = element_blank(),         
        axis.text.x = element_blank(),          
        axis.ticks.x = element_blank(),         
        panel.grid.major.x = element_blank(),   
        panel.grid.minor.x = element_blank(),   
        axis.title.y = element_text(size = 15), 
        axis.text = element_text(size = 12),    
        strip.text = element_text(size = 15),   
        strip.background = element_rect(fill = "gray96")) +
  ggtitle('PO Intercept')

PA_INT_1C <- extrap.scenario.df_1C  %>% 
  filter(!is.na(PA_intercept)) %>% 
  ggplot(aes(x = bias.type, y = PA_intercept, fill = bias.type)) +  
  geom_violin(alpha = 0.2, trim = TRUE, color = NA) +  # Transparent violin plot without outline
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +  # Boxplot with no outliers
  geom_point(position = position_jitter(width = 0.15, height = 0), 
             alpha = 0.1, 
             size = 0.1, 
             aes(color = bias.type), 
             show.legend = FALSE) +  # Jittered points, no color legend
  geom_hline(yintercept = beta0, linetype = "dashed", color = "red") +  # Dashed horizontal line
  labs(x = NULL, y = expression(beta[0]), color = NULL) +
  scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  scale_color_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) + 
  coord_cartesian(ylim = c(-7, 1)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "PA", 
                                      m.PO = "PO")), 
             scales = "free_x") +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  
        axis.title.x = element_blank(),         
        axis.text.x = element_blank(),          
        axis.ticks.x = element_blank(),         
        panel.grid.major.x = element_blank(),   
        panel.grid.minor.x = element_blank(),   
        axis.title.y = element_text(size = 15), 
        axis.text = element_text(size = 12),    
        strip.text = element_text(size = 15),   
        strip.background = element_rect(fill = "gray96")) +
  ggtitle('PA Intercept')


intercepts_1C <- ggarrange(PO_INT_1C, PA_INT_1C, common.legend = T,  ncol = 2, nrow = 1, legend = "bottom")

#ggsave(plot = intercepts_1C, filename = paste0(file.path(result_path),"/Figure_1C_INTERCEPTS_Scenario_", scenario_name, "_test.png"), w = 22, h = 11, units = "cm", dpi = 400, device = "png")


### FIGURE 2. COMBINED 1 AND 1 C PLOT ###

Fig.2 <- ggarrange(RMSE_1 + rremove("xlab"), RMSE_1C, common.legend = T,  ncol = 1, nrow = 2, legend = "bottom", labels = c("(a)", "(b)"))

ggsave(plot = Fig.2, filename = paste0(file.path(result_path),"/FIGURE_2.png"), w = 23.5, h = 25, units = "cm", dpi = 400, device = "png")

Supp.Fig.1 <- ggarrange(RMSE_A_1 + rremove("xlab"), RMSE_A_1C, common.legend = T,  ncol = 1, nrow = 2, legend = "bottom", labels = c("(a)", "(b)"))

ggsave(plot = Supp.Fig.1, filename = paste0(file.path(result_path),"/Supp_FIGURE_1.png"), w = 23.5, h = 24, units = "cm", dpi = 400, device = "png")

Supp.Fig.1b <- ggarrange(COR_A_1 + rremove("xlab"), COR_A_1C, common.legend = T,  ncol = 1, nrow = 2, legend = "bottom", labels = c("(a)", "(b)"))

ggsave(plot = Supp.Fig.1b, filename = paste0(file.path(result_path),"/Supp_FIGURE_1b.png"), w = 23.5, h = 24, units = "cm", dpi = 400, device = "png")

##### COEFFICIENT PLOT combined 1 and 1 C

Supp.Fig.2 <- ggarrange(beta_1 + rremove("xlab"), beta_1C, common.legend = T,  ncol = 1, nrow = 2, legend = "bottom", labels = c("(a)", "(b)"))

ggsave(plot = Supp.Fig.2, filename = paste0(file.path(result_path),"/Supp_FIGURE_2.png"), w = 20, h = 30, units = "cm", dpi = 400, device = "png")


##### COEFFICIENT CREDIBLE INTERVAL PLOT combined 1 and 1 C

Supp.Fig.3 <- ggarrange(beta_CI_width_plot + rremove("xlab"), beta_CI_width_plot_1C, common.legend = T,  ncol = 1, nrow = 2, legend = "bottom", labels = c("(a)", "(b)"))

ggsave(plot = Supp.Fig.3, filename = paste0(file.path(result_path),"/Supp_FIGURE_3.png"), w = 20, h = 30, units = "cm", dpi = 400, device = "png")

Supp.Fig.3b <- ggarrange(betaSD_1 + rremove("xlab"), betaSD_1C, common.legend = T,  ncol = 1, nrow = 2, legend = "bottom", labels = c("(a)", "(b)"))

ggsave(plot = Supp.Fig.3b, filename = paste0(file.path(result_path),"/Supp_FIGURE_3b.png"), w = 20, h = 30, units = "cm", dpi = 400, device = "png")

###### INTERCEPTS #######

Supp.Fig.4 <- ggarrange(intercepts + rremove("xlab"), intercepts_1C, common.legend = T,  ncol = 1, nrow = 2, legend = "bottom", labels = c("(a)", "(b)"))

ggsave(plot = Supp.Fig.4, filename = paste0(file.path(result_path),"/Supp_FIGURE_4.png"), w = 20, h = 25, units = "cm", dpi = 400, device = "png")


######### REALISED ENVIRO. EXTRAPOLATION ###########

# true.validation.df %>% 
#   filter(mod.type == "m.int") %>%
#   ggplot(aes(x = mean.extrap, y = meanPAPO.extrap, color = mod.type2, shape = bias.type, linetype = bias.type)) +
#   geom_point(alpha = 0.4, size= 0.6) +
#   geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
#   labs(x = x.label, y = "Realised extrap", fill = "Model Type", color = "Model Type") +
#   scale_color_manual(values = fill.colours, guide = "none") +
#   scale_fill_manual(values = fill.colours, guide = "none") +
#   scale_shape_manual(values = c("Without bias cov" = 16, "With bias cov" = 17)) +  # Different shapes for bias types
#   scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
#   #coord_cartesian(ylim = c(NA, 4.5)) +
#   theme_bw() +
#   facet_grid(cols = vars(mod.type2), labeller = as_labeller(c(m.int = "Integrated", m.PA = "PA", m.PO = "PO"))) +
#   theme(legend.position = "bottom",
#         legend.key.size = unit(1.5, "line"),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 14),  # Increase legend text size
#         axis.title = element_text(size = 15),   # Increase axis titles
#         axis.text = element_text(size = 12),    # Increase axis text
#         strip.text = element_text(size = 15),   # Increase facet title size
#         strip.background = element_rect(fill = "gray96"),
#         plot.title = element_text(hjust = 1, size = 15, face = "italic")) +  # Move title to the right)
#   ggtitle('PO max. detection probability 0.2')


#### Realised extrap vs. RMSE ###

R <- true.validation.df %>% 
  filter(mod.type %in% c("m.int", "m.PA", "m.PO")) %>% 
  mutate(realised.extrap = ifelse(mod.type == "m.int", meanPAPO.extrap, 
                                 ifelse(mod.type == "m.PA", meanPA.extrap, meanPO.extrap))) %>%
           ggplot(aes(x = realised.extrap, y = RMSE, color = mod.type)) +
           geom_point(alpha = 0.4, size= 0.6) +
           geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2), alpha = 0.3) +
           labs(x = "Realised extrap", y = "RMSE", fill = "Model Type", color = "Model Type") +
           scale_color_manual(values = fill.colours, guide = "none") +
           scale_fill_manual(values = fill.colours, guide = "none") +
           coord_cartesian(xlim = c(NA, 350), ylim = c(0, 5)) +
           theme_bw() +
           facet_grid(cols = vars(mod.type2), labeller = as_labeller(c(m.int = "Integrated", m.PA = "PA", m.PO = "PO"))) +
           theme(legend.position = "bottom",
                 legend.key.size = unit(1.5, "line"),
                 legend.title = element_blank(),
                 legend.text = element_text(size = 14),  # Increase legend text size
                 axis.title = element_text(size = 15),   # Increase axis titles
                 axis.text = element_text(size = 12),    # Increase axis text
                 strip.text = element_text(size = 15),   # Increase facet title size
                 strip.background = element_rect(fill = "gray96"),
                 plot.title = element_text(hjust = 1, size = 15, face = "italic")) +  # Move title to the right)
           ggtitle('PO max. detection probability 0.2')

RC <- true.validation.df_1C %>% 
  filter(mod.type %in% c("m.int", "m.PA", "m.PO")) %>% 
  mutate(realised.extrap = ifelse(mod.type == "m.int", meanPAPO.extrap, 
                                  ifelse(mod.type == "m.PA", meanPA.extrap, meanPO.extrap))) %>%
  ggplot(aes(x = realised.extrap, y = RMSE, color = mod.type)) +
  geom_point(alpha = 0.4, size= 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2), alpha = 0.3) +
  labs(x = "Realised extrap", y = "RMSE", fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(xlim = c(NA, 350), ylim = c(0, 5)) +
  theme_bw() +
  facet_grid(cols = vars(mod.type2), labeller = as_labeller(c(m.int = "Integrated", m.PA = "PA", m.PO = "PO"))) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  # Increase legend text size
        axis.title = element_text(size = 15),   # Increase axis titles
        axis.text = element_text(size = 12),    # Increase axis text
        strip.text = element_text(size = 15),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +  # Move title to the right)
  ggtitle('PO max. detection probability 0.05')
                                
#### Realised extrap per mod type ###
                                
R2 <- true.validation.df %>% 
  filter(mod.type %in% c("m.int", "m.PA", "m.PO")) %>% 
  mutate(realised.extrap = ifelse(mod.type == "m.int", meanPAPO.extrap, 
                                  ifelse(mod.type == "m.PA", meanPA.extrap, meanPO.extrap))) %>%
  ggplot(aes(x = mod.type, y = realised.extrap, fill = mod.type)) +
  geom_violin(alpha = 0.2, trim = TRUE, color = NA) +  # Transparent violin plot without outline
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +  # Boxplot with no outliers
  geom_point(position = position_jitter(width = 0.15, height = 0), 
             alpha = 0.2, 
             size = 0.1, 
             aes(color = mod.type), 
             show.legend = FALSE) +  # Jittered points, no color legend
  labs(y = "Realised enviro. dissimilarity", fill = "Model Type", color = "Model Type") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_color_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(NA, 450)) +
  theme_bw() +
  facet_wrap(~mod.type, labeller = as_labeller(c(m.int = "Integrated", m.PA = "PA", m.PO = "PO")), scales = "free_x") +  # Wrap facets and use free_x to adjust x-axis
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  # Increase legend text size
        axis.title.x = element_blank(),         # Remove x-axis title
        axis.text.x = element_blank(),          # Remove x-axis text
        axis.ticks.x = element_blank(),         # Remove x-axis ticks
        panel.grid.major.x = element_blank(),   # Remove major grid lines
        panel.grid.minor.x = element_blank(),   # Remove minor grid lines
        axis.title.y = element_text(size = 15), # Keep y-axis title
        axis.text = element_text(size = 12),    # Increase axis text
        strip.text = element_text(size = 15),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +  # Move title to the right)
  ggtitle('PO max. detection probability 0.2')

R2C <- true.validation.df_1C %>% 
  filter(mod.type %in% c("m.int", "m.PA", "m.PO")) %>% 
  mutate(realised.extrap = ifelse(mod.type == "m.int", meanPAPO.extrap, 
                                  ifelse(mod.type == "m.PA", meanPA.extrap, meanPO.extrap))) %>%
  ggplot(aes(x = mod.type, y = realised.extrap, fill = mod.type)) +
  geom_violin(alpha = 0.2, trim = TRUE, color = NA) +  # Transparent violin plot without outline
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +  # Boxplot with no outliers
  geom_point(position = position_jitter(width = 0.15, height = 0), 
             alpha = 0.2, 
             size = 0.1, 
             aes(color = mod.type), 
             show.legend = FALSE) +  # Jittered points, no color legend
  labs(y = "Realised enviro. dissimilarity", fill = "Model Type", color = "Model Type") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_color_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(NA, 450)) +
  theme_bw() +
  facet_wrap(~mod.type, labeller = as_labeller(c(m.int = "Integrated", m.PA = "PA", m.PO = "PO")), scales = "free_x") +  # Wrap facets and use free_x to adjust x-axis
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  # Increase legend text size
        axis.title.x = element_blank(),         # Remove x-axis title
        axis.text.x = element_blank(),          # Remove x-axis text
        axis.ticks.x = element_blank(),         # Remove x-axis ticks
        panel.grid.major.x = element_blank(),   # Remove major grid lines
        panel.grid.minor.x = element_blank(),   # Remove minor grid lines
        axis.title.y = element_text(size = 15), # Keep y-axis title
        axis.text = element_text(size = 12),    # Increase axis text
        strip.text = element_text(size = 15),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +  # Move title to the right)
  ggtitle('PO max. detection probability 0.05')


#### Difference between site enviro. dissimilarity & realised extrap ###

R3 <- true.validation.df %>% 
  filter(mod.type %in% c("m.int", "m.PA", "m.PO")) %>% 
  mutate(realised.extrap = ifelse(mod.type == "m.int", meanPAPO.extrap, 
                                  ifelse(mod.type == "m.PA", meanPA.extrap, meanPO.extrap))) %>% 
  mutate(diff = realised.extrap - mean.extrap) %>%
  ggplot(aes(x = mod.type, y = diff, fill = mod.type)) +
  geom_boxplot() +
  labs(y = "Difference between site diss. and realised diss.", fill = "Model Type", color = "Model Type") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(NA, 300)) +
  theme_bw() +
  facet_wrap(~mod.type, labeller = as_labeller(c(m.int = "Integrated", m.PA = "PA", m.PO = "PO")), scales = "free_x") +  # Wrap facets and use free_x to adjust x-axis
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  # Increase legend text size
        axis.title.x = element_blank(),         # Remove x-axis title
        axis.text.x = element_blank(),          # Remove x-axis text
        axis.ticks.x = element_blank(),         # Remove x-axis ticks
        panel.grid.major.x = element_blank(),   # Remove major grid lines
        panel.grid.minor.x = element_blank(),   # Remove minor grid lines
        axis.title.y = element_text(size = 15), # Keep y-axis title
        axis.text = element_text(size = 12),    # Increase axis text
        strip.text = element_text(size = 15),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +  # Move title to the right)
  ggtitle('PO max. detection probability 0.2')
 

R3C <- true.validation.df_1C %>% 
  filter(mod.type %in% c("m.int", "m.PA", "m.PO")) %>% 
  mutate(realised.extrap = ifelse(mod.type == "m.int", meanPAPO.extrap, 
                                  ifelse(mod.type == "m.PA", meanPA.extrap, meanPO.extrap))) %>% 
  mutate(diff = realised.extrap - mean.extrap) %>%
  ggplot(aes(x = mod.type, y = diff, fill = mod.type)) +
  geom_boxplot() +
  labs(y = "Difference between site diss. and realised diss.", fill = "Model Type", color = "Model Type") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(NA, 300)) +
  theme_bw() +
  facet_wrap(~mod.type, labeller = as_labeller(c(m.int = "Integrated", m.PA = "PA", m.PO = "PO")), scales = "free_x") +  # Wrap facets and use free_x to adjust x-axis
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  # Increase legend text size
        axis.title.x = element_blank(),         # Remove x-axis title
        axis.text.x = element_blank(),          # Remove x-axis text
        axis.ticks.x = element_blank(),         # Remove x-axis ticks
        panel.grid.major.x = element_blank(),   # Remove major grid lines
        panel.grid.minor.x = element_blank(),   # Remove minor grid lines
        axis.title.y = element_text(size = 15), # Keep y-axis title
        axis.text = element_text(size = 12),    # Increase axis text
        strip.text = element_text(size = 15),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +  # Move title to the right)
  ggtitle('PO max. detection probability 0.05')


# Change in enviro. dissimilarity with integration (per replicate) --------

test <- true.validation.df %>% 
  filter(mod.type %in% c("m.int")) %>% 
  mutate(PA.to.int = meanPA.extrap - meanPAPO.extrap) %>% 
  mutate(PO.to.int = meanPO.extrap - meanPAPO.extrap) %>%
  mutate(PA.to.PO = meanPA.extrap - meanPO.extrap) 

mean(test$PA.to.int)

mean(test$PO.to.int)

mean(test$PA.to.PO)

#########################################################################

Fig.X <- ggarrange(R + rremove("xlab"), RC, common.legend = T,  ncol = 1, nrow = 2, legend = "bottom", labels = c("(a)", "(b)"))

ggsave(plot = Fig.X, filename = paste0(file.path(result_path),"/FIGURE_X1.png"), w = 23.5, h = 25, units = "cm", dpi = 400, device = "png")

Fig.x <- ggarrange(R2 + rremove("xlab"), R2C, common.legend = T,  ncol = 1, nrow = 2, legend = "bottom", labels = c("(a)", "(b)"))

ggsave(plot = Fig.x, filename = paste0(file.path(result_path),"/Supp_FIGURE_X2.png"), w = 23.5, h = 24, units = "cm", dpi = 400, device = "png")

Fig.x <- ggarrange(R3 + rremove("xlab"), R3C, common.legend = T,  ncol = 1, nrow = 2, legend = "bottom", labels = c("(a)", "(b)"))

ggsave(plot = Fig.x, filename = paste0(file.path(result_path),"/FIGURE_X3.png"), w = 23.5, h = 24, units = "cm", dpi = 400, device = "png")


######################################################################
################ SCENARIO 2 - SPATIAL AUTOCORRELATION ################
######################################################################

scenario_name = "2"

load(file = paste0(file.path(outpath, scenario_name), "/Scenario_", scenario_name, "_Input_Params.RData"))

# Load and join replicates together ---------------------------------------


# List all the files with validation dataframes (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_Job", full.names = TRUE, recursive = TRUE) 

true.validation.df.list <- lapply(file_list, read.csv)

true.validation.df <- do.call(rbind, true.validation.df.list)


# THEN list all the files with validation dataframes for Site A (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_SiteA_Job", full.names = TRUE, recursive = TRUE) 

true.validation.SiteA.df.list <- lapply(file_list, read.csv)

true.validation.SiteA.df <- do.call(rbind, true.validation.SiteA.df.list)


# FINALLY list all the files with model outputs (for each job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "Results_Summary", full.names = TRUE, recursive = TRUE) 

# Read all the CSV files and save them to a list
extrap.scenario.df.list <- lapply(file_list, read.csv)

# Combine all dataframes in the list into one dataframe using rbind
extrap.scenario.df <- do.call(rbind, extrap.scenario.df.list)


if(scenario.type == "Enviro.Extrap") {
  
  x.label <- "Environmental dissimilarity"
  
  x.discrete.label <- c("Low", "Mod", "High")
  
}

if(scenario.type == "Spatial.Auto") {
  
  x.label <- "Spatial autocorrelation range"
  
  scal.list <- scal
  
  x.discrete.label <- c(as.character(scal))
  
}

true.validation.df <- true.validation.df %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF")) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type = factor(mod.type, levels = c("m.PA", "m.PA.GRF", "m.PO", "m.PO.GRF", "m.int", "m.int.GRF")))

RMSE_2 <- true.validation.df %>% 
  ggplot(aes(x = extrap.type, y = RMSE, fill = mod.type)) +
  geom_boxplot() +
  labs(x = x.label, y = "RMSE", fill = "Model Type") +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(0, 7)) +
  theme_bw() +
  facet_wrap(~ mod.type, 
             labeller = as_labeller(c(
               m.int = "Integrated", 
               m.PA = "PA", 
               m.PO = "PO", 
               m.int.GRF = "Integrated w GRF", 
               m.PA.GRF = "PA w GRF",
               m.PO.GRF = "PO w GRF")),
             nrow = 3, ncol = 2) +
  theme(axis.title = element_text(size = 11),   # Increase axis titles
        axis.text = element_text(size = 9),    # Increase axis text
        strip.text = element_text(size = 11),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +  # Move title to the right)
  ggtitle('PO max. detection probability 0.2')



RMSE_2v2 <- true.validation.df %>%
  ggplot(aes(x = bias.type, y = RMSE, fill = bias.type)) +
  geom_violin(alpha = 0.2, trim = TRUE, color = NA) +  # Add a transparent violin plot with no outline
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +      # Add a boxplot without outliers
  geom_point(position = position_jitter(width = 0.15, height = 0), alpha = 0.1, size = 0.1, aes(color = bias.type), show.legend = F) +      # Add a boxplot without outliers
  labs(x = NULL, y = "RMSE", color = NULL) +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  coord_cartesian(ylim = c(NA, 4.5)) +
  theme_bw() +
  facet_wrap(~mod.type2,
             labeller = as_labeller(c(m.int = "Integrated",
                                      m.PA = "PA",
                                      m.PO = "PO")),
             scales = "free_x") +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(size = 15),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 15),
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic"))


# Version separated by scale
INT.SCORE_2 <- true.validation.df %>% 
  ggplot(aes(x = extrap.type, y = Mean.Int.Score, fill = mod.type)) +
  geom_boxplot() +
  labs(x = x.label, y = "Mean interval score", fill = "Model Type") +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(0, 5)) +
  theme_bw() +
  facet_wrap(~ mod.type, 
             labeller = as_labeller(c(
               m.int = "Integrated", 
               m.PA = "PA", 
               m.PO = "PO", 
               m.int.GRF = "Integrated w GRF", 
               m.PA.GRF = "PA w GRF",
               m.PO.GRF = "PO w GRF")),
             nrow = 3, ncol = 2) +
  theme(axis.title = element_text(size = 11),   # Increase axis titles
        axis.text = element_text(size = 9),    # Increase axis text
        strip.text = element_text(size = 11),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +  # Move title to the right)
  ggtitle('PO max. detection probability 0.2')


# Version separated by model type
INT.SCORE_2v2 <- true.validation.df %>%
  ggplot(aes(x = bias.type, y = Mean.Int.Score, fill = bias.type)) +
  geom_violin(alpha = 0.2, trim = TRUE, color = NA) +  # Add a transparent violin plot with no outline
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +      # Add a boxplot without outliers
  geom_point(position = position_jitter(width = 0.15, height = 0), alpha = 0.1, size = 0.1, aes(color = bias.type), show.legend = F) +      # Add a boxplot without outliers
  labs(x = NULL, y = "Mean interval score", color = NULL) +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  coord_cartesian(ylim = c(NA, 3)) +
  theme_bw() +
  facet_wrap(~mod.type2,
             labeller = as_labeller(c(m.int = "Integrated",
                                      m.PA = "PA",
                                      m.PO = "PO")),
             scales = "free_x") +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(size = 15),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 15),
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic"))


########### COEFFICIENT RECOVERY ###############

extrap.scenario.df <- extrap.scenario.df %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF")) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type = factor(mod.type, levels = c("m.PA", "m.PA.GRF", "m.PO", "m.PO.GRF", "m.int", "m.int.GRF")))


coef1 <- extrap.scenario.df %>% 
  ggplot(aes(x = mod.type, y = beta1.mean, fill = mod.type)) +
  geom_boxplot() +
  labs(x = x.label, y = expression(beta[1]), fill = "Model Type") +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = fill.colours, guide = "none") +
  geom_hline(yintercept = beta1, linetype = "dashed", color = "red") +
  coord_cartesian(ylim = c(-1.5, 1.75)) +
  theme_bw() +
  facet_wrap(~ mod.type, 
             labeller = as_labeller(c(
               m.int = "Integrated", 
               m.PA = "PA", 
               m.PO = "PO", 
               m.int.GRF = "Integrated w GRF", 
               m.PA.GRF = "PA w GRF",
               m.PO.GRF = "PO w GRF")),
             scales = "free_x",
             nrow = 3, ncol = 2) +
  theme(axis.title = element_text(size = 11),   # Increase axis titles
        axis.text = element_text(size = 9),    # Increase axis text
        axis.title.x = element_blank(),         # Remove x-axis title
        axis.text.x = element_blank(),          # Remove x-axis text
        axis.ticks.x = element_blank(),         # Remove x-axis ticks
        strip.text = element_text(size = 11),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) 

# coef1 <- extrap.scenario.df %>% 
#   ggplot(aes(x = bias.type, y = RMSE, fill = bias.type)) +
#   geom_violin(alpha = 0.2, trim = TRUE, color = NA) +  # Add a transparent violin plot with no outline
#   geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +      # Add a boxplot without outliers
#   geom_point(position = position_jitter(width = 0.15, height = 0), alpha = 0.1, size = 0.1, aes(color = bias.type), show.legend = F) +      # Add a boxplot without outliers
#   labs(x = NULL, y = "Prediction error (RMSE)", color = NULL) +
#   scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
#   scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
#   coord_cartesian(ylim = c(NA, 4.5)) +
#   theme_bw() +
#   facet_wrap(~mod.type2,
#              labeller = as_labeller(c(m.int = "Integrated",
#                                       m.PA = "PA",
#                                       m.PO = "PO")),
#              scales = "free_x") +
#   theme(legend.position = "bottom",
#         legend.key.size = unit(1.5, "line"),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 14),
#         axis.title.x = element_blank(),
#         axis.text.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         axis.title.y = element_text(size = 15),
#         axis.text = element_text(size = 12),
#         strip.text = element_text(size = 15),
#         strip.background = element_rect(fill = "gray96"),
#         plot.title = element_text(hjust = 1, size = 15, face = "italic"))

coef2 <- extrap.scenario.df %>% 
  ggplot(aes(x = mod.type, y = beta2.mean, fill = mod.type)) +
  geom_boxplot() +
  labs(x = x.label, y = expression(beta[2]), fill = "Model Type") +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = fill.colours, guide = "none") +
  geom_hline(yintercept = beta2, linetype = "dashed", color = "red") +
  coord_cartesian(ylim = c(-1, 1)) +
  theme_bw() +
  facet_wrap(~ mod.type, 
             labeller = as_labeller(c(
               m.int = "Integrated", 
               m.PA = "PA", 
               m.PO = "PO", 
               m.int.GRF = "Integrated w GRF", 
               m.PA.GRF = "PA w GRF",
               m.PO.GRF = "PO w GRF")),
             scales = "free_x",
             nrow = 3, ncol = 2) +
  theme(axis.title = element_text(size = 11),   # Increase axis titles
        axis.text = element_text(size = 9),    # Increase axis text
        axis.title.x = element_blank(),         # Remove x-axis title
        axis.text.x = element_blank(),          # Remove x-axis text
        axis.ticks.x = element_blank(),         # Remove x-axis ticks
        strip.text = element_text(size = 11),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) 

Supp.Fig.X <- ggarrange(coef1 + rremove("xlab"), coef2 + rremove("xlab") , common.legend = T,  ncol = 2, nrow = 1, legend = "bottom", labels = c("(a)", "(b)"))

ggsave(plot = Supp.Fig.X, filename = paste0(file.path(result_path),"/Coefficients_Scenario_2.png"), w = 20.5, h = 19, units = "cm", dpi = 400, device = "png")


PO_int <- extrap.scenario.df %>% 
  filter(PO_intercept != 0) %>%
  ggplot(aes(x = mod.type, y = PO_intercept, fill = mod.type)) +
  geom_boxplot() +
  labs(x = x.label, y = expression(beta[0]), fill = "Model Type") +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = fill.colours, guide = "none") +
  geom_hline(yintercept = beta0, linetype = "dashed", color = "red") +
  coord_cartesian(ylim = c(-6, 1)) +
  theme_bw() +
  facet_wrap(~ mod.type, 
             labeller = as_labeller(c(
               m.int = "Integrated", 
               m.PA = "PA", 
               m.PO = "PO", 
               m.int.GRF = "Integrated w GRF", 
               m.PA.GRF = "PA w GRF",
               m.PO.GRF = "PO w GRF")),
             scales = "free_x",
             nrow = 3, ncol = 2) +
  theme(axis.title = element_text(size = 11),   # Increase axis titles
        axis.text = element_text(size = 9),    # Increase axis text
        axis.title.x = element_blank(),         # Remove x-axis title
        axis.text.x = element_blank(),          # Remove x-axis text
        axis.ticks.x = element_blank(),         # Remove x-axis ticks
        strip.text = element_text(size = 11),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +
  ggtitle('PO intercept')

PA_int <- extrap.scenario.df %>% 
  filter(PA_intercept != 0) %>%
  ggplot(aes(x = mod.type, y = PA_intercept, fill = mod.type)) +
  geom_boxplot() +
  labs(x = x.label, y = expression(beta[0]), fill = "Model Type") +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = fill.colours, guide = "none") +
  geom_hline(yintercept = beta0, linetype = "dashed", color = "red") +
  coord_cartesian(ylim = c(-6, 1)) +
  theme_bw() +
  facet_wrap(~ mod.type, 
             labeller = as_labeller(c(
               m.int = "Integrated", 
               m.PA = "PA", 
               m.PO = "PO", 
               m.int.GRF = "Integrated w GRF", 
               m.PA.GRF = "PA w GRF",
               m.PO.GRF = "PO w GRF")),
             scales = "free_x",
             nrow = 3, ncol = 2) +
  theme(axis.title = element_text(size = 11),   # Increase axis titles
        axis.text = element_text(size = 9),    # Increase axis text
        axis.title.x = element_blank(),         # Remove x-axis title
        axis.text.x = element_blank(),          # Remove x-axis text
        axis.ticks.x = element_blank(),         # Remove x-axis ticks
        strip.text = element_text(size = 11),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +
  ggtitle('PA intercept')

Supp.Fig.X <- ggarrange(PO_int + rremove("xlab"), PA_int + rremove("xlab") , common.legend = T,  ncol = 2, nrow = 1, legend = "bottom", labels = c("(a)", "(b)"))

ggsave(plot = Supp.Fig.X, filename = paste0(file.path(result_path),"/Intercepts_Scenario_2.png"), w = 20.5, h = 19, units = "cm", dpi = 400, device = "png")


##### PLOT the range parameters and the random / fixed effects

extrap.scenario.GRF.df <- extrap.scenario.df %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF")) 

# version with no cutoff of y axis
g1 <- extrap.scenario.GRF.df  %>% 
  filter(bias.type == "With GRF") %>% 
  mutate(mod.type = factor(mod.type, levels = c("m.PA.GRF", "m.PO.GRF", "m.int.GRF"))) %>% 
  ggplot(aes(x = extrap.type, y = GRF.range.mean,  fill = mod.type)) +
  geom_boxplot() +
  labs(x = x.label, y = "GRF range", fill = "Model Type") +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(NA, 300)) +
  theme_bw() +
  facet_wrap(~ mod.type, 
             labeller = as_labeller(c(
               m.int.GRF = "Integrated", 
               m.PA.GRF = "PA",
               m.PO.GRF = "PO")),
             scales = "free_x",
             nrow = 1, ncol = 3) +
  theme(axis.title = element_text(size = 13),   # Increase axis titles
        axis.text = element_text(size = 11),    # Increase axis text
        strip.text = element_text(size = 13),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"))

# extrap.scenario.GRF.df %>%
#   ggplot(aes(x = extrap.type, y = GRF.range.mean, fill = mod.type)) +
#   geom_boxplot() +
#   labs(x = x.label, y = "GRF range", fill = "Model Type") +
#   # Add dashed lines at y=20, 100, and 200
#   geom_segment(aes(x = as.numeric(as.factor(extrap.type)) - 0.25, xend = as.numeric(as.factor(extrap.type)) + 0.25,
#                    y = 20, yend = 20), linetype = "dashed", color = "red") +
#   geom_segment(aes(x = as.numeric(as.factor(extrap.type)) - 0.25, xend = as.numeric(as.factor(extrap.type)) + 0.25,
#                    y = 100, yend = 100), linetype = "dashed", color = "red") +
#   geom_segment(aes(x = as.numeric(as.factor(extrap.type)) - 0.25, xend = as.numeric(as.factor(extrap.type)) + 0.25,
#                    y = 200, yend = 200), linetype = "dashed", color = "red") +
#   scale_x_discrete(labels = x.discrete.label) +
#   scale_fill_manual(values = fill.colours, guide = "none") +
#   coord_cartesian(ylim = c(NA, 200)) +
#   theme_bw() +
#   facet_wrap(~ mod.type, 
#              labeller = as_labeller(c(
#                m.int.GRF = "Integrated", 
#                m.PA.GRF = "PA",
#                m.PO.GRF = "PO")),
#              scales = "free_x",
#              nrow = 1, ncol = 3) +
#   theme(axis.title = element_text(size = 11),   # Increase axis titles
#         axis.text = element_text(size = 9),    # Increase axis text
#         strip.text = element_text(size = 11),   # Increase facet title size
#         strip.background = element_rect(fill = "gray96"))


g2 <- extrap.scenario.GRF.df  %>% 
  filter(bias.type == "With GRF") %>% 
  mutate(mod.type = factor(mod.type, levels = c("m.PA.GRF", "m.PO.GRF", "m.int.GRF"))) %>% 
  ggplot(aes(x = extrap.type, y = GRF.sd.mean,  fill = mod.type)) +
  geom_boxplot() +
  labs(x = x.label, y = "GRF var", fill = "Model Type") +
  geom_hline(yintercept = variance, linetype = "dashed", color = "red") +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(NA, 4)) +
  theme_bw() +
  facet_wrap(~ mod.type, 
             labeller = as_labeller(c(
               m.int.GRF = "Integrated", 
               m.PA.GRF = "PA",
               m.PO.GRF = "PO")),
             scales = "free_x",
             nrow = 1, ncol = 3) +
  theme(axis.title = element_text(size = 13),   # Increase axis titles
        axis.text = element_text(size = 11),    # Increase axis text
        strip.text = element_text(size = 13),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"))


GRF_plot <- ggarrange(g1 + rremove("xlab"), g2, common.legend = T,  ncol = 1, nrow = 2)
print(GRF_plot)


# ggsave(plot = GRF_plot, filename = paste0(file.path(result_path),"/Fig.2_Scenario_", scenario_name, "_GRF_PLOT.png"), w = 20, h = 15, units = "cm", dpi = 400, device = "png")


########## CORRELATION BETWEEN ESTIMATED AND TRUE FIXED & GRF #############

true.validation.SiteA.df <- true.validation.SiteA.df %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF")) 

# Version without range on x axis 

cor.GRF <- true.validation.SiteA.df %>% 
  filter(bias.type == "With GRF") %>% 
  mutate(mod.type = factor(mod.type, levels = c("m.PA.GRF", "m.PO.GRF", "m.int.GRF"))) %>% 
  filter(!is.na(cor.GRF)) %>% 
  ggplot(aes(x = mod.type, y = cor.GRF, fill = mod.type)) +
  geom_boxplot() +
  labs(x = x.label, y = "Correlation random", fill = "Model Type") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(-0.25, 1)) +
  theme_bw() +
  facet_wrap(~ mod.type, 
             labeller = as_labeller(c(
               m.int.GRF = "Integrated", 
               m.PA.GRF = "PA",
               m.PO.GRF = "PO")),
             scales = "free_x",
             nrow = 1, ncol = 3) +
  theme(axis.title = element_text(size = 13),   # Increase axis titles
        axis.text = element_text(size = 11),    # Increase axis text
        strip.text = element_text(size = 13),   # Increase facet title size
        axis.text.x = element_blank(),          # Remove x-axis text
        axis.ticks.x = element_blank(), 
        axis.title.x = element_blank(),
        strip.background = element_rect(fill = "gray96"))

cor.FIXED <- true.validation.SiteA.df %>% 
  filter(bias.type == "With GRF") %>% 
  mutate(mod.type = factor(mod.type, levels = c("m.PA.GRF", "m.PO.GRF", "m.int.GRF"))) %>%  
  filter(!is.na(cor.FIXED)) %>% 
  ggplot(aes(x = mod.type, y = cor.FIXED, fill = mod.type)) +
  geom_boxplot() +
  labs(x = x.label, y = "Correlation fixed", fill = "Model Type") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(-0.5, 1)) +
  theme_bw() +
  facet_wrap(~ mod.type, 
             labeller = as_labeller(c(
               m.int.GRF = "Integrated", 
               m.PA.GRF = "PA",
               m.PO.GRF = "PO")),
             scales = "free_x",
             nrow = 1, ncol = 3) +
  theme(axis.title = element_text(size = 13),   # Increase axis titles
        axis.text = element_text(size = 11),    # Increase axis text
        strip.text = element_text(size = 13),   # Increase facet title size
        axis.text.x = element_blank(),          # Remove x-axis text
        axis.ticks.x = element_blank(), 
        axis.title.x = element_blank(),
        strip.background = element_rect(fill = "gray96"))


COR_GRF_FIXED_plot <- ggarrange(cor.GRF + rremove("xlab"), cor.FIXED, common.legend = T,  ncol = 2, nrow = 1)
print(COR_GRF_FIXED_plot)


Fig.4 <- ggarrange(COR_GRF_FIXED_plot, g1 + rremove("xlab"), g2, ncol = 1, nrow = 3, labels = c("(a)", "(b)", "(c)"))

ggsave(plot = Fig.4, filename = paste0(file.path(result_path),"/FIGURE_4.png"), w = 21, h = 24, units = "cm", dpi = 400, device = "png")



######################################################################
################ SCENARIO 2C - SPATIAL AUTOCORRELATION C ################
######################################################################

scenario_name = "2C"

load(file = paste0(file.path(outpath, scenario_name), "/Scenario_", scenario_name, "_Input_Params.RData"))

# Load and join replicates together ---------------------------------------


# List all the files with validation dataframes (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_Job", full.names = TRUE, recursive = TRUE) 

true.validation.df.list <- lapply(file_list, read.csv)

true.validation.df_2C <- do.call(rbind, true.validation.df.list)


# THEN list all the files with validation dataframes for Site A (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_SiteA_Job", full.names = TRUE, recursive = TRUE) 

true.validation.SiteA.df.list <- lapply(file_list, read.csv)

true.validation.SiteA.df_2C <- do.call(rbind, true.validation.SiteA.df.list)


# FINALLY list all the files with model outputs (for each job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "Results_Summary", full.names = TRUE, recursive = TRUE) 

# Read all the CSV files and save them to a list
extrap.scenario.df.list <- lapply(file_list, read.csv)

# Combine all dataframes in the list into one dataframe using rbind
extrap.scenario.df_2C <- do.call(rbind, extrap.scenario.df.list)


if(scenario.type == "Enviro.Extrap") {
  
  x.label <- "Environmental dissimilarity"
  
  x.discrete.label <- c("Low", "Mod", "High")
  
}

if(scenario.type == "Spatial.Auto") {
  
  x.label <- "Spatial autocorrelation range"
  
  scal.list <- scal
  
  x.discrete.label <- c(as.character(scal))
  
}


true.validation.df_2C <- true.validation.df_2C %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF")) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type = factor(mod.type, levels = c("m.PA", "m.PA.GRF", "m.PO", "m.PO.GRF", "m.int", "m.int.GRF")))

RMSE_2C <- true.validation.df_2C %>% 
  ggplot(aes(x = extrap.type, y = RMSE, fill = mod.type)) +
  geom_boxplot() +
  labs(x = x.label, y = "RMSE", fill = "Model Type") +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(0, 7)) +
  theme_bw() +
  facet_wrap(~ mod.type, 
             labeller = as_labeller(c(
               m.int = "Integrated", 
               m.PA = "PA", 
               m.PO = "PO", 
               m.int.GRF = "Integrated w GRF", 
               m.PA.GRF = "PA w GRF",
               m.PO.GRF = "PO w GRF")),
             nrow = 3, ncol = 2) +
  theme(axis.title = element_text(size = 11),   # Increase axis titles
        axis.text = element_text(size = 9),    # Increase axis text
        strip.text = element_text(size = 11),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +  # Move title to the right)
  ggtitle('PO max. detection probability 0.05')

RMSE_2Cv2 <- true.validation.df_2C %>% 
  ggplot(aes(x = mod.type, y = RMSE, fill = mod.type)) +
  geom_boxplot() +
  labs(x = x.label, y = "RMSE", fill = "Model Type") +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(0, 5)) +
  theme_bw() +
  facet_wrap(~ mod.type, 
             labeller = as_labeller(c(
               m.int = "Integrated", 
               m.PA = "PA", 
               m.PO = "PO", 
               m.int.GRF = "Integrated w GRF", 
               m.PA.GRF = "PA w GRF",
               m.PO.GRF = "PO w GRF")),
             scales = "free_x",
             nrow = 3, ncol = 2) +
  theme(axis.title = element_text(size = 11),   # Increase axis titles
        axis.text = element_text(size = 9),    # Increase axis text
        strip.text = element_text(size = 11),   # Increase facet title size
        axis.title.x = element_blank(),         # Remove x-axis title
        axis.text.x = element_blank(),          # Remove x-axis text
        axis.ticks.x = element_blank(),         # Remove x-axis ticks
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +  # Move title to the right)
  ggtitle('PO max. detection probability 0.05')


INT.SCORE_2C <- true.validation.df_2C %>% 
  ggplot(aes(x = extrap.type, y = Mean.Int.Score, fill = mod.type)) +
  geom_boxplot() +
  labs(x = x.label, y = "Mean Interval Score", fill = "Model Type") +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(0, 4)) +
  theme_bw() +
  facet_wrap(~ mod.type, 
             labeller = as_labeller(c(
               m.int = "Integrated", 
               m.PA = "PA", 
               m.PO = "PO", 
               m.int.GRF = "Integrated w GRF", 
               m.PA.GRF = "PA w GRF",
               m.PO.GRF = "PO w GRF")),
             nrow = 3, ncol = 2) +
  theme(axis.title = element_text(size = 11),   # Increase axis titles
        axis.text = element_text(size = 9),    # Increase axis text
        strip.text = element_text(size = 11),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +  # Move title to the right)
  ggtitle('PO max. detection probability 0.05')

INT.SCORE_2Cv2 <- true.validation.df_2C %>% 
  ggplot(aes(x = mod.type, y = Mean.Int.Score, fill = mod.type)) +
  geom_boxplot() +
  labs(x = x.label, y = "Mean Interval Score", fill = "Model Type") +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(0, 5)) +
  theme_bw() +
  facet_wrap(~ mod.type, 
             labeller = as_labeller(c(
               m.int = "Integrated", 
               m.PA = "PA", 
               m.PO = "PO", 
               m.int.GRF = "Integrated w GRF", 
               m.PA.GRF = "PA w GRF",
               m.PO.GRF = "PO w GRF")),
             scales = "free_x",
             nrow = 3, ncol = 2) +
  theme(axis.title = element_text(size = 11),   # Increase axis titles
        axis.text = element_text(size = 9),    # Increase axis text
        strip.text = element_text(size = 11),   # Increase facet title size
        axis.title.x = element_blank(),         # Remove x-axis title
        axis.text.x = element_blank(),          # Remove x-axis text
        axis.ticks.x = element_blank(),         # Remove x-axis ticks
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +  # Move title to the right)
  ggtitle('PO max. detection probability 0.05')

##########
##### FIGURE 3 ARRANGE RMSE AND INTERVAL SCORE
########

RMSE_2v2 <- true.validation.df %>% 
  ggplot(aes(x = mod.type, y = RMSE, fill = mod.type)) +
  geom_boxplot() +
  labs(x = x.label, y = "RMSE", fill = "Model Type") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(0, 4)) +
  theme_bw() +
  facet_wrap(~ mod.type, 
             labeller = as_labeller(c(
               m.int = "Integrated", 
               m.PA = "PA", 
               m.PO = "PO", 
               m.int.GRF = "Integrated w GRF", 
               m.PA.GRF = "PA w GRF",
               m.PO.GRF = "PO w GRF")),
             scales = "free_x",
             nrow = 3, ncol = 2) +
  theme(axis.title = element_text(size = 12),   # Increase axis titles
        axis.text = element_text(size = 10),    # Increase axis text
        axis.title.x = element_blank(),         # Remove x-axis title
        axis.text.x = element_blank(),          # Remove x-axis text
        axis.ticks.x = element_blank(),         # Remove x-axis ticks
        strip.text = element_text(size = 12.5),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic"))  # Move title to the right

INT.SCORE_2v2 <- true.validation.df %>% 
  ggplot(aes(x = mod.type, y = Mean.Int.Score, fill = mod.type)) +
  geom_boxplot() +
  labs(x = x.label, y = "Mean Interval Score", fill = "Model Type") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(0, 4)) +
  theme_bw() +
  facet_wrap(~ mod.type, 
             labeller = as_labeller(c(
               m.int = "Integrated", 
               m.PA = "PA", 
               m.PO = "PO", 
               m.int.GRF = "Integrated w GRF", 
               m.PA.GRF = "PA w GRF",
               m.PO.GRF = "PO w GRF")),
             scales = "free_x",
             nrow = 3, ncol = 2) +
  theme(axis.title = element_text(size = 12),   # Increase axis titles
        axis.text = element_text(size = 10),    # Increase axis text
        axis.title.x = element_blank(),         # Remove x-axis title
        axis.text.x = element_blank(),          # Remove x-axis text
        axis.ticks.x = element_blank(),         # Remove x-axis ticks
        strip.text = element_text(size = 12.5),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic"))  # Move title to the right


Fig.3 <- ggarrange(RMSE_2v2 + rremove("xlab"), INT.SCORE_2v2 + rremove("xlab") , common.legend = T,  ncol = 2, nrow = 1, legend = "bottom", labels = c("(a)", "(b)"))

ggsave(plot = Fig.3, filename = paste0(file.path(result_path),"/FIGURE_3.png"), w = 20.5, h = 19, units = "cm", dpi = 400, device = "png")


### Supplementary Fig. 5. COMBINED 1 AND 1 C PLOT ###

Supp.Fig.5 <- ggarrange(RMSE_2 + rremove("xlab"), RMSE_2C + rremove("xlab") + rremove("ylab"), common.legend = T,  ncol = 2, nrow = 1, legend = "bottom", labels = c("(a)", "(b)"))

# Add a shared x-axis title
Supp.Fig.5 <- annotate_figure(
  Supp.Fig.5,
  bottom = text_grob(
    x.label,             # Shared x-axis label
    size = 11,           # Increase text size to match y-axis title
    vjust = 0.25         # Move the title lower
  )
)

ggsave(plot = Supp.Fig.5, filename = paste0(file.path(result_path),"/Supp_FIGURE_5.png"), w = 23.5, h = 24, units = "cm", dpi = 400, device = "png")

# VERSION WITHOUT SCALE PART

# Supp.Fig.5b <- ggarrange(RMSE_2v2 + rremove("xlab"), RMSE_2Cv2 + rremove("xlab") + rremove("ylab"), common.legend = T,  ncol = 2, nrow = 1, legend = "bottom", labels = c("(a)", "(b)"))
# 
# # Add a shared x-axis title
# Supp.Fig.5b <- annotate_figure(
#   Supp.Fig.5b,
#   bottom = text_grob(
#     x.label,             # Shared x-axis label
#     size = 11,           # Increase text size to match y-axis title
#     vjust = 0.25         # Move the title lower
#   )
# )
# 
# ggsave(plot = Supp.Fig.5b, filename = paste0(file.path(result_path),"/Supp_FIGURE_5b.png"), w = 20, h = 17, units = "cm", dpi = 400, device = "png")

######   INTERVAL SCORE #######

# Fig.x <- ggarrange(INT.SCORE_2 + rremove("xlab"), INT.SCORE_2C + rremove("xlab") + rremove("ylab"), common.legend = T,  ncol = 2, nrow = 1, legend = "bottom", labels = c("(a)", "(b)"))
# 
# # Add a shared x-axis title
# Fig.x <- annotate_figure(
#   Fig.x,
#   bottom = text_grob(
#     x.label,             # Shared x-axis label
#     size = 11,           # Increase text size to match y-axis title
#     vjust = 0.25         # Move the title lower
#   )
# )
# 
# ggsave(plot = Fig.x, filename = paste0(file.path(result_path),"/FIGURE_X.png"), w = 23.5, h = 24, units = "cm", dpi = 400, device = "png")
# 
# # VERSION WITHOUT SCALE PART
# 
# Fig.xb <- ggarrange(INT.SCORE_2v2 + rremove("xlab"), INT.SCORE_2Cv2 + rremove("xlab") + rremove("ylab"), common.legend = T,  ncol = 2, nrow = 1, legend = "bottom", labels = c("(a)", "(b)"))
# 
# # Add a shared x-axis title
# Fig.xb <- annotate_figure(
#   Fig.xb,
#   bottom = text_grob(
#     x.label,             # Shared x-axis label
#     size = 11,           # Increase text size to match y-axis title
#     vjust = 0.25         # Move the title lower
#   )
# )
# 
# ggsave(plot = Fig.xb, filename = paste0(file.path(result_path),"/FIGURE_Xb.png"), w = 20, h = 17, units = "cm", dpi = 400, device = "png")


### Supplementary Fig. 6. PLOT ###

true.validation.SiteA.df <- true.validation.SiteA.df %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF")) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type = factor(mod.type, levels = c("m.PA", "m.PA.GRF", "m.PO", "m.PO.GRF", "m.int", "m.int.GRF")))

RMSE_2v2 <- true.validation.SiteA.df %>% 
  ggplot(aes(x = bias.type, y = RMSE, fill = bias.type)) +
  geom_violin(alpha = 0.2, trim = TRUE, color = NA) +  # Add a transparent violin plot with no outline
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +      # Add a boxplot without outliers
  geom_point(position = position_jitter(width = 0.15, height = 0), alpha = 0.1, size = 0.1, aes(color = bias.type), show.legend = F) +      # Add a boxplot without outliers
  labs(x = NULL, y = "RMSE", color = NULL) +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  coord_cartesian(ylim = c(NA, 2)) +
  theme_bw() +
  facet_wrap(~mod.type2,
             labeller = as_labeller(c(m.int = "Integrated",
                                      m.PA = "PA",
                                      m.PO = "PO")),
             scales = "free_x") +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(size = 15),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 15),
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic"))

INT.SCORE_2v2 <- true.validation.SiteA.df %>% 
  ggplot(aes(x = bias.type, y = Mean.Int.Score, fill = bias.type)) +
  geom_violin(alpha = 0.2, trim = TRUE, color = NA) +  # Add a transparent violin plot with no outline
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +      # Add a boxplot without outliers
  geom_point(position = position_jitter(width = 0.15, height = 0), alpha = 0.1, size = 0.1, aes(color = bias.type), show.legend = F) +      # Add a boxplot without outliers
  labs(x = NULL, y = "Mean interval score", color = NULL) +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  coord_cartesian(ylim = c(NA, 1)) +
  theme_bw() +
  facet_wrap(~mod.type2,
             labeller = as_labeller(c(m.int = "Integrated",
                                      m.PA = "PA",
                                      m.PO = "PO")),
             scales = "free_x") +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(size = 15),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 15),
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic"))



Supp.Fig.6 <- ggarrange(RMSE_2v2 + rremove("xlab"), INT.SCORE_2v2 + rremove("xlab") , common.legend = T,  ncol = 1, nrow = 2, legend = "bottom", labels = c("(a)", "(b)"))

ggsave(plot = Supp.Fig.6, filename = paste0(file.path(result_path),"/Supp_FIGURE_6.png"), w = 20.5, h = 19, units = "cm", dpi = 400, device = "png")


### Supplementary Fig. 7. PLOT ###


true.validation.SiteA.df_2C <- true.validation.SiteA.df_2C %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF")) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type = factor(mod.type, levels = c("m.PA", "m.PA.GRF", "m.PO", "m.PO.GRF", "m.int", "m.int.GRF")))


RMSE_2 <- true.validation.SiteA.df %>% 
  ggplot(aes(x = extrap.type, y = RMSE, fill = mod.type)) +
  geom_boxplot() +
  labs(x = x.label, y = "RMSE", fill = "Model Type") +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(0, 4)) +
  theme_bw() +
  facet_wrap(~ mod.type, 
             labeller = as_labeller(c(
               m.int = "Integrated", 
               m.PA = "PA", 
               m.PO = "PO", 
               m.int.GRF = "Integrated w GRF", 
               m.PA.GRF = "PA w GRF",
               m.PO.GRF = "PO w GRF")),
             nrow = 3, ncol = 2) +
  theme(axis.title = element_text(size = 11),   # Increase axis titles
        axis.text = element_text(size = 9),    # Increase axis text
        strip.text = element_text(size = 11),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +  # Move title to the right)
  ggtitle('PO max. detection probability 0.2')

RMSE_2C <- true.validation.SiteA.df_2C %>% 
  ggplot(aes(x = extrap.type, y = RMSE, fill = mod.type)) +
  geom_boxplot() +
  labs(x = x.label, y = "RMSE", fill = "Model Type") +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(0, 4)) +
  theme_bw() +
  facet_wrap(~ mod.type, 
             labeller = as_labeller(c(
               m.int = "Integrated", 
               m.PA = "PA", 
               m.PO = "PO", 
               m.int.GRF = "Integrated w GRF", 
               m.PA.GRF = "PA w GRF",
               m.PO.GRF = "PO w GRF")),
             nrow = 3, ncol = 2) +
  theme(axis.title = element_text(size = 11),   # Increase axis titles
        axis.text = element_text(size = 9),    # Increase axis text
        strip.text = element_text(size = 11),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +  # Move title to the right)
  ggtitle('PO max. detection probability 0.05')


Supp.Fig.7 <- ggarrange(RMSE_2 + rremove("xlab"), RMSE_2C + rremove("xlab") + rremove("ylab"), common.legend = T,  ncol = 2, nrow = 1, legend = "bottom", labels = c("(a)", "(b)"))

# Add a shared x-axis title
Supp.Fig.7 <- annotate_figure(
  Supp.Fig.7,
  bottom = text_grob(
    x.label,             # Shared x-axis label
    size = 11,           # Increase text size to match y-axis title
    vjust = 0.25         # Move the title lower
  )
)

ggsave(plot = Supp.Fig.7, filename = paste0(file.path(result_path),"/Supp_FIGURE_7.png"), w = 23.5, h = 24, units = "cm", dpi = 400, device = "png")



############################################################################

# Figure 5. Scenario 3 ----------------------------------------------------

############################################################################

scenario_name = "3"


load(file = paste0(file.path(outpath, scenario_name), "/Scenario_", scenario_name, "_Input_Params.RData"))

# Load and join replicates together ---------------------------------------


# List all the files with validation dataframes (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_Job", full.names = TRUE, recursive = TRUE) 

true.validation.df.list <- lapply(file_list, read.csv)

true.validation.df <- do.call(rbind, true.validation.df.list)


# THEN list all the files with validation dataframes for Site A (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_SiteA_Job", full.names = TRUE, recursive = TRUE) 

true.validation.SiteA.df.list <- lapply(file_list, read.csv)

true.validation.SiteA.df <- do.call(rbind, true.validation.SiteA.df.list)


# FINALLY list all the files with model outputs (for each job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "Results_Summary", full.names = TRUE, recursive = TRUE) 

# Read all the CSV files and save them to a list
extrap.scenario.df.list <- lapply(file_list, read.csv)

# Combine all dataframes in the list into one dataframe using rbind
extrap.scenario.df <- do.call(rbind, extrap.scenario.df.list)


if(scenario.type == "Enviro.Extrap") {
  
  x.label <- "Environmental dissimilarity"
  
  x.discrete.label <- c("Low", "Mod", "High")
  
}

if(scenario.type == "Spatial.Auto") {
  
  x.label <- "Spatial autocorrelation range"
  
  scal.list <- scal
  
  x.discrete.label <- c(as.character(scal))
  
}

# Make the figure

true.validation.df <- true.validation.df %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF")) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type = factor(mod.type, levels = c("m.PA", "m.PA.GRF", "m.PO", "m.PO.GRF", "m.int", "m.int.GRF")))

RMSE <- true.validation.df %>% 
  ggplot(aes(x = mean.extrap, y = RMSE, color = mod.type)) +
  geom_point(alpha = 0.3, size= 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type, color = mod.type), alpha = 0.3) +
  labs(x = x.label, y = "RMSE", fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(NA, 5)) +
  theme_bw() +
  facet_wrap(~ mod.type, 
             labeller = as_labeller(c(
               m.int = "Integrated", 
               m.PA = "PA", 
               m.PO = "PO", 
               m.int.GRF = "Integrated w GRF", 
               m.PA.GRF = "PA w GRF",
               m.PO.GRF = "PO w GRF")),
             nrow = 3, ncol = 2) +
  theme(legend.position = "top",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),  # Increase legend text size
        axis.title = element_text(size = 12),   # Increase axis titles
        axis.text = element_text(size = 10),    # Increase axis text
        strip.text = element_text(size = 12.5),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +   # Move title to the right)
  ggtitle('PO max. detection probability 0.2')



############################################################################
# Figure 5. Scenario 3C. ---------------------------------------------------
# Environmental extrap with spat autocorr PO detection prob 0.005
############################################################################

scenario_name = "3C"

load(file = paste0(file.path(outpath, scenario_name), "/Scenario_", scenario_name, "_Input_Params.RData"))

# Load and join replicates together ---------------------------------------


# List all the files with validation dataframes (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_Job", full.names = TRUE, recursive = TRUE) 

true.validation.df.list <- lapply(file_list, read.csv)

true.validation.df_3C <- do.call(rbind, true.validation.df.list)


# THEN list all the files with validation dataframes for Site A (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_SiteA_Job", full.names = TRUE, recursive = TRUE) 

true.validation.SiteA.df.list <- lapply(file_list, read.csv)

true.validation.SiteA.df_3C <- do.call(rbind, true.validation.SiteA.df.list)


# FINALLY list all the files with model outputs (for each job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "Results_Summary", full.names = TRUE, recursive = TRUE) 

# Read all the CSV files and save them to a list
extrap.scenario.df.list <- lapply(file_list, read.csv)

# Combine all dataframes in the list into one dataframe using rbind
extrap.scenario.df_3C <- do.call(rbind, extrap.scenario.df.list)


if(scenario.type == "Enviro.Extrap") {
  
  x.label <- "Environmental dissimilarity"
  
  x.discrete.label <- c("Low", "Mod", "High")
  
}

if(scenario.type == "Spatial.Auto") {
  
  x.label <- "Spatial autocorrelation range"
  
  scal.list <- scal
  
  x.discrete.label <- c(as.character(scal))
  
}

# Make the figure

true.validation.df_3C <- true.validation.df_3C %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF")) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type = factor(mod.type, levels = c("m.PA", "m.PA.GRF", "m.PO", "m.PO.GRF", "m.int", "m.int.GRF")))

RMSE_3C <- true.validation.df_3C %>% 
  ggplot(aes(x = mean.extrap, y = RMSE, color = mod.type)) +
  geom_point(alpha = 0.3, size= 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type, color = mod.type), alpha = 0.3) +
  labs(x = x.label, y = "RMSE", fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(NA, 5)) +
  theme_bw() +
  facet_wrap(~ mod.type, 
             labeller = as_labeller(c(
               m.int = "Integrated", 
               m.PA = "PA", 
               m.PO = "PO", 
               m.int.GRF = "Integrated w GRF", 
               m.PA.GRF = "PA w GRF",
               m.PO.GRF = "PO w GRF")),
             nrow = 3, ncol = 2) +
  theme(legend.position = "top",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),  # Increase legend text size
        axis.title = element_text(size = 12),   # Increase axis titles
        axis.text = element_text(size = 10),    # Increase axis text
        strip.text = element_text(size = 12.5),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +  # Move title to the right)
  ggtitle('PO max. detection probability 0.05')


Fig.5 <- ggarrange(
  RMSE + rremove("xlab"), 
  RMSE_3C + rremove("xlab"), 
  common.legend = TRUE, 
  ncol = 2, 
  nrow = 1, 
  legend = "bottom", 
  labels = c("(a)", "(b)")
)

# Add a shared x-axis label
Fig.5 <- annotate_figure(
  Fig.5,
  bottom = text_grob("Environmental dissimilarity", size = 12)
)


ggsave(plot = Fig.5, filename = paste0(file.path(result_path),"/FIGURE_5.png"), w = 22.5, h = 21, units = "cm", dpi = 400, device = "png")



############################################################################

# Figure X. Scenario 6 ----------------------------------------------------

############################################################################

scenario_name = "6"


load(file = paste0(file.path(outpath, scenario_name), "/Scenario_", scenario_name, "_Input_Params.RData"))

# Load and join replicates together ---------------------------------------


# List all the files with validation dataframes (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_Job", full.names = TRUE, recursive = TRUE) 

true.validation.df.list <- lapply(file_list, read.csv)

true.validation.df <- do.call(rbind, true.validation.df.list)


# THEN list all the files with validation dataframes for Site A (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_SiteA_Job", full.names = TRUE, recursive = TRUE) 

true.validation.SiteA.df.list <- lapply(file_list, read.csv)

true.validation.SiteA.df <- do.call(rbind, true.validation.SiteA.df.list)


# FINALLY list all the files with model outputs (for each job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "Results_Summary", full.names = TRUE, recursive = TRUE) 

# Read all the CSV files and save them to a list
extrap.scenario.df.list <- lapply(file_list, read.csv)

# Combine all dataframes in the list into one dataframe using rbind
extrap.scenario.df <- do.call(rbind, extrap.scenario.df.list)


if(scenario.type == "Enviro.Extrap") {
  
  x.label <- "Environmental dissimilarity"
  
  x.discrete.label <- c("Low", "Mod", "High")
  
}

if(scenario.type == "Spatial.Auto") {
  
  x.label <- "Spatial autocorrelation range"
  
  scal.list <- scal
  
  x.discrete.label <- c(as.character(scal))
  
}

# Make the figure

true.validation.df <- true.validation.df %>%
  mutate(bias.type = ifelse(grepl("bias", mod.type, fixed = T), "With bias cov", "Without bias cov")) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PA", "m.PA.GRF", "m.PO", "m.PO.GRF", "m.int", "m.int.GRF")))


projection1 <- true.validation.df %>% 
  ggplot(aes(x = mean.extrap, y = RMSE.global, color = bias.type)) +
  geom_point(alpha = 0.3, size= 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = bias.type, color = bias.type), alpha = 0.3) +
  labs(x = x.label, y = "RMSE") +
  scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) +
  scale_color_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) +
  coord_cartesian(ylim = c(NA, 5)) +
  theme_bw() +
  facet_wrap(~ mod.type2, 
             labeller = as_labeller(c(
               m.int = "Integrated", 
               m.PA = "PA", 
               m.PO = "PO", 
               m.int.GRF = "Integrated w GRF", 
               m.PA.GRF = "PA w GRF",
               m.PO.GRF = "PO w GRF")),
             nrow = 3, ncol = 2) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),  # Increase legend text size
        axis.title = element_text(size = 12),   # Increase axis titles
        axis.text = element_text(size = 10),    # Increase axis text
        strip.text = element_text(size = 12.5),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +   # Move title to the right)
  ggtitle('PO max. detection probability 0.2')

projection2 <- true.validation.df %>% 
  ggplot(aes(x = mean.extrap, y = Mean.Int.Score, color = bias.type)) +
  geom_point(alpha = 0.3, size= 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = bias.type, color = bias.type), alpha = 0.3) +
  labs(x = x.label, y = "Mean interval score") +
  scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) +
  scale_color_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) +
  coord_cartesian(ylim = c(NA, 2.5)) +
  theme_bw() +
  facet_wrap(~ mod.type2, 
             labeller = as_labeller(c(
               m.int = "Integrated", 
               m.PA = "PA", 
               m.PO = "PO", 
               m.int.GRF = "Integrated w GRF", 
               m.PA.GRF = "PA w GRF",
               m.PO.GRF = "PO w GRF")),
             nrow = 3, ncol = 2) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),  # Increase legend text size
        axis.title = element_text(size = 12),   # Increase axis titles
        axis.text = element_text(size = 10),    # Increase axis text
        strip.text = element_text(size = 12.5),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +   # Move title to the right)
  ggtitle('PO max. detection probability 0.2')


projection3 <- true.validation.df %>% 
  ggplot(aes(x = mean.extrap, y = correlation, color = bias.type)) +
  geom_point(alpha = 0.3, size= 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = bias.type, color = bias.type), alpha = 0.3) +
  labs(x = x.label, y = "Correlation") +
  scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) +
  scale_color_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) +
  coord_cartesian(ylim = c(-0.5, NA)) +
  theme_bw() +
  facet_wrap(~ mod.type2, 
             labeller = as_labeller(c(
               m.int = "Integrated", 
               m.PA = "PA", 
               m.PO = "PO", 
               m.int.GRF = "Integrated w GRF", 
               m.PA.GRF = "PA w GRF",
               m.PO.GRF = "PO w GRF")),
             nrow = 3, ncol = 2) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),  # Increase legend text size
        axis.title = element_text(size = 12),   # Increase axis titles
        axis.text = element_text(size = 10),    # Increase axis text
        strip.text = element_text(size = 12.5),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +   # Move title to the right)
  ggtitle('PO max. detection probability 0.2')


projection4 <- true.validation.df %>% 
  ggplot(aes(x = mean.extrap, y = coverage.rate, color = bias.type)) +
  geom_point(alpha = 0.3, size= 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = bias.type, color = bias.type), alpha = 0.3) +
  labs(x = x.label, y = "Mean coverage probability") +
  scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) +
  scale_color_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) +
  # coord_cartesian(ylim = c(0, 0.1)) +
  theme_bw() +
  facet_wrap(~ mod.type2, 
             labeller = as_labeller(c(
               m.int = "Integrated", 
               m.PA = "PA", 
               m.PO = "PO", 
               m.int.GRF = "Integrated w GRF", 
               m.PA.GRF = "PA w GRF",
               m.PO.GRF = "PO w GRF")),
             nrow = 3, ncol = 2) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),  # Increase legend text size
        axis.title = element_text(size = 12),   # Increase axis titles
        axis.text = element_text(size = 10),    # Increase axis text
        strip.text = element_text(size = 12.5),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +   # Move title to the right)
  ggtitle('PO max. detection probability 0.2')


# RMSE Site A

true.validation.SiteA.df <- true.validation.SiteA.df %>%
  mutate(bias.type = ifelse(grepl("bias", mod.type, fixed = T), "With bias cov", "Without bias cov")) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PA", "m.PA.GRF", "m.PO", "m.PO.GRF", "m.int", "m.int.GRF")))

RMSE_2v2 <- true.validation.SiteA.df %>% 
  ggplot(aes(x = mod.type2, y = RMSE, fill = bias.type)) +
  geom_boxplot() +
  labs(x = x.label, y = "RMSE", fill = "Model Type") +
  scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) +
  coord_cartesian(ylim = c(0, 4.5)) +
  theme_bw() +
  facet_wrap(~ mod.type2, 
             labeller = as_labeller(c(
               m.int = "Integrated", 
               m.PA = "PA", 
               m.PO = "PO", 
               m.int.GRF = "Integrated w GRF", 
               m.PA.GRF = "PA w GRF",
               m.PO.GRF = "PO w GRF")),
             scales = "free_x",
             nrow = 3, ncol = 2) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14), 
        axis.title = element_text(size = 12),   # Increase axis titles
        axis.text = element_text(size = 10),    # Increase axis text
        axis.title.x = element_blank(),         # Remove x-axis title
        axis.text.x = element_blank(),          # Remove x-axis text
        axis.ticks.x = element_blank(),         # Remove x-axis ticks
        strip.text = element_text(size = 12.5),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic"))   +   # Move title to the right)
  ggtitle('PO max. detection probability 0.2')



# GRF and FIXED correlation

GRF <- true.validation.SiteA.df %>% 
  filter(mod.type2 %in% c("m.int.GRF", "m.PO.GRF", "m.PA.GRF")) %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PA.GRF", "m.PO.GRF", "m.int.GRF"))) %>% 
  ggplot(aes(x = mod.type2, y = cor.GRF, fill = bias.type)) +
  geom_boxplot() +
  labs(x = x.label, y = "Correlation GRF", fill = "Model Type") +
  scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) +
  coord_cartesian(ylim = c(-0.3, 1)) +
  theme_bw() +
  facet_wrap(~ mod.type2, 
             labeller = as_labeller(c(
               m.int.GRF = "Integrated", 
               m.PA.GRF = "PA",
               m.PO.GRF = "PO")),
             scales = "free_x",
             nrow = 1, ncol = 3) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14), 
        axis.title = element_text(size = 12),   # Increase axis titles
        axis.text = element_text(size = 10),    # Increase axis text
        axis.title.x = element_blank(),         # Remove x-axis title
        axis.text.x = element_blank(),          # Remove x-axis text
        axis.ticks.x = element_blank(),         # Remove x-axis ticks
        strip.text = element_text(size = 12.5),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic"))   +   # Move title to the right)
  ggtitle('PO max. detection probability 0.2')

Fixed <- true.validation.SiteA.df %>% 
  filter(mod.type2 %in% c("m.int.GRF", "m.PO.GRF", "m.PA.GRF")) %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PA.GRF", "m.PO.GRF", "m.int.GRF"))) %>% 
  ggplot(aes(x = mod.type2, y = cor.FIXED, fill = bias.type)) +
  geom_boxplot() +
  labs(x = x.label, y = "Correlation fixed", fill = "Model Type") +
  scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) +
  coord_cartesian(ylim = c(-0.3, 1)) +
  theme_bw() +
  facet_wrap(~ mod.type2, 
             labeller = as_labeller(c(
               m.int.GRF = "Integrated", 
               m.PA.GRF = "PA",
               m.PO.GRF = "PO")),
             scales = "free_x",
             nrow = 1, ncol = 3) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14), 
        axis.title = element_text(size = 12),   # Increase axis titles
        axis.text = element_text(size = 10),    # Increase axis text
        axis.title.x = element_blank(),         # Remove x-axis title
        axis.text.x = element_blank(),          # Remove x-axis text
        axis.ticks.x = element_blank(),         # Remove x-axis ticks
        strip.text = element_text(size = 12.5),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic"))   +   # Move title to the right)
  ggtitle('PO max. detection probability 0.2')

CORR <- ggarrange(GRF, Fixed, common.legend = TRUE, ncol = 2, nrow = 1, legend = "bottom")

############################################################################
# Figure X. Scenario 6C. ---------------------------------------------------
# Environmental extrap with spat autocorr PO detection prob 0.005
############################################################################

scenario_name = "6C"

load(file = paste0(file.path(outpath, scenario_name), "/Scenario_", scenario_name, "_Input_Params.RData"))

# Load and join replicates together ---------------------------------------


# List all the files with validation dataframes (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_Job", full.names = TRUE, recursive = TRUE) 

true.validation.df.list <- lapply(file_list, read.csv)

true.validation.df_6C <- do.call(rbind, true.validation.df.list)


# THEN list all the files with validation dataframes for Site A (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_SiteA_Job", full.names = TRUE, recursive = TRUE) 

true.validation.SiteA.df.list <- lapply(file_list, read.csv)

true.validation.SiteA.df_6C <- do.call(rbind, true.validation.SiteA.df.list)


# FINALLY list all the files with model outputs (for each job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "Results_Summary", full.names = TRUE, recursive = TRUE) 

# Read all the CSV files and save them to a list
extrap.scenario.df.list <- lapply(file_list, read.csv)

# Combine all dataframes in the list into one dataframe using rbind
extrap.scenario.df_6C <- do.call(rbind, extrap.scenario.df.list)


if(scenario.type == "Enviro.Extrap") {
  
  x.label <- "Environmental dissimilarity"
  
  x.discrete.label <- c("Low", "Mod", "High")
  
}

if(scenario.type == "Spatial.Auto") {
  
  x.label <- "Spatial autocorrelation range"
  
  scal.list <- scal
  
  x.discrete.label <- c(as.character(scal))
  
}

# Make the figure

true.validation.df_6C <- true.validation.df_6C %>%
  mutate(bias.type = ifelse(grepl("bias", mod.type, fixed = T), "With bias cov", "Without bias cov")) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PA", "m.PA.GRF", "m.PO", "m.PO.GRF", "m.int", "m.int.GRF")))

RMSE_6C <- true.validation.df_6C %>% 
  ggplot(aes(x = mean.extrap, y = RMSE, color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.3, size= 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  labs(x = x.label, y = "RMSE", fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_shape_manual(values = c("Without bias cov" = 16, "With bias cov" = 17)) +  # Different shapes for bias types
  scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
  coord_cartesian(ylim = c(NA, 5)) +
  theme_bw() +
  facet_wrap(~ mod.type2, 
             labeller = as_labeller(c(
               m.int = "Integrated", 
               m.PA = "PA", 
               m.PO = "PO", 
               m.int.GRF = "Integrated w GRF", 
               m.PA.GRF = "PA w GRF",
               m.PO.GRF = "PO w GRF")),
             nrow = 3, ncol = 2) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),  # Increase legend text size
        axis.title = element_text(size = 12),   # Increase axis titles
        axis.text = element_text(size = 10),    # Increase axis text
        strip.text = element_text(size = 12.5),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +   # Move title to the right)
  ggtitle('PO max. detection probability 0.05')

# Interval score
Int.score_6C <- true.validation.df_6C %>% 
  ggplot(aes(x = mean.extrap, y = Mean.Int.Score, color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.3, size= 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  labs(x = x.label, y = "Mean Interval Score", fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_shape_manual(values = c("Without bias cov" = 16, "With bias cov" = 17)) +  # Different shapes for bias types
  scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
  coord_cartesian(ylim = c(NA, 3)) +
  theme_bw() +
  facet_wrap(~ mod.type2, 
             labeller = as_labeller(c(
               m.int = "Integrated", 
               m.PA = "PA", 
               m.PO = "PO", 
               m.int.GRF = "Integrated w GRF", 
               m.PA.GRF = "PA w GRF",
               m.PO.GRF = "PO w GRF")),
             nrow = 3, ncol = 2) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),  # Increase legend text size
        axis.title = element_text(size = 12),   # Increase axis titles
        axis.text = element_text(size = 10),    # Increase axis text
        strip.text = element_text(size = 12.5),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +   # Move title to the right)
  ggtitle('PO max. detection probability 0.05')

## RMSE SITE A 

true.validation.SiteA.df_6C <- true.validation.SiteA.df_6C %>%
  mutate(bias.type = ifelse(grepl("bias", mod.type, fixed = T), "With bias cov", "Without bias cov")) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PA", "m.PA.GRF", "m.PO", "m.PO.GRF", "m.int", "m.int.GRF")))

RMSE_2v2C <- true.validation.SiteA.df_6C %>% 
  ggplot(aes(x = mod.type2, y = RMSE, fill = bias.type)) +
  geom_boxplot() +
  labs(x = x.label, y = "RMSE", fill = "Model Type") +
  scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) +
  coord_cartesian(ylim = c(0, 4.5)) +
  theme_bw() +
  facet_wrap(~ mod.type2, 
             labeller = as_labeller(c(
               m.int = "Integrated", 
               m.PA = "PA", 
               m.PO = "PO", 
               m.int.GRF = "Integrated w GRF", 
               m.PA.GRF = "PA w GRF",
               m.PO.GRF = "PO w GRF")),
             scales = "free_x",
             nrow = 3, ncol = 2) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14), 
        axis.title = element_text(size = 12),   # Increase axis titles
        axis.text = element_text(size = 10),    # Increase axis text
        axis.title.x = element_blank(),         # Remove x-axis title
        axis.text.x = element_blank(),          # Remove x-axis text
        axis.ticks.x = element_blank(),         # Remove x-axis ticks
        strip.text = element_text(size = 12.5),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +   # Move title to the right)
  ggtitle('PO max. detection probability 0.05')


# GRF and FIXED correlation

GRF_6C <- true.validation.SiteA.df_6C %>% 
  filter(mod.type2 %in% c("m.int.GRF", "m.PO.GRF", "m.PA.GRF")) %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PA.GRF", "m.PO.GRF", "m.int.GRF"))) %>% 
  ggplot(aes(x = mod.type2, y = cor.GRF, fill = bias.type)) +
  geom_boxplot() +
  labs(x = x.label, y = "Correlation GRF", fill = "Model Type") +
  scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) +
  coord_cartesian(ylim = c(-0.3, 1)) +
  theme_bw() +
  facet_wrap(~ mod.type2, 
             labeller = as_labeller(c(
               m.int.GRF = "Integrated", 
               m.PA.GRF = "PA",
               m.PO.GRF = "PO")),
             scales = "free_x",
             nrow = 1, ncol = 3) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14), 
        axis.title = element_text(size = 12),   # Increase axis titles
        axis.text = element_text(size = 10),    # Increase axis text
        axis.title.x = element_blank(),         # Remove x-axis title
        axis.text.x = element_blank(),          # Remove x-axis text
        axis.ticks.x = element_blank(),         # Remove x-axis ticks
        strip.text = element_text(size = 12.5),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic"))   +   # Move title to the right)
  ggtitle('PO max. detection probability 0.05')

Fixed_6C <- true.validation.SiteA.df_6C %>% 
  filter(mod.type2 %in% c("m.int.GRF", "m.PO.GRF", "m.PA.GRF")) %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PA.GRF", "m.PO.GRF", "m.int.GRF"))) %>% 
  ggplot(aes(x = mod.type2, y = cor.FIXED, fill = bias.type)) +
  geom_boxplot() +
  labs(x = x.label, y = "Correlation fixed", fill = "Model Type") +
  scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) +
  coord_cartesian(ylim = c(-0.3, 1)) +
  theme_bw() +
  facet_wrap(~ mod.type2, 
             labeller = as_labeller(c(
               m.int.GRF = "Integrated", 
               m.PA.GRF = "PA",
               m.PO.GRF = "PO")),
             scales = "free_x",
             nrow = 1, ncol = 3) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14), 
        axis.title = element_text(size = 12),   # Increase axis titles
        axis.text = element_text(size = 10),    # Increase axis text
        axis.title.x = element_blank(),         # Remove x-axis title
        axis.text.x = element_blank(),          # Remove x-axis text
        axis.ticks.x = element_blank(),         # Remove x-axis ticks
        strip.text = element_text(size = 12.5),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic"))   +   # Move title to the right)
  ggtitle('PO max. detection probability 0.05')

CORR_6C <- ggarrange(GRF_6C, Fixed_6C, common.legend = TRUE, ncol = 2, nrow = 1, legend = "bottom")

############################################################################

### ARRANGE THE FIGURE RMSE ### 

# Extract the legend separately
legend <- get_legend(
  RMSE + 
    theme(legend.position = "bottom", 
          legend.key.size = unit(1.5, "line"),
          legend.title = element_blank(),
          legend.text = element_text(size = 10))
)

# Remove legends from individual plots
RMSE <- RMSE + theme(legend.position = "none")
RMSE_6C <- RMSE_6C + theme(legend.position = "none")

# Arrange the plots without their legends
plots <- ggarrange(
  RMSE + rremove("xlab"), 
  RMSE_6C + rremove("xlab"), 
  ncol = 2, 
  nrow = 1, 
  labels = c("(a)", "(b)")
)

# Create a text grob for the shared x-axis label
x_axis_label <- text_grob("Environmental dissimilarity", size = 12)

# Arrange plots, x-axis label, and legend
Fig.X <- ggarrange(
  plots,
  x_axis_label,
  legend,
  ncol = 1,
  heights = c(1, 0.05, 0.1)  # Adjust relative heights
)

ggsave(plot = Fig.X, filename = paste0(file.path(result_path),"/FIGURE_5_Scenario_6.png"), w = 22.5, h = 21, units = "cm", dpi = 400, device = "png")


### ARRANGE THE FIGURE INTERVAL SCORE ### 

# Extract the legend separately
legend <- get_legend(
  Int.score + 
    theme(legend.position = "bottom", 
          legend.key.size = unit(1.5, "line"),
          legend.title = element_blank(),
          legend.text = element_text(size = 10))
)

# Remove legends from individual plots
Int.score <- Int.score + theme(legend.position = "none")
Int.score_6C <- Int.score_6C + theme(legend.position = "none")

# Arrange the plots without their legends
plots <- ggarrange(
  Int.score + rremove("xlab"), 
  Int.score_6C + rremove("xlab"), 
  ncol = 2, 
  nrow = 1, 
  labels = c("(a)", "(b)")
)

# Create a text grob for the shared x-axis label
x_axis_label <- text_grob("Environmental dissimilarity", size = 12)

# Arrange plots, x-axis label, and legend
Fig.X <- ggarrange(
  plots,
  x_axis_label,
  legend,
  ncol = 1,
  heights = c(1, 0.05, 0.1)  # Adjust relative heights
)

ggsave(plot = Fig.X, filename = paste0(file.path(result_path),"/INTERVAL_SCORE_Scenario_6.png"), w = 22.5, h = 21, units = "cm", dpi = 400, device = "png")


### ARRANGE THE FIGURE RMSE SITE A ### 

# Extract the legend separately
legend <- get_legend(
  RMSE_2v2 + 
    theme(legend.position = "bottom", 
          legend.key.size = unit(1.5, "line"),
          legend.title = element_blank(),
          legend.text = element_text(size = 10))
)

# Remove legends from individual plots
RMSE_2v2 <- RMSE_2v2 + theme(legend.position = "none")
RMSE_2v2C <- RMSE_2v2C + theme(legend.position = "none")

# Arrange the plots without their legends
plots <- ggarrange(
  RMSE_2v2 + rremove("xlab"), 
  RMSE_2v2C + rremove("xlab"), 
  ncol = 2, 
  nrow = 1, 
  labels = c("(a)", "(b)")
)

# Create a text grob for the shared x-axis label
x_axis_label <- text_grob("Environmental dissimilarity", size = 12)

# Arrange plots, x-axis label, and legend
Fig.X <- ggarrange(
  plots,
  x_axis_label,
  legend,
  ncol = 1,
  heights = c(1, 0.05, 0.1)  # Adjust relative heights
)

ggsave(plot = Fig.X, filename = paste0(file.path(result_path),"/RMSE_SITE_A_Scenario_6.png"), w = 22.5, h = 21, units = "cm", dpi = 400, device = "png")

############# ARRANGE THE FIGURE CORRELATION GRD + FIXED ###########

FIG.X <- ggarrange(CORR, CORR_6C, ncol = 1, nrow = 2, labels = c("(a)", "(b)"))

ggsave(plot = FIG.X, filename = paste0(file.path(result_path),"/CORR_Scenario_6.png"), w = 22.5, h = 21, units = "cm", dpi = 400, device = "png")

#ggsave(plot = CORR, filename = paste0(file.path(result_path),"/CORR_V2Scenario_6.png"), w = 22.5, h = 9, units = "cm", dpi = 400, device = "png")
