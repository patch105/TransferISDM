
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


ggsave(plot = RMSE, filename = paste0(file.path(result_path),"/Figure_1a_Scenario_", scenario_name, ".png"), w = 23.5, h = 14, units = "cm", dpi = 400, device = "png")


Mean.Int.Score <- true.validation.df %>% 
  ggplot(aes(x = mean.extrap, y = Mean.Int.Score, color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  labs(x = x.label, y = "Mean Interval Score", fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_shape_manual(values = c("Without bias cov" = 16, "With bias cov" = 17)) +  # Different shapes for bias types
  scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
  coord_cartesian(ylim = c(NA, 2.5)) +
  theme_bw() +
  facet_grid(cols = vars(mod.type2), labeller = as_labeller(c(m.int = "Integrated", m.PA = "PA", m.PO = "PO"))) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 13),  # Increase legend text size
        axis.title = element_text(size = 14),   # Increase axis titles
        axis.text = element_text(size = 12),    # Increase axis text
        strip.text = element_text(size = 14),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"))


ggsave(plot = Mean.Int.Score, filename = paste0(file.path(result_path),"/Figure_1_Int.Score_Scenario_", scenario_name, ".png"), w = 23.5, h = 14, units = "cm", dpi = 400, device = "png")


### SITE A ###

true.validation.SiteA.df <- true.validation.SiteA.df %>%
  mutate(bias.type = ifelse(grepl("bias", mod.type, fixed = T), "With bias cov", "Without bias cov")) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PA", "m.PO", "m.int")))

RMSE <- true.validation.SiteA.df %>% 
  ggplot(aes(x = mean.extrap, y = RMSE, color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.4, size= 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  labs(x = x.label, y = "RMSE", fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_shape_manual(values = c("Without bias cov" = 16, "With bias cov" = 17)) +  # Different shapes for bias types
  scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
  coord_cartesian(ylim = c(NA, 2.75)) +
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


ggsave(plot = RMSE, filename = paste0(file.path(result_path),"/Figure_1a_SITEA_Scenario_", scenario_name, ".png"), w = 23.5, h = 14, units = "cm", dpi = 400, device = "png")


Mean.Int.Score <- true.validation.SiteA.df %>% 
  ggplot(aes(x = mean.extrap, y = Mean.Int.Score, color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  labs(x = x.label, y = "Mean Interval Score", fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_shape_manual(values = c("Without bias cov" = 16, "With bias cov" = 17)) +  # Different shapes for bias types
  scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
  #coord_cartesian(ylim = c(NA, 0.15)) +
  theme_bw() +
  facet_grid(cols = vars(mod.type2), labeller = as_labeller(c(m.int = "Integrated", m.PA = "PA", m.PO = "PO"))) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 13),  # Increase legend text size
        axis.title = element_text(size = 14),   # Increase axis titles
        axis.text = element_text(size = 12),    # Increase axis text
        strip.text = element_text(size = 14),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"))


ggsave(plot = Mean.Int.Score, filename = paste0(file.path(result_path),"/Figure_1_Int.Score_SITEA_Scenario_", scenario_name, ".png"), w = 23.5, h = 14, units = "cm", dpi = 400, device = "png")

ggsave(plot = Mean.Int.Score, filename = paste0(file.path(result_path),"/Figure_1B_Int.Score_SITEA_Scenario_", scenario_name, ".png"), w = 23.5, h = 14, units = "cm", dpi = 400, device = "png")

### COEFFICIENTS ###

extrap.scenario.df  <- extrap.scenario.df %>% 
  mutate(bias.type = ifelse(grepl("bias", mod.type, fixed = T), "With bias cov", "Without bias cov")) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PA", "m.PO", "m.int"))) 

extrap.scenario.df <- extrap.scenario.df[!is.na(extrap.scenario.df$mod.type2), ]

b1 <- extrap.scenario.df %>% 
  ggplot(aes(x = mean.extrap, y = beta1.mean,  color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  geom_hline(yintercept = beta1, linetype = "dashed", color = "red") +
  labs(x = x.label, y = expression(beta[1]), fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_shape_manual(values = c("Without bias cov" = 16, "With bias cov" = 17)) +  # Different shapes for bias types
  scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
  coord_cartesian(ylim = c(-0.5, 0.5)) +
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

b2 <- extrap.scenario.df %>% 
  ggplot(aes(x = mean.extrap, y = beta2.mean,  color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  geom_hline(yintercept = beta2, linetype = "dashed", color = "red") +
  labs(x = x.label, y = expression(beta[2]), fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_shape_manual(values = c("Without bias cov" = 16, "With bias cov" = 17)) +  # Different shapes for bias types
  scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
  coord_cartesian(ylim = c(-0.25, 0.75)) +
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

beta <- ggarrange(b1 + rremove("xlab"), b2, common.legend = T,  ncol = 1, nrow = 2, legend = "bottom")

ggsave(plot = beta, filename = paste0(file.path(result_path),"/Figure_1_BETAS_Scenario_", scenario_name, "_test.png"), w = 20, h = 15, units = "cm", dpi = 400, device = "png")


#### BETA CREDIBLE INTERVAL WIDTH ####
##### Plot the mean width of the credible interval #####

extrap.scenario.df.CI <- extrap.scenario.df %>% 
  mutate(beta1.cred.int = beta1_975 - beta1_25,
         beta2.cred.int = beta2_975 - beta2_25,
         beta1.cred.int.true = ifelse(beta1 >= beta1_25 &  beta1 <= beta1_975, 1, 0),
         beta2.cred.int.true = ifelse(beta2 >= beta2_25 & beta2 <= beta2_975, 1, 0))

b1.CI.width <- extrap.scenario.df.CI %>% 
  ggplot(aes(x = mean.extrap, y = beta1.cred.int,  color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  labs(x = x.label, y = bquote(beta[1] ~ " Credible Interval Width"), fill = "Model Type", color = "Model Type") +
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

b2.CI.width <- extrap.scenario.df.CI %>% 
  ggplot(aes(x = mean.extrap, y = beta2.cred.int,  color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  labs(x = x.label, y = bquote(beta[2] ~ " Credible Interval Width"), fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_shape_manual(values = c("Without bias cov" = 16, "With bias cov" = 17)) +  # Different shapes for bias types
  scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
  coord_cartesian(ylim = c(NA, 1.5)) +
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

beta_CI_width_plot <- ggarrange(b1.CI.width + rremove("xlab"), b2.CI.width, common.legend = T,  ncol = 1, nrow = 2, legend = "bottom")

ggsave(plot = beta_CI_width_plot, filename = paste0(file.path(result_path),"/Figure_1_BETAS_CI_WIDTH_Scenario_", scenario_name, "_test.png"), w = 20, h = 15, units = "cm", dpi = 400, device = "png")


#### INTERCEPTS ####
PO_INT <- extrap.scenario.df %>% 
  filter(!is.na(PO_intercept)) %>% 
  ggplot(aes(x = mean.extrap, y = PO_intercept,  color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  geom_hline(yintercept = beta0, linetype = "dashed", color = "red") +
  labs(x = x.label, y = expression(beta[0]), fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_shape_manual(values = c("Without bias cov" = 16, "With bias cov" = 17)) +  # Different shapes for bias types
  scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
  coord_cartesian(ylim = c(-5.5, -1)) +
  theme_bw() +
  facet_grid(cols = vars(mod.type2), labeller = as_labeller(c(m.int = "Integrated", m.PA = "PA", m.PO = "PO"))) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 13),  # Increase legend text size
        axis.title = element_text(size = 14),   # Increase axis titles
        axis.text = element_text(size = 11),    # Increase axis text
        strip.text = element_text(size = 14),   # Increase facet title size
        strip.background = element_rect(fill = "gray96")) +
  ggtitle('PO Intercept')

PA_INT <- extrap.scenario.df %>% 
  filter(!is.na(PA_intercept)) %>% 
  ggplot(aes(x = mean.extrap, y = PA_intercept,  color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  geom_hline(yintercept = beta0, linetype = "dashed", color = "red") +
  labs(x = x.label, y = expression(beta[0]), fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_shape_manual(values = c("Without bias cov" = 16, "With bias cov" = 17)) +  # Different shapes for bias types
  scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
  coord_cartesian(ylim = c(-5.5, -1)) +
  theme_bw() +
  facet_grid(cols = vars(mod.type2), labeller = as_labeller(c(m.int = "Integrated", m.PA = "PA", m.PO = "PO"))) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 13),  # Increase legend text size
        axis.title = element_text(size = 14),   # Increase axis titles
        axis.text = element_text(size = 11),    # Increase axis text
        strip.text = element_text(size = 14),   # Increase facet title size
        strip.background = element_rect(fill = "gray96")) +
  ggtitle('PA Intercept')


intercepts <- ggarrange(PO_INT, PA_INT, common.legend = T,  ncol = 2, nrow = 1, legend = "bottom")

ggsave(plot = intercepts, filename = paste0(file.path(result_path),"/Figure_1_INTERCEPTS_Scenario_", scenario_name, "_test.png"), w = 20, h = 15, units = "cm", dpi = 400, device = "png")


#### CAUSES FOR DIFFERENT RMSE & INT.SCORE VALUES ####

### Number of records ###

n_records.list <- list.files(path = file.path(outpath, scenario_name), pattern = "N_Presences_EACH", full.names = TRUE, recursive = TRUE)

n_records.list <- lapply(n_records.list, read.csv)

n_records.df <- do.call(rbind, n_records.list) %>% 
  rename(job_index = job)

# Combine with validation

newdf <- n_records.df %>%
  left_join(
    true.validation.df %>% select(extrap.type, rep, job_index, mod.type, mean.extrap, RMSE, correlation, Mean.Int.Score, bias.type, mod.type2),
    by = c("extrap.type", "rep", "job_index")
  )

newdf %>% 
  ggplot(aes(x = mean.extrap, y = RMSE, color = n_po_gridA, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.4, size= 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = n_po_gridA, color = n_po_gridA, linetype = bias.type), alpha = 0.3) +
  labs(x = x.label, y = "RMSE", fill = "Number of PO", color = "Number of PO") +
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


newdfPA <- newdf %>% filter(!mod.type2 == "m.PO")
newdfPO <- newdf %>% filter(!mod.type2 == "m.PA")

newdfPA %>% 
  ggplot(aes(x = n_presence_gridA, y = RMSE, color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.4, size= 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  labs(x = "Number of presence PA", y = "RMSE", fill = "Model Type", color = "Model Type") +
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


newdfPO %>% 
  ggplot(aes(x = n_po_gridA, y = RMSE, color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.4, size= 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  labs(x = "Number of PO", y = "RMSE", fill = "Model Type", color = "Model Type") +
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

newdfPA %>% 
  ggplot(aes(x = n_presence_gridA, y = correlation, color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.4, size= 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  labs(x = "Number of presence PA", y = "correlation", fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_shape_manual(values = c("Without bias cov" = 16, "With bias cov" = 17)) +  # Different shapes for bias types
  scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
  coord_cartesian(ylim = c(NA, 1)) +
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


newdfPO %>% 
  ggplot(aes(x = n_po_gridA, y = correlation, color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.4, size= 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  labs(x = "Number of PO", y = "correlation", fill = "Model Type", color = "Model Type") +
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

test <- lm(newdfPO$RMSE ~ newdfPO$n_po_gridA + newdfPO$mod.type2 + newdfPO$bias.type + newdfPO$mean.extrap)
summary(test)

test <- lm(newdfPA$RMSE ~ newdfPA$n_absence_gridA + newdfPA$mod.type2 + newdfPA$bias.type + newdfPA$mean.extrap)
summary(test)


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


ggsave(plot = RMSE, filename = paste0(file.path(result_path),"/Figure_6_Scenario_1B", scenario_name, ".png"), w = 23.5, h = 14, units = "cm", dpi = 400, device = "png")

Mean.Int.Score <- true.validation.df %>% 
  ggplot(aes(x = mean.extrap, y = Mean.Int.Score, color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  labs(x = x.label, y = "Mean Interval Score", fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_shape_manual(values = c("Without bias cov" = 16, "With bias cov" = 17)) +  # Different shapes for bias types
  scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
  coord_cartesian(ylim = c(NA, 5)) +
  theme_bw() +
  facet_grid(cols = vars(mod.type2), labeller = as_labeller(c(m.int = "Integrated", m.PA = "PA", m.PO = "PO"))) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 13),  # Increase legend text size
        axis.title = element_text(size = 14),   # Increase axis titles
        axis.text = element_text(size = 12),    # Increase axis text
        strip.text = element_text(size = 14),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"))

ggsave(plot = Mean.Int.Score, filename = paste0(file.path(result_path),"/Figure_6_Int.Score_Scenario_", scenario_name, ".png"), w = 23.5, h = 14, units = "cm", dpi = 400, device = "png")


### SITE A ###

true.validation.SiteA.df <- true.validation.SiteA.df %>%
  mutate(bias.type = ifelse(grepl("bias", mod.type, fixed = T), "With bias cov", "Without bias cov")) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PA", "m.PO", "m.int")))


RMSE <- true.validation.SiteA.df %>% 
  ggplot(aes(x = BA, y = RMSE, color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  labs(x = x.label, y = "RMSE", fill = "Model Type", color = "Model Type") +
  scale_x_reverse() +  # Reverse x-axis from 1 to 0
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_shape_manual(values = c("Without bias cov" = 16, "With bias cov" = 17)) +  # Different shapes for bias types
  scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
  coord_cartesian(ylim = c(NA, 5)) +
  theme_bw() +
  facet_grid(cols = vars(mod.type2), labeller = as_labeller(c(m.int = "Integrated", m.PA = "PA", m.PO = "PO"))) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 13),  # Increase legend text size
        axis.title = element_text(size = 14),   # Increase axis titles
        axis.text = element_text(size = 12),    # Increase axis text
        strip.text = element_text(size = 14),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"))


ggsave(plot = RMSE, filename = paste0(file.path(result_path),"/Figure_6_SITEA_Scenario_", scenario_name, ".png"), w = 23.5, h = 14, units = "cm", dpi = 400, device = "png")


Mean.Int.Score <- true.validation.SiteA.df %>% 
  ggplot(aes(x = BA, y = Mean.Int.Score, color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  labs(x = x.label, y = "Mean Interval Score", fill = "Model Type", color = "Model Type") +
  scale_x_reverse() +  # Reverse x-axis from 1 to 0
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_shape_manual(values = c("Without bias cov" = 16, "With bias cov" = 17)) +  # Different shapes for bias types
  scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
  coord_cartesian(ylim = c(NA, 5)) +
  theme_bw() +
  facet_grid(cols = vars(mod.type2), labeller = as_labeller(c(m.int = "Integrated", m.PA = "PA", m.PO = "PO"))) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 13),  # Increase legend text size
        axis.title = element_text(size = 14),   # Increase axis titles
        axis.text = element_text(size = 12),    # Increase axis text
        strip.text = element_text(size = 14),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"))


ggsave(plot = Mean.Int.Score, filename = paste0(file.path(result_path),"/Figure_6_Int.Score_SITEA_Scenario_", scenario_name, ".png"), w = 23.5, h = 14, units = "cm", dpi = 400, device = "png")

### COEFFICIENTS ###

extrap.scenario.df  <- extrap.scenario.df %>% 
  mutate(bias.type = ifelse(grepl("bias", mod.type, fixed = T), "With bias cov", "Without bias cov")) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PA", "m.PO", "m.int"))) 

extrap.scenario.df <- extrap.scenario.df[!is.na(extrap.scenario.df$mod.type2), ]

b1 <- extrap.scenario.df %>% 
  ggplot(aes(x = mean.extrap, y = beta1.mean,  color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  geom_hline(yintercept = beta1, linetype = "dashed", color = "red") +
  labs(x = x.label, y = expression(beta[1]), fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_shape_manual(values = c("Without bias cov" = 16, "With bias cov" = 17)) +  # Different shapes for bias types
  scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
  coord_cartesian(ylim = c(-0.5, 0.5)) +
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

b2 <- extrap.scenario.df %>% 
  ggplot(aes(x = mean.extrap, y = beta2.mean,  color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  geom_hline(yintercept = beta2, linetype = "dashed", color = "red") +
  labs(x = x.label, y = expression(beta[2]), fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_shape_manual(values = c("Without bias cov" = 16, "With bias cov" = 17)) +  # Different shapes for bias types
  scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
  coord_cartesian(ylim = c(-0.25, 0.75)) +
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

beta <- ggarrange(b1 + rremove("xlab"), b2, common.legend = T,  ncol = 1, nrow = 2, legend = "bottom")

ggsave(plot = beta, filename = paste0(file.path(result_path),"/Figure_6_BETAS_Scenario_", scenario_name, "_test.png"), w = 20, h = 15, units = "cm", dpi = 400, device = "png")


#### BETA CREDIBLE INTERVAL WIDTH ####
##### Plot the mean width of the credible interval #####

extrap.scenario.df.CI <- extrap.scenario.df %>% 
  mutate(beta1.cred.int = beta1_975 - beta1_25,
         beta2.cred.int = beta2_975 - beta2_25,
         beta1.cred.int.true = ifelse(beta1 >= beta1_25 &  beta1 <= beta1_975, 1, 0),
         beta2.cred.int.true = ifelse(beta2 >= beta2_25 & beta2 <= beta2_975, 1, 0))

b1.CI.width <- extrap.scenario.df.CI %>% 
  ggplot(aes(x = mean.extrap, y = beta1.cred.int,  color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  labs(x = x.label, y = bquote(beta[1] ~ " Credible Interval Width"), fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_shape_manual(values = c("Without bias cov" = 16, "With bias cov" = 17)) +  # Different shapes for bias types
  scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
  coord_cartesian(ylim = c(NA, 2.5)) +
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

b2.CI.width <- extrap.scenario.df.CI %>% 
  ggplot(aes(x = mean.extrap, y = beta2.cred.int,  color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  labs(x = x.label, y = bquote(beta[2] ~ " Credible Interval Width"), fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_shape_manual(values = c("Without bias cov" = 16, "With bias cov" = 17)) +  # Different shapes for bias types
  scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
  coord_cartesian(ylim = c(NA, 1.5)) +
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

beta_CI_width_plot <- ggarrange(b1.CI.width + rremove("xlab"), b2.CI.width, common.legend = T,  ncol = 1, nrow = 2, legend = "bottom")

ggsave(plot = beta_CI_width_plot, filename = paste0(file.path(result_path),"/Figure_6_BETAS_CI_WIDTH_Scenario_", scenario_name, "_test.png"), w = 20, h = 15, units = "cm", dpi = 400, device = "png")


#### INTERCEPTS ####
PO_INT <- extrap.scenario.df %>% 
  filter(!is.na(PO_intercept)) %>% 
  ggplot(aes(x = mean.extrap, y = PO_intercept,  color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  geom_hline(yintercept = beta0, linetype = "dashed", color = "red") +
  labs(x = x.label, y = expression(beta[0]), fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_shape_manual(values = c("Without bias cov" = 16, "With bias cov" = 17)) +  # Different shapes for bias types
  scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
  coord_cartesian(ylim = c(-10, 10)) +
  theme_bw() +
  facet_grid(cols = vars(mod.type2), labeller = as_labeller(c(m.int = "Integrated", m.PA = "PA", m.PO = "PO"))) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 13),  # Increase legend text size
        axis.title = element_text(size = 14),   # Increase axis titles
        axis.text = element_text(size = 11),    # Increase axis text
        strip.text = element_text(size = 14),   # Increase facet title size
        strip.background = element_rect(fill = "gray96")) +
  ggtitle('PO Intercept')

PA_INT <- extrap.scenario.df %>% 
  filter(!is.na(PA_intercept)) %>% 
  ggplot(aes(x = mean.extrap, y = PA_intercept,  color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  geom_hline(yintercept = beta0, linetype = "dashed", color = "red") +
  labs(x = x.label, y = expression(beta[0]), fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_shape_manual(values = c("Without bias cov" = 16, "With bias cov" = 17)) +  # Different shapes for bias types
  scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
  coord_cartesian(ylim = c(-10, 10)) +
  theme_bw() +
  facet_grid(cols = vars(mod.type2), labeller = as_labeller(c(m.int = "Integrated", m.PA = "PA", m.PO = "PO"))) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 13),  # Increase legend text size
        axis.title = element_text(size = 14),   # Increase axis titles
        axis.text = element_text(size = 11),    # Increase axis text
        strip.text = element_text(size = 14),   # Increase facet title size
        strip.background = element_rect(fill = "gray96")) +
  ggtitle('PA Intercept')


intercepts <- ggarrange(PO_INT, PA_INT, common.legend = T,  ncol = 2, nrow = 1, legend = "bottom")

ggsave(plot = intercepts, filename = paste0(file.path(result_path),"/Figure_6_INTERCEPTS_Scenario_", scenario_name, "_test.png"), w = 20, h = 15, units = "cm", dpi = 400, device = "png")


############################################################################
# Figure 2. Scenario 2. ---------------------------------------------------
# Spatial autocorrelation
############################################################################

scenario_name = "2_MESH1"

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
  ggplot(aes(x = extrap.type, y = RMSE, fill = mod.type)) +
  geom_boxplot() +
  labs(x = x.label, y = "RMSE", fill = "Model Type") +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(0, 10)) +
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
        strip.background = element_rect(fill = "gray96"))

ggsave(plot = RMSE, filename = paste0(file.path(result_path),"/Figure_2b_Scenario_", scenario_name, ".png"), w = 11, h = 15, units = "cm", dpi = 400, device = "png")
 
#### SITE A FIGURE ####

true.validation.SiteA.df <- true.validation.SiteA.df %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF")) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type = factor(mod.type, levels = c("m.PA", "m.PO", "m.int", "m.PA.GRF", "m.PO.GRF", "m.int.GRF")))

RMSE <- true.validation.SiteA.df %>% 
  ggplot(aes(x = extrap.type, y = RMSE, fill = mod.type)) +
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
             nrow = 2, ncol = 3) +
  theme(axis.title = element_text(size = 13),   # Increase axis titles
        axis.text = element_text(size = 11),    # Increase axis text
        strip.text = element_text(size = 13),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"))

ggsave(plot = RMSE, filename = paste0(file.path(result_path),"/Figure_2_SITEA_Scenario_", scenario_name, ".png"), w = 19.5, h = 16, units = "cm", dpi = 400, device = "png")


true.validation.SiteA.df <- true.validation.SiteA.df %>%
  mutate(mod.type = factor(mod.type, levels = c("m.PA", "m.PA.GRF", "m.PO", "m.PO.GRF", "m.int", "m.int.GRF")))

RMSE <- true.validation.SiteA.df %>% 
  ggplot(aes(x = extrap.type, y = RMSE, fill = mod.type)) +
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
             nrow = 3, ncol = 2) +
  theme(axis.title = element_text(size = 11),   # Increase axis titles
        axis.text = element_text(size = 9),    # Increase axis text
        strip.text = element_text(size = 11),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"))

ggsave(plot = RMSE, filename = paste0(file.path(result_path),"/Figure_2_SITEA_Scenario_", scenario_name, ".png"), w = 11, h = 15, units = "cm", dpi = 400, device = "png")

####################################
#### INTERVAL SCORE ##########
####################################

INT.SCORE <- true.validation.df %>% 
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
        strip.background = element_rect(fill = "gray96"))

ggsave(plot = INT.SCORE, filename = paste0(file.path(result_path),"/Figure_2_interval_score_Scenario_", scenario_name, ".png"), w = 11, h = 15, units = "cm", dpi = 400, device = "png")


#####################################
#### GRF COEFFICIENT RECOVERY ####
#####################################


extrap.scenario.GRF.df <- extrap.scenario.df %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF")) %>% 
  filter(bias.type == "With GRF") %>% 
  mutate(mod.type = factor(mod.type, levels = c("m.PA.GRF", "m.PO.GRF", "m.int.GRF")))


# version with no cutoff of y axis
g1 <- extrap.scenario.GRF.df  %>% 
  ggplot(aes(x = extrap.type, y = GRF.range.mean,  fill = mod.type)) +
  geom_boxplot() +
  labs(x = x.label, y = "GRF range", fill = "Model Type") +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = fill.colours,
                    labels = c("m.int.GRF" = "Integrated", "m.PA.GRF" = "PA", "m.PO.GRF" = "PO")) +
 coord_cartesian(ylim = c(NA, 300)) +
  theme_bw() +
  theme(axis.title = element_text(size = 14),   # Increase axis titles
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 12))

g2 <- extrap.scenario.GRF.df  %>% 
  ggplot(aes(x = extrap.type, y = GRF.sd.mean,  fill = mod.type)) +
  geom_boxplot() +
  labs(x = x.label, y = "GRF var", fill = "Model Type") +
  geom_hline(yintercept = variance, linetype = "dashed", color = "red") +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = fill.colours,
                    labels = c("m.int.GRF" = "Integrated", "m.PA.GRF" = "PA", "m.PO.GRF" = "PO")) +
  #coord_cartesian(ylim = c(NA, 5)) +
  theme_bw() +
  theme(axis.title = element_text(size = 14),   # Increase axis titles
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 12)) 

GRF_plot <- ggarrange(g1, g2, common.legend = T,  ncol = 2, nrow = 1)
print(GRF_plot)


ggsave(plot = GRF_plot, filename = paste0(file.path(result_path),"/Fig.2_Scenario_", scenario_name, "_GRF_PLOT.png"), w = 20, h = 15, units = "cm", dpi = 400, device = "png")


########## CORRELATION BETWEEN ESTIMATED AND TRUE GRF #############
cor.GRF <- true.validation.SiteA.df %>% 
  filter(!is.na(cor.GRF)) %>% 
  ggplot(aes(x = extrap.type, y = cor.GRF, fill = mod.type)) +
  geom_boxplot() +
  labs(x = x.label, y = "Correlation random", fill = "Model Type") +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = fill.colours, guide = "none") +
  theme_bw() +
  facet_wrap(~ mod.type, 
             labeller = as_labeller(c(
               m.int.GRF = "Integrated", 
               m.PA.GRF = "PA",
               m.PO.GRF = "PO")),
             nrow = 1, ncol = 3) +
  theme(axis.title = element_text(size = 11),   # Increase axis titles
        axis.text = element_text(size = 9),    # Increase axis text
        strip.text = element_text(size = 11),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"))

cor.FIXED <- true.validation.SiteA.df %>% 
  filter(!is.na(cor.FIXED)) %>% 
  ggplot(aes(x = extrap.type, y = cor.FIXED, fill = mod.type)) +
  geom_boxplot() +
  labs(x = x.label, y = "Correlation fixed", fill = "Model Type") +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(-0.8, NA)) +
  theme_bw() +
  facet_wrap(~ mod.type, 
             labeller = as_labeller(c(
               m.int.GRF = "Integrated", 
               m.PA.GRF = "PA",
               m.PO.GRF = "PO")),
             nrow = 1, ncol = 3) +
  theme(axis.title = element_text(size = 11),   # Increase axis titles
        axis.text = element_text(size = 9),    # Increase axis text
        strip.text = element_text(size = 11),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"))


COR_GRF_FIXED_plot <- ggarrange(cor.GRF + rremove("xlab"), cor.FIXED, common.legend = T,  ncol =1, nrow = 2)

ggsave(plot = COR_GRF_FIXED_plot, filename = paste0(file.path(result_path),"/Figure_2_GRF_Corr_Scenario_", scenario_name, ".png"), w = 20, h = 15, units = "cm", dpi = 400, device = "png")

# Version without range on x axis 

cor.GRF <- true.validation.SiteA.df %>% 
  filter(!is.na(cor.GRF)) %>% 
  ggplot(aes(x = mod.type, y = cor.GRF, fill = mod.type)) +
  geom_boxplot() +
  labs(x = x.label, y = "Correlation random", fill = "Model Type") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  theme_bw() +
  facet_wrap(~ mod.type, 
             labeller = as_labeller(c(
               m.int.GRF = "Integrated", 
               m.PA.GRF = "PA",
               m.PO.GRF = "PO")),
             scales = "free_x",
             nrow = 1, ncol = 3) +
  theme(axis.title = element_text(size = 11),   # Increase axis titles
        axis.text = element_text(size = 9),    # Increase axis text
        strip.text = element_text(size = 11),   # Increase facet title size
        axis.text.x = element_blank(),          # Remove x-axis text
        axis.ticks.x = element_blank(), 
        axis.title.x = element_blank(),
        strip.background = element_rect(fill = "gray96"))

cor.FIXED <- true.validation.SiteA.df %>% 
  filter(!is.na(cor.FIXED)) %>% 
  ggplot(aes(x = mod.type, y = cor.FIXED, fill = mod.type)) +
  geom_boxplot() +
  labs(x = x.label, y = "Correlation fixed", fill = "Model Type") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  theme_bw() +
  facet_wrap(~ mod.type, 
             labeller = as_labeller(c(
               m.int.GRF = "Integrated", 
               m.PA.GRF = "PA",
               m.PO.GRF = "PO")),
             scales = "free_x",
             nrow = 1, ncol = 3) +
  theme(axis.title = element_text(size = 11),   # Increase axis titles
        axis.text = element_text(size = 9),    # Increase axis text
        strip.text = element_text(size = 11),   # Increase facet title size
        axis.text.x = element_blank(),          # Remove x-axis text
        axis.ticks.x = element_blank(), 
        axis.title.x = element_blank(),
        strip.background = element_rect(fill = "gray96"))


COR_GRF_FIXED_plot <- ggarrange(cor.GRF + rremove("xlab"), cor.FIXED, common.legend = T,  ncol =1, nrow = 2)

ggsave(plot = COR_GRF_FIXED_plot, filename = paste0(file.path(result_path),"/Figure_2_GRF_Corr_V2_Scenario_", scenario_name, ".png"), w = 12, h = 8, units = "cm", dpi = 400, device = "png")


########## CORRELATION BETWEEN GRF ACCURACY AND BETAS ############

extrap.scenario.df <- extrap.scenario.df %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF")) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type = factor(mod.type, levels = c("m.PA", "m.PO", "m.int", "m.PA.GRF", "m.PO.GRF", "m.int.GRF")))

# Add the fixed and random effects columns to the extrap.scenario dataframe by matching extrap, rep, job, and model type

newdf <- extrap.scenario.df %>%
  left_join(
    true.validation.SiteA.df %>% select(extrap.type, rep, job_index, mod.type, cor.GRF),
    by = c("extrap.type", "rep", "job_index", "mod.type")
  )

newdf <- newdf %>%
  left_join(
    true.validation.SiteA.df %>% select(extrap.type, rep, job_index, mod.type, cor.FIXED),
    by = c("extrap.type", "rep", "job_index", "mod.type")
  )

p1 <- newdf %>% 
  filter(!is.na(cor.GRF)) %>% 
  ggplot(aes(x = cor.GRF, y = beta1.mean, color = mod.type)) +
  geom_point(alpha = 0.4, size= 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type), alpha = 0.3) +
  geom_hline(yintercept = beta1, linetype = "dashed", color = "red") +
  labs(x = "Correlation random", y = expression(beta[1]), fill = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(-1, 1)) +
  theme_bw() +
  facet_wrap(~ mod.type, 
             labeller = as_labeller(c(
               m.int.GRF = "Integrated", 
               m.PA.GRF = "PA",
               m.PO.GRF = "PO")),
            nrow = 1, ncol = 3) +
  theme(axis.title = element_text(size = 11),   # Increase axis titles
        axis.text = element_text(size = 9),    # Increase axis text
        strip.text = element_text(size = 11),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"))

p2 <- newdf %>% 
  filter(!is.na(cor.GRF)) %>% 
  ggplot(aes(x = cor.GRF, y = beta2.mean, color = mod.type)) +
  geom_point(alpha = 0.4, size= 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type), alpha = 0.3) +
  geom_hline(yintercept = beta2, linetype = "dashed", color = "red") +
  labs(x = "Correlation random", y = expression(beta[2]), fill = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(-0.25, 0.75)) +
  theme_bw() +
  facet_wrap(~ mod.type, 
             labeller = as_labeller(c(
               m.int.GRF = "Integrated", 
               m.PA.GRF = "PA",
               m.PO.GRF = "PO")),
             nrow = 1, ncol = 3) +
  theme(axis.title = element_text(size = 11),   # Increase axis titles
        axis.text = element_text(size = 9),    # Increase axis text
        strip.text = element_text(size = 11),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"))

p3 <- newdf %>% 
  filter(!is.na(cor.GRF)) %>% 
  filter(!is.na(PO_intercept)) %>% 
  ggplot(aes(x = cor.GRF, y = PO_intercept, color = mod.type)) +
  geom_point(alpha = 0.4, size= 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type), alpha = 0.3) +
  geom_hline(yintercept = beta0, linetype = "dashed", color = "red") +
  labs(x = "Correlation random", y = expression(beta[0]), fill = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(-5.5, -1)) +
  theme_bw() +
  facet_wrap(~ mod.type, 
             labeller = as_labeller(c(
               m.int.GRF = "Integrated", 
               m.PA.GRF = "PA",
               m.PO.GRF = "PO")),
             nrow = 1, ncol = 3) +
  theme(axis.title = element_text(size = 11),   # Increase axis titles
        axis.text = element_text(size = 9),    # Increase axis text
        strip.text = element_text(size = 11),   # Increase facet title size
        strip.background = element_rect(fill = "gray96")) +
  ggtitle('PO Intercept')

p4 <- newdf %>% 
  filter(!is.na(cor.GRF)) %>% 
  filter(!is.na(PA_intercept)) %>% 
  ggplot(aes(x = cor.GRF, y = PA_intercept, color = mod.type)) +
  geom_point(alpha = 0.4, size= 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type), alpha = 0.3) +
  geom_hline(yintercept = beta0, linetype = "dashed", color = "red") +
  labs(x = "Correlation random", y = expression(beta[0]), fill = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(-5.5, -1)) +
  theme_bw() +
  facet_wrap(~ mod.type, 
             labeller = as_labeller(c(
               m.int.GRF = "Integrated", 
               m.PA.GRF = "PA",
               m.PO.GRF = "PO")),
             nrow = 1, ncol = 3) +
  theme(axis.title = element_text(size = 11),   # Increase axis titles
        axis.text = element_text(size = 9),    # Increase axis text
        strip.text = element_text(size = 11),   # Increase facet title size
        strip.background = element_rect(fill = "gray96")) +
ggtitle('PA Intercept')

intercepts <- ggarrange(p3, p4, common.legend = T,  ncol = 2, nrow = 1)

COR_GRF_vs_Params_plot <- ggarrange(p1 + rremove("xlab"), p2 + rremove("xlab"), intercepts, common.legend = T,  ncol =1, nrow = 3)

ggsave(plot = COR_GRF_vs_Params_plot, filename = paste0(file.path(result_path),"/Figure_2_GRF_Corr_vs_Params_Scenario_", scenario_name, ".png"), w = 20, h = 22, units = "cm", dpi = 400, device = "png")

########## CORRELATION BETWEEN GRF ACCURACY AND BETA ACCURACY ############

extrap.scenario.df.CI <- extrap.scenario.df %>% 
  mutate(beta1.cred.int = beta1_975 - beta1_25,
         beta2.cred.int = beta2_975 - beta2_25,
         beta1.cred.int.true = ifelse(beta1 >= beta1_25 &  beta1 <= beta1_975, 1, 0),
         beta2.cred.int.true = ifelse(beta2 >= beta2_25 & beta2 <= beta2_975, 1, 0))

# Add the fixed and random effects columns to the extrap.scenario dataframe by matching extrap, rep, job, and model type

newdf.CI <- newdf %>%
  left_join(
    extrap.scenario.df.CI %>% select(extrap.type, rep, job_index, mod.type, beta1.cred.int),
    by = c("extrap.type", "rep", "job_index", "mod.type")
  )

newdf.CI <- newdf.CI %>%
  left_join(
    extrap.scenario.df.CI %>% select(extrap.type, rep, job_index, mod.type, beta2.cred.int),
    by = c("extrap.type", "rep", "job_index", "mod.type")
  )

p1 <- newdf.CI %>% 
  filter(!is.na(cor.GRF)) %>% 
  ggplot(aes(x = cor.GRF, y = beta1.cred.int, color = mod.type)) +
  geom_point(alpha = 0.4, size= 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type), alpha = 0.3) +
  labs(x = "Correlation random", y = bquote(beta[1] ~ " Credible Interval Width"), fill = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_bw() +
  facet_wrap(~ mod.type, 
             labeller = as_labeller(c(
               m.int.GRF = "Integrated", 
               m.PA.GRF = "PA",
               m.PO.GRF = "PO")),
             nrow = 1, ncol = 3) +
  theme(axis.title = element_text(size = 11),   # Increase axis titles
        axis.text = element_text(size = 9),    # Increase axis text
        strip.text = element_text(size = 11),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"))

p2 <- newdf.CI %>% 
  filter(!is.na(cor.GRF)) %>% 
  ggplot(aes(x = cor.GRF, y = beta2.cred.int, color = mod.type)) +
  geom_point(alpha = 0.4, size= 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type), alpha = 0.3) +
  labs(x = "Correlation random", y = bquote(beta[2] ~ " Credible Interval Width"), fill = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(0, 0.5)) +
  theme_bw() +
  facet_wrap(~ mod.type, 
             labeller = as_labeller(c(
               m.int.GRF = "Integrated", 
               m.PA.GRF = "PA",
               m.PO.GRF = "PO")),
             nrow = 1, ncol = 3) +
  theme(axis.title = element_text(size = 11),   # Increase axis titles
        axis.text = element_text(size = 9),    # Increase axis text
        strip.text = element_text(size = 11),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"))


COR_GRF_vs_Beta_CI <- ggarrange(p1 + rremove("xlab"), p2, common.legend = T,  ncol = 1, nrow = 2)

ggsave(plot = COR_GRF_vs_Beta_CI, filename = paste0(file.path(result_path),"/Figure_2_GRF_Corr_vs_Beta_CI_Width_Scenario_", scenario_name, ".png"), w = 20, h = 22, units = "cm", dpi = 400, device = "png")


########## CORRELATION BETWEEN FIXED ACCURACY AND BETAS ############

# p1 <- newdf %>% 
#   filter(!is.na(cor.FIXED)) %>% 
#   ggplot(aes(x = cor.FIXED, y = beta1.mean, color = mod.type)) +
#   geom_point(alpha = 0.4, size= 0.6) +
#   geom_smooth(method = "loess", se = T, aes(fill = mod.type), alpha = 0.3) +
#   geom_hline(yintercept = beta1, linetype = "dashed", color = "red") +
#   labs(x = "Correlation fixed", y = expression(beta[1]), fill = "Model Type") +
#   scale_color_manual(values = fill.colours, guide = "none") +
#   scale_fill_manual(values = fill.colours, guide = "none") +
#   coord_cartesian(ylim = c(-1, 1)) +
#   theme_bw() +
#   facet_wrap(~ mod.type, 
#              labeller = as_labeller(c(
#                m.int.GRF = "Integrated", 
#                m.PA.GRF = "PA",
#                m.PO.GRF = "PO")),
#              nrow = 1, ncol = 3) +
#   theme(axis.title = element_text(size = 11),   # Increase axis titles
#         axis.text = element_text(size = 9),    # Increase axis text
#         strip.text = element_text(size = 11),   # Increase facet title size
#         strip.background = element_rect(fill = "gray96"))
# 
# p2 <- newdf %>% 
#   filter(!is.na(cor.FIXED)) %>% 
#   ggplot(aes(x = cor.FIXED, y = beta2.mean, color = mod.type)) +
#   geom_point(alpha = 0.4, size= 0.6) +
#   geom_smooth(method = "loess", se = T, aes(fill = mod.type), alpha = 0.3) +
#   geom_hline(yintercept = beta2, linetype = "dashed", color = "red") +
#   labs(x = "Correlation fixed", y = expression(beta[2]), fill = "Model Type") +
#   scale_color_manual(values = fill.colours, guide = "none") +
#   scale_fill_manual(values = fill.colours, guide = "none") +
#   coord_cartesian(ylim = c(-0.25, 0.75)) +
#   theme_bw() +
#   facet_wrap(~ mod.type, 
#              labeller = as_labeller(c(
#                m.int.GRF = "Integrated", 
#                m.PA.GRF = "PA",
#                m.PO.GRF = "PO")),
#              nrow = 1, ncol = 3) +
#   theme(axis.title = element_text(size = 11),   # Increase axis titles
#         axis.text = element_text(size = 9),    # Increase axis text
#         strip.text = element_text(size = 11),   # Increase facet title size
#         strip.background = element_rect(fill = "gray96"))
# 
# p3 <- newdf %>% 
#   filter(!is.na(cor.FIXED)) %>% 
#   filter(!is.na(PO_intercept)) %>% 
#   ggplot(aes(x = cor.FIXED, y = PO_intercept, color = mod.type)) +
#   geom_point(alpha = 0.4, size= 0.6) +
#   geom_smooth(method = "loess", se = T, aes(fill = mod.type), alpha = 0.3) +
#   geom_hline(yintercept = beta0, linetype = "dashed", color = "red") +
#   labs(x = "Correlation fixed", y = expression(beta[0]), fill = "Model Type") +
#   scale_color_manual(values = fill.colours, guide = "none") +
#   scale_fill_manual(values = fill.colours, guide = "none") +
#   # coord_cartesian(ylim = c(-5.5, -1)) +
#   theme_bw() +
#   facet_wrap(~ mod.type, 
#              labeller = as_labeller(c(
#                m.int.GRF = "Integrated", 
#                m.PA.GRF = "PA",
#                m.PO.GRF = "PO")),
#              nrow = 1, ncol = 3) +
#   theme(axis.title = element_text(size = 11),   # Increase axis titles
#         axis.text = element_text(size = 9),    # Increase axis text
#         strip.text = element_text(size = 11),   # Increase facet title size
#         strip.background = element_rect(fill = "gray96")) +
#   ggtitle('PO Intercept')
# 
# p4 <- newdf %>% 
#   filter(!is.na(cor.FIXED)) %>% 
#   filter(!is.na(PA_intercept)) %>% 
#   ggplot(aes(x = cor.FIXED, y = PA_intercept, color = mod.type)) +
#   geom_point(alpha = 0.4, size= 0.6) +
#   geom_smooth(method = "loess", se = T, aes(fill = mod.type), alpha = 0.3) +
#   geom_hline(yintercept = beta0, linetype = "dashed", color = "red") +
#   labs(x = "Correlation random", y = expression(beta[0]), fill = "Model Type") +
#   scale_color_manual(values = fill.colours, guide = "none") +
#   scale_fill_manual(values = fill.colours, guide = "none") +
#   coord_cartesian(ylim = c(-5.5, -1)) +
#   theme_bw() +
#   facet_wrap(~ mod.type, 
#              labeller = as_labeller(c(
#                m.int.GRF = "Integrated", 
#                m.PA.GRF = "PA",
#                m.PO.GRF = "PO")),
#              nrow = 1, ncol = 3) +
#   theme(axis.title = element_text(size = 11),   # Increase axis titles
#         axis.text = element_text(size = 9),    # Increase axis text
#         strip.text = element_text(size = 11),   # Increase facet title size
#         strip.background = element_rect(fill = "gray96")) +
#   ggtitle('PA Intercept')
# 
# intercepts <- ggarrange(p3, p4, common.legend = T,  ncol = 2, nrow = 1)
# 
# COR_GRF_vs_Params_plot <- ggarrange(p1 + rremove("xlab"), p2 + rremove("xlab"), intercepts, common.legend = T,  ncol =1, nrow = 3)
# 
# ggsave(plot = COR_GRF_vs_Params_plot, filename = paste0(file.path(result_path),"/Figure_2_GRF_Corr_vs_Params_Scenario_", scenario_name, ".png"), w = 20, h = 22, units = "cm", dpi = 400, device = "png")


########## CORRELATION BETWEEN GRF ACCURACY AND RMSE (prediction) ############

# Add in RMSE from prediction site
newdf <- newdf %>%
  left_join(
    true.validation.df %>% select(extrap.type, rep, job_index, mod.type, RMSE),
    by = c("extrap.type", "rep", "job_index", "mod.type")
  )

newdfA <- newdf %>%
  select(-RMSE) %>% 
  left_join(
    true.validation.SiteA.df %>% select(extrap.type, rep, job_index, mod.type, RMSE),
    by = c("extrap.type", "rep", "job_index", "mod.type")
  )

GRF_RMSE <- newdf %>% 
  filter(!is.na(cor.GRF)) %>% 
  ggplot(aes(x = cor.GRF, y = RMSE, color = mod.type)) +
  geom_point(alpha = 0.4, size= 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type), alpha = 0.3) +
  labs(x = "Correlation random", y = "RMSE", fill = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(NA, 7)) +
  theme_bw() +
  facet_wrap(~ mod.type, 
             labeller = as_labeller(c(
               m.int.GRF = "Integrated", 
               m.PA.GRF = "PA",
               m.PO.GRF = "PO")),
             nrow = 1, ncol = 3) +
  theme(axis.title = element_text(size = 11),   # Increase axis titles
        axis.text = element_text(size = 9),    # Increase axis text
        strip.text = element_text(size = 11),   # Increase facet title size
        strip.background = element_rect(fill = "gray96")) +
  ggtitle("Projection Site")

GRF_RMSE_A <- newdfA %>% 
  filter(!is.na(cor.GRF)) %>% 
  ggplot(aes(x = cor.GRF, y = RMSE, color = mod.type)) +
  geom_point(alpha = 0.4, size= 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type), alpha = 0.3) +
  labs(x = "Correlation random", y = "RMSE", fill = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(NA, 7)) +
  theme_bw() +
  facet_wrap(~ mod.type, 
             labeller = as_labeller(c(
               m.int.GRF = "Integrated", 
               m.PA.GRF = "PA",
               m.PO.GRF = "PO")),
             nrow = 1, ncol = 3) +
  theme(axis.title = element_text(size = 11),   # Increase axis titles
        axis.text = element_text(size = 9),    # Increase axis text
        strip.text = element_text(size = 11),   # Increase facet title size
        strip.background = element_rect(fill = "gray96")) +
  ggtitle("Training Site")

plot <- ggarrange(GRF_RMSE + rremove("xlab"), GRF_RMSE_A, common.legend = T,  ncol = 1, nrow = 2)

ggsave(plot = plot, filename = paste0(file.path(result_path),"/Figure_2_GRF_vs_RMSE_Scenario_", scenario_name, ".png"), w = 20, h = 15, units = "cm", dpi = 400, device = "png")


########## CORRELATION BETWEEN SITE A and SITE B GRF CORR AND RMSE (prediction) ############

# Add in RMSE from prediction site
newdf <- true.validation.df %>%
  left_join(
    true.validation.SiteA.df %>% select(extrap.type, rep, job_index, mod.type, cor.GRFA.GRFB),
    by = c("extrap.type", "rep", "job_index", "mod.type")
  )

GRFAB_RMSE <- newdf %>% 
  filter(!is.na(cor.GRFA.GRFB)) %>% 
  ggplot(aes(x = cor.GRFA.GRFB, y = RMSE, color = mod.type)) +
  geom_point(alpha = 0.4, size= 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type), alpha = 0.3) +
  labs(x = "Correlation random Site A Site B", y = "RMSE", fill = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(NA, 6)) +
  theme_bw() +
  facet_wrap(~ mod.type, 
             labeller = as_labeller(c(
               m.int.GRF = "Integrated", 
               m.PA.GRF = "PA",
               m.PO.GRF = "PO")),
             nrow = 1, ncol = 3) +
  theme(axis.title = element_text(size = 11),   # Increase axis titles
        axis.text = element_text(size = 9),    # Increase axis text
        strip.text = element_text(size = 11),   # Increase facet title size
        strip.background = element_rect(fill = "gray96")) +
  ggtitle("Projection Site")

ggsave(plot = GRFAB_RMSE, filename = paste0(file.path(result_path),"/Figure_2_GRFSiteA_B_vs_RMSE_Scenario_", scenario_name, ".png"), w = 20, h = 15, units = "cm", dpi = 400, device = "png")



extrap.scenario.df %>% 
  ggplot(aes(x = bias.type, y = beta1.mean, fill = bias.type)) +  # Change x to bias.type for one boxplot per facet
  geom_boxplot() +
  geom_hline(yintercept = beta1, linetype = "dashed", color = "red") +
  labs(x = NULL, y = expression(beta[1]), color = NULL) +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +  # Define custom colors
  coord_cartesian(ylim = c(-1.5, 1.5)) +
  theme_bw() +
  facet_wrap(~mod.type2, labeller = as_labeller(c(m.int = "Integrated", m.PA = "PA", m.PO = "PO")), scales = "free_x") +  # Wrap facets and use free_x to adjust x-axis
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

extrap.scenario.df %>% 
  ggplot(aes(x = bias.type, y = beta2.mean, fill = bias.type)) +  # Change x to bias.type for one boxplot per facet
  geom_boxplot() +
  geom_hline(yintercept = beta2, linetype = "dashed", color = "red") +
  labs(x = NULL, y = expression(beta[2]), color = NULL) +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +  # Define custom colors
  coord_cartesian(ylim = c(-1.5, 1.5)) +
  theme_bw() +
  facet_wrap(~mod.type2, labeller = as_labeller(c(m.int = "Integrated", m.PA = "PA", m.PO = "PO")), scales = "free_x") +  # Wrap facets and use free_x to adjust x-axis
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

extrap.scenario.df.CI  %>% 
  ggplot(aes(x = bias.type, y = beta1.cred.int,  , fill = bias.type)) +  # Change x to bias.type for one boxplot per facet
  geom_boxplot() +
  labs(x = NULL, y = bquote(beta[1] ~ " CI Width"), color = NULL) +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +  # Define custom colors
  coord_cartesian(ylim = c(NA, 4.5)) +
  theme_bw() +
  facet_wrap(~mod.type2, labeller = as_labeller(c(m.int = "Integrated", m.PA = "PA", m.PO = "PO")), scales = "free_x") +  # Wrap facets and use free_x to adjust x-axis
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

extrap.scenario.df.CI  %>% 
  ggplot(aes(x = bias.type, y = beta2.cred.int,  , fill = bias.type)) +  # Change x to bias.type for one boxplot per facet
  geom_boxplot() +
  labs(x = NULL, y = bquote(beta[2] ~ " CI Width"), color = NULL) +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +  # Define custom colors
  coord_cartesian(ylim = c(NA, 3)) +
  theme_bw() +
  facet_wrap(~mod.type2, labeller = as_labeller(c(m.int = "Integrated", m.PA = "PA", m.PO = "PO")), scales = "free_x") +  # Wrap facets and use free_x to adjust x-axis
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


#### INTERCEPTS ####
PO_INT <- extrap.scenario.df  %>% 
  filter(!is.na(PO_intercept)) %>% 
  ggplot(aes(x = bias.type, y = PO_intercept,  , fill = bias.type)) +  # Change x to bias.type for one boxplot per facet
  geom_boxplot() +
  labs(x = NULL, y = expression(beta[0]), color = NULL) +
  geom_hline(yintercept = beta0, linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +  # Define custom colors
  coord_cartesian(ylim = c(-7, 1)) +
  theme_bw() +
  facet_wrap(~mod.type2, labeller = as_labeller(c(m.int = "Integrated", m.PA = "PA", m.PO = "PO")), scales = "free_x") +  # Wrap facets and use free_x to adjust x-axis
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
        strip.background = element_rect(fill = "gray96")) +
  ggtitle('PO Intercept')

PA_INT <- extrap.scenario.df  %>% 
  filter(!is.na(PA_intercept)) %>% 
  ggplot(aes(x = bias.type, y = PA_intercept,  , fill = bias.type)) +  # Change x to bias.type for one boxplot per facet
  geom_boxplot() +
  labs(x = NULL, y = expression(beta[0]), color = NULL) +
  geom_hline(yintercept = beta0, linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +  # Define custom colors
  coord_cartesian(ylim = c(-7, 1)) +
  theme_bw() +
  facet_wrap(~mod.type2, labeller = as_labeller(c(m.int = "Integrated", m.PA = "PA", m.PO = "PO")), scales = "free_x") +  # Wrap facets and use free_x to adjust x-axis
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
        strip.background = element_rect(fill = "gray96")) +
  ggtitle('PA Intercept')

ggarrange(PO_INT, PA_INT, common.legend = T,  ncol = 2, nrow = 1, legend = "none")




# Supp FIG 2. Spat Auto Range relative to Site Dist -----------------------

# If Low = 20, Mod = 100, High = 200

true.validation.df <- true.validation.df %>% mutate(scal = ifelse(extrap.type == "Low", 20, 
                                                           ifelse(extrap.type == "Moderate", 100, 
                                                                  ifelse(extrap.type == "High", 200, NA))))

true.validation.df <- true.validation.df %>% 
  mutate(range_relative = scal / Site.distance)

# Update to separate out GRF or non-GRF

true.validation.df <- true.validation.df %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF")) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type = factor(mod.type, levels = c("m.PA", "m.PO", "m.int")))

# Then do a continuous plot with RMSE and range_relative

RMSE_range <- true.validation.df %>% 
  ggplot(aes(x = range_relative, y = RMSE, color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  labs(x = "Relative autocorrelation range to site distance", y = "RMSE", fill = "Model Type", color = "Model Type") +
  # scale_x_reverse() +  # Reverse x-axis from 1 to 0
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_shape_manual(values = c("Without GRF" = 16, "With GRF" = 17)) +  # Different shapes for bias types
  scale_linetype_manual(values = c("Without GRF" = "solid", "With GRF" = "dotted")) +  # Different line types for bias types
  coord_cartesian(ylim = c(NA, 10)) +
  theme_bw() +
  facet_grid(cols = vars(mod.type2), labeller = as_labeller(c(m.int = "Integrated", m.PA = "PA", m.PO = "PO"))) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 13),  # Increase legend text size
        axis.title = element_text(size = 14),   # Increase axis titles
        axis.text = element_text(size = 12),    # Increase axis text
        strip.text = element_text(size = 14),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"))


ggsave(plot = RMSE, filename = paste0(file.path(result_path),"/Figure_1b_Scenario_", scenario_name, ".png"), w = 23.5, h = 14, units = "cm", dpi = 400, device = "png")

############################################################################
# Figure 3. Scenario 6. ---------------------------------------------------
# Environmental extrap and spatial autocorrelation with bias
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


true.validation.df <- true.validation.df %>%
  mutate(bias.type = ifelse(grepl("bias", mod.type, fixed = T), "With bias cov", "Without bias cov")) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PA", "m.PA.GRF", "m.PO", "m.PO.GRF", "m.int", "m.int.GRF")))


RMSE <- true.validation.df %>% 
  ggplot(aes(x = mean.extrap, y = RMSE, color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.3, size= 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  labs(x = x.label, y = "RMSE", fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_shape_manual(values = c("Without bias cov" = 16, "With bias cov" = 17)) +  # Different shapes for bias types
  scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
  coord_cartesian(ylim = c(NA, 4)) +
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
        axis.title = element_text(size = 11),   # Increase axis titles
        axis.text = element_text(size = 9),    # Increase axis text
        strip.text = element_text(size = 11),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"))


ggsave(plot = RMSE, filename = paste0(file.path(result_path),"/Figure_3_Scenario_", scenario_name, ".png"), w = 13, h = 16, units = "cm", dpi = 400, device = "png")

##### COEFFICIENTS #########

extrap.scenario.df  <- extrap.scenario.df %>% 
  mutate(bias.type = ifelse(grepl("bias", mod.type, fixed = T), "With bias cov", "Without bias cov")) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PA", "m.PA.GRF", "m.PO", "m.PO.GRF", "m.int", "m.int.GRF")))

extrap.scenario.df <- extrap.scenario.df[!is.na(extrap.scenario.df$mod.type2), ]

b1 <- extrap.scenario.df %>% 
  ggplot(aes(x = mean.extrap, y = beta1.mean,  color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  geom_hline(yintercept = beta1, linetype = "dashed", color = "red") +
  labs(x = x.label, y = expression(beta[1]), fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_shape_manual(values = c("Without bias cov" = 16, "With bias cov" = 17)) +  # Different shapes for bias types
  scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
  coord_cartesian(ylim = c(-0.75, 0.75)) +
  theme_bw() +
  facet_wrap(~mod.type2,
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
        legend.text = element_text(size = 13),  # Increase legend text size
        axis.title = element_text(size = 14),   # Increase axis titles
        axis.text = element_text(size = 11),    # Increase axis text
        strip.text = element_text(size = 14),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"))

b2 <- extrap.scenario.df %>% 
  ggplot(aes(x = mean.extrap, y = beta2.mean,  color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  geom_hline(yintercept = beta2, linetype = "dashed", color = "red") +
  labs(x = x.label, y = expression(beta[2]), fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_shape_manual(values = c("Without bias cov" = 16, "With bias cov" = 17)) +  # Different shapes for bias types
  scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
  coord_cartesian(ylim = c(-0.25, 0.75)) +
  theme_bw() +
  facet_wrap(~mod.type2,
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
        legend.text = element_text(size = 13),  # Increase legend text size
        axis.title = element_text(size = 14),   # Increase axis titles
        axis.text = element_text(size = 11),    # Increase axis text
        strip.text = element_text(size = 14),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"))

ggsave(plot = b1, filename = paste0(file.path(result_path),"/Figure_3_BETA1_Scenario_", scenario_name, "_test.png"), w = 15, h = 20, units = "cm", dpi = 400, device = "png")

ggsave(plot = b2, filename = paste0(file.path(result_path),"/Figure_3_BETA2_Scenario_", scenario_name, "_test.png"), w = 15, h = 20, units = "cm", dpi = 400, device = "png")

######### INTERCEPTS ###########

PO_INT <- extrap.scenario.df %>% 
  filter(!is.na(PO_intercept)) %>% 
  ggplot(aes(x = mean.extrap, y = PO_intercept,  color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  geom_hline(yintercept = beta0, linetype = "dashed", color = "red") +
  labs(x = x.label, y = expression(beta[0]), fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_shape_manual(values = c("Without bias cov" = 16, "With bias cov" = 17)) +  # Different shapes for bias types
  scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
  coord_cartesian(ylim = c(-5.5, 0.5)) +
  theme_bw() +
  facet_grid(cols = vars(mod.type2), labeller = as_labeller(c(m.int = "Integrated", 
                                                              m.PA = "PA", 
                                                              m.PO = "PO", 
                                                              m.int.GRF = "Integrated w GRF",
                                                              m.PA.GRF = "PA w GRF",
                                                              m.PO.GRF = "PO w GRF"))) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 13),  # Increase legend text size
        axis.title = element_text(size = 14),   # Increase axis titles
        axis.text = element_text(size = 11),    # Increase axis text
        strip.text = element_text(size = 14),   # Increase facet title size
        strip.background = element_rect(fill = "gray96")) +
  ggtitle('PO Intercept')

PA_INT <- extrap.scenario.df %>% 
  filter(!is.na(PA_intercept)) %>% 
  ggplot(aes(x = mean.extrap, y = PA_intercept,  color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  geom_hline(yintercept = beta0, linetype = "dashed", color = "red") +
  labs(x = x.label, y = expression(beta[0]), fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_shape_manual(values = c("Without bias cov" = 16, "With bias cov" = 17)) +  # Different shapes for bias types
  scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
  coord_cartesian(ylim = c(-5.5, 0.5)) +
  theme_bw() +
  facet_grid(cols = vars(mod.type2), labeller = as_labeller(c(m.int = "Integrated", 
                                                              m.PA = "PA", 
                                                              m.PO = "PO", 
                                                              m.int.GRF = "Integrated w GRF",
                                                              m.PA.GRF = "PA w GRF",
                                                              m.PO.GRF = "PO w GRF"))) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 13),  # Increase legend text size
        axis.title = element_text(size = 14),   # Increase axis titles
        axis.text = element_text(size = 11),    # Increase axis text
        strip.text = element_text(size = 14),   # Increase facet title size
        strip.background = element_rect(fill = "gray96")) +
  ggtitle('PA Intercept')


intercepts <- ggarrange(PO_INT + rremove("xlab"), PA_INT, common.legend = T,  ncol = 1, nrow = 2, legend = "bottom")

ggsave(plot = intercepts, filename = paste0(file.path(result_path),"/Figure_3_INTERCEPTS_Scenario_", scenario_name, "_test.png"), w = 20, h = 20, units = "cm", dpi = 400, device = "png")


##### GRF Parameters #########

extrap.scenario.df  <- extrap.scenario.df %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PA.GRF", "m.PO.GRF","m.int.GRF")))

extrap.scenario.df <- extrap.scenario.df[!is.na(extrap.scenario.df$GRF.range.mean), ]

g1 <- extrap.scenario.df %>% 
  ggplot(aes(x = mean.extrap, y = GRF.range.mean,  color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  geom_hline(yintercept = scal, linetype = "dashed", color = "red") +
  labs(x = x.label, y = "GRF range", fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_shape_manual(values = c("Without bias cov" = 16, "With bias cov" = 17)) +  # Different shapes for bias types
  scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
  coord_cartesian(ylim = c(NA, 75)) +
  theme_bw() +
  facet_wrap(~mod.type2,
             labeller = as_labeller(c(
               m.int.GRF = "Integrated w GRF", 
               m.PA.GRF = "PA w GRF",
               m.PO.GRF = "PO w GRF")),
             nrow = 1, ncol =3 ) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 13),  # Increase legend text size
        axis.title = element_text(size = 14),   # Increase axis titles
        axis.text = element_text(size = 11),    # Increase axis text
        strip.text = element_text(size = 14),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"))

g2 <- extrap.scenario.df %>% 
  ggplot(aes(x = mean.extrap, y = GRF.sd.mean,  color = mod.type2, shape = bias.type, linetype = bias.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type2, color = mod.type2, linetype = bias.type), alpha = 0.3) +
  geom_hline(yintercept = variance, linetype = "dashed", color = "red") +
  labs(x = x.label, y = "GRF var", fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_shape_manual(values = c("Without bias cov" = 16, "With bias cov" = 17)) +  # Different shapes for bias types
  scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
  coord_cartesian(ylim = c(NA, 4)) +
  theme_bw() +
  facet_wrap(~mod.type2,
             labeller = as_labeller(c(
               m.int.GRF = "Integrated w GRF", 
               m.PA.GRF = "PA w GRF",
               m.PO.GRF = "PO w GRF")),
             nrow = 1, ncol =3 ) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 13),  # Increase legend text size
        axis.title = element_text(size = 14),   # Increase axis titles
        axis.text = element_text(size = 11),    # Increase axis text
        strip.text = element_text(size = 14),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"))

GRF <- ggarrange(g1, g2, common.legend = T,  ncol = 1, nrow = 2, legend = "bottom")

ggsave(plot = GRF, filename = paste0(file.path(result_path),"/Figure_3_GRF_Params_Scenario_", scenario_name, "_test.png"), w = 20, h = 15, units = "cm", dpi = 400, device = "png")


############################################################################
# Figure 4. Scenario 3. ---------------------------------------------------
# Enviro. Extrap & Spatial Autocorrelation
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
        axis.title = element_text(size = 11),   # Increase axis titles
        axis.text = element_text(size = 9),    # Increase axis text
        strip.text = element_text(size = 11),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"))


ggsave(plot = RMSE, filename = paste0(file.path(result_path),"/Figure_4_Scenario_", scenario_name, ".png"), w = 15, h = 20, units = "cm", dpi = 400, device = "png")

######## COEFFICIENTS ############

extrap.scenario.df <- extrap.scenario.df %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF")) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type = factor(mod.type, levels = c("m.PA", "m.PA.GRF", "m.PO", "m.PO.GRF", "m.int", "m.int.GRF")))

b1 <- extrap.scenario.df %>% 
  ggplot(aes(x = mean.extrap, y = beta1.mean, color = mod.type)) +
  geom_point(alpha = 0.3, size= 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type, color = mod.type), alpha = 0.3) +
  geom_hline(yintercept = beta1, linetype = "dashed", color = "red") +
  labs(x = x.label, y = expression(beta[1]), fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(-0.75, 0.75)) +
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
        axis.title = element_text(size = 11),   # Increase axis titles
        axis.text = element_text(size = 9),    # Increase axis text
        strip.text = element_text(size = 11),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"))

b2 <- extrap.scenario.df %>% 
  ggplot(aes(x = mean.extrap, y = beta2.mean, color = mod.type)) +
  geom_point(alpha = 0.3, size= 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type, color = mod.type), alpha = 0.3) +
  geom_hline(yintercept = beta1, linetype = "dashed", color = "red") +
  labs(x = x.label, y = expression(beta[2]), fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(-0.25, 0.75)) +
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
        axis.title = element_text(size = 11),   # Increase axis titles
        axis.text = element_text(size = 9),    # Increase axis text
        strip.text = element_text(size = 11),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"))

ggsave(plot = b1, filename = paste0(file.path(result_path),"/Figure_4_BETA1_Scenario_", scenario_name, "_test.png"), w = 15, h = 20, units = "cm", dpi = 400, device = "png")

ggsave(plot = b2, filename = paste0(file.path(result_path),"/Figure_4_BETA2_Scenario_", scenario_name, "_test.png"), w = 15, h = 20, units = "cm", dpi = 400, device = "png")

##### GRF Parameters #########

extrap.scenario.df  <- extrap.scenario.df %>% 
  mutate(mod.type = factor(mod.type, levels = c("m.PA.GRF", "m.PO.GRF","m.int.GRF")))

extrap.scenario.df <- extrap.scenario.df[!is.na(extrap.scenario.df$GRF.range.mean), ]


g1 <- extrap.scenario.df %>% 
  ggplot(aes(x = mean.extrap, y = GRF.range.mean, color = mod.type)) +
  geom_point(alpha = 0.3, size= 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type, color = mod.type), alpha = 0.3) +
  geom_hline(yintercept = scal, linetype = "dashed", color = "red") +
  labs(x = x.label, y = "GRF range", fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(0, 75)) +
  theme_bw() +
  facet_wrap(~mod.type,
             labeller = as_labeller(c(
               m.int.GRF = "Integrated w GRF", 
               m.PA.GRF = "PA w GRF",
               m.PO.GRF = "PO w GRF")),
             nrow = 1, ncol =3 ) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 13),  # Increase legend text size
        axis.title = element_text(size = 14),   # Increase axis titles
        axis.text = element_text(size = 11),    # Increase axis text
        strip.text = element_text(size = 14),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"))


g2 <- extrap.scenario.df %>% 
  ggplot(aes(x = mean.extrap, y = GRF.sd.mean, color = mod.type)) +
  geom_point(alpha = 0.3, size= 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type, color = mod.type), alpha = 0.3) +
  geom_hline(yintercept = variance, linetype = "dashed", color = "red") +
  labs(x = x.label, y = "GRF var", fill = "Model Type", color = "Model Type") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(NA, 4)) +
  theme_bw() +
  facet_wrap(~mod.type,
             labeller = as_labeller(c(
               m.int.GRF = "Integrated w GRF", 
               m.PA.GRF = "PA w GRF",
               m.PO.GRF = "PO w GRF")),
             nrow = 1, ncol =3 ) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 13),  # Increase legend text size
        axis.title = element_text(size = 14),   # Increase axis titles
        axis.text = element_text(size = 11),    # Increase axis text
        strip.text = element_text(size = 14),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"))


GRF <- ggarrange(g1, g2, common.legend = T,  ncol = 1, nrow = 2, legend = "bottom")

ggsave(plot = GRF, filename = paste0(file.path(result_path),"/Figure_4_GRF_Params_Scenario_", scenario_name, "_test.png"), w = 20, h = 15, units = "cm", dpi = 400, device = "png")

############################################################################
# Figure 5. Scenario 5. ---------------------------------------------------
# Spatial autocorrelation w bias
############################################################################


scenario_name = "5"

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

# RMSE <- true.validation.df %>% 
#   filter(bias.type == "Without bias cov") %>% 
#   ggplot(aes(x = extrap.type, y = RMSE, fill = mod.type2)) +
#   geom_boxplot() +
#   labs(x = x.label, y = "RMSE", fill = "Model Type") +
#   scale_x_discrete(labels = x.discrete.label) +
#   scale_fill_manual(values = fill.colours, guide = "none") +
#   coord_cartesian(ylim = c(0, 10)) +
#   theme_bw() +
#   facet_wrap(~ mod.type2, 
#              labeller = as_labeller(c(
#                m.int = "Integrated", 
#                m.PA = "PA", 
#                m.PO = "PO", 
#                m.int.GRF = "Integrated w GRF", 
#                m.PA.GRF = "PA w GRF",
#                m.PO.GRF = "PO w GRF")),
#              nrow = 3, ncol = 2) +
#   theme(axis.title = element_text(size = 11),   # Increase axis titles
#         axis.text = element_text(size = 9),    # Increase axis text
#         strip.text = element_text(size = 11),   # Increase facet title size
#         strip.background = element_rect(fill = "gray96"))
# 
# ggsave(plot = RMSE, filename = paste0(file.path(result_path),"/Figure_5a_No_BIAS_COV_Scenario_", scenario_name, ".png"), w = 15, h = 20, units = "cm", dpi = 400, device = "png")
# 
# RMSE <- true.validation.df %>% 
#   filter(bias.type == "With bias cov") %>% 
#   ggplot(aes(x = extrap.type, y = RMSE, fill = mod.type2)) +
#   geom_boxplot() +
#   labs(x = x.label, y = "RMSE", fill = "Model Type") +
#   scale_x_discrete(labels = x.discrete.label) +
#   scale_fill_manual(values = fill.colours, guide = "none") +
#   coord_cartesian(ylim = c(0, 10)) +
#   theme_bw() +
#   facet_wrap(~ mod.type2, 
#              labeller = as_labeller(c(
#                m.int = "Integrated", 
#                m.PA = "PA", 
#                m.PO = "PO", 
#                m.int.GRF = "Integrated w GRF", 
#                m.PA.GRF = "PA w GRF",
#                m.PO.GRF = "PO w GRF")),
#              nrow = 3, ncol = 2) +
#   theme(axis.title = element_text(size = 11),   # Increase axis titles
#         axis.text = element_text(size = 9),    # Increase axis text
#         strip.text = element_text(size = 11),   # Increase facet title size
#         strip.background = element_rect(fill = "gray96"))
# 
# 
# ggsave(plot = RMSE, filename = paste0(file.path(result_path),"/Figure_5b_BIAS_COV_Scenario_", scenario_name, ".png"), w = 15, h = 10, units = "cm", dpi = 400, device = "png")



RMSE <- true.validation.df %>% 
  ggplot(aes(x = bias.type, y = RMSE, fill = bias.type)) +
  geom_boxplot() +
  labs(y = "RMSE", fill = "Model Type") +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) +  # Define custom colors
  coord_cartesian(ylim = c(0, 6)) +
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
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  # Increase legend text size
        axis.title = element_text(size = 11),   # Increase axis titles
        axis.text = element_text(size = 9),    # Increase axis text
        strip.text = element_text(size = 11),   # Increase facet title size
        axis.title.x = element_blank(),         # Remove x-axis title
        axis.text.x = element_blank(),          # Remove x-axis text
        axis.ticks.x = element_blank(),         # Remove x-axis ticks
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +  # Move title to the right)
  ggtitle('Projection site')


ggsave(plot = RMSE, filename = paste0(file.path(result_path),"/Figure_5_Scenario_", scenario_name, ".png"), w = 10, h = 15, units = "cm", dpi = 400, device = "png")



#### SITE A #######


true.validation.SiteA.df <- true.validation.SiteA.df %>%
  mutate(bias.type = ifelse(grepl("bias", mod.type, fixed = T), "With bias cov", "Without bias cov")) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PA", "m.PA.GRF", "m.PO", "m.PO.GRF", "m.int", "m.int.GRF")))

RMSE_A <- true.validation.SiteA.df %>% 
  ggplot(aes(x = bias.type, y = RMSE, fill = bias.type)) +
  geom_boxplot() +
  labs(y = "RMSE", fill = "Model Type") +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without bias cov" = "#D55E00", "With bias cov" = "#0072B2")) +  # Define custom colors
  coord_cartesian(ylim = c(0, 6)) +
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
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  # Increase legend text size
        axis.title = element_text(size = 11),   # Increase axis titles
        axis.text = element_text(size = 9),    # Increase axis text
        strip.text = element_text(size = 11),   # Increase facet title size
        axis.title.x = element_blank(),         # Remove x-axis title
        axis.text.x = element_blank(),          # Remove x-axis text
        axis.ticks.x = element_blank(),         # Remove x-axis ticks
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +  # Move title to the right)
  ggtitle('Training site')


ggsave(plot = RMSE, filename = paste0(file.path(result_path),"/Figure_5_SITE_A_Scenario_", scenario_name, ".png"), w = 10, h = 15, units = "cm", dpi = 400, device = "png")


p <- ggarrange(RMSE + rremove("xlab"), RMSE_A + rremove("ylab") + rremove("xlab"), ncol = 2, nrow = 1, common.legend = T, legend = "bottom", labels = c("(a)", "(b)"))

ggsave(plot = p, filename = paste0(file.path(result_path),"/Figure_5_COMBINED_cenario_", scenario_name, ".png"), w = 20, h = 17, units = "cm", dpi = 400, device = "png")





