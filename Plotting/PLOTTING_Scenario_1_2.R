
library(ggplot2)
library(dplyr)
library(ggpubr)
library(viridis)
library(purrr)
library(readr)

outpath <- file.path(dirname(getwd()), "output")

result_path <- file.path(getwd(), "PLOTS")

# Make dir if not already there
if(!dir.exists(result_path)) {
  
  dir.create(result_path, recursive = TRUE)
  
}

#####################################################
####### CHOOSE NUMBER OF REPLICATES TO KEEP ########
#####################################################

replicates <- 300


############################################################################
# Scenario 1. ---------------------------------------------------
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


##########################################################
### REMOVE REPLICATES THAT DON'T HAVE ALL THREE OUTPUTS ###
##########################################################

validation <- true.validation.df %>%
  group_by(job_index) %>% 
  summarise(n = n()) %>% 
  select(job_index)

extrap <- extrap.scenario.df %>%
  group_by(job_index) %>% 
  summarise(n = n()) %>% 
  select(job_index)

validationA <- true.validation.SiteA.df %>%
  group_by(job_index) %>% 
  summarise(n = n()) %>% 
  select(job_index)

common_jobs <- validation %>% 
  inner_join(extrap, by = "job_index") %>% 
  inner_join(validationA, by = "job_index")


#######################################################
## OPTIONAL - TRIM JOBS TO DESIRED REPLICATE NUMBER ##
#######################################################

common_jobs <- common_jobs %>%
  slice(1:replicates)

true.validation.df <- true.validation.df %>%
  filter(job_index %in% common_jobs$job_index)

true.validation.SiteA.df <- true.validation.SiteA.df %>%
  filter(job_index %in% common_jobs$job_index)

extrap.scenario.df <- extrap.scenario.df %>%
  filter(job_index %in% common_jobs$job_index)


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
  mutate(bias.type = ifelse(grepl("bias", mod.type, fixed = T), "With bias covariate", "Without bias covariate")) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int"))) %>% 
  mutate(bias.type = factor(bias.type, levels = c("Without bias covariate", "With bias covariate")))


# NON-COLOURED VERSION (for supp)
RMSE_2 <- true.validation.df %>% 
  ggplot(aes(x = mean.extrap, y = RMSE.global, color = bias.type)) +
  geom_point(alpha = 0.05, size = 1.2) +
  geom_smooth(method = "loess", se = T, aes(fill = bias.type, color = bias.type), alpha = 0.3) +
  labs(x = x.label, y = "Mean RMSE projection site") +
  scale_fill_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) +
  scale_color_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) +
  #coord_cartesian(ylim = c(NA, 4.25)) +
  theme_bw() +
  facet_grid(cols = vars(mod.type2), labeller = as_labeller(c(m.int = "Integrated", m.PA = "Presence-absence", m.PO = "Presence-only"))) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  # Increase legend text size
        axis.title = element_text(size = 15),   # Increase axis titles
        axis.text = element_text(size = 12),    # Increase axis text
        strip.text = element_text(size = 15),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +  # Move title to the right
  ggtitle('High PO record numbers')

# For FIG. 3

RMSE_2_Fig3 <- true.validation.df %>% 
  ggplot(aes(x = mean.extrap, y = RMSE.global, color = bias.type)) +
  geom_point(alpha = 0.05, size = 1.2) +
  geom_smooth(method = "loess", se = T, aes(fill = bias.type, color = bias.type), alpha = 0.3) +
  labs(x = x.label, y = "Mean RMSE projection site") +
  scale_fill_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) +
  scale_color_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) +
  #coord_cartesian(ylim = c(NA, 4.25)) +
  theme_bw() +
  facet_grid(cols = vars(mod.type2), labeller = as_labeller(c(m.int = "Integrated", m.PA = "Presence-absence", m.PO = "Presence-only"))) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  # Increase legend text size
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title = element_text(size = 15),   # Increase axis titles
        axis.text = element_text(size = 12),    # Increase axis text
        strip.text = element_text(size = 15),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) 


# INTERVAL SCORE
Int.score_1 <- true.validation.df %>% 
  ggplot(aes(x = mean.extrap, y = Mean.Int.Score, color = bias.type)) +
  geom_point(alpha = 0.05, size = 1.2) +
  geom_smooth(method = "loess", se = T, aes(fill = bias.type, color = bias.type), alpha = 0.3) +
  labs(x = x.label, y = "Mean interval score") +
  scale_fill_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) +
  scale_color_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) +
  coord_cartesian(ylim = c(NA, 3.5)) +
  theme_bw() +
  facet_grid(cols = vars(mod.type2), labeller = as_labeller(c(m.int = "Integrated", m.PA = "Presence-absence", m.PO = "Presence-only"))) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  # Increase legend text size
        axis.title = element_text(size = 15),   # Increase axis titles
        axis.text = element_text(size = 12),    # Increase axis text
        strip.text = element_text(size = 15),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +  # Move title to the right
  ggtitle('High PO record numbers')


# CORRELATION ESTIMATED VS. TRUE
CORR_1 <- true.validation.df %>% 
  ggplot(aes(x = mean.extrap, y = correlation, color = bias.type)) +
  geom_point(alpha = 0.05, size = 1.2) +
  geom_smooth(method = "loess", se = T, aes(fill = bias.type, color = bias.type), alpha = 0.3) +
  labs(x = x.label, y = "Correlation") +
  scale_fill_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) +
  scale_color_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) +
  coord_cartesian(ylim = c(0.25, NA)) +
  theme_bw() +
  facet_grid(cols = vars(mod.type2), labeller = as_labeller(c(m.int = "Integrated", m.PA = "Presence-absence", m.PO = "Presence-only"))) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  # Increase legend text size
        axis.title = element_text(size = 15),   # Increase axis titles
        axis.text = element_text(size = 12),    # Increase axis text
        strip.text = element_text(size = 15),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +  # Move title to the right
  ggtitle('High PO record numbers')


# # COVERAGE RATE
cov.rate <- true.validation.df %>%
  ggplot(aes(x = mean.extrap, y = coverage.rate, color = bias.type)) +
  geom_point(alpha = 0.05, size = 1.2) +
  geom_smooth(method = "loess", se = T, aes(fill = bias.type, color = bias.type), alpha = 0.3) +
  labs(x = x.label, y = "Mean coverage probability") +
  scale_fill_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) +
  scale_color_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) +
  #coord_cartesian(ylim = c(NA, 3.5)) +
  theme_bw() +
  facet_grid(cols = vars(mod.type2), labeller = as_labeller(c(m.int = "Integrated", m.PA = "Presence-absence", m.PO = "Presence-only"))) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  # Increase legend text size
        axis.title = element_text(size = 15),   # Increase axis titles
        axis.text = element_text(size = 12),    # Increase axis text
        strip.text = element_text(size = 15),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +  # Move title to the right
  ggtitle('High PO record numbers')

######## SITE A #################

true.validation.SiteA.df <- true.validation.SiteA.df %>%
  mutate(bias.type = ifelse(grepl("bias", mod.type, fixed = T), "With bias covariate", "Without bias covariate")) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int"))) %>% 
  mutate(bias.type = factor(bias.type, levels = c("Without bias covariate", "With bias covariate")))

## NON-COLOURED VERSION
RMSE_A_1 <- true.validation.SiteA.df %>% 
  ggplot(aes(x = bias.type, y = RMSE.global, fill = bias.type)) +  
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = NULL, y = "Mean RMSE training site", color = NULL) +
  scale_fill_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) + 
  scale_color_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) + 
  coord_cartesian(ylim = c(NA, 4.3)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "Presence-absence", 
                                      m.PO = "Presence-only")), 
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
  ggtitle('High PO record numbers')


###### COEFFICIENTS #######

extrap.scenario.df  <- extrap.scenario.df %>% 
  mutate(bias.type = ifelse(grepl("bias", mod.type, fixed = T), "With bias covariate", "Without bias covariate")) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int"))) %>% 
  mutate(bias.type = factor(bias.type, levels = c("Without bias covariate", "With bias covariate")))

extrap.scenario.df <- extrap.scenario.df[!is.na(extrap.scenario.df$mod.type2), ]

b1_1 <- extrap.scenario.df %>% 
  ggplot(aes(x = bias.type, y = beta1.mean, fill = bias.type)) +  
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +  # Add a boxplot without outliers
  geom_hline(yintercept = beta1, linetype = "dashed", color = "red") +  # Add the horizontal dashed line
  labs(x = NULL, y = expression(beta[1]), color = NULL) +
  scale_fill_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) + 
  scale_color_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) + 
  coord_cartesian(ylim = c(-1, 1)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "Presence-absence", 
                                      m.PO = "Presence-only")), 
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
  ggtitle('High PO record numbers')

b2_1 <- extrap.scenario.df %>% 
  ggplot(aes(x = bias.type, y = beta2.mean, fill = bias.type)) +  
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +  # Add a boxplot without outliers
  geom_hline(yintercept = beta2, linetype = "dashed", color = "red") +  # Add the horizontal dashed line
  labs(x = NULL, y = expression(beta[2]), color = NULL) +
  scale_fill_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) + 
  scale_color_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) + 
  coord_cartesian(ylim = c(-1, 1)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "Presence-absence", 
                                      m.PO = "Presence-only")), 
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
  ggtitle('High PO record numbers')

beta_1 <- ggarrange(b1_1 + rremove("xlab"), b2_1, common.legend = T,  ncol = 1, nrow = 2, legend = "none")

##########################################
##### COEFFICIENT STANDARD DEVIATION #####
##########################################

extrap.scenario.df <- extrap.scenario.df[!is.na(extrap.scenario.df$mod.type2), ]

b1SD_1 <- extrap.scenario.df %>% 
  ggplot(aes(x = bias.type, y = beta1.sd, fill = bias.type)) +  
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +  # Add a boxplot without outliers
  labs(x = NULL, y = expression(beta[1]~"SD"), color = NULL) +
  scale_fill_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) + 
  scale_color_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) + 
  coord_cartesian(ylim = c(0, 0.6)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "Presence-absence", 
                                      m.PO = "Presence-only")), 
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
  ggtitle('High PO record numbers')

b2SD_1 <- extrap.scenario.df %>% 
  ggplot(aes(x = bias.type, y = beta2.sd, fill = bias.type)) +  
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +  # Add a boxplot without outliers
  labs(x = NULL, y = expression(beta[2]~"SD"), color = NULL) +
  scale_fill_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) + 
  scale_color_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) + 
  coord_cartesian(ylim = c(0, 0.6)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "Presence-absence", 
                                      m.PO = "Presence-only")), 
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
  ggtitle('High PO record numbers')

betaSD_1 <- ggarrange(b1SD_1 + rremove("xlab"), b2SD_1, common.legend = T,  ncol = 1, nrow = 2, legend = "none")


#### INTERCEPTS ####
PO_INT <- extrap.scenario.df %>% 
  filter(!is.na(PO_intercept)) %>% 
  ggplot(aes(x = bias.type, y = PO_intercept, fill = bias.type)) +  
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +  # Boxplot with no outliers
  geom_hline(yintercept = beta0, linetype = "dashed", color = "red") +  # Dashed horizontal line
  labs(x = NULL, y = expression(beta[0]), color = NULL) +
  scale_fill_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) + 
  scale_color_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) + 
  coord_cartesian(ylim = c(-6, 1)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "Presence-absence", 
                                      m.PO = "Presence-only")), 
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
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +  # Boxplot with no outliers
  geom_hline(yintercept = beta0, linetype = "dashed", color = "red") +  # Dashed horizontal line
  labs(x = NULL, y = expression(beta[0]), color = NULL) +
  scale_fill_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) + 
  scale_color_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) + 
  coord_cartesian(ylim = c(-6, 1)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "Presence-absence", 
                                      m.PO = "Presence-only")), 
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


############################################################################
# Scenario 2. ---------------------------------------------------
# Environmental extrap with bias & lower PO record numbers
############################################################################

scenario_name = "2"

load(file = paste0(file.path(outpath, scenario_name), "/Scenario_", scenario_name, "_Input_Params.RData"))

# Load and join replicates together ---------------------------------------


# List all the files with validation dataframes (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_Job", full.names = TRUE, recursive = TRUE) 

true.validation.df.list <- lapply(file_list, read.csv)

true.validation.df_2 <- do.call(rbind, true.validation.df.list)


# THEN list all the files with validation dataframes for Site A (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_SiteA_Job", full.names = TRUE, recursive = TRUE) 

true.validation.SiteA.df.list <- lapply(file_list, read.csv)

true.validation.SiteA.df_2 <- do.call(rbind, true.validation.SiteA.df.list)


# FINALLY list all the files with model outputs (for each job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "Results_Summary", full.names = TRUE, recursive = TRUE) 

# Read all the CSV files and save them to a list
extrap.scenario.df.list <- lapply(file_list, read.csv)

# Combine all dataframes in the list into one dataframe using rbind
extrap.scenario.df_2 <- do.call(rbind, extrap.scenario.df.list)


##########################################################
### REMOVE REPLICATES THAT DON'T HAVE ALL THREE OUTPUTS ###
##########################################################

validation <- true.validation.df_2 %>%
  group_by(job_index) %>% 
  summarise(n = n()) %>% 
  select(job_index)

extrap <- extrap.scenario.df_2 %>%
  group_by(job_index) %>% 
  summarise(n = n()) %>% 
  select(job_index)

validationA <- true.validation.SiteA.df_2 %>%
  group_by(job_index) %>% 
  summarise(n = n()) %>% 
  select(job_index)

common_jobs <- validation %>% 
  inner_join(extrap, by = "job_index") %>% 
  inner_join(validationA, by = "job_index")


#######################################################
## OPTIONAL - TRIM JOBS TO DESIRED REPLICATE NUMBER ##
#######################################################

common_jobs <- common_jobs %>%
  slice(1:replicates)

true.validation.df_2 <- true.validation.df_2 %>%
  filter(job_index %in% common_jobs$job_index)

true.validation.SiteA.df_2 <- true.validation.SiteA.df_2 %>%
  filter(job_index %in% common_jobs$job_index)

extrap.scenario.df_2 <- extrap.scenario.df_2 %>%
  filter(job_index %in% common_jobs$job_index)


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

true.validation.df_2 <- true.validation.df_2 %>%
  mutate(bias.type = ifelse(grepl("bias", mod.type, fixed = T), "With bias covariate", "Without bias covariate")) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int"))) %>% 
  mutate(bias.type = factor(bias.type, levels = c("Without bias covariate", "With bias covariate")))

## NON-COLOURED VERSION
RMSE_2C <- true.validation.df_2 %>% 
  ggplot(aes(x = mean.extrap, y = RMSE.global, color = bias.type)) +
  geom_point(alpha = 0.05, size = 1.2) +
  geom_smooth(method = "loess", se = T, aes(fill = bias.type, color = bias.type), alpha = 0.3) +
  labs(x = x.label, y = "Mean RMSE projection site", fill = "Model Type", color = "Model Type") +
  scale_fill_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) +
  scale_color_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) +
  coord_cartesian(ylim = c(NA, 4.25)) +
  theme_bw() +
  facet_grid(cols = vars(mod.type2), labeller = as_labeller(c(m.int = "Integrated", m.PA = "Presence-absence", m.PO = "Presence-only"))) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  # Increase legend text size
        axis.title = element_text(size = 15),   # Increase axis titles
        axis.text = element_text(size = 12),    # Increase axis text
        strip.text = element_text(size = 15),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +  # Move title to the right
  ggtitle('Low PO record numbers')

# NON-COLOURED VERSION
Int.score_2 <- true.validation.df_2 %>% 
  ggplot(aes(x = mean.extrap, y = Mean.Int.Score, color = bias.type)) +
  geom_point(alpha = 0.05, size = 1.2) +
  geom_smooth(method = "loess", se = T, aes(fill = bias.type, color = bias.type), alpha = 0.3) +
  labs(x = x.label, y = "Mean interval score") +
  scale_fill_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) +
  scale_color_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) +
  coord_cartesian(ylim = c(NA, 3.5)) +
  theme_bw() +
  facet_grid(cols = vars(mod.type2), labeller = as_labeller(c(m.int = "Integrated", m.PA = "Presence-absence", m.PO = "Presence-only"))) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  # Increase legend text size
        axis.title = element_text(size = 15),   # Increase axis titles
        axis.text = element_text(size = 12),    # Increase axis text
        strip.text = element_text(size = 15),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +  # Move title to the right
  ggtitle('Low PO record numbers')


# NON-COLOURED VERSION
CORR_2 <- true.validation.df_2 %>% 
  ggplot(aes(x = mean.extrap, y = correlation, color = bias.type)) +
  geom_point(alpha = 0.05, size = 1.2) +
  geom_smooth(method = "loess", se = T, aes(fill = bias.type, color = bias.type), alpha = 0.3) +
  labs(x = x.label, y = "Correlation") +
  scale_fill_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) +
  scale_color_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) +
  coord_cartesian(ylim = c(0.25, NA)) +
  theme_bw() +
  facet_grid(cols = vars(mod.type2), labeller = as_labeller(c(m.int = "Integrated", m.PA = "Presence-absence", m.PO = "Presence-only"))) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  # Increase legend text size
        axis.title = element_text(size = 15),   # Increase axis titles
        axis.text = element_text(size = 12),    # Increase axis text
        strip.text = element_text(size = 15),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +  # Move title to the right
  ggtitle('Low PO record numbers')

# # COVERAGE RATE
cov.rate_2 <- true.validation.df_2 %>%
  ggplot(aes(x = mean.extrap, y = coverage.rate, color = bias.type)) +
  geom_point(alpha = 0.05, size = 1.2) +
  geom_smooth(method = "loess", se = T, aes(fill = bias.type, color = bias.type), alpha = 0.3) +
  labs(x = x.label, y = "Mean coverage probability") +
  scale_fill_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) +
  scale_color_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) +
  #coord_cartesian(ylim = c(NA, 3.5)) +
  theme_bw() +
  facet_grid(cols = vars(mod.type2), labeller = as_labeller(c(m.int = "Integrated", m.PA = "Presence-absence", m.PO = "Presence-only"))) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  # Increase legend text size
        axis.title = element_text(size = 15),   # Increase axis titles
        axis.text = element_text(size = 12),    # Increase axis text
        strip.text = element_text(size = 15),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +  # Move title to the right
  ggtitle('Low PO record numbers')

############## SITE A ##################

true.validation.SiteA.df_2 <- true.validation.SiteA.df_2 %>%
  mutate(bias.type = ifelse(grepl("bias", mod.type, fixed = T), "With bias covariate", "Without bias covariate")) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int"))) %>% 
  mutate(bias.type = factor(bias.type, levels = c("Without bias covariate", "With bias covariate")))

RMSE_A_2 <- true.validation.SiteA.df_2 %>% 
  ggplot(aes(x = bias.type, y = RMSE.global, fill = bias.type)) +  
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = NULL, y = "Mean RMSE training site", color = NULL) +
  scale_fill_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) + 
  scale_color_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) + 
  coord_cartesian(ylim = c(NA, 4.3)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "Presence-absence", 
                                      m.PO = "Presence-only")), 
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
  ggtitle('Low PO record numbers')

###### COEFFICIENTS #######

extrap.scenario.df_2  <- extrap.scenario.df_2 %>% 
  mutate(bias.type = ifelse(grepl("bias", mod.type, fixed = T), "With bias covariate", "Without bias covariate")) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int"))) %>% 
  mutate(bias.type = factor(bias.type, levels = c("Without bias covariate", "With bias covariate")))

b1_2 <- extrap.scenario.df_2 %>% 
  ggplot(aes(x = bias.type, y = beta1.mean, fill = bias.type)) +  
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +  # Add a boxplot without outliers
  geom_hline(yintercept = beta1, linetype = "dashed", color = "red") +  # Add the horizontal dashed line
  labs(x = NULL, y = expression(beta[1]), color = NULL) +
  scale_fill_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) + 
  scale_color_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) + 
  coord_cartesian(ylim = c(-1, 1)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "Presence-absence", 
                                      m.PO = "Presence-only")), 
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
  ggtitle('Low PO record numbers')


b2_2 <- extrap.scenario.df_2 %>% 
  ggplot(aes(x = bias.type, y = beta2.mean, fill = bias.type)) +  
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +  # Add a boxplot without outliers
  geom_hline(yintercept = beta2, linetype = "dashed", color = "red") +  # Add the horizontal dashed line
  labs(x = NULL, y = expression(beta[2]), color = NULL) +
  scale_fill_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) + 
  scale_color_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) + 
  coord_cartesian(ylim = c(-1, 1)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "Presence-absence", 
                                      m.PO = "Presence-only")), 
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

beta_2 <- ggarrange(b1_2 + rremove("xlab"), b2_2, common.legend = T,  ncol = 1, nrow = 2, legend = "bottom")


##########################################
##### COEFFICIENT STANDARD DEVIATION #####
##########################################

b1SD_2 <- extrap.scenario.df_2 %>% 
  ggplot(aes(x = bias.type, y = beta1.sd, fill = bias.type)) +  
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +  # Add a boxplot without outliers
  labs(x = NULL, y = expression(beta[1]~"SD"), color = NULL) +
  scale_fill_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) + 
  scale_color_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) + 
  coord_cartesian(ylim = c(0, 0.6)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "Presence-absence", 
                                      m.PO = "Presence-only")), 
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
  ggtitle('Low PO record numbers')

b2SD_2 <- extrap.scenario.df_2 %>% 
  ggplot(aes(x = bias.type, y = beta2.sd, fill = bias.type)) +  
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +  # Add a boxplot without outliers
  labs(x = NULL, y = expression(beta[2]~"SD"), color = NULL) +
  scale_fill_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) + 
  scale_color_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) + 
  coord_cartesian(ylim = c(0, 0.6)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "Presence-absence", 
                                      m.PO = "Presence-only")), 
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
  ggtitle('Low PO record numbers')

betaSD_2 <- ggarrange(b1SD_2 + rremove("xlab"), b2SD_2, common.legend = T,  ncol = 1, nrow = 2, legend = "bottom")

#### INTERCEPTS ####
PO_INT_2 <- extrap.scenario.df_2  %>% 
  filter(!is.na(PO_intercept)) %>% 
  ggplot(aes(x = bias.type, y = PO_intercept, fill = bias.type)) +  
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +  # Boxplot with no outliers
  geom_hline(yintercept = beta0, linetype = "dashed", color = "red") +  # Dashed horizontal line
  labs(x = NULL, y = expression(beta[0]), color = NULL) +
  scale_fill_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) + 
  scale_color_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) + 
  coord_cartesian(ylim = c(-6, 1)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "Presence-absence", 
                                      m.PO = "Presence-only")), 
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

PA_INT_2 <- extrap.scenario.df_2  %>% 
  filter(!is.na(PA_intercept)) %>% 
  ggplot(aes(x = bias.type, y = PA_intercept, fill = bias.type)) +  
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA) +  # Boxplot with no outliers
  geom_hline(yintercept = beta0, linetype = "dashed", color = "red") +  # Dashed horizontal line
  labs(x = NULL, y = expression(beta[0]), color = NULL) +
  scale_fill_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) + 
  scale_color_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4")) + 
  coord_cartesian(ylim = c(-6, 1)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "Presence-absence", 
                                      m.PO = "Presence-only")), 
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


intercepts_2 <- ggarrange(PO_INT_2, PA_INT_2, common.legend = T,  ncol = 2, nrow = 1, legend = "bottom")

###########################################
### FIGURES: COMBINED 1 AND 1 C PLOTS ####
##########################################

Fig.3 <- RMSE_2_Fig3

ggsave(plot = Fig.3, filename = paste0(file.path(result_path),"/FIGURE_3.png"), w = 23.5, h = 12.5, units = "cm", dpi = 400, device = "png")

Supp.Fig.1 <- ggarrange(RMSE_2 + rremove("xlab"), RMSE_2C, common.legend = T,  ncol = 1, nrow = 2, legend = "bottom", labels = c("(a)", "(b)"))

ggsave(plot = Supp.Fig.1, filename = paste0(file.path(result_path),"/Supp_FIGURE_1.png"), w = 23.5, h = 20, units = "cm", dpi = 400, device = "png")

Supp.Fig.2 <- ggarrange(Int.score_1 + rremove("xlab"), Int.score_2, common.legend = T,  ncol = 1, nrow = 2, legend = "bottom", labels = c("(a)", "(b)"))

ggsave(plot = Supp.Fig.2, filename = paste0(file.path(result_path),"/Supp_FIGURE_2.png"), w = 23.5, h = 20, units = "cm", dpi = 400, device = "png")

Supp.Fig.3 <- ggarrange(CORR_1 + rremove("xlab"), CORR_2, common.legend = T,  ncol = 1, nrow = 2, legend = "bottom", labels = c("(a)", "(b)"))

ggsave(plot = Supp.Fig.3, filename = paste0(file.path(result_path),"/Supp_FIGURE_3.png"), w = 23.5, h = 20, units = "cm", dpi = 400, device = "png")

Supp.Fig.4 <- ggarrange(cov.rate + rremove("xlab"), cov.rate_2, common.legend = T,  ncol = 1, nrow = 2, legend = "bottom", labels = c("(a)", "(b)"))

ggsave(plot = Supp.Fig.4, filename = paste0(file.path(result_path),"/Supp_FIGURE_4.png"), w = 23.5, h = 20, units = "cm", dpi = 400, device = "png")

## Training site

Supp.Fig.5 <- ggarrange(RMSE_A_1 + rremove("xlab"), RMSE_A_2, common.legend = T,  ncol = 1, nrow = 2, legend = "bottom", labels = c("(a)", "(b)"))

ggsave(plot = Supp.Fig.5, filename = paste0(file.path(result_path),"/Supp_FIGURE_5.png"), w = 23.5, h = 20, units = "cm", dpi = 400, device = "png")


##### COEFFICIENT PLOT combined 1 and 1 C

Supp.Fig.6 <- ggarrange(beta_1 + rremove("xlab"), beta_2, common.legend = T,  ncol = 1, nrow = 2, legend = "bottom", labels = c("(a)", "(b)"))

ggsave(plot = Supp.Fig.6, filename = paste0(file.path(result_path),"/Supp_FIGURE_6.png"), w = 20, h = 30, units = "cm", dpi = 400, device = "png")


##### COEFFICIENT SD PLOT combined 1 and 1 C

Supp.Fig.7 <- ggarrange(betaSD_1 + rremove("xlab"), betaSD_2, common.legend = T,  ncol = 1, nrow = 2, legend = "bottom", labels = c("(a)", "(b)"))

ggsave(plot = Supp.Fig.7, filename = paste0(file.path(result_path),"/Supp_FIGURE_7.png"), w = 20, h = 30, units = "cm", dpi = 400, device = "png")

###### INTERCEPTS #######

Supp.Fig.8 <- ggarrange(intercepts + rremove("xlab"), intercepts_2, common.legend = T,  ncol = 1, nrow = 2, legend = "bottom", labels = c("(a)", "(b)"))

ggsave(plot = Supp.Fig.8, filename = paste0(file.path(result_path),"/Supp_FIGURE_8.png"), w = 23, h = 17, units = "cm", dpi = 400, device = "png")


######### REALISED ENVIRO. EXTRAPOLATION ###########

#### Realised extrap per mod type ###


true.validation.df$extrap.type <- factor(true.validation.df$extrap.type, levels = c("Low", "Moderate", "High"))

R2a <- true.validation.df %>% 
  filter(mod.type %in% c("m.int", "m.PA", "m.PO")) %>% 
  mutate(realised.extrap = ifelse(mod.type == "m.int", meanPAPO.extrap, 
                                  ifelse(mod.type == "m.PA", meanPA.extrap, meanPO.extrap))) %>%
  mutate(mod.type = factor(mod.type, levels = c("m.PO", "m.PA", "m.int"))) %>% 
  ggplot(aes(x = extrap.type, y = realised.extrap)) +
  geom_boxplot(width = 0.25, outlier.shape = NA, fill = "#0072B2") +  # Boxplot with no outliers
  labs(y = "Realised enviro. dissimilarity", x = "Projection site to training site environmental dissimilarity") +
  coord_cartesian(ylim = c(NA, 400)) +
  theme_bw() +
  facet_wrap(~mod.type, labeller = as_labeller(c(m.int = "Integrated", m.PA = "Presence-absence", m.PO = "Presence-only")), scales = "free_x") +  # Wrap facets and use free_x to adjust x-axis
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  # Increase legend text size
        # axis.title.x = element_blank(),         # Remove x-axis title
        # axis.text.x = element_blank(),          # Remove x-axis text
        # axis.ticks.x = element_blank(),         # Remove x-axis ticks
        panel.grid.major.x = element_blank(),   # Remove major grid lines
        panel.grid.minor.x = element_blank(),   # Remove minor grid lines
        axis.title.y = element_text(size = 15), # Keep y-axis title
        axis.title.x = element_text(size = 15), # Keep y-axis title
        axis.text = element_text(size = 12),    # Increase axis text
        strip.text = element_text(size = 15),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +  # Move title to the right)
  ggtitle('High PO record numbers')

true.validation.df_2$extrap.type <- factor(true.validation.df_2$extrap.type, levels = c("Low", "Moderate", "High"))

R2Ca <- true.validation.df_2 %>% 
  filter(mod.type %in% c("m.int", "m.PA", "m.PO")) %>% 
  mutate(realised.extrap = ifelse(mod.type == "m.int", meanPAPO.extrap, 
                                  ifelse(mod.type == "m.PA", meanPA.extrap, meanPO.extrap))) %>%
  mutate(mod.type = factor(mod.type, levels = c("m.PO", "m.PA", "m.int"))) %>% 
  ggplot(aes(x = extrap.type, y = realised.extrap)) +
  geom_boxplot(width = 0.25, outlier.shape = NA, fill = "#0072B2") +  # Boxplot with no outliers
  labs(y = "Realised enviro. dissimilarity", x = "Projection site to training site environmental dissimilarity") +
  coord_cartesian(ylim = c(NA, 400)) +
  theme_bw() +
  facet_wrap(~mod.type, labeller = as_labeller(c(m.int = "Integrated", m.PA = "Presence-absence", m.PO = "Presence-only")), scales = "free_x") +  # Wrap facets and use free_x to adjust x-axis
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),  # Increase legend text size
        # axis.title.x = element_blank(),         # Remove x-axis title
        # axis.text.x = element_blank(),          # Remove x-axis text
        # axis.ticks.x = element_blank(),         # Remove x-axis ticks
        panel.grid.major.x = element_blank(),   # Remove major grid lines
        panel.grid.minor.x = element_blank(),   # Remove minor grid lines
        axis.title.y = element_text(size = 15), # Keep y-axis title
        axis.title.x = element_text(size = 15), # Keep y-axis title
        axis.text = element_text(size = 12),    # Increase axis text
        strip.text = element_text(size = 15),   # Increase facet title size
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +  # Move title to the right)
  ggtitle('High PO record numbers')


#########################################################################

Supp.Fig.9 <- ggarrange(R2a + rremove("xlab"), R2Ca, common.legend = T,  ncol = 1, nrow = 2, legend = "bottom", labels = c("(a)", "(b)"))

ggsave(plot = Supp.Fig.9, filename = paste0(file.path(result_path),"/Supp_FIGURE_9.png"), w = 23.5, h = 20, units = "cm", dpi = 400, device = "png")


