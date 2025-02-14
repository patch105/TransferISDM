
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


#####################################################
####### CHOOSE NUMBER OF REPLICATES TO KEEP ########
#####################################################

replicates <- 300

############################################################################
# Figure 5. Scenario 6. ---------------------------------------------------
# 
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

true.validation.df <- true.validation.df %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF")) %>% 
  mutate(bias.type2 = ifelse(grepl("bias", mod.type, fixed = T), "With bias cov", "Without bias cov")) %>%
  mutate(bias.type2 = factor(bias.type2, levels = c("Without bias cov", "With bias cov"))) %>%
  mutate(bias.type = factor(bias.type, levels = c("Without GRF", "With GRF"))) %>%
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type2, fixed = T), gsub(".bias", "", mod.type2), mod.type2)) %>% 
  mutate(mod.type3 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type3 = factor(mod.type3, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))

true.validation.SiteA.df <- true.validation.SiteA.df %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF")) %>% 
  mutate(bias.type2 = ifelse(grepl("bias", mod.type, fixed = T), "With bias cov", "Without bias cov")) %>%
  mutate(bias.type2 = factor(bias.type2, levels = c("Without bias cov", "With bias cov"))) %>%
  mutate(bias.type = factor(bias.type, levels = c("Without GRF", "With GRF"))) %>%
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type2, fixed = T), gsub(".bias", "", mod.type2), mod.type2)) %>% 
  mutate(mod.type3 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type3 = factor(mod.type3, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))

extrap.scenario.df <- extrap.scenario.df %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF")) %>% 
  mutate(bias.type2 = ifelse(grepl("bias", mod.type, fixed = T), "With bias cov", "Without bias cov")) %>%
  mutate(bias.type2 = factor(bias.type2, levels = c("Without bias cov", "With bias cov"))) %>%
  mutate(bias.type = factor(bias.type, levels = c("Without GRF", "With GRF"))) %>%
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type2, fixed = T), gsub(".bias", "", mod.type2), mod.type2)) %>% 
  mutate(mod.type3 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type3 = factor(mod.type3, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))



############################################################################
# Figure 5. Scenario 6C. ---------------------------------------------------
# 
############################################################################

# Set a new replicate number (due to HPC, only ran 280)

replicates <- 300

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


##########################################################
### REMOVE REPLICATES THAT DON'T HAVE ALL THREE OUTPUTS ###
##########################################################

validation <- true.validation.df_6C %>%
  group_by(job_index) %>% 
  summarise(n = n()) %>% 
  select(job_index)

extrap <- extrap.scenario.df_6C %>%
  group_by(job_index) %>% 
  summarise(n = n()) %>% 
  select(job_index)

validationA <- true.validation.SiteA.df_6C %>%
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

true.validation.df_6C <- true.validation.df_6C %>%
  filter(job_index %in% common_jobs$job_index)

true.validation.SiteA.df_6C <- true.validation.SiteA.df_6C %>%
  filter(job_index %in% common_jobs$job_index)

extrap.scenario.df_6C <- extrap.scenario.df_6C %>%
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

########################################################################
################### ADD EXTRAP REPS FROM ANOTHER RUN #################
########################################################################


true.validation.df_6C <- true.validation.df_6C %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF")) %>% 
  mutate(bias.type2 = ifelse(grepl("bias", mod.type, fixed = T), "With bias cov", "Without bias cov")) %>%
  mutate(bias.type2 = factor(bias.type2, levels = c("Without bias cov", "With bias cov"))) %>%
  mutate(bias.type = factor(bias.type, levels = c("Without GRF", "With GRF"))) %>%
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type2, fixed = T), gsub(".bias", "", mod.type2), mod.type2)) %>% 
  mutate(mod.type3 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type3 = factor(mod.type3, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))

true.validation.SiteA.df_6C <- true.validation.SiteA.df_6C %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF")) %>% 
  mutate(bias.type2 = ifelse(grepl("bias", mod.type, fixed = T), "With bias cov", "Without bias cov")) %>%
  mutate(bias.type2 = factor(bias.type2, levels = c("Without bias cov", "With bias cov"))) %>%
  mutate(bias.type = factor(bias.type, levels = c("Without GRF", "With GRF"))) %>%
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type2, fixed = T), gsub(".bias", "", mod.type2), mod.type2)) %>% 
  mutate(mod.type3 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type3 = factor(mod.type3, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))

extrap.scenario.df_6C <- extrap.scenario.df_6C %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF")) %>% 
  mutate(bias.type2 = ifelse(grepl("bias", mod.type, fixed = T), "With bias cov", "Without bias cov")) %>%
  mutate(bias.type2 = factor(bias.type2, levels = c("Without bias cov", "With bias cov"))) %>%
  mutate(bias.type = factor(bias.type, levels = c("Without GRF", "With GRF"))) %>%
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type2, fixed = T), gsub(".bias", "", mod.type2), mod.type2)) %>% 
  mutate(mod.type3 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type3 = factor(mod.type3, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))

#########################################################
############### PLOTTING TOGETHER ######################
#########################################################


true.validation.df %>% 
  ggplot(aes(x = mean.extrap, y = RMSE.global, color = mod.type3, linetype = bias.type2)) +
  geom_point(alpha = 0.05, size = 1.2) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type3, color = mod.type3), alpha = 0.3) +
  labs(x = x.label, y = "RMSE") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_linetype_manual(values = c("Without bias cov" = "solid", "With bias cov" = "dotted")) +  # Different line types for bias types
  coord_cartesian(ylim = c(NA, 5)) +
  theme_bw() +
  facet_wrap(~ mod.type3, 
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
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) 


true.validation.df %>% 
  filter(bias.type2 == "With bias cov" | mod.type2 == "m.PA") %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int"))) %>%
  ggplot(aes(x = mean.extrap, y = RMSE.global, color = bias.type)) +
  geom_point(alpha = 0.05, size = 1.2) +
  geom_smooth(method = "loess", se = T, aes(fill = bias.type, color = bias.type), alpha = 0.3) +
  labs(x = x.label, y = "RMSE") +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  coord_cartesian(ylim = c(NA,2)) +
  theme_bw() +
  facet_wrap(~mod.type2,
             labeller = as_labeller(c(m.int = "Integrated",
                                      m.PA = "Presence-absence",
                                      m.PO = "Presence-only"))) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 15),
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) 


projection1 <- true.validation.df %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int"))) %>%
  ggplot(aes(x = mean.extrap, y = RMSE.global, color = bias.type, linetype = bias.type2)) +
  geom_point(alpha = 0.02, size = 1.2) +
  geom_smooth(method = "loess", se = T, aes(fill = bias.type, color = bias.type), alpha = 0.3) +
  labs(x = x.label, y = "Mean RMSE projection site") +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_linetype_manual(values = c("Without bias cov" = "dotted", "With bias cov" = "solid")) +  # Different line types for bias types
  coord_cartesian(ylim = c(NA,4)) +
  theme_bw() +
  facet_wrap(~mod.type2,
             labeller = as_labeller(c(m.int = "Integrated",
                                      m.PA = "Presence-absence",
                                      m.PO = "Presence-only"))) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 13.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(size = 14, vjust = 1.7),
        axis.title.x = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +
  guides(color = guide_legend(order = 1), 
         fill = guide_legend(order = 1), 
         linetype = guide_legend(order = 2))


projection2 <- true.validation.df %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int"))) %>%
  ggplot(aes(x = mean.extrap, y = Mean.Int.Score, color = bias.type, linetype = bias.type2)) +
  geom_point(alpha = 0.02, size = 1.2) +
  geom_smooth(method = "loess", se = T, aes(fill = bias.type, color = bias.type), alpha = 0.3) +
  labs(x = x.label, y = "Mean Interval Score") +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_linetype_manual(values = c("Without bias cov" = "dotted", "With bias cov" = "solid")) +  # Different line types for bias types
  coord_cartesian(ylim = c(NA, 2.3)) +
  theme_bw() +
  facet_wrap(~mod.type2,
             labeller = as_labeller(c(m.int = "Integrated",
                                      m.PA = "Presence-absence",
                                      m.PO = "Presence-only"))) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 13.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(size = 14, vjust = 1.7),
        axis.title.x = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +
  guides(color = guide_legend(order = 1), 
         fill = guide_legend(order = 1), 
         linetype = guide_legend(order = 2))


projection3 <- true.validation.df %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int"))) %>%
  ggplot(aes(x = mean.extrap, y = correlation, color = bias.type, linetype = bias.type2)) +
  geom_point(alpha = 0.02, size = 1.2) +
  geom_smooth(method = "loess", se = T, aes(fill = bias.type, color = bias.type), alpha = 0.3) +
  labs(x = x.label, y = "Correlation") +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_linetype_manual(values = c("Without bias cov" = "dotted", "With bias cov" = "solid")) +  # Different line types for bias types
  coord_cartesian(ylim = c(-0.5, NA)) +
  theme_bw() +
  facet_wrap(~mod.type2,
             labeller = as_labeller(c(m.int = "Integrated",
                                      m.PA = "Presence-absence",
                                      m.PO = "Presence-only"))) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 13.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(size = 14, vjust = 1.7),
        axis.title.x = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +
  guides(color = guide_legend(order = 1), 
         fill = guide_legend(order = 1), 
         linetype = guide_legend(order = 2))


projection4 <- true.validation.df %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int"))) %>%
  ggplot(aes(x = mean.extrap, y = coverage.rate, color = bias.type, linetype = bias.type2)) +
  geom_point(alpha = 0.02, size = 1.2) +
  geom_smooth(method = "loess", se = T, aes(fill = bias.type, color = bias.type), alpha = 0.3) +
  labs(x = x.label, y = "Mean coverage probability") +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_linetype_manual(values = c("Without bias cov" = "dotted", "With bias cov" = "solid")) +  # Different line types for bias types
  coord_cartesian(ylim = c(0, NA)) +
  theme_bw() +
  facet_wrap(~mod.type2,
             labeller = as_labeller(c(m.int = "Integrated",
                                      m.PA = "Presence-absence",
                                      m.PO = "Presence-only"))) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 13.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(size = 14, vjust = 1.7),
        axis.title.x = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +
  guides(color = guide_legend(order = 1), 
         fill = guide_legend(order = 1), 
         linetype = guide_legend(order = 2))


fig2a <- ggarrange(projection1 + rremove("xlab"), projection4, common.legend = TRUE, ncol = 1, nrow = 2, legend = "bottom", labels = c("(a)", "(b)"), align = "v")

ggsave(plot = fig2a, filename = paste0(file.path(result_path),"/Scenario_6_tests/projection_site.png"), w = 21, h = 18, units = "cm", dpi = 400, device = "png")

fig2b <- ggarrange(projection1 + rremove("xlab"), projection2, common.legend = TRUE, ncol = 1, nrow = 2, legend = "bottom", labels = c("(a)", "(b)"), align = "v")

ggsave(plot = fig2b, filename = paste0(file.path(result_path),"/Scenario_6_tests/projection_site2.png"), w = 21, h = 18, units = "cm", dpi = 400, device = "png")


##########################################################################

############### GRF RECOVERY ###############

true.validation.SiteA.df %>% 
  filter(bias.type == "With GRF") %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int"))) %>%
  ggplot(aes(x = bias.type2, y = cor.GRF, fill = bias.type2)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, aes(fill = bias.type2)) +      # Add a boxplot without outliers
  labs(x = x.label, y = "Correlation random") +
  scale_fill_manual(values = c("Without bias cov" = "green4", "With bias cov" = "purple4")) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_bw() +
  facet_wrap(~mod.type2,
             labeller = as_labeller(c(m.int = "Integrated",
                                      m.PA = "Presence-absence",
                                      m.PO = "Presence-only")),
             scales = "free_x") +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 13.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(size = 14, vjust = 1.7),
        axis.title.x = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +
  guides(color = guide_legend(order = 1), 
         fill = guide_legend(order = 1), 
         linetype = guide_legend(order = 2))


GRF <- true.validation.SiteA.df %>% 
  filter(bias.type == "With GRF") %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int"))) %>%
  ggplot(aes(x = bias.type2, y = cor.GRF, fill = bias.type2)) +  
  geom_boxplot(alpha = 0.6, width = 0.25, outlier.shape = NA, aes(fill = bias.type2)) +      # Add a boxplot without outliers
  labs(x = NULL, y = "Correlation random", color = NULL) +
  scale_fill_manual(values = c("Without bias cov" = "green4", "With bias cov" = "purple4")) + 
  scale_color_manual(values = c("Without bias cov" = "green4", "With bias cov" = "purple4")) + 
  coord_cartesian(ylim = c(0, NA)) +
  theme_bw() +
  facet_wrap(~mod.type2, 
             labeller = as_labeller(c(m.int = "Integrated", 
                                      m.PA = "Presence-absence", 
                                      m.PO = "Presence-only")), 
             scales = "free_x") +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 13.5),  
        axis.title.x = element_blank(),         
        axis.text.x = element_blank(),          
        axis.ticks.x = element_blank(),         
        panel.grid.major.x = element_blank(),   
        panel.grid.minor.x = element_blank(),   
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 12),    
        strip.text = element_text(size = 14),   
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic"))

figx <- ggarrange(fig2a, GRF, common.legend = FALSE, ncol = 1, nrow = 2, labels = c("", "", "(c)"), align = "v", heights = c(2, 1))

ggsave(plot = figx, filename = paste0(file.path(result_path),"/Scenario_6_tests/projection_site_w_GRF.png"), w = 21, h = 27, units = "cm", dpi = 400, device = "png")



extrap.scenario.df %>% 
  filter(!is.na(PA_intercept)) %>% 
  ggplot(aes(x = bias.type, y = PA_intercept, fill = bias.type, linetype = bias.type2)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = NULL, y = "beta1", color = NULL) +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  geom_hline(yintercept = beta0, linetype = "dashed", color = "red") +
  scale_linetype_manual(values = c("Without bias cov" = "dotted", "With bias cov" = "solid")) +  # Different line types for bias types
  #coord_cartesian(ylim = c(-1, 1)) +
  theme_bw() +
  facet_wrap(~mod.type2,
             labeller = as_labeller(c(m.int = "Integrated",
                                      m.PA = "Presence-absence",
                                      m.PO = "Presence-only")),
             scales = "free_x") +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 13.5),  
        axis.title.x = element_blank(),         
        axis.text.x = element_blank(),          
        axis.ticks.x = element_blank(),         
        panel.grid.major.x = element_blank(),   
        panel.grid.minor.x = element_blank(),   
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 12),    
        strip.text = element_text(size = 14),   
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +
  guides(color = guide_legend(order = 1), 
         fill = guide_legend(order = 1), 
         linetype = guide_legend(order = 2))



