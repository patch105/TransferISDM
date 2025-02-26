
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


#############################

# Scenario 1 (env extrap) -------------------------------------------------

############################

####### CHOOSE NUMBER OF REPLICATES TO KEEP ########


replicates <- 300


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


#############################

# Scenario 1C (env extrap) -------------------------------------------------

############################


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


##########################################################
### REMOVE REPLICATES THAT DON'T HAVE ALL THREE OUTPUTS ###
##########################################################

validation <- true.validation.df_1C %>%
  group_by(job_index) %>% 
  summarise(n = n()) %>% 
  select(job_index)

extrap <- extrap.scenario.df_1C %>%
  group_by(job_index) %>% 
  summarise(n = n()) %>% 
  select(job_index)

validationA <- true.validation.SiteA.df_1C %>%
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

true.validation.df_1C <- true.validation.df_1C %>%
  filter(job_index %in% common_jobs$job_index)

true.validation.SiteA.df_1C <- true.validation.SiteA.df_1C %>%
  filter(job_index %in% common_jobs$job_index)

extrap.scenario.df_1C <- extrap.scenario.df_1C %>%
  filter(job_index %in% common_jobs$job_index)



######################################################################
################ SCENARIO 5 - SPATIAL AUTOCORRELATION ################
######################################################################

#####################################################
####### CHOOSE NUMBER OF REPLICATES TO KEEP ########
#####################################################

replicates <- 100

######################################################################
################ SCENARIO 5 - SPATIAL AUTOCORRELATION ################
######################################################################

scenario_name = "5"

load(file = paste0(file.path(outpath, scenario_name), "/Scenario_", scenario_name, "_Input_Params.RData"))

# Load and join replicates together ---------------------------------------


# List all the files with validation dataframes (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_Job", full.names = TRUE, recursive = TRUE) 

true.validation.df.list <- lapply(file_list, read.csv)

true.validation.df_5 <- do.call(rbind, true.validation.df.list)


# THEN list all the files with validation dataframes for Site A (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_SiteA_Job", full.names = TRUE, recursive = TRUE) 

true.validation.SiteA.df.list <- lapply(file_list, read.csv)

true.validation.SiteA.df_5 <- do.call(rbind, true.validation.SiteA.df.list)


# FINALLY list all the files with model outputs (for each job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "Results_Summary", full.names = TRUE, recursive = TRUE) 

# Read all the CSV files and save them to a list
extrap.scenario.df.list <- lapply(file_list, read.csv)

# Combine all dataframes in the list into one dataframe using rbind
extrap.scenario.df_5 <- do.call(rbind, extrap.scenario.df.list)

##########################################################
### REMOVE REPLICATES THAT DON'T HAVE ALL THREE OUTPUTS ###
##########################################################

validation <- true.validation.df_5 %>%
  group_by(job_index) %>% 
  summarise(n = n()) %>% 
  select(job_index)

extrap <- extrap.scenario.df_5 %>%
  group_by(job_index) %>% 
  summarise(n = n()) %>% 
  select(job_index)

validationA <- true.validation.SiteA.df_5 %>%
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

true.validation.df_5 <- true.validation.df_5 %>%
  filter(job_index %in% common_jobs$job_index)

true.validation.SiteA.df_5 <- true.validation.SiteA.df_5 %>%
  filter(job_index %in% common_jobs$job_index)

extrap.scenario.df_5 <- extrap.scenario.df_5 %>%
  filter(job_index %in% common_jobs$job_index)

######################################################################
################ SCENARIO 5C - SPATIAL AUTOCORRELATION C ################
######################################################################

scenario_name = "5C"

load(file = paste0(file.path(outpath, scenario_name), "/Scenario_", scenario_name, "_Input_Params.RData"))

# Load and join replicates together ---------------------------------------


# List all the files with validation dataframes (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_Job", full.names = TRUE, recursive = TRUE) 

true.validation.df.list <- lapply(file_list, read.csv)

true.validation.df_5C <- do.call(rbind, true.validation.df.list)


# THEN list all the files with validation dataframes for Site A (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_SiteA_Job", full.names = TRUE, recursive = TRUE) 

true.validation.SiteA.df.list <- lapply(file_list, read.csv)

true.validation.SiteA.df_5C <- do.call(rbind, true.validation.SiteA.df.list)


# FINALLY list all the files with model outputs (for each job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "Results_Summary", full.names = TRUE, recursive = TRUE) 

# Read all the CSV files and save them to a list
extrap.scenario.df.list <- lapply(file_list, read.csv)

# Combine all dataframes in the list into one dataframe using rbind
extrap.scenario.df_5C <- do.call(rbind, extrap.scenario.df.list)

##########################################################
### REMOVE REPLICATES THAT DON'T HAVE ALL THREE OUTPUTS ###
##########################################################

validation <- true.validation.df_5C %>%
  group_by(job_index) %>% 
  summarise(n = n()) %>% 
  select(job_index)

extrap <- extrap.scenario.df_5C %>%
  group_by(job_index) %>% 
  summarise(n = n()) %>% 
  select(job_index)

validationA <- true.validation.SiteA.df_5C %>%
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

true.validation.df_5C <- true.validation.df_5C %>%
  filter(job_index %in% common_jobs$job_index)

true.validation.SiteA.df_5C <- true.validation.SiteA.df_5C %>%
  filter(job_index %in% common_jobs$job_index)

extrap.scenario.df_5C <- extrap.scenario.df_5C %>%
  filter(job_index %in% common_jobs$job_index)

######################################################################
################ SCENARIO 5v0.2 - SPATIAL AUTOCORRELATION ################
######################################################################

scenario_name = "5v0.2"

load(file = paste0(file.path(outpath, scenario_name), "/Scenario_", scenario_name, "_Input_Params.RData"))

# Load and join replicates together ---------------------------------------


# List all the files with validation dataframes (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_Job", full.names = TRUE, recursive = TRUE) 

true.validation.df.list <- lapply(file_list, read.csv)

true.validation.dfv0.2 <- do.call(rbind, true.validation.df.list)


# THEN list all the files with validation dataframes for Site A (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_SiteA_Job", full.names = TRUE, recursive = TRUE) 

true.validation.SiteA.df.list <- lapply(file_list, read.csv)

true.validation.SiteA.dfv0.2 <- do.call(rbind, true.validation.SiteA.df.list)


# FINALLY list all the files with model outputs (for each job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "Results_Summary", full.names = TRUE, recursive = TRUE) 

# Read all the CSV files and save them to a list
extrap.scenario.df.list <- lapply(file_list, read.csv)

# Combine all dataframes in the list into one dataframe using rbind
extrap.scenario.dfv0.2 <- do.call(rbind, extrap.scenario.df.list)

##########################################################
### REMOVE REPLICATES THAT DON'T HAVE ALL THREE OUTPUTS ###
##########################################################

validation <- true.validation.dfv0.2 %>%
  group_by(job_index) %>% 
  summarise(n = n()) %>% 
  select(job_index)

extrap <- extrap.scenario.dfv0.2 %>%
  group_by(job_index) %>% 
  summarise(n = n()) %>% 
  select(job_index)

validationA <- true.validation.SiteA.dfv0.2 %>%
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

true.validation.dfv0.2 <- true.validation.dfv0.2 %>%
  filter(job_index %in% common_jobs$job_index)

true.validation.SiteA.dfv0.2 <- true.validation.SiteA.dfv0.2 %>%
  filter(job_index %in% common_jobs$job_index)

extrap.scenario.dfv0.2 <- extrap.scenario.dfv0.2 %>%
  filter(job_index %in% common_jobs$job_index)

######################################################################
################ SCENARIO 5Cv0.2 - SPATIAL AUTOCORRELATION C ################
######################################################################

scenario_name = "5Cv0.2"

load(file = paste0(file.path(outpath, scenario_name), "/Scenario_", scenario_name, "_Input_Params.RData"))

# Load and join replicates together ---------------------------------------


# List all the files with validation dataframes (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_Job", full.names = TRUE, recursive = TRUE) 

true.validation.df.list <- lapply(file_list, read.csv)

true.validation.df_5Cv0.2 <- do.call(rbind, true.validation.df.list)


# THEN list all the files with validation dataframes for Site A (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_SiteA_Job", full.names = TRUE, recursive = TRUE) 

true.validation.SiteA.df.list <- lapply(file_list, read.csv)

true.validation.SiteA.df_5Cv0.2 <- do.call(rbind, true.validation.SiteA.df.list)


# FINALLY list all the files with model outputs (for each job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "Results_Summary", full.names = TRUE, recursive = TRUE) 

# Read all the CSV files and save them to a list
extrap.scenario.df.list <- lapply(file_list, read.csv)

# Combine all dataframes in the list into one dataframe using rbind
extrap.scenario.df_5Cv0.2 <- do.call(rbind, extrap.scenario.df.list)

##########################################################
### REMOVE REPLICATES THAT DON'T HAVE ALL THREE OUTPUTS ###
##########################################################

validation <- true.validation.df_5Cv0.2 %>%
  group_by(job_index) %>% 
  summarise(n = n()) %>% 
  select(job_index)

extrap <- extrap.scenario.df_5Cv0.2 %>%
  group_by(job_index) %>% 
  summarise(n = n()) %>% 
  select(job_index)

validationA <- true.validation.SiteA.df_5Cv0.2 %>%
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

true.validation.df_5Cv0.2 <- true.validation.df_5Cv0.2 %>%
  filter(job_index %in% common_jobs$job_index)

true.validation.SiteA.df_5Cv0.2 <- true.validation.SiteA.df_5Cv0.2 %>%
  filter(job_index %in% common_jobs$job_index)

extrap.scenario.df_5Cv0.2 <- extrap.scenario.df_5Cv0.2 %>%
  filter(job_index %in% common_jobs$job_index)

######################################################################
################ SCENARIO 5v5 - SPATIAL AUTOCORRELATION ################
######################################################################

scenario_name = "5v5"

load(file = paste0(file.path(outpath, scenario_name), "/Scenario_", scenario_name, "_Input_Params.RData"))

# Load and join replicates together ---------------------------------------


# List all the files with validation dataframes (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_Job", full.names = TRUE, recursive = TRUE) 

true.validation.df.list <- lapply(file_list, read.csv)

true.validation.dfv5 <- do.call(rbind, true.validation.df.list)


# THEN list all the files with validation dataframes for Site A (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_SiteA_Job", full.names = TRUE, recursive = TRUE) 

true.validation.SiteA.df.list <- lapply(file_list, read.csv)

true.validation.SiteA.dfv5 <- do.call(rbind, true.validation.SiteA.df.list)


# FINALLY list all the files with model outputs (for each job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "Results_Summary", full.names = TRUE, recursive = TRUE) 

# Read all the CSV files and save them to a list
extrap.scenario.df.list <- lapply(file_list, read.csv)

# Combine all dataframes in the list into one dataframe using rbind
extrap.scenario.dfv5 <- do.call(rbind, extrap.scenario.df.list)


##########################################################
### REMOVE REPLICATES THAT DON'T HAVE ALL THREE OUTPUTS ###
##########################################################

validation <- true.validation.dfv5 %>%
  group_by(job_index) %>% 
  summarise(n = n()) %>% 
  select(job_index)

extrap <- extrap.scenario.dfv5 %>%
  group_by(job_index) %>% 
  summarise(n = n()) %>% 
  select(job_index)

validationA <- true.validation.SiteA.dfv5 %>%
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

true.validation.dfv5 <- true.validation.dfv5 %>%
  filter(job_index %in% common_jobs$job_index)

true.validation.SiteA.dfv5 <- true.validation.SiteA.dfv5 %>%
  filter(job_index %in% common_jobs$job_index)

extrap.scenario.dfv5 <- extrap.scenario.dfv5 %>%
  filter(job_index %in% common_jobs$job_index)

######################################################################
################ SCENARIO 5Cv5 - SPATIAL AUTOCORRELATION C ################
######################################################################

scenario_name = "5Cv5"

load(file = paste0(file.path(outpath, scenario_name), "/Scenario_", scenario_name, "_Input_Params.RData"))

# Load and join replicates together ---------------------------------------


# List all the files with validation dataframes (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_Job", full.names = TRUE, recursive = TRUE) 

true.validation.df.list <- lapply(file_list, read.csv)

true.validation.df_5Cv5 <- do.call(rbind, true.validation.df.list)


# THEN list all the files with validation dataframes for Site A (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_SiteA_Job", full.names = TRUE, recursive = TRUE) 

true.validation.SiteA.df.list <- lapply(file_list, read.csv)

true.validation.SiteA.df_5Cv5 <- do.call(rbind, true.validation.SiteA.df.list)


# FINALLY list all the files with model outputs (for each job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "Results_Summary", full.names = TRUE, recursive = TRUE) 

# Read all the CSV files and save them to a list
extrap.scenario.df.list <- lapply(file_list, read.csv)

# Combine all dataframes in the list into one dataframe using rbind
extrap.scenario.df_5Cv5 <- do.call(rbind, extrap.scenario.df.list)


##########################################################
### REMOVE REPLICATES THAT DON'T HAVE ALL THREE OUTPUTS ###
##########################################################

validation <- true.validation.df_5Cv5 %>%
  group_by(job_index) %>% 
  summarise(n = n()) %>% 
  select(job_index)

extrap <- extrap.scenario.df_5Cv5 %>%
  group_by(job_index) %>% 
  summarise(n = n()) %>% 
  select(job_index)

validationA <- true.validation.SiteA.df_5Cv5 %>%
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

true.validation.df_5Cv5 <- true.validation.df_5Cv5 %>%
  filter(job_index %in% common_jobs$job_index)

true.validation.SiteA.df_5Cv5 <- true.validation.SiteA.df_5Cv5 %>%
  filter(job_index %in% common_jobs$job_index)

extrap.scenario.df_5Cv5 <- extrap.scenario.df_5Cv5 %>%
  filter(job_index %in% common_jobs$job_index)




######################################################################

# PLOTTING ALL  -----------------------------------------------------------

######################################################################

############################
## (Fig. 3a) PROJECTION SITE  
############################

true.validation.df <- true.validation.df %>%
  mutate(scenario = "1")

true.validation.df_1C <- true.validation.df_1C %>%
  mutate(scenario = "1C")

true.validation.df_5 <- true.validation.df_5 %>%
  mutate(scenario = "5")

true.validation.df_5C <- true.validation.df_5C %>%
  mutate(scenario = "5C")

true.validation.dfv0.2 <- true.validation.dfv0.2 %>%
  mutate(scenario = "5v0.2")

true.validation.df_5Cv0.2 <- true.validation.df_5Cv0.2 %>%
  mutate(scenario = "5Cv0.2")

true.validation.dfv5 <- true.validation.dfv5 %>%
  mutate(scenario = "5v5")

true.validation.df_5Cv5 <- true.validation.df_5Cv5 %>%
  mutate(scenario = "5Cv5")

projection.df <- rbind(true.validation.df, true.validation.df_1C, true.validation.df_5, true.validation.df_5C, true.validation.dfv0.2, true.validation.df_5Cv0.2, true.validation.dfv5, true.validation.df_5Cv5)


# Create a new column with the model type
projection.df <- projection.df %>% 
  mutate(mod.type2 = case_when(
    grepl("PO", mod.type) ~ "m.PO",
    grepl("PA", mod.type) ~ "m.PA",
    grepl("int", mod.type) ~ "m.int"
  ))


projection.df$mod.type2 <- factor(projection.df$mod.type2, levels = c("m.PO", "m.PA", "m.int"))

df <- projection.df

# First trim half model outputs for PO and Integrated so they're even
projection.df <- rbind(
  df[df$mod.type2 == "m.PA", ],  # Keep all m.PA rows
  df[df$mod.type2 == "m.int", ][sample(nrow(df[df$mod.type2 == "m.int", ]), 
                                       size = nrow(df[df$mod.type2 == "m.int", ])/2), ],
  df[df$mod.type2 == "m.PO", ][sample(nrow(df[df$mod.type2 == "m.PO", ]), 
                                      size = nrow(df[df$mod.type2 == "m.PO", ])/2), ]
)


projection.plot <- projection.df %>%
  ggplot(aes(x = mod.type2, y = RMSE.global)) +
  geom_point(position = position_jitter(width = 0.15, height = 0), alpha = 0.01, size = 0.25, color =  "maroon", show.legend = F) +      # Add a boxplot without outliers
  geom_boxplot(alpha = 0.6, outlier.shape = NA, fill = "maroon", width = 0.15) +      # Add a boxplot without outliers
  labs(x = "Model", y = "Mean RMSE projection site", color = NULL) +
  coord_cartesian(ylim = c(NA, 4)) +
  scale_x_discrete(labels = c("Presence-only", "Presence-absence", "Integrated")) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +
  ggtitle("Projection site")

projection.plot2 <- projection.df %>%
  ggplot(aes(x = mod.type2, y = Mean.Int.Score)) +
  geom_point(position = position_jitter(width = 0.15, height = 0), alpha = 0.01, size = 0.25, color =  "maroon", show.legend = F) +      # Add a boxplot without outliers
  geom_boxplot(alpha = 0.6, outlier.shape = NA, fill = "maroon", width = 0.15) +      # Add a boxplot without outliers
  labs(x = "Model", y = "Mean interval score", color = NULL) +
  coord_cartesian(ylim = c(NA, 3.5)) +
  scale_x_discrete(labels = c("Presence-only", "Presence-absence", "Integrated")) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +
  ggtitle("Projection site")


#########################
## (Fig. 3b) TRAINING SITE
#########################

true.validation.SiteA.df <- true.validation.SiteA.df %>%
  mutate(scenario = "1")

true.validation.SiteA.df_1C <- true.validation.SiteA.df_1C %>%
  mutate(scenario = "1C")

true.validation.SiteA.df_5 <- true.validation.SiteA.df_5 %>%
  mutate(scenario = "5")

true.validation.SiteA.df_5C <- true.validation.SiteA.df_5C %>%
  mutate(scenario = "5C")

true.validation.SiteA.dfv0.2 <- true.validation.SiteA.dfv0.2 %>%
  mutate(scenario = "5v0.2")

true.validation.SiteA.df_5Cv0.2 <- true.validation.SiteA.df_5Cv0.2 %>%
  mutate(scenario = "5Cv0.2")

true.validation.SiteA.dfv5 <- true.validation.SiteA.dfv5 %>%
  mutate(scenario = "5v5")

true.validation.SiteA.df_5Cv5 <- true.validation.SiteA.df_5Cv5 %>%
  mutate(scenario = "5Cv5")

training.df <- rbind(true.validation.SiteA.df, true.validation.SiteA.df_1C, true.validation.SiteA.df_5, true.validation.SiteA.df_5C, true.validation.SiteA.dfv0.2, true.validation.SiteA.df_5Cv0.2, true.validation.SiteA.dfv5, true.validation.SiteA.df_5Cv5)

# Create a new column with the model type
training.df <- training.df %>% 
  mutate(mod.type2 = case_when(
    grepl("PO", mod.type) ~ "m.PO",
    grepl("PA", mod.type) ~ "m.PA",
    grepl("int", mod.type) ~ "m.int"
  ))


training.df$mod.type2 <- factor(training.df$mod.type2, levels = c("m.PO", "m.PA", "m.int"))

df <- training.df

# First trim half model outputs for PO and Integrated so they're even
training.df <- rbind(
  df[df$mod.type2 == "m.PA", ],  # Keep all m.PA rows
  df[df$mod.type2 == "m.int", ][sample(nrow(df[df$mod.type2 == "m.int", ]), 
                                       size = nrow(df[df$mod.type2 == "m.int", ])/2), ],
  df[df$mod.type2 == "m.PO", ][sample(nrow(df[df$mod.type2 == "m.PO", ]), 
                                      size = nrow(df[df$mod.type2 == "m.PO", ])/2), ]
)

training.plot <- training.df %>%
  ggplot(aes(x = mod.type2, y = RMSE.global)) +
  geom_point(position = position_jitter(width = 0.15, height = 0), alpha = 0.01, size = 0.25, color =  "maroon", show.legend = F) +      # Add a boxplot without outliers
  geom_boxplot(alpha = 0.6, outlier.shape = NA, fill = "maroon", width = 0.15) +      # Add a boxplot without outliers
  labs(x = "Model", y = "Mean RMSE training site", color = NULL) +
  coord_cartesian(ylim = c(NA, 4)) +
  scale_x_discrete(labels = c("Presence-only", "Presence-absence", "Integrated")) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +
  ggtitle("Training site")

training.plot2 <- training.df %>%
  ggplot(aes(x = mod.type2, y = Mean.Int.Score)) +
  geom_point(position = position_jitter(width = 0.15, height = 0), alpha = 0.01, size = 0.25, color =  "maroon", show.legend = F) +      # Add a boxplot without outliers
  geom_boxplot(alpha = 0.6, outlier.shape = NA, fill = "maroon", width = 0.15) +      # Add a boxplot without outliers
  labs(x = "Model", y = "Mean interval score", color = NULL) +
  coord_cartesian(ylim = c(NA, 3.5)) +
  scale_x_discrete(labels = c("Presence-only", "Presence-absence", "Integrated")) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +
  ggtitle("Training site")


# Combine the two plots
Figure_3 <- ggarrange(projection.plot + rremove("xlab"), training.plot + rremove("xlab") , common.legend = T,  ncol = 1, nrow = 2, legend = "bottom", labels = c("(a)", "(b)"))

Figure_3b <- ggarrange(projection.plot2 + rremove("xlab"), training.plot2 + rremove("xlab") , common.legend = T,  ncol = 1, nrow = 2, legend = "bottom", labels = c("(a)", "(b)"))


ggsave(plot = Figure_3, filename = paste0(file.path(result_path),"/ALL_Scenarios/Figure_3.png"), w = 17, h = 17.5, units = "cm", dpi = 400, device = "png")


#############################

# Comparative values ------------------------------------------------------

#############################

# Calculate difference in projection RMSE between integrated and PA models per replicate

summary1 <- projection.df %>% 
  group_by(mod.type2) %>% 
  summarise(mean.RMSE = mean(RMSE.global, na.rm = TRUE),
            sd.RMSE = sd(RMSE.global, na.rm = TRUE))

print(paste0("RMSE is on average ", ((0.954-0.749)/0.954)*100, " lower integrated than PA models"))
print(paste0("RMSE is on average ", ((2.1807886-0.749)/2.1807886)*100, " lower in integrated than PO models"))

summary2 <- training.df %>% 
  group_by(mod.type2) %>% 
  summarise(mean.RMSE = mean(RMSE.global, na.rm = TRUE),
            sd.RMSE = sd(RMSE.global, na.rm = TRUE))

##############################
# Making summary tables ---------------------------------------------------
##############################

library(flextable)
library(officer)


######## SCENARIO 1.##########

# Calculate the mean and standard deviation of the RMSE.global for each model type and scenario

projection.df <- rbind(true.validation.df, true.validation.df_1C, true.validation.df_5, true.validation.df_5C, true.validation.dfv0.2, true.validation.df_5Cv0.2, true.validation.dfv5, true.validation.df_5Cv5)

# Create a new column with the model type
projection.df <- projection.df %>% 
  mutate(mod.type2 = case_when(
    grepl("PO", mod.type) ~ "m.PO",
    grepl("PA", mod.type) ~ "m.PA",
    grepl("int", mod.type) ~ "m.int"
  ))


projection.df$mod.type2 <- factor(projection.df$mod.type2, levels = c("m.PO", "m.PA", "m.int"))

projection.df$mod.type <- factor(projection.df$mod.type, levels = c("m.PO", "m.PO.bias","m.PA", "m.PA.bias", "m.int", "m.int.bias"))

projection.df$extrap.type <- factor(projection.df$extrap.type, levels = c("Low", "Moderate", "High"))

# SCENARIO 1.
table1 <- projection.df %>%
  filter(scenario %in% c("1", "1C")) %>% 
  group_by(extrap.type, scenario, mod.type) %>%
  summarise(mean.RMSE = mean(RMSE.global, na.rm = TRUE),
            sd.RMSE = sd(RMSE.global, na.rm = TRUE),
            mean.int = mean(Mean.Int.Score, na.rm = TRUE),
            sd.int = sd(Mean.Int.Score, na.rm = T))
  
ft <- flextable(table1)

doc <- read_docx() %>%
  body_add_flextable(ft)

print(doc, target = "Scenario_1_projection_table_output.docx")

training.df <- rbind(true.validation.SiteA.df, true.validation.SiteA.df_1C, true.validation.SiteA.df_5, true.validation.SiteA.df_5C, true.validation.SiteA.dfv0.2, true.validation.SiteA.df_5Cv0.2, true.validation.SiteA.dfv5, true.validation.SiteA.df_5Cv5)

# Create a new column with the model type
training.df <- training.df %>% 
  mutate(mod.type2 = case_when(
    grepl("PO", mod.type) ~ "m.PO",
    grepl("PA", mod.type) ~ "m.PA",
    grepl("int", mod.type) ~ "m.int"
  ))


training.df$mod.type2 <- factor(training.df$mod.type2, levels = c("m.PO", "m.PA", "m.int"))


table2 <- training.df %>%
  filter(scenario %in% c("1", "1C")) %>% 
  group_by(extrap.type, scenario, mod.type) %>%
  summarise(mean.RMSE = mean(RMSE.global, na.rm = TRUE),
            sd.RMSE = sd(RMSE.global, na.rm = TRUE),
            mean.int = mean(Mean.Int.Score, na.rm = TRUE),
            sd.int = sd(Mean.Int.Score, na.rm = T))

ft <- flextable(table2)

doc <- read_docx() %>%
  body_add_flextable(ft)

print(doc, target = "Scenario_1_training_table_output.docx") 


# Coefficients

extrap.scenario.df <- extrap.scenario.df %>%
  mutate(scenario = "1")

extrap.scenario.df_1C <- extrap.scenario.df_1C %>%
  mutate(scenario = "1C")

extrap.scenario.df_5 <- extrap.scenario.df_5 %>%
  mutate(scenario = "5")

extrap.scenario.df_5C <- extrap.scenario.df_5C %>%
  mutate(scenario = "5C")

extrap.scenario.dfv0.2 <- extrap.scenario.dfv0.2 %>%
  mutate(scenario = "5v0.2")

extrap.scenario.df_5Cv0.2 <- extrap.scenario.df_5Cv0.2 %>%
  mutate(scenario = "5Cv0.2")

extrap.scenario.dfv5 <- extrap.scenario.dfv5 %>%
  mutate(scenario = "5v5")

extrap.scenario.df_5Cv5 <- extrap.scenario.df_5Cv5 %>%
  mutate(scenario = "5Cv5")



extrap.df <- rbind(extrap.scenario.df, extrap.scenario.df_1C, extrap.scenario.df_5, extrap.scenario.df_5C, extrap.scenario.dfv0.2, extrap.scenario.df_5Cv0.2, extrap.scenario.dfv5, extrap.scenario.df_5Cv5)

# Create a new column with the model type
extrap.df <- extrap.df %>% 
  mutate(mod.type2 = case_when(
    grepl("PO", mod.type) ~ "m.PO",
    grepl("PA", mod.type) ~ "m.PA",
    grepl("int", mod.type) ~ "m.int"
  ))


extrap.df$mod.type2 <- factor(extrap.df$mod.type2, levels = c("m.PO", "m.PA", "m.int"))


table3 <- extrap.df %>%
  filter(scenario %in% c("1", "1C")) %>% 
  group_by(extrap.type, scenario, mod.type) %>%
  summarise(mean.beta1 = mean(beta1.mean, na.rm = TRUE),
            sd.beta1 = sd(beta1.mean, na.rm = TRUE),
            mean.beta2 = mean(beta2.mean, na.rm = TRUE),
            sd.beta2 = sd(beta2.mean, na.rm = T),
            mean.PO.int = mean(PO_intercept, na.rm = T),
            sd.PO.int = sd(PO_intercept, na.rm = T),
            mean.PA.int = mean(PA_intercept, na.rm = T),
            sd.PA.int = sd(PA_intercept, na.rm = T))

ft <- flextable(table3)

doc <- read_docx() %>%
  body_add_flextable(ft)

print(doc, target = "Scenario_1_coefficients_table_output.docx") 



######## SCENARIO 5.##########

# Calculate the mean and standard deviation of the RMSE.global for each model type and scenario

table1 <- projection.df %>%
  filter(scenario %in% c("5", "5C", "5V0.2", "5Cv0.2", "5V5", "5cV5")) %>% 
  group_by(extrap.type, scenario, mod.type) %>%
  summarise(mean.RMSE = mean(RMSE.global, na.rm = TRUE),
            sd.RMSE = sd(RMSE.global, na.rm = TRUE),
            mean.int = mean(Mean.Int.Score, na.rm = TRUE),
            sd.int = sd(Mean.Int.Score, na.rm = T))

ft <- flextable(table1)

doc <- read_docx() %>%
  body_add_flextable(ft)

print(doc, target = "Scenario_5_projection_table_output.docx")


table2 <- training.df %>%
  filter(scenario %in% c("5", "5C", "5V0.2", "5Cv0.2", "5V5", "5cV5")) %>% 
  group_by(extrap.type, scenario, mod.type) %>%
  summarise(mean.RMSE = mean(RMSE.global, na.rm = TRUE),
            sd.RMSE = sd(RMSE.global, na.rm = TRUE),
            mean.int = mean(Mean.Int.Score, na.rm = TRUE),
            sd.int = sd(Mean.Int.Score, na.rm = T))

ft <- flextable(table2)

doc <- read_docx() %>%
  body_add_flextable(ft)

print(doc, target = "Scenario_5_training_table_output.docx") 


# Coefficients


table3 <- extrap.df %>%
  filter(scenario %in% c("5", "5C", "5V0.2", "5Cv0.2", "5V5", "5cV5")) %>% 
  group_by(extrap.type, scenario, mod.type) %>%
  summarise(mean.beta1 = mean(beta1.mean, na.rm = TRUE),
            sd.beta1 = sd(beta1.mean, na.rm = TRUE),
            mean.beta2 = mean(beta2.mean, na.rm = TRUE),
            sd.beta2 = sd(beta2.mean, na.rm = T),
            mean.PO.int = mean(PO_intercept, na.rm = T),
            sd.PO.int = sd(PO_intercept, na.rm = T),
            mean.PA.int = mean(PA_intercept, na.rm = T),
            sd.PA.int = sd(PA_intercept, na.rm = T))

ft <- flextable(table3)

doc <- read_docx() %>%
  body_add_flextable(ft)

print(doc, target = "Scenario_5_coefficients_table_output.docx") 


