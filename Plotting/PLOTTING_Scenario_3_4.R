
library(ggplot2)
library(dplyr)
library(ggpubr)
library(viridis)
library(purrr)
library(readr)

outpath <- file.path(getwd(), "output")

result_path <- file.path(getwd(), "output/PLOTS")

# Make dir if not already there
if(!dir.exists(result_path)) {
  
  dir.create(result_path, recursive = TRUE)
  
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

x.discrete.label <- c("Low", "Mod", "High")  

#####################################################
####### CHOOSE NUMBER OF REPLICATES TO KEEP ########
#####################################################

replicates <- 100

######################################################################
################ SCENARIO 3 - SPATIAL AUTOCORRELATION ################
######################################################################

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

true.validation.df <- true.validation.df %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With Gaussian random field", "Without Gaussian random field")) %>% 
  mutate(bias.type2 = ifelse(grepl("PA", mod.type, fixed = TRUE), NA_character_,
                             ifelse(grepl("bias", mod.type, fixed = TRUE) & !grepl("PA", mod.type, fixed = T), "With bias covariate", "Without bias covariate"))) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type2, fixed = T), gsub(".bias", "", mod.type2), mod.type2)) %>% 
  mutate(mod.type3 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type3 = factor(mod.type3, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))

true.validation.SiteA.df <- true.validation.SiteA.df %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With Gaussian random field", "Without Gaussian random field")) %>% 
  mutate(bias.type2 = ifelse(grepl("PA", mod.type, fixed = TRUE), NA_character_,
                             ifelse(grepl("bias", mod.type, fixed = TRUE) & !grepl("PA", mod.type, fixed = T), "With bias covariate", "Without bias covariate"))) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type2, fixed = T), gsub(".bias", "", mod.type2), mod.type2)) %>% 
  mutate(mod.type3 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type3 = factor(mod.type3, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))

extrap.scenario.df <- extrap.scenario.df %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With Gaussian random field", "Without Gaussian random field")) %>% 
  mutate(bias.type2 = ifelse(grepl("PA", mod.type, fixed = TRUE), NA_character_,
                             ifelse(grepl("bias", mod.type, fixed = TRUE) & !grepl("PA", mod.type, fixed = T), "With bias covariate", "Without bias covariate"))) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type2, fixed = T), gsub(".bias", "", mod.type2), mod.type2)) %>% 
  mutate(mod.type3 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type3 = factor(mod.type3, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))

######################################################################
######### SCENARIO 4 - SPATIAL AUTOCORRELATION with Lower PO records ####
######################################################################

scenario_name = "4"

load(file = paste0(file.path(outpath, scenario_name), "/Scenario_", scenario_name, "_Input_Params.RData"))

# Load and join replicates together ---------------------------------------


# List all the files with validation dataframes (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_Job", full.names = TRUE, recursive = TRUE) 

true.validation.df.list <- lapply(file_list, read.csv)

true.validation.df_4 <- do.call(rbind, true.validation.df.list)


# THEN list all the files with validation dataframes for Site A (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_SiteA_Job", full.names = TRUE, recursive = TRUE) 

true.validation.SiteA.df.list <- lapply(file_list, read.csv)

true.validation.SiteA.df_4 <- do.call(rbind, true.validation.SiteA.df.list)


# FINALLY list all the files with model outputs (for each job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "Results_Summary", full.names = TRUE, recursive = TRUE) 

# Read all the CSV files and save them to a list
extrap.scenario.df.list <- lapply(file_list, read.csv)

# Combine all dataframes in the list into one dataframe using rbind
extrap.scenario.df_4 <- do.call(rbind, extrap.scenario.df.list)

##########################################################
### REMOVE REPLICATES THAT DON'T HAVE ALL THREE OUTPUTS ###
##########################################################

validation <- true.validation.df_4 %>%
  group_by(job_index) %>% 
  summarise(n = n()) %>% 
  select(job_index)

extrap <- extrap.scenario.df_4 %>%
  group_by(job_index) %>% 
  summarise(n = n()) %>% 
  select(job_index)

validationA <- true.validation.SiteA.df_4 %>%
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

true.validation.df_4 <- true.validation.df_4 %>%
  filter(job_index %in% common_jobs$job_index)

true.validation.SiteA.df_4 <- true.validation.SiteA.df_4 %>%
  filter(job_index %in% common_jobs$job_index)

extrap.scenario.df_4 <- extrap.scenario.df_4 %>%
  filter(job_index %in% common_jobs$job_index)


true.validation.df_4 <- true.validation.df_4 %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With Gaussian random field", "Without Gaussian random field")) %>% 
  mutate(bias.type2 = ifelse(grepl("PA", mod.type, fixed = TRUE), NA_character_,
                             ifelse(grepl("bias", mod.type, fixed = TRUE) & !grepl("PA", mod.type, fixed = T), "With bias covariate", "Without bias covariate"))) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type2, fixed = T), gsub(".bias", "", mod.type2), mod.type2)) %>% 
  mutate(mod.type3 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type3 = factor(mod.type3, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))

true.validation.SiteA.df_4 <- true.validation.SiteA.df_4 %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With Gaussian random field", "Without Gaussian random field")) %>% 
  mutate(bias.type2 = ifelse(grepl("PA", mod.type, fixed = TRUE), NA_character_,
                             ifelse(grepl("bias", mod.type, fixed = TRUE) & !grepl("PA", mod.type, fixed = T), "With bias covariate", "Without bias covariate"))) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type2, fixed = T), gsub(".bias", "", mod.type2), mod.type2)) %>% 
  mutate(mod.type3 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type3 = factor(mod.type3, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))

extrap.scenario.df_4 <- extrap.scenario.df_4 %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With Gaussian random field", "Without Gaussian random field")) %>% 
  mutate(bias.type2 = ifelse(grepl("PA", mod.type, fixed = TRUE), NA_character_,
                             ifelse(grepl("bias", mod.type, fixed = TRUE) & !grepl("PA", mod.type, fixed = T), "With bias covariate", "Without bias covariate"))) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type2, fixed = T), gsub(".bias", "", mod.type2), mod.type2)) %>% 
  mutate(mod.type3 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type3 = factor(mod.type3, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))

######################################################################
################ SCENARIO 3v0.2 - SPATIAL AUTOCORRELATION ################
######################################################################

scenario_name = "3v0.2"

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


true.validation.dfv0.2 <- true.validation.dfv0.2 %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With Gaussian random field", "Without Gaussian random field")) %>% 
  mutate(bias.type2 = ifelse(grepl("PA", mod.type, fixed = TRUE), NA_character_,
                             ifelse(grepl("bias", mod.type, fixed = TRUE) & !grepl("PA", mod.type, fixed = T), "With bias covariate", "Without bias covariate"))) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type2, fixed = T), gsub(".bias", "", mod.type2), mod.type2)) %>% 
  mutate(mod.type3 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type3 = factor(mod.type3, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))

true.validation.SiteA.dfv0.2 <- true.validation.SiteA.dfv0.2 %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With Gaussian random field", "Without Gaussian random field")) %>% 
  mutate(bias.type2 = ifelse(grepl("PA", mod.type, fixed = TRUE), NA_character_,
                             ifelse(grepl("bias", mod.type, fixed = TRUE) & !grepl("PA", mod.type, fixed = T), "With bias covariate", "Without bias covariate"))) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type2, fixed = T), gsub(".bias", "", mod.type2), mod.type2)) %>% 
  mutate(mod.type3 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type3 = factor(mod.type3, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))

extrap.scenario.dfv0.2 <- extrap.scenario.dfv0.2 %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With Gaussian random field", "Without Gaussian random field")) %>% 
  mutate(bias.type2 = ifelse(grepl("PA", mod.type, fixed = TRUE), NA_character_,
                             ifelse(grepl("bias", mod.type, fixed = TRUE) & !grepl("PA", mod.type, fixed = T), "With bias covariate", "Without bias covariate"))) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type2, fixed = T), gsub(".bias", "", mod.type2), mod.type2)) %>% 
  mutate(mod.type3 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type3 = factor(mod.type3, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))

######################################################################
################ SCENARIO 4v0.2 - SPATIAL AUTOCORRELATION C ################
######################################################################

scenario_name = "4v0.2"

load(file = paste0(file.path(outpath, scenario_name), "/Scenario_", scenario_name, "_Input_Params.RData"))

# Load and join replicates together ---------------------------------------


# List all the files with validation dataframes (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_Job", full.names = TRUE, recursive = TRUE) 

true.validation.df.list <- lapply(file_list, read.csv)

true.validation.df_4v0.2 <- do.call(rbind, true.validation.df.list)


# THEN list all the files with validation dataframes for Site A (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_SiteA_Job", full.names = TRUE, recursive = TRUE) 

true.validation.SiteA.df.list <- lapply(file_list, read.csv)

true.validation.SiteA.df_4v0.2 <- do.call(rbind, true.validation.SiteA.df.list)


# FINALLY list all the files with model outputs (for each job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "Results_Summary", full.names = TRUE, recursive = TRUE) 

# Read all the CSV files and save them to a list
extrap.scenario.df.list <- lapply(file_list, read.csv)

# Combine all dataframes in the list into one dataframe using rbind
extrap.scenario.df_4v0.2 <- do.call(rbind, extrap.scenario.df.list)

##########################################################
### REMOVE REPLICATES THAT DON'T HAVE ALL THREE OUTPUTS ###
##########################################################

validation <- true.validation.df_4v0.2 %>%
  group_by(job_index) %>% 
  summarise(n = n()) %>% 
  select(job_index)

extrap <- extrap.scenario.df_4v0.2 %>%
  group_by(job_index) %>% 
  summarise(n = n()) %>% 
  select(job_index)

validationA <- true.validation.SiteA.df_4v0.2 %>%
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

true.validation.df_4v0.2 <- true.validation.df_4v0.2 %>%
  filter(job_index %in% common_jobs$job_index)

true.validation.SiteA.df_4v0.2 <- true.validation.SiteA.df_4v0.2 %>%
  filter(job_index %in% common_jobs$job_index)

extrap.scenario.df_4v0.2 <- extrap.scenario.df_4v0.2 %>%
  filter(job_index %in% common_jobs$job_index)



true.validation.df_4v0.2 <- true.validation.df_4v0.2 %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With Gaussian random field", "Without Gaussian random field")) %>% 
  mutate(bias.type2 = ifelse(grepl("PA", mod.type, fixed = TRUE), NA_character_,
                             ifelse(grepl("bias", mod.type, fixed = TRUE) & !grepl("PA", mod.type, fixed = T), "With bias covariate", "Without bias covariate"))) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type2, fixed = T), gsub(".bias", "", mod.type2), mod.type2)) %>% 
  mutate(mod.type3 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type3 = factor(mod.type3, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))

true.validation.SiteA.df_4v0.2 <- true.validation.SiteA.df_4v0.2 %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With Gaussian random field", "Without Gaussian random field")) %>% 
  mutate(bias.type2 = ifelse(grepl("PA", mod.type, fixed = TRUE), NA_character_,
                             ifelse(grepl("bias", mod.type, fixed = TRUE) & !grepl("PA", mod.type, fixed = T), "With bias covariate", "Without bias covariate"))) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type2, fixed = T), gsub(".bias", "", mod.type2), mod.type2)) %>% 
  mutate(mod.type3 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type3 = factor(mod.type3, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))

extrap.scenario.df_4v0.2 <- extrap.scenario.df_4v0.2 %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With Gaussian random field", "Without Gaussian random field")) %>% 
  mutate(bias.type2 = ifelse(grepl("PA", mod.type, fixed = TRUE), NA_character_,
                             ifelse(grepl("bias", mod.type, fixed = TRUE) & !grepl("PA", mod.type, fixed = T), "With bias covariate", "Without bias covariate"))) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type2, fixed = T), gsub(".bias", "", mod.type2), mod.type2)) %>% 
  mutate(mod.type3 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type3 = factor(mod.type3, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))

######################################################################
################ SCENARIO 3v5 - SPATIAL AUTOCORRELATION ################
######################################################################

scenario_name = "3v5"

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


true.validation.dfv5 <- true.validation.dfv5 %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With Gaussian random field", "Without Gaussian random field")) %>% 
  mutate(bias.type2 = ifelse(grepl("PA", mod.type, fixed = TRUE), NA_character_,
                             ifelse(grepl("bias", mod.type, fixed = TRUE) & !grepl("PA", mod.type, fixed = T), "With bias covariate", "Without bias covariate"))) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type2, fixed = T), gsub(".bias", "", mod.type2), mod.type2)) %>% 
  mutate(mod.type3 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type3 = factor(mod.type3, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))

true.validation.SiteA.dfv5 <- true.validation.SiteA.dfv5 %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With Gaussian random field", "Without Gaussian random field")) %>% 
  mutate(bias.type2 = ifelse(grepl("PA", mod.type, fixed = TRUE), NA_character_,
                             ifelse(grepl("bias", mod.type, fixed = TRUE) & !grepl("PA", mod.type, fixed = T), "With bias covariate", "Without bias covariate"))) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type2, fixed = T), gsub(".bias", "", mod.type2), mod.type2)) %>% 
  mutate(mod.type3 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type3 = factor(mod.type3, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))

extrap.scenario.dfv5 <- extrap.scenario.dfv5 %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With Gaussian random field", "Without Gaussian random field")) %>% 
  mutate(bias.type2 = ifelse(grepl("PA", mod.type, fixed = TRUE), NA_character_,
                             ifelse(grepl("bias", mod.type, fixed = TRUE) & !grepl("PA", mod.type, fixed = T), "With bias covariate", "Without bias covariate"))) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type2, fixed = T), gsub(".bias", "", mod.type2), mod.type2)) %>% 
  mutate(mod.type3 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type3 = factor(mod.type3, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))

######################################################################
################ SCENARIO 4v5 - SPATIAL AUTOCORRELATION C ################
######################################################################

scenario_name = "4v5"

load(file = paste0(file.path(outpath, scenario_name), "/Scenario_", scenario_name, "_Input_Params.RData"))

# Load and join replicates together ---------------------------------------


# List all the files with validation dataframes (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_Job", full.names = TRUE, recursive = TRUE) 

true.validation.df.list <- lapply(file_list, read.csv)

true.validation.df_4v5 <- do.call(rbind, true.validation.df.list)


# THEN list all the files with validation dataframes for Site A (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_SiteA_Job", full.names = TRUE, recursive = TRUE) 

true.validation.SiteA.df.list <- lapply(file_list, read.csv)

true.validation.SiteA.df_4v5 <- do.call(rbind, true.validation.SiteA.df.list)


# FINALLY list all the files with model outputs (for each job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "Results_Summary", full.names = TRUE, recursive = TRUE) 

# Read all the CSV files and save them to a list
extrap.scenario.df.list <- lapply(file_list, read.csv)

# Combine all dataframes in the list into one dataframe using rbind
extrap.scenario.df_4v5 <- do.call(rbind, extrap.scenario.df.list)


##########################################################
### REMOVE REPLICATES THAT DON'T HAVE ALL THREE OUTPUTS ###
##########################################################

validation <- true.validation.df_4v5 %>%
  group_by(job_index) %>% 
  summarise(n = n()) %>% 
  select(job_index)

extrap <- extrap.scenario.df_4v5 %>%
  group_by(job_index) %>% 
  summarise(n = n()) %>% 
  select(job_index)

validationA <- true.validation.SiteA.df_4v5 %>%
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

true.validation.df_4v5 <- true.validation.df_4v5 %>%
  filter(job_index %in% common_jobs$job_index)

true.validation.SiteA.df_4v5 <- true.validation.SiteA.df_4v5 %>%
  filter(job_index %in% common_jobs$job_index)

extrap.scenario.df_4v5 <- extrap.scenario.df_4v5 %>%
  filter(job_index %in% common_jobs$job_index)



true.validation.df_4v5 <- true.validation.df_4v5 %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With Gaussian random field", "Without Gaussian random field")) %>% 
  mutate(bias.type2 = ifelse(grepl("PA", mod.type, fixed = TRUE), NA_character_,
                             ifelse(grepl("bias", mod.type, fixed = TRUE) & !grepl("PA", mod.type, fixed = T), "With bias covariate", "Without bias covariate"))) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type2, fixed = T), gsub(".bias", "", mod.type2), mod.type2)) %>% 
  mutate(mod.type3 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type3 = factor(mod.type3, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))


true.validation.SiteA.df_4v5 <- true.validation.SiteA.df_4v5 %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With Gaussian random field", "Without Gaussian random field")) %>% 
  mutate(bias.type2 = ifelse(grepl("PA", mod.type, fixed = TRUE), NA_character_,
                             ifelse(grepl("bias", mod.type, fixed = TRUE) & !grepl("PA", mod.type, fixed = T), "With bias covariate", "Without bias covariate"))) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type2, fixed = T), gsub(".bias", "", mod.type2), mod.type2)) %>% 
  mutate(mod.type3 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type3 = factor(mod.type3, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))


extrap.scenario.df_4v5 <- extrap.scenario.df_4v5 %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With Gaussian random field", "Without Gaussian random field")) %>% 
  mutate(bias.type2 = ifelse(grepl("PA", mod.type, fixed = TRUE), NA_character_,
                             ifelse(grepl("bias", mod.type, fixed = TRUE) & !grepl("PA", mod.type, fixed = T), "With bias covariate", "Without bias covariate"))) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type2 = ifelse(grepl("bias", mod.type2, fixed = T), gsub(".bias", "", mod.type2), mod.type2)) %>% 
  mutate(mod.type3 = ifelse(grepl("bias", mod.type, fixed = T), gsub(".bias", "", mod.type), mod.type)) %>% 
  mutate(mod.type3 = factor(mod.type3, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))


###################################################
############## PLOT ALL TOGETHER #################
###################################################

true.validation.df <- true.validation.df %>% 
  mutate(relative.GRF = "1")

true.validation.dfv0.2 <- true.validation.dfv0.2 %>% 
  mutate(relative.GRF = "0.2")

true.validation.dfv5 <- true.validation.dfv5 %>% 
  mutate(relative.GRF = "5")

final.df <- rbind(true.validation.df, true.validation.dfv0.2, true.validation.dfv5)

final.df$bias.type <- factor(final.df$bias.type, levels = c("Without Gaussian random field", "With Gaussian random field"))

final.df$bias.type2 <- factor(final.df$bias.type2, levels = c("Without bias covariate", "With bias covariate"))

final.df$mod.type2 <- factor(final.df$mod.type2, levels = c("m.PO", "m.PA", "m.int"))

# Save a version for bias comparison
final.df.bias <- final.df

######## Projection site (Figure 4) ############

# Filter out just the models that account for bias with covariate
final.df <- final.df %>% 
  filter(bias.type2 == "With bias covariate" | mod.type2 == "m.PA")

projection1 <- final.df %>%
  ggplot(aes(x = relative.GRF, y = RMSE.global, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Mean RMSE projection site", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  scale_color_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  coord_cartesian(ylim = c(NA, 3)) +
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
        axis.title.x = element_text(size = 13.5),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) 


projection2 <-  final.df %>%
  ggplot(aes(x = relative.GRF, y = Mean.Int.Score, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Mean interval score", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  scale_color_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  coord_cartesian(ylim = c(NA, 1.75)) +
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
        plot.title = element_text(hjust = 1, size = 15, face = "italic"))   


# Correlation
projection3 <-  final.df %>%
  ggplot(aes(x = relative.GRF, y = correlation, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Correlation", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  scale_color_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
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
        plot.title = element_text(hjust = 1, size = 15, face = "italic"))  


# Coverage probability
projection4 <- final.df %>%
  ggplot(aes(x = relative.GRF, y = coverage.rate, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Mean coverage probability", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "red") +  # Dashed horizontal line
  scale_fill_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  scale_color_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  #coord_cartesian(ylim = c(0, 0.1)) +
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
        axis.title.x = element_text(size = 13.5),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) 

Fig4 <- ggarrange(projection1 + rremove("xlab"), projection2 + rremove("xlab"), projection4 + rremove("xlab"), projection3, common.legend = TRUE, ncol = 1, nrow = 4, legend = "bottom", labels = c("(a)", "(b)", "(c)", "(d)"), align = "v")

ggsave(plot = Fig4, filename = paste0(file.path(result_path),"/Figure_4.png"), w = 21, h = 30, units = "cm", dpi = 400, device = "png")


###### FIGURE 5 ##########

cor.GRF <- final.SiteA.df.bias %>% 
  filter(bias.type == "With Gaussian random field") %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int"))) %>%
  mutate(bias.type2 = factor(bias.type2, levels = c("Without bias covariate", "With bias covariate"))) %>%
  ggplot(aes(x = relative.GRF, y = cor.GRF, fill = bias.type2)) +  
  geom_boxplot(alpha = 0.6, width = 0.5, outlier.shape = NA, aes(fill = bias.type2)) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Correlation", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.3)) +
  scale_fill_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4", "grey70"),
                    breaks = c("Without bias covariate", "With bias covariate")) +
  scale_color_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4", "grey70"),
                     breaks = c("Without bias covariate", "With bias covariate")) + 
  coord_cartesian(ylim = c(-0.2, 1)) +
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
        axis.title.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14),
        axis.text = element_text(size = 12),    
        strip.text = element_text(size = 14),   
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic"))

cor.FIXED <- final.SiteA.df.bias %>% 
  filter(bias.type == "With Gaussian random field") %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int"))) %>%
  mutate(bias.type2 = factor(bias.type2, levels = c("Without bias covariate", "With bias covariate"))) %>%
  ggplot(aes(x = relative.GRF, y = cor.FIXED, fill = bias.type2)) +  
  geom_boxplot(alpha = 0.6, width = 0.5, outlier.shape = NA, aes(fill = bias.type2)) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Correlation", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.3)) +
  scale_fill_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4", "grey70"),
                    breaks = c("Without bias covariate", "With bias covariate")) +
  scale_color_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4", "grey70"),
                     breaks = c("Without bias covariate", "With bias covariate")) +
  coord_cartesian(ylim = c(-0.2, 1)) +
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
        axis.title.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14),
        axis.text = element_text(size = 12),    
        strip.text = element_text(size = 14),   
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic"))


COR_GRF_FIXED_plot <- ggarrange(
  cor.GRF + rremove("xlab"),  # Remove individual x labels
  cor.FIXED + rremove("xlab"),
  common.legend = TRUE,  
  ncol = 1, nrow = 2, 
  legend = "bottom",
  align = "v",
  labels = c("(a)", "(b)"),
  heights = c(1, 1)
)

COR_GRF_FIXED_plot <- annotate_figure(
  COR_GRF_FIXED_plot, 
  bottom = text_grob("Relative random effect contribution", size = 14, vjust = 0.5)  # Adjust vertical position
) 


ggsave(plot = COR_GRF_FIXED_plot, filename = paste0(file.path(result_path),"/Figure_5.png"), w = 21, h = 16, units = "cm", dpi = 400, device = "png")


########### COEFFICIENT RECOVERY ###############

extrap.scenario.df <- extrap.scenario.df %>% 
  mutate(relative.GRF = "1")

extrap.scenario.dfv0.2 <- extrap.scenario.dfv0.2 %>% 
  mutate(relative.GRF = "0.2")

extrap.scenario.dfv5 <- extrap.scenario.dfv5 %>% 
  mutate(relative.GRF = "5")

final.extrap.df <- rbind(extrap.scenario.df, extrap.scenario.dfv0.2, extrap.scenario.dfv5)


final.extrap.df$bias.type <- factor(final.extrap.df$bias.type, levels = c("Without Gaussian random field", "With Gaussian random field"))
final.extrap.df$mod.type2 <- factor(final.extrap.df$mod.type2, levels = c("m.PO", "m.PA", "m.int"))

# Save a version for bias comparison
final.extrap.df.bias <- final.extrap.df

final.extrap.df <- final.extrap.df %>% 
  filter(bias.type2 == "With bias covariate" | mod.type2 == "m.PA")

coef1 <- final.extrap.df %>%
  ggplot(aes(x = relative.GRF, y = beta1.mean, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = expression(beta[1]), color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  scale_color_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  geom_hline(yintercept = beta1, linetype = "dashed", color = "red") +
  coord_cartesian(ylim = c(-0.35, 0.35)) +
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

coef2 <- final.extrap.df %>%
  ggplot(aes(x = relative.GRF, y = beta2.mean, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = expression(beta[2]), color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  scale_color_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  geom_hline(yintercept = beta2, linetype = "dashed", color = "red") +
  coord_cartesian(ylim = c(-0.35, 0.35)) +
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

Supp.Fig.X <- ggarrange(coef1 + rremove("xlab"), coef2 + rremove("xlab") , common.legend = T,  ncol = 1, nrow = 2, legend = "bottom", labels = c("(a)", "(b)"))

ggsave(plot = Supp.Fig.X, filename = paste0(file.path(result_path),"/Supp.Fig.9.png"), w = 21, h = 17.5, units = "cm", dpi = 400, device = "png")

PO_int <- final.extrap.df %>%
  filter(PO_intercept != 0 ) %>% 
  ggplot(aes(x = relative.GRF, y = PO_intercept, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = expression(beta[0]), color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  scale_color_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  geom_hline(yintercept = beta0, linetype = "dashed", color = "red") +
  coord_cartesian(ylim = c(-6, 1)) +
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
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +
  ggtitle('PO intercept')

PA_int <- final.extrap.df %>%
  filter(PA_intercept != 0 ) %>% 
  ggplot(aes(x = relative.GRF, y = PA_intercept, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = expression(beta[0]), color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  scale_color_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  geom_hline(yintercept = beta0, linetype = "dashed", color = "red") +
  coord_cartesian(ylim = c(-6, 1)) +
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
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +
  ggtitle('PA intercept')

Supp.Fig.X <- ggarrange(PO_int + rremove("xlab"), PA_int + rremove("xlab") , common.legend = T,  ncol = 2, nrow = 1, legend = "bottom", labels = c("(a)", "(b)"))

ggsave(plot = Supp.Fig.X, filename = paste0(file.path(result_path),"/Supp.Fig.11.png"), w = 23.5, h = 15, units = "cm", dpi = 400, device = "png")

coefSD1 <- final.extrap.df %>%
  ggplot(aes(x = relative.GRF, y = beta1.sd, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = expression(beta[1]~"SD"), color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  scale_color_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  coord_cartesian(ylim = c(NA, 0.575)) +
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

coefSD2 <- final.extrap.df %>%
  ggplot(aes(x = relative.GRF, y = beta2.sd, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = expression(beta[2]~"SD"), color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  scale_color_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  coord_cartesian(ylim = c(0, 0.575)) +
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

Supp.Fig.X <- ggarrange(coefSD1 + rremove("xlab"), coefSD2 + rremove("xlab") , common.legend = T,  ncol = 1, nrow = 2, legend = "bottom", labels = c("(a)", "(b)"))

ggsave(plot = Supp.Fig.X, filename = paste0(file.path(result_path),"/Supp.Fig.10.png"), w = 21, h = 17.5, units = "cm", dpi = 400, device = "png")

#########################################################
################## TRAINING SITE #########################
#########################################################

true.validation.SiteA.df <- true.validation.SiteA.df %>% 
  mutate(relative.GRF = "1")

true.validation.SiteA.dfv0.2 <- true.validation.SiteA.dfv0.2 %>% 
  mutate(relative.GRF = "0.2")

true.validation.SiteA.dfv5 <- true.validation.SiteA.dfv5 %>% 
  mutate(relative.GRF = "5")

final.SiteA.df <- rbind(true.validation.SiteA.df, true.validation.SiteA.dfv0.2, true.validation.SiteA.dfv5)


final.SiteA.df$bias.type <- factor(final.SiteA.df$bias.type, levels = c("Without Gaussian random field", "With Gaussian random field"))
final.SiteA.df$mod.type2 <- factor(final.SiteA.df$mod.type2, levels = c("m.PO", "m.PA", "m.int"))

# Save a version for bias comparison
final.SiteA.df.bias <- final.SiteA.df

final.SiteA.df <- final.SiteA.df %>% 
  filter(bias.type2 == "With bias covariate" | mod.type2 == "m.PA")


training1 <- final.SiteA.df %>%
  ggplot(aes(x = relative.GRF, y = RMSE.global, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Mean RMSE projection site", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  scale_color_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  coord_cartesian(ylim = c(NA, 3)) +
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



training2 <- final.SiteA.df %>%
  ggplot(aes(x = relative.GRF, y = Mean.Int.Score, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Mean interval score", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  scale_color_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  coord_cartesian(ylim = c(NA, 1.75)) +
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

training3 <- final.SiteA.df %>%
  ggplot(aes(x = relative.GRF, y = correlation, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Correlation", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  scale_color_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  coord_cartesian(ylim = c(0, NA)) +
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

training4 <- final.SiteA.df %>%
  ggplot(aes(x = relative.GRF, y = coverage.rate, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Mean coverage probability", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  scale_color_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  #coord_cartesian(ylim = c(-0.5, NA)) +
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

fig2b <- ggarrange(training1 + rremove("xlab"), training2 + rremove("xlab"), training4 + rremove("xlab"), training3, common.legend = TRUE, ncol = 1, nrow = 4, legend = "bottom", labels = c("(a)", "(b)", "(c)", "(d)"), align = "v")

ggsave(plot = fig2b, filename = paste0(file.path(result_path),"/Supp.Fig.8.png"), w = 21, h = 30, units = "cm", dpi = 400, device = "png")


######################################################
########### LOOKING AT RESULTS RANGE SCENARIO ##########
######################################################

x.discrete.label <- c(as.character(scal))

final.df <- final.df %>% 
  mutate(spat.auto.range = ifelse(extrap.type == "Low", 20, 
                                  ifelse(extrap.type == "Moderate", 300, 
                                         700))) %>% 
  mutate(relative.range.dist = spat.auto.range/ Site.distance)


projection1 <- final.df %>%
  ggplot(aes(x = extrap.type, y = RMSE.global, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Spatial autocorrelation range", y = "Mean RMSE projection site", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  scale_color_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  coord_cartesian(ylim = c(NA, 3)) +
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


projection2 <-  final.df %>%
  ggplot(aes(x = extrap.type, y = Mean.Int.Score, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Spatial autocorrelation range", y = "Mean interval score", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  scale_color_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  coord_cartesian(ylim = c(NA, 1.75)) +
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


# Correlation
projection3 <-  final.df %>%
  ggplot(aes(x = extrap.type, y = correlation, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Spatial autocorrelation range", y = "Correlation", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  scale_color_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  coord_cartesian(ylim = c(0, NA)) +
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


# Coverage probability
projection4 <-  final.df %>%
  ggplot(aes(x = extrap.type, y = coverage.rate, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Spatial autocorrelation range", y = "Mean coverage probability", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  scale_color_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  #coord_cartesian(ylim = c(0, 0.1)) +
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

fig2b <- ggarrange(projection1 + rremove("xlab"), projection2 + rremove("xlab"), projection4 + rremove("xlab"), projection3, common.legend = TRUE, ncol = 1, nrow = 4, legend = "bottom", labels = c("(a)", "(b)", "(c)", "(d)"), align = "v")

ggsave(plot = fig2b, filename = paste0(file.path(result_path),"/Supp.Fig.12.png"), w = 21, h = 30, units = "cm", dpi = 400, device = "png")


##########################################################################
##########################################################################
# Scenario Without bias covariate -----------------------------------------------
##########################################################################
##########################################################################
final.df.bias <- final.df.bias %>%
  mutate(bias.type2 = case_when(
    mod.type2 == "m.PA" ~ "Without bias covariate",
    TRUE ~ bias.type2
  ))


projection1 <- final.df.bias %>%
  ggplot(aes(x = relative.GRF, y = RMSE.global, fill = bias.type, linetype = bias.type2)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Mean RMSE projection site", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  scale_color_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  scale_linetype_manual(values = c("Without bias covariate" = "dotted", "With bias covariate" = "solid")) +  # Different line types for bias types
  coord_cartesian(ylim = c(NA, 3.5)) +
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
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +
  guides(color = guide_legend(order = 1), 
         fill = guide_legend(order = 1), 
         linetype = guide_legend(order = 2))


projection2 <-  final.df.bias %>%
  ggplot(aes(x = relative.GRF, y = Mean.Int.Score, fill = bias.type, linetype = bias.type2)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Mean interval score", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  scale_color_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  scale_linetype_manual(values = c("Without bias covariate" = "dotted", "With bias covariate" = "solid")) +  # Different line types for bias types
  coord_cartesian(ylim = c(NA, 2.3)) +
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
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +
  guides(color = guide_legend(order = 1), 
         fill = guide_legend(order = 1), 
         linetype = guide_legend(order = 2))


# Correlation
projection3 <-  final.df.bias %>%
  ggplot(aes(x = relative.GRF, y = correlation, fill = bias.type, linetype = bias.type2)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Correlation", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  scale_color_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  scale_linetype_manual(values = c("Without bias covariate" = "dotted", "With bias covariate" = "solid")) +  # Different line types for bias types
  coord_cartesian(ylim = c(-0.5, NA)) +
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
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +
  guides(color = guide_legend(order = 1), 
         fill = guide_legend(order = 1), 
         linetype = guide_legend(order = 2))


# Coverage probability
projection4 <-  final.df.bias %>%
  ggplot(aes(x = relative.GRF, y = coverage.rate, fill = bias.type, linetype = bias.type2)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Mean coverage probability", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  scale_color_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  scale_linetype_manual(values = c("Without bias covariate" = "dotted", "With bias covariate" = "solid")) +  # Different line types for bias types
  #coord_cartesian(ylim = c(0, 0.1)) +
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
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) +
  guides(color = guide_legend(order = 1), 
         fill = guide_legend(order = 1), 
         linetype = guide_legend(order = 2))

fig2b <- ggarrange(projection1 + rremove("xlab"), projection2 + rremove("xlab"), projection4 + rremove("xlab"), projection3, common.legend = TRUE, ncol = 1, nrow = 4, legend = "bottom", labels = c("(a)", "(b)", "(c)", "(d)"), align = "v")

ggsave(plot = fig2b, filename = paste0(file.path(result_path),"/Supp.Fig.13.png"), w = 21, h = 30, units = "cm", dpi = 400, device = "png")



# SCENARIO 4 VERSION -----------------------------------------------------

###################################################
############## PLOT ALL TOGETHER #################
###################################################

x.discrete.label <- c("Low", "Mod", "High")  

true.validation.df_4 <- true.validation.df_4 %>% 
  mutate(relative.GRF = "1")

true.validation.df_4v0.2 <- true.validation.df_4v0.2 %>% 
  mutate(relative.GRF = "0.2")

true.validation.df_4v5 <- true.validation.df_4v5 %>% 
  mutate(relative.GRF = "5")


final.df <- rbind(true.validation.df_4, true.validation.df_4v0.2, true.validation.dfv5)

final.df$bias.type <- factor(final.df$bias.type, levels = c("Without Gaussian random field", "With Gaussian random field"))

final.df$bias.type2 <- factor(final.df$bias.type2, levels = c("Without bias covariate", "With bias covariate"))

final.df$mod.type2 <- factor(final.df$mod.type2, levels = c("m.PO", "m.PA", "m.int"))

# Save a version for bias comparison
final.df.bias <- final.df


######## Projection site  ############

# Filter out just the models that account for bias with covariate
final.df <- final.df %>% 
  filter(bias.type2 == "With bias covariate" | mod.type2 == "m.PA")


# Filter out just the models that account for bias with covariate
final.df <- final.df %>% 
  filter(bias.type2 == "With bias covariate" | mod.type2 == "m.PA")

projection1 <- final.df %>%
  ggplot(aes(x = relative.GRF, y = RMSE.global, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Mean RMSE projection site", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  scale_color_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  coord_cartesian(ylim = c(NA, 3)) +
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
        axis.title.x = element_text(size = 13.5),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) 


projection2 <-  final.df %>%
  ggplot(aes(x = relative.GRF, y = Mean.Int.Score, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Mean interval score", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  scale_color_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  coord_cartesian(ylim = c(NA, 1.75)) +
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
        plot.title = element_text(hjust = 1, size = 15, face = "italic"))  


# Correlation
projection3 <-  final.df %>%
  ggplot(aes(x = relative.GRF, y = correlation, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Correlation", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  scale_color_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
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
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) 


# Coverage probability
projection4 <-  final.df %>%
  ggplot(aes(x = relative.GRF, y = coverage.rate, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Mean coverage probability", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  scale_color_manual(values = c("Without Gaussian random field" = "#D55E00", "With Gaussian random field" = "#0072B2")) +
  #coord_cartesian(ylim = c(0, 0.1)) +
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
        axis.title.x = element_text(size = 13.5),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) 


Fig4 <- ggarrange(projection1 + rremove("xlab"), projection2 + rremove("xlab"), projection4 + rremove("xlab"), projection3, common.legend = TRUE, ncol = 1, nrow = 4, legend = "bottom", labels = c("(a)", "(b)", "(c)", "(d)"), align = "v")

ggsave(plot = Fig4, filename = paste0(file.path(result_path),"/Supp.Fig.14.png"), w = 21, h = 30, units = "cm", dpi = 400, device = "png")


########## CORRELATION BETWEEN ESTIMATED AND TRUE FIXED & GRF #############

cor.GRF <- final.SiteA.df.bias %>% 
  filter(bias.type == "With Gaussian random field") %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int"))) %>%
  mutate(bias.type2 = factor(bias.type2, levels = c("Without bias covariate", "With bias covariate"))) %>%
  ggplot(aes(x = relative.GRF, y = cor.GRF, fill = bias.type2)) +  
  geom_boxplot(alpha = 0.6, width = 0.5, outlier.shape = NA, aes(fill = bias.type2)) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Correlation", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.3)) +
  scale_fill_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4", "grey70"),
                    breaks = c("Without bias covariate", "With bias covariate")) +
  scale_color_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4", "grey70"),
                     breaks = c("Without bias covariate", "With bias covariate")) +
  coord_cartesian(ylim = c(-0.2, 1)) +
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
        axis.title.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14),
        axis.text = element_text(size = 12),    
        strip.text = element_text(size = 14),   
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic"))

cor.FIXED <- final.SiteA.df.bias %>% 
  filter(bias.type == "With Gaussian random field") %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int"))) %>%
  mutate(bias.type2 = factor(bias.type2, levels = c("Without bias covariate", "With bias covariate"))) %>%
  ggplot(aes(x = relative.GRF, y = cor.FIXED, fill = bias.type2)) +  
  geom_boxplot(alpha = 0.6, width = 0.5, outlier.shape = NA, aes(fill = bias.type2)) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Correlation", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.3)) +
  scale_fill_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4", "grey70"),
                    breaks = c("Without bias covariate", "With bias covariate")) +
  scale_color_manual(values = c("Without bias covariate" = "green4", "With bias covariate" = "purple4", "grey70"),
                     breaks = c("Without bias covariate", "With bias covariate")) +
  coord_cartesian(ylim = c(-0.2, 1)) +
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
        axis.title.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14),
        axis.text = element_text(size = 12),    
        strip.text = element_text(size = 14),   
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic"))


COR_GRF_FIXED_plot <- ggarrange(
  cor.GRF + rremove("xlab"),  # Remove individual x labels
  cor.FIXED + rremove("xlab"),
  common.legend = TRUE,  
  ncol = 1, nrow = 2, 
  legend = "bottom",
  align = "v",
  labels = c("(a)", "(b)"),
  heights = c(1, 1)
)

COR_GRF_FIXED_plot <- annotate_figure(
  COR_GRF_FIXED_plot, 
  bottom = text_grob("Relative random effect contribution", size = 14, vjust = 0.5)  # Adjust vertical position
) 


ggsave(plot = COR_GRF_FIXED_plot, filename = paste0(file.path(result_path),"/Supp.Fig.15.png"), w = 21, h = 16, units = "cm", dpi = 400, device = "png")


