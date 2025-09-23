
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

final.df2 <- final.df %>% 
  mutate(mod.name = ifelse(mod.type2 == "m.PA", "Presence-absence", 
                           ifelse(mod.type2 == "m.PO", "Presence-only", "Integrated")))

final.df2$mod.name <- factor(final.df2$mod.name, 
                             levels = c("Presence-only", "Presence-absence", "Integrated"))


projection1 <- final.df2 %>%
  ggplot(aes(x = relative.GRF, y = RMSE.global, fill = mod.name)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "RMSE projection site", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  scale_color_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  coord_cartesian(ylim = c(NA, 3)) +
  theme_bw() +
  facet_wrap(~bias.type) +
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

projection2 <-  final.df2 %>%
  ggplot(aes(x = relative.GRF, y = log(Mean.Int.Score), fill = mod.name)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Log of mean interval score", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  scale_color_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  coord_cartesian(ylim = c(NA, 1.75)) +
  theme_bw() +
  facet_wrap(~bias.type) +
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
projection3 <-final.df2 %>%
  ggplot(aes(x = relative.GRF, y = correlation, fill = mod.name)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Correlation", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  scale_color_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_bw() +
  facet_wrap(~bias.type) +
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
projection4 <- final.df2 %>%
  ggplot(aes(x = relative.GRF, y = coverage.rate, fill = mod.name)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Coverage probability", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "red") +  # Dashed horizontal line
  scale_fill_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  scale_color_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  #coord_cartesian(ylim = c(0, 0.1)) +
  theme_bw() +
  facet_wrap(~bias.type) +
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



# Supplementary Figure 8  -------------------------------------------------

# Figure 4 but mean interval score on original scale

projection2 <-  final.df2 %>%
  ggplot(aes(x = relative.GRF, y = Mean.Int.Score, fill = mod.name)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Mean interval score", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  scale_color_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  coord_cartesian(ylim = c(NA, 1.75)) +
  theme_bw() +
  facet_wrap(~bias.type) +
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


Supp.Fig.8 <- ggarrange(projection1 + rremove("xlab"), projection2 + rremove("xlab"), projection4 + rremove("xlab"), projection3, common.legend = TRUE, ncol = 1, nrow = 4, legend = "bottom", labels = c("(a)", "(b)", "(c)", "(d)"), align = "v")

ggsave(plot = Supp.Fig.8, filename = paste0(file.path(result_path),"/Supp.Fig.8.png"), w = 21, h = 30, units = "cm", dpi = 400, device = "png")


######################################################
########### LOOKING AT RESULTS RANGE SCENARIO ##########
######################################################


# Supplementary Figure 7 --------------------------------------------------

x.discrete.label <- c(as.character(scal))

final.df2 <- final.df2 %>% 
  mutate(spat.auto.range = ifelse(extrap.type == "Low", 20, 
                                  ifelse(extrap.type == "Moderate", 300, 
                                         700))) %>% 
  mutate(relative.range.dist = spat.auto.range/ Site.distance)


projection1 <- final.df2 %>%
  ggplot(aes(x = extrap.type, y = RMSE.global, fill = mod.name)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Spatial autocorrelation range", y = "RMSE projection site", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  scale_color_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  coord_cartesian(ylim = c(NA, 3)) +
  theme_bw() +
  facet_wrap(~bias.type) +
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

projection2 <-  final.df2 %>%
  ggplot(aes(x = extrap.type, y = log(Mean.Int.Score), fill = mod.name)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Spatial autocorrelation range", y = "Log of mean interval score", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  scale_color_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  coord_cartesian(ylim = c(NA, 1.75)) +
  theme_bw() +
  facet_wrap(~bias.type) +
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
projection3 <-final.df2 %>%
  ggplot(aes(x = extrap.type, y = correlation, fill = mod.name)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Spatial autocorrelation range", y = "Correlation", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  scale_color_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_bw() +
  facet_wrap(~bias.type) +
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
projection4 <- final.df2 %>%
  ggplot(aes(x = extrap.type, y = coverage.rate, fill = mod.name)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Spatial autocorrelation range", y = "Coverage probability", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "red") +  # Dashed horizontal line
  scale_fill_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  scale_color_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  #coord_cartesian(ylim = c(0, 0.1)) +
  theme_bw() +
  facet_wrap(~bias.type) +
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

Supp.Fig.7 <- ggarrange(projection1 + rremove("xlab"), projection2 + rremove("xlab"), projection4 + rremove("xlab"), projection3, common.legend = TRUE, ncol = 1, nrow = 4, legend = "bottom", labels = c("(a)", "(b)", "(c)", "(d)"), align = "v")

ggsave(plot = Supp.Fig.7, filename = paste0(file.path(result_path),"/Supp.Fig.7.png"), w = 21, h = 30, units = "cm", dpi = 400, device = "png")



##########################################################################
##########################################################################
# Scenario Without bias covariate -----------------------------------------------
##########################################################################
##########################################################################


# Supplementary Figure 10 --------------------------------------------------


final.df.bias <- final.df.bias %>%
  mutate(bias.type2 = case_when(
    mod.type2 == "m.PA" ~ "Without bias covariate",
    TRUE ~ bias.type2
  ))

final.df.bias2 <- final.df.bias %>% 
  mutate(mod.name = ifelse(grepl("m.PA", mod.type2), "Presence-absence", 
                           ifelse(grepl("m.PO", mod.type2), "Presence-only", "Integrated")))

final.df.bias2$mod.name <- factor(final.df.bias2$mod.name, 
                                  levels = c("Presence-only", "Presence-absence", "Integrated"))

projection1 <- final.df.bias2 %>%
  ggplot(aes(x = extrap.type, y = RMSE.global, fill = mod.name, linetype = bias.type2)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Spatial autocorrelation range", y = "RMSE projection site", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  scale_color_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  scale_linetype_manual(values = c("Without bias covariate" = "dotted", "With bias covariate" = "solid")) +  # Different line types for bias types
  coord_cartesian(ylim = c(NA, 3.5)) +
  theme_bw() +
  facet_wrap(~bias.type) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(size = 14, vjust = 1.7),
        axis.title.x = element_text(size = 13.5),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic"))

projection2 <-  final.df.bias2 %>%
  ggplot(aes(x = extrap.type, y = log(Mean.Int.Score), fill = mod.name, linetype = bias.type2)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Spatial autocorrelation range", y = "Log of mean interval score", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  scale_color_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  scale_linetype_manual(values = c("Without bias covariate" = "dotted", "With bias covariate" = "solid")) +  # Different line types for bias types
  coord_cartesian(ylim = c(NA, 2.3)) +
  theme_bw() +
  facet_wrap(~bias.type) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(size = 14, vjust = 1.7),
        axis.title.x = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) 

# Correlation
projection3 <-final.df.bias2 %>%
  ggplot(aes(x = extrap.type, y = correlation, fill = mod.name, linetype = bias.type2)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Spatial autocorrelation range", y = "Correlation", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  scale_color_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  scale_linetype_manual(values = c("Without bias covariate" = "dotted", "With bias covariate" = "solid")) +  # Different line types for bias types
  coord_cartesian(ylim = c(0, NA)) +
  theme_bw() +
  facet_wrap(~bias.type) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(size = 14, vjust = 1.7),
        axis.title.x = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) 

# Coverage probability
projection4 <- final.df.bias2 %>%
  ggplot(aes(x = extrap.type, y = coverage.rate, fill = mod.name, linetype = bias.type2)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Spatial autocorrelation range", y = "Coverage probability", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "red") +  # Dashed horizontal line
  scale_fill_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  scale_color_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  scale_linetype_manual(values = c("Without bias covariate" = "dotted", "With bias covariate" = "solid")) +  # Different line types for bias types
  #coord_cartesian(ylim = c(0, 0.1)) +
  theme_bw() +
  facet_wrap(~bias.type) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(size = 14, vjust = 1.7),
        axis.title.x = element_text(size = 13.5),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) 

Supp.Fig.10 <- ggarrange(projection1 + rremove("xlab"), projection2 + rremove("xlab"), projection4 + rremove("xlab"), projection3, common.legend = TRUE, ncol = 1, nrow = 4, legend = "bottom", labels = c("(a)", "(b)", "(c)", "(d)"), align = "v")

ggsave(plot = Supp.Fig.10, filename = paste0(file.path(result_path),"/Supp.Fig.10.png"), w = 21, h = 30, units = "cm", dpi = 400, device = "png")


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


###### FIGURE 5 ##########

final.SiteA.df.bias2 <- final.SiteA.df.bias %>% 
  mutate(mod.name = ifelse(mod.type2 == "m.PA", "Presence-absence", 
                           ifelse(mod.type2 == "m.PO", "Presence-only", "Integrated"))) %>% 
  mutate(bias.type2 = ifelse(is.na(bias.type2), "Bias absent", 
                             ifelse(bias.type2 == "Without bias covariate", "Bias present - no bias covariate",
                                    "Bias present - with bias covariate")))



final.df2$mod.name <- factor(final.df2$mod.name, 
                             levels = c("Presence-only", "Presence-absence", "Integrated"))

cor.GRF <- final.SiteA.df.bias2 %>% 
  filter(bias.type == "With Gaussian random field") %>% 
  mutate(mod.name = factor(mod.name, levels = c("Presence-only", "Presence-absence", "Integrated"))) %>%
  mutate(bias.type2 = factor(bias.type2, levels = c("Bias present - no bias covariate", "Bias present - with bias covariate", "Bias absent"))) %>%
  ggplot(aes(x = relative.GRF, y = cor.GRF, fill = mod.name)) +  
  geom_boxplot(alpha = 0.6, width = 0.5, outlier.shape = NA, aes(fill = mod.name)) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Correlation", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.3)) +
  scale_fill_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  scale_color_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) + 
  coord_cartesian(ylim = c(-0.2, 1)) +
  theme_bw() +
  facet_wrap(~bias.type2) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 13.5),  
        panel.grid.major.x = element_blank(),   
        panel.grid.minor.x = element_blank(),   
        axis.title.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14),
        axis.text = element_text(size = 12),    
        strip.text = element_text(size = 11.5),   
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic"))

RMSE.GRF <- final.SiteA.df.bias2 %>% 
  filter(bias.type == "With Gaussian random field") %>% 
  mutate(mod.name = factor(mod.name, levels = c("Presence-only", "Presence-absence", "Integrated"))) %>%
  mutate(bias.type2 = factor(bias.type2, levels = c("Bias present - no bias covariate", "Bias present - with bias covariate", "Bias absent"))) %>%
  ggplot(aes(x = relative.GRF, y = RMSE.global.GRF, fill = mod.name)) +  
  geom_boxplot(alpha = 0.6, width = 0.5, outlier.shape = NA, aes(fill = mod.name)) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "RMSE", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  scale_color_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) + 
  coord_cartesian(ylim = c(NA, 1.6)) +
  theme_bw() +
  facet_wrap(~bias.type2) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 13.5),  
        panel.grid.major.x = element_blank(),   
        panel.grid.minor.x = element_blank(),   
        axis.title.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14),
        axis.text = element_text(size = 12),    
        strip.text = element_text(size = 11.5),   
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic"))

COR_RMSE_GRF_plot <- ggarrange(
  cor.GRF + rremove("xlab"),  # Remove individual x labels
  RMSE.GRF + rremove("xlab"),
  common.legend = TRUE,  
  ncol = 1, nrow = 2, 
  legend = "bottom",
  align = "v",
  labels = c("(a)", "(b)"),
  heights = c(1, 1)
)

COR_RMSE_GRF_plot <- annotate_figure(
  COR_RMSE_GRF_plot, 
  bottom = text_grob("Relative random effect contribution", size = 14, vjust = 0.5)  # Adjust vertical position
) 


ggsave(plot = COR_RMSE_GRF_plot, filename = paste0(file.path(result_path),"/Figure_5.png"), w = 21, h = 16, units = "cm", dpi = 400, device = "png")  




# Supplementary Figure 11 -------------------------------------------------

# Figure 4 but training site
final.SiteA.df <- final.SiteA.df %>% 
  mutate(mod.name = ifelse(mod.type2 == "m.PA", "Presence-absence", 
                           ifelse(mod.type2 == "m.PO", "Presence-only", "Integrated")))



final.SiteA.df$mod.name <- factor(final.SiteA.df$mod.name, 
                                  levels = c("Presence-only", "Presence-absence", "Integrated"))

training1 <- final.SiteA.df %>%
  ggplot(aes(x = relative.GRF, y = RMSE.global, fill = mod.name)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "RMSE projection site", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  scale_color_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  coord_cartesian(ylim = c(NA, 3)) +
  theme_bw() +
  facet_wrap(~bias.type) +
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

training2 <-  final.SiteA.df %>%
  ggplot(aes(x = relative.GRF, y = log(Mean.Int.Score), fill = mod.name)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Log of mean interval score", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  scale_color_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  coord_cartesian(ylim = c(NA, 1.75)) +
  theme_bw() +
  facet_wrap(~bias.type) +
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
training3 <-final.SiteA.df %>%
  ggplot(aes(x = relative.GRF, y = correlation, fill = mod.name)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Correlation", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  scale_color_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_bw() +
  facet_wrap(~bias.type) +
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
training4 <- final.SiteA.df %>%
  ggplot(aes(x = relative.GRF, y = coverage.rate, fill = mod.name)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Coverage probability", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "red") +  # Dashed horizontal line
  scale_fill_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  scale_color_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  #coord_cartesian(ylim = c(0, 0.1)) +
  theme_bw() +
  facet_wrap(~bias.type) +
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

Supp.Fig.11 <- ggarrange(training1 + rremove("xlab"), training2 + rremove("xlab"), training4 + rremove("xlab"), training3, common.legend = TRUE, ncol = 1, nrow = 4, legend = "bottom", labels = c("(a)", "(b)", "(c)", "(d)"), align = "v")

ggsave(plot = Supp.Fig.11, filename = paste0(file.path(result_path),"/Supp.Fig.11.png"), w = 21, h = 30, units = "cm", dpi = 400, device = "png")


#################################################
# SCENARIO 4 VERSION -----------------------------------------------------
# Lower PO record numbers
#################################################


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


################## SUPP. FIGURE 9 #####################
# Figure 4 but Lower PO record numbers


# Filter out just the models that account for bias with covariate
final.df <- final.df %>% 
  filter(bias.type2 == "With bias covariate" | mod.type2 == "m.PA")

final.df2 <- final.df %>% 
  mutate(mod.name = ifelse(mod.type2 == "m.PA", "Presence-absence", 
                           ifelse(mod.type2 == "m.PO", "Presence-only", "Integrated")))

final.df2$mod.name <- factor(final.df2$mod.name, 
                                levels = c("Presence-only", "Presence-absence", "Integrated"))


projection1 <- final.df2 %>%
  ggplot(aes(x = relative.GRF, y = RMSE.global, fill = mod.name)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "RMSE projection site", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  scale_color_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  coord_cartesian(ylim = c(NA, 3)) +
  theme_bw() +
  facet_wrap(~bias.type) +
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

projection2 <-  final.df2 %>%
  ggplot(aes(x = relative.GRF, y = log(Mean.Int.Score), fill = mod.name)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Log of mean interval score", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  scale_color_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  coord_cartesian(ylim = c(NA, 1.75)) +
  theme_bw() +
  facet_wrap(~bias.type) +
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
projection3 <-final.df2 %>%
  ggplot(aes(x = relative.GRF, y = correlation, fill = mod.name)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Correlation", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  scale_color_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_bw() +
  facet_wrap(~bias.type) +
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
projection4 <- final.df2 %>%
  ggplot(aes(x = relative.GRF, y = coverage.rate, fill = mod.name)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Coverage probability", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "red") +  # Dashed horizontal line
  scale_fill_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  scale_color_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  #coord_cartesian(ylim = c(0, 0.1)) +
  theme_bw() +
  facet_wrap(~bias.type) +
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

Supp.Fig.9 <- ggarrange(projection1 + rremove("xlab"), projection2 + rremove("xlab"), projection4 + rremove("xlab"), projection3, common.legend = TRUE, ncol = 1, nrow = 4, legend = "bottom", labels = c("(a)", "(b)", "(c)", "(d)"), align = "v")

ggsave(plot = Supp.Fig.9, filename = paste0(file.path(result_path),"/Supp.Fig.9.png"), w = 21, h = 30, units = "cm", dpi = 400, device = "png")


#########################################
###### Supplementary Figure 12 ##########
#########################################


true.validation.SiteA.df_4 <- true.validation.SiteA.df_4 %>% 
  mutate(relative.GRF = "1")

true.validation.SiteA.df_4v0.2 <- true.validation.SiteA.df_4v0.2 %>% 
  mutate(relative.GRF = "0.2")

true.validation.SiteA.df_4v5 <- true.validation.SiteA.df_4v5 %>% 
  mutate(relative.GRF = "5")

final.SiteA.df_5C <- rbind(true.validation.SiteA.df_4, true.validation.SiteA.df_4v0.2, true.validation.SiteA.df_4v5)


final.SiteA.df_4$bias.type <- factor(final.SiteA.df_4$bias.type, levels = c("Without Gaussian random field", "With Gaussian random field"))
final.SiteA.df_4$mod.type2 <- factor(final.SiteA.df_4$mod.type2, levels = c("m.PO", "m.PA", "m.int"))

# Save a version for bias comparison
final.SiteA.df_4.bias <- final.SiteA.df_4

final.SiteA.df_4 <- final.SiteA.df_4 %>% 
  filter(bias.type2 == "With bias covariate" | mod.type2 == "m.PA")

# Figure 5 but with Lower PO record numbers

final.SiteA.df_4.bias <- final.SiteA.df_4.bias %>% 
  mutate(mod.name = ifelse(mod.type2 == "m.PA", "Presence-absence", 
                           ifelse(mod.type2 == "m.PO", "Presence-only", "Integrated"))) %>% 
  mutate(bias.type2 = ifelse(is.na(bias.type2), "Bias absent", 
                             ifelse(bias.type2 == "Without bias covariate", "Bias present - no bias covariate",
                                    "Bias present - with bias covariate")))



final.SiteA.df_4.bias$mod.name <- factor(final.SiteA.df_4.bias$mod.name, 
                                          levels = c("Presence-only", "Presence-absence", "Integrated"))


cor.GRF <- final.SiteA.df_4.bias %>% 
  filter(bias.type == "With Gaussian random field") %>% 
  mutate(mod.name = factor(mod.name, levels = c("Presence-only", "Presence-absence", "Integrated"))) %>%
  mutate(bias.type2 = factor(bias.type2, levels = c("Bias present - no bias covariate", "Bias present - with bias covariate", "Bias absent"))) %>%
  ggplot(aes(x = relative.GRF, y = cor.GRF, fill = mod.name)) +  
  geom_boxplot(alpha = 0.6, width = 0.5, outlier.shape = NA, aes(fill = mod.name)) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Correlation", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.3)) +
  scale_fill_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  scale_color_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) + 
  coord_cartesian(ylim = c(-0.2, 1)) +
  theme_bw() +
  facet_wrap(~bias.type2) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 13.5),  
        panel.grid.major.x = element_blank(),   
        panel.grid.minor.x = element_blank(),   
        axis.title.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14),
        axis.text = element_text(size = 12),    
        strip.text = element_text(size = 11.5),   
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic"))

RMSE.GRF <- final.SiteA.df_4.bias %>% 
  filter(bias.type == "With Gaussian random field") %>% 
  mutate(mod.name = factor(mod.name, levels = c("Presence-only", "Presence-absence", "Integrated"))) %>%
  mutate(bias.type2 = factor(bias.type2, levels = c("Bias present - no bias covariate", "Bias present - with bias covariate", "Bias absent"))) %>%
  ggplot(aes(x = relative.GRF, y = RMSE.global.GRF, fill = mod.name)) +  
  geom_boxplot(alpha = 0.6, width = 0.5, outlier.shape = NA, aes(fill = mod.name)) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "RMSE", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) +
  scale_color_manual(values = c("Presence-only" = "skyblue", "Presence-absence" = "orange", "Integrated" = "purple")) + 
  coord_cartesian(ylim = c(NA, 1.6)) +
  theme_bw() +
  facet_wrap(~bias.type2) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 13.5),  
        panel.grid.major.x = element_blank(),   
        panel.grid.minor.x = element_blank(),   
        axis.title.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14),
        axis.text = element_text(size = 12),    
        strip.text = element_text(size = 11.5),   
        strip.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 1, size = 15, face = "italic"))

COR_RMSE_GRF_plot <- ggarrange(
  cor.GRF + rremove("xlab"),  # Remove individual x labels
  RMSE.GRF + rremove("xlab"),
  common.legend = TRUE,  
  ncol = 1, nrow = 2, 
  legend = "bottom",
  align = "v",
  labels = c("(a)", "(b)"),
  heights = c(1, 1)
)

Supp.Fig.12 <- annotate_figure(
  COR_RMSE_GRF_plot, 
  bottom = text_grob("Relative random effect contribution", size = 14, vjust = 0.5)  # Adjust vertical position
) 


ggsave(plot = COR_RMSE_GRF_plot, filename = paste0(file.path(result_path),"/Supp.Fig.12.png"), w = 21, h = 16, units = "cm", dpi = 400, device = "png")  






