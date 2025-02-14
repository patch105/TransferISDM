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


x.discrete.label <- c("Low", "Mod", "High")  

#####################################################
####### CHOOSE NUMBER OF REPLICATES TO KEEP ########
#####################################################

replicates <- 100

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
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF")) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type = factor(mod.type, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))

true.validation.SiteA.df <- true.validation.SiteA.df %>%
  mutate(
    bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF"),
    mod.type2 = ifelse(grepl("GRF", as.character(mod.type), fixed = T), 
                       gsub(".GRF", "", as.character(mod.type)), 
                       as.character(mod.type))
  ) %>%
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int")))

extrap.scenario.df <- extrap.scenario.df %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF")) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type = factor(mod.type, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))

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

##########################################################
### REMOVE REPLICATES THAT DON'T HAVE ALL THREE OUTPUTS ###
##########################################################

validation <- true.validation.df_2C %>%
  group_by(job_index) %>% 
  summarise(n = n()) %>% 
  select(job_index)

extrap <- extrap.scenario.df_2C %>%
  group_by(job_index) %>% 
  summarise(n = n()) %>% 
  select(job_index)

validationA <- true.validation.SiteA.df_2C %>%
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

true.validation.df_2C <- true.validation.df_2C %>%
  filter(job_index %in% common_jobs$job_index)

true.validation.SiteA.df_2C <- true.validation.SiteA.df_2C %>%
  filter(job_index %in% common_jobs$job_index)

extrap.scenario.df_2C <- extrap.scenario.df_2C %>%
  filter(job_index %in% common_jobs$job_index)


true.validation.df_2C <- true.validation.df_2C %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF")) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type = factor(mod.type, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))

true.validation.SiteA.df_2C <- true.validation.SiteA.df_2C %>%
  mutate(
    bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF"),
    mod.type2 = ifelse(grepl("GRF", as.character(mod.type), fixed = T), 
                       gsub(".GRF", "", as.character(mod.type)), 
                       as.character(mod.type))
  ) %>%
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int")))

extrap.scenario.df_2C <- extrap.scenario.df_2C %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF")) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type = factor(mod.type, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))

######################################################################
################ SCENARIO 2v0.2 - SPATIAL AUTOCORRELATION ################
######################################################################

scenario_name = "2v0.2"

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
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF")) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type = factor(mod.type, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))

true.validation.SiteA.dfv0.2 <- true.validation.SiteA.dfv0.2 %>%
  mutate(
    bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF"),
    mod.type2 = ifelse(grepl("GRF", as.character(mod.type), fixed = T), 
                       gsub(".GRF", "", as.character(mod.type)), 
                       as.character(mod.type))
  ) %>%
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int")))

extrap.scenario.dfv0.2 <- extrap.scenario.dfv0.2 %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF")) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type = factor(mod.type, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))

######################################################################
################ SCENARIO 2Cv0.2 - SPATIAL AUTOCORRELATION C ################
######################################################################

scenario_name = "2Cv0.2"

load(file = paste0(file.path(outpath, scenario_name), "/Scenario_", scenario_name, "_Input_Params.RData"))

# Load and join replicates together ---------------------------------------


# List all the files with validation dataframes (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_Job", full.names = TRUE, recursive = TRUE) 

true.validation.df.list <- lapply(file_list, read.csv)

true.validation.df_2Cv0.2 <- do.call(rbind, true.validation.df.list)


# THEN list all the files with validation dataframes for Site A (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_SiteA_Job", full.names = TRUE, recursive = TRUE) 

true.validation.SiteA.df.list <- lapply(file_list, read.csv)

true.validation.SiteA.df_2Cv0.2 <- do.call(rbind, true.validation.SiteA.df.list)


# FINALLY list all the files with model outputs (for each job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "Results_Summary", full.names = TRUE, recursive = TRUE) 

# Read all the CSV files and save them to a list
extrap.scenario.df.list <- lapply(file_list, read.csv)

# Combine all dataframes in the list into one dataframe using rbind
extrap.scenario.df_2Cv0.2 <- do.call(rbind, extrap.scenario.df.list)

##########################################################
### REMOVE REPLICATES THAT DON'T HAVE ALL THREE OUTPUTS ###
##########################################################

validation <- true.validation.df_2Cv0.2 %>%
  group_by(job_index) %>% 
  summarise(n = n()) %>% 
  select(job_index)

extrap <- extrap.scenario.df_2Cv0.2 %>%
  group_by(job_index) %>% 
  summarise(n = n()) %>% 
  select(job_index)

validationA <- true.validation.SiteA.df_2Cv0.2 %>%
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

true.validation.df_2Cv0.2 <- true.validation.df_2Cv0.2 %>%
  filter(job_index %in% common_jobs$job_index)

true.validation.SiteA.df_2Cv0.2 <- true.validation.SiteA.df_2Cv0.2 %>%
  filter(job_index %in% common_jobs$job_index)

extrap.scenario.df_2Cv0.2 <- extrap.scenario.df_2Cv0.2 %>%
  filter(job_index %in% common_jobs$job_index)



true.validation.df_2Cv0.2 <- true.validation.df_2Cv0.2 %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF")) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type = factor(mod.type, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))

true.validation.SiteA.df_2Cv0.2 <- true.validation.SiteA.df_2Cv0.2 %>%
  mutate(
    bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF"),
    mod.type2 = ifelse(grepl("GRF", as.character(mod.type), fixed = T), 
                       gsub(".GRF", "", as.character(mod.type)), 
                       as.character(mod.type))
  ) %>%
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int")))

extrap.scenario.df_2Cv0.2 <- extrap.scenario.df_2Cv0.2 %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF")) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type = factor(mod.type, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))

######################################################################
################ SCENARIO 2v5 - SPATIAL AUTOCORRELATION ################
######################################################################

scenario_name = "2v5"

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
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF")) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type = factor(mod.type, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))

true.validation.SiteA.dfv5 <- true.validation.SiteA.dfv5 %>%
  mutate(
    bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF"),
    mod.type2 = ifelse(grepl("GRF", as.character(mod.type), fixed = T), 
                       gsub(".GRF", "", as.character(mod.type)), 
                       as.character(mod.type))
  ) %>%
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int")))

extrap.scenario.dfv5 <- extrap.scenario.dfv5 %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF")) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type = factor(mod.type, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))

######################################################################
################ SCENARIO 2Cv5 - SPATIAL AUTOCORRELATION C ################
######################################################################

scenario_name = "2Cv5"

load(file = paste0(file.path(outpath, scenario_name), "/Scenario_", scenario_name, "_Input_Params.RData"))

# Load and join replicates together ---------------------------------------


# List all the files with validation dataframes (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_Job", full.names = TRUE, recursive = TRUE) 

true.validation.df.list <- lapply(file_list, read.csv)

true.validation.df_2Cv5 <- do.call(rbind, true.validation.df.list)


# THEN list all the files with validation dataframes for Site A (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation_SiteA_Job", full.names = TRUE, recursive = TRUE) 

true.validation.SiteA.df.list <- lapply(file_list, read.csv)

true.validation.SiteA.df_2Cv5 <- do.call(rbind, true.validation.SiteA.df.list)


# FINALLY list all the files with model outputs (for each job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "Results_Summary", full.names = TRUE, recursive = TRUE) 

# Read all the CSV files and save them to a list
extrap.scenario.df.list <- lapply(file_list, read.csv)

# Combine all dataframes in the list into one dataframe using rbind
extrap.scenario.df_2Cv5 <- do.call(rbind, extrap.scenario.df.list)


##########################################################
### REMOVE REPLICATES THAT DON'T HAVE ALL THREE OUTPUTS ###
##########################################################

validation <- true.validation.df_2Cv5 %>%
  group_by(job_index) %>% 
  summarise(n = n()) %>% 
  select(job_index)

extrap <- extrap.scenario.df_2Cv5 %>%
  group_by(job_index) %>% 
  summarise(n = n()) %>% 
  select(job_index)

validationA <- true.validation.SiteA.df_2Cv5 %>%
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

true.validation.df_2Cv5 <- true.validation.df_2Cv5 %>%
  filter(job_index %in% common_jobs$job_index)

true.validation.SiteA.df_2Cv5 <- true.validation.SiteA.df_2Cv5 %>%
  filter(job_index %in% common_jobs$job_index)

extrap.scenario.df_2Cv5 <- extrap.scenario.df_2Cv5 %>%
  filter(job_index %in% common_jobs$job_index)



true.validation.df_2Cv5 <- true.validation.df_2Cv5 %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF")) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type = factor(mod.type, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))


true.validation.SiteA.df_2Cv5 <- true.validation.SiteA.df_2Cv5 %>%
  mutate(
    bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF"),
    mod.type2 = ifelse(grepl("GRF", as.character(mod.type), fixed = T), 
                       gsub(".GRF", "", as.character(mod.type)), 
                       as.character(mod.type))
  ) %>%
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int")))

extrap.scenario.df_2Cv5 <- extrap.scenario.df_2Cv5 %>%
  mutate(bias.type = ifelse(grepl("GRF", mod.type, fixed = T), "With GRF", "Without GRF")) %>% 
  mutate(mod.type2 = ifelse(grepl("GRF", mod.type, fixed = T), gsub(".GRF", "", mod.type), mod.type)) %>% 
  mutate(mod.type = factor(mod.type, levels = c("m.PO", "m.PO.GRF", "m.PA", "m.PA.GRF", "m.int", "m.int.GRF")))


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

final.df$bias.type <- factor(final.df$bias.type, levels = c("Without GRF", "With GRF"))

final.df$mod.type2 <- factor(final.df$mod.type2, levels = c("m.PO", "m.PA", "m.int"))

######## Projection site  ############

projection1 <- final.df %>%
  ggplot(aes(x = relative.GRF, y = RMSE.global, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Mean RMSE", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  coord_cartesian(ylim = c(NA, 4)) +
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
  ggplot(aes(x = relative.GRF, y = Mean.Int.Score, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Mean Interval Score", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  coord_cartesian(ylim = c(NA, 4.5)) +
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
  ggplot(aes(x = relative.GRF, y = correlation, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Correlation", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
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
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) 


fig2 <- ggarrange(projection1 + rremove("xlab"), projection2 + rremove("xlab"), projection3, common.legend = TRUE, ncol = 1, nrow = 3, legend = "bottom", labels = c("(a)", "(b)", "(c)"), align = "v")

ggsave(plot = fig2, filename = paste0(file.path(result_path),"/Scenario_2_tests/projection_site.png"), w = 21, h = 27, units = "cm", dpi = 400, device = "png")

# Coverage probability
projection4 <-  final.df %>%
  ggplot(aes(x = relative.GRF, y = coverage.rate, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Mean coverage probability", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
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

ggsave(plot = fig2b, filename = paste0(file.path(result_path),"/Scenario_2_tests/projection_siteV2.png"), w = 21, h = 30, units = "cm", dpi = 400, device = "png")

########### COEFFICIENT RECOVERY ###############

extrap.scenario.df <- extrap.scenario.df %>% 
  mutate(relative.GRF = "1")

extrap.scenario.dfv0.2 <- extrap.scenario.dfv0.2 %>% 
  mutate(relative.GRF = "0.2")

extrap.scenario.dfv5 <- extrap.scenario.dfv5 %>% 
  mutate(relative.GRF = "5")

final.extrap.df <- rbind(extrap.scenario.df, extrap.scenario.dfv0.2, extrap.scenario.dfv5)


final.extrap.df$bias.type <- factor(final.extrap.df$bias.type, levels = c("Without GRF", "With GRF"))
final.extrap.df$mod.type2 <- factor(final.extrap.df$mod.type2, levels = c("m.PO", "m.PA", "m.int"))

coef1 <- final.extrap.df %>%
  ggplot(aes(x = relative.GRF, y = beta1.mean, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = expression(beta[1]), color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  geom_hline(yintercept = beta1, linetype = "dashed", color = "red") +
  coord_cartesian(ylim = c(-1, 1)) +
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
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  geom_hline(yintercept = beta2, linetype = "dashed", color = "red") +
  coord_cartesian(ylim = c(-1, 1)) +
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

ggsave(plot = Supp.Fig.X, filename = paste0(file.path(result_path),"/Scenario_2_tests/Coefficients.png"), w = 21, h = 17.5, units = "cm", dpi = 400, device = "png")

PO_int <- final.extrap.df %>%
  filter(PO_intercept != 0 ) %>% 
  ggplot(aes(x = relative.GRF, y = PO_intercept, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = expression(beta[0]), color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
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
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
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

ggsave(plot = Supp.Fig.X, filename = paste0(file.path(result_path),"/Scenario_2_tests/Intercepts.png"), w = 23.5, h = 15, units = "cm", dpi = 400, device = "png")

coefSD1 <- final.extrap.df %>%
  ggplot(aes(x = relative.GRF, y = beta1.sd, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = expression(beta[1]~"SD"), color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  coord_cartesian(ylim = c(NA, 0.7)) +
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
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  coord_cartesian(ylim = c(0, 0.7)) +
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

ggsave(plot = Supp.Fig.X, filename = paste0(file.path(result_path),"/Scenario_2_tests/Coefficient_SDs.png"), w = 21, h = 17.5, units = "cm", dpi = 400, device = "png")

#######################################################
################ GRF HYPERPARAMETERS ##################
#######################################################

g1 <- final.extrap.df %>%
  filter(bias.type == "With GRF") %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int"))) %>% 
  mutate(scal = ifelse(extrap.type == "Low", 20, ifelse(extrap.type == "Moderate", 100, 200))) %>%
  ggplot(aes(x = relative.GRF, y = scal - GRF.range.mean, fill = mod.type2)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Bias of GRF range", color = NULL, fill = "Model Type") +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(-300, 200)) +
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

g2 <- final.extrap.df %>%
  filter(bias.type == "With GRF") %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int"))) %>% 
  ggplot(aes(x = relative.GRF, y = GRF.variance - (GRF.sd.mean)^2, fill = mod.type2)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Bias of GRF variance", color = NULL, fill = "Model Type") +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(-5, 2)) +
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


Supp.Fig.X <- ggarrange(g1 + rremove("xlab"), g2 + rremove("xlab") , common.legend = T,  ncol = 1, nrow = 2, legend = "bottom", labels = c("(a)", "(b)"))

ggsave(plot = Supp.Fig.X, filename = paste0(file.path(result_path),"/Scenario_2_tests/True_vs_estimated_GRF_params.png"), w = 21, h = 17.5, units = "cm", dpi = 400, device = "png")


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


final.SiteA.df$bias.type <- factor(final.SiteA.df$bias.type, levels = c("Without GRF", "With GRF"))
final.SiteA.df$mod.type2 <- factor(final.SiteA.df$mod.type2, levels = c("m.PO", "m.PA", "m.int"))


training1 <- final.SiteA.df %>%
  ggplot(aes(x = relative.GRF, y = RMSE.global, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Mean RMSE", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  coord_cartesian(ylim = c(NA, 4)) +
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
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
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
        plot.title = element_text(hjust = 1, size = 15, face = "italic"))

training3 <- final.SiteA.df %>%
  ggplot(aes(x = relative.GRF, y = correlation, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Correlation", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
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
        plot.title = element_text(hjust = 1, size = 15, face = "italic"))

fig2 <- ggarrange(training1 + rremove("xlab"), training2 + rremove("xlab"), training3, common.legend = TRUE, ncol = 1, nrow = 3, legend = "bottom", labels = c("(a)", "(b)", "(c)"), align = "v")

ggsave(plot = fig2, filename = paste0(file.path(result_path),"/Scenario_2_tests/training_site.png"), w = 21, h = 27, units = "cm", dpi = 400, device = "png")


# Figure3 <- ggarrange(fig + rremove("xlab"), fig2, common.legend = TRUE, ncol = 1, nrow = 2, legend = "bottom")
# 
# ggsave(plot = Figure3, filename = paste0(file.path(result_path),"/Scenario_2_tests/FIGURE_3.png"), w = 21, h = 32, units = "cm", dpi = 400, device = "png")

training4 <- final.SiteA.df %>%
  ggplot(aes(x = relative.GRF, y = coverage.rate, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Mean coverage probability", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
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

ggsave(plot = fig2b, filename = paste0(file.path(result_path),"/Scenario_2_tests/training_site.png"), w = 21, h = 30, units = "cm", dpi = 400, device = "png")

########## CORRELATION BETWEEN ESTIMATED AND TRUE FIXED & GRF #############

cor.GRF <- final.SiteA.df %>%
  filter(bias.type == "With GRF") %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int"))) %>% 
  mutate(scal = ifelse(extrap.type == "Low", 20, ifelse(extrap.type == "Moderate", 100, 200))) %>%
  ggplot(aes(x = relative.GRF, y = cor.GRF, fill = mod.type2)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Correlation random", color = NULL, fill = "Model Type") +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = fill.colours, guide = "none") +
  #coord_cartesian(ylim = c(-300, 200)) +
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

RMSE.GRF <- final.SiteA.df %>%
  filter(bias.type == "With GRF") %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int"))) %>% 
  mutate(scal = ifelse(extrap.type == "Low", 20, ifelse(extrap.type == "Moderate", 100, 200))) %>%
  ggplot(aes(x = relative.GRF, y = RMSE.global.GRF, fill = mod.type2)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Mean RMSE random", color = NULL, fill = "Model Type") +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = fill.colours, guide = "none") +
  #coord_cartesian(ylim = c(-300, 200)) +
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

cor.FIXED <- final.SiteA.df %>%
  filter(bias.type == "With GRF") %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int"))) %>% 
  mutate(scal = ifelse(extrap.type == "Low", 20, ifelse(extrap.type == "Moderate", 100, 200))) %>%
  ggplot(aes(x = relative.GRF, y = cor.FIXED, fill = mod.type2)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Correlation fixed", color = NULL, fill = "Model Type") +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = fill.colours, guide = "none") +
  #coord_cartesian(ylim = c(-300, 200)) +
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

RMSE.FIXED <- final.SiteA.df %>%
  filter(bias.type == "With GRF") %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int"))) %>% 
  mutate(scal = ifelse(extrap.type == "Low", 20, ifelse(extrap.type == "Moderate", 100, 200))) %>%
  ggplot(aes(x = relative.GRF, y = RMSE.global.FIXED, fill = mod.type2)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Mean RMSE fixed", color = NULL, fill = "Model Type") +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = fill.colours, guide = "none") +
  #coord_cartesian(ylim = c(-300, 200)) +
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

COR_GRF_FIXED_plot <- ggarrange(cor.GRF + rremove("xlab"), cor.FIXED + rremove("xlab"), common.legend = T,  ncol = 2, nrow = 1)

RMSE_GRF_FIXED_plot <- ggarrange(RMSE.GRF + rremove("xlab"), RMSE.FIXED + rremove("xlab"), common.legend = T,  ncol = 2, nrow = 1)

Fig.4 <- ggarrange(COR_GRF_FIXED_plot, g1 + rremove("xlab"), g2, ncol = 1, nrow = 3, labels = c("(a)", "(b)", "(c)"))

Fig.4b <- ggarrange(RMSE_GRF_FIXED_plot, g1 + rremove("xlab"), g2, ncol = 1, nrow = 3, labels = c("(a)", "(b)", "(c)"))

ggsave(plot = Fig.4, filename = paste0(file.path(result_path),"/Scenario_2_tests/Correlation_random_fixed_GRF_params.png"), w = 21, h = 24, units = "cm", dpi = 400, device = "png")

ggsave(plot = Fig.4b, filename = paste0(file.path(result_path),"/Scenario_2_tests/RMSE_random_fixed.png"), w = 21, h = 24, units = "cm", dpi = 400, device = "png")


final.df %>% 
  ggplot(aes(x = Site.distance, y = RMSE.global, color = mod.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = T, aes(fill = mod.type), alpha = 0.3) +
  labs(x = "Site distance", y = "Mean RMSE", fill = "Model Type") +
  # scale_x_reverse() +  # Reverse x-axis from 1 to 0
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  #coord_cartesian(ylim = c(NA, 10)) +
  theme_bw() +
  facet_grid(
    mod.type ~ relative.GRF,
  ) +
  theme(
    axis.title = element_text(size = 11),    # Increase axis titles
    axis.text = element_text(size = 9),     # Increase axis text
    strip.text = element_text(size = 11),   # Increase facet title size
    strip.background = element_rect(fill = "gray96")
  )

final.df %>% 
  ggplot(aes(x = Site.distance, y = RMSE.global)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = T, alpha = 0.3) +
  theme_bw()




############################################################
########### 1. CORR BETWEEN GRF ACCURACY AND RESULTS  ########
############################################################

# Add the fixed and random effects columns to the extrap.scenario dataframe by matching extrap, rep, job, and model type

newdf <- final.extrap.df %>% 
  left_join(final.SiteA.df %>% 
              select(relative.GRF, extrap.type, job_index, mod.type, cor.GRF),
            by = c("relative.GRF", "extrap.type", "job_index", "mod.type"))


newdf <- newdf %>% 
  left_join(final.SiteA.df %>% 
              select(relative.GRF, extrap.type, job_index, mod.type, cor.FIXED),
            by = c("relative.GRF", "extrap.type", "job_index", "mod.type"))

newdf <- newdf %>% 
  left_join(final.df %>% 
              select(relative.GRF, extrap.type, job_index, mod.type, Mean.Int.Score),
            by = c("relative.GRF", "extrap.type", "job_index", "mod.type"))

newdf <- newdf %>% 
  left_join(final.SiteA.df %>% 
              select(relative.GRF, extrap.type, job_index, mod.type, RMSE.global.GRF),
            by = c("relative.GRF", "extrap.type", "job_index", "mod.type"))

plot1 <- newdf %>% 
  filter(!is.na(cor.GRF)) %>% 
  mutate(dist_coef1 = abs(beta1 - beta1.mean)) %>%
  mutate(dist_coef2 = abs(beta2 - beta2.mean)) %>%
  mutate(dist_coef.mean = (dist_coef1 + dist_coef2) / 2) %>% 
  ggplot(aes(x = cor.GRF, y = dist_coef.mean, color = mod.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = TRUE, aes(fill = mod.type), alpha = 0.3) +
  labs(
    x = "Correlation random",
    y = "Mean bias of coefficients",
    fill = "Model Type"
  ) +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  theme_bw() +
  facet_grid(
    mod.type ~ relative.GRF,
    labeller = labeller(
      mod.type = c(
        "m.int.GRF" = "Integrated", 
        "m.PA.GRF" = "Presence-absence",
        "m.PO.GRF" = "Presence-only"
      ),
      relative.GRF = c(
        "low" = "Low GRF",
        "medium" = "Medium GRF",
        "high" = "High GRF"
      )
    )
  ) +
  theme(
    axis.title = element_text(size = 11),    # Increase axis titles
    axis.text = element_text(size = 9),     # Increase axis text
    strip.text = element_text(size = 11),   # Increase facet title size
    strip.background = element_rect(fill = "gray96")
  )

 


ggsave(plot = plot1, filename = paste0(file.path(result_path),"/Scenario_2_tests/Correlation_random_vs_Av_Dist_Coef.png"), w = 21, h = 24, units = "cm", dpi = 400, device = "png")

plot1b <- newdf %>% 
  filter(!is.na(RMSE.global.GRF)) %>% 
  mutate(dist_coef1 = abs(beta1 - beta1.mean)) %>%
  mutate(dist_coef2 = abs(beta2 - beta2.mean)) %>%
  mutate(dist_coef.mean = (dist_coef1 + dist_coef2) / 2) %>% 
  ggplot(aes(x = RMSE.global.GRF, y = dist_coef.mean, color = mod.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = TRUE, aes(fill = mod.type), alpha = 0.3) +
  labs(
    x = "Mean RMSE random",
    y = "Mean bias of coefficients",
    fill = "Model Type"
  ) +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  theme_bw() +
  facet_grid(
    mod.type ~ relative.GRF,
    labeller = labeller(
      mod.type = c(
        "m.int.GRF" = "Integrated", 
        "m.PA.GRF" = "Presence-absence",
        "m.PO.GRF" = "Presence-only"
      ),
      relative.GRF = c(
        "low" = "Low GRF",
        "medium" = "Medium GRF",
        "high" = "High GRF"
      )
    )
  ) +
  theme(
    axis.title = element_text(size = 11),    # Increase axis titles
    axis.text = element_text(size = 9),     # Increase axis text
    strip.text = element_text(size = 11),   # Increase facet title size
    strip.background = element_rect(fill = "gray96")
  )


ggsave(plot = plot1b, filename = paste0(file.path(result_path),"/Scenario_2_tests/RMSE_random_vs_Av_Dist_Coef.png"), w = 21, h = 24, units = "cm", dpi = 400, device = "png")

plot2 <- newdf %>% 
  filter(!is.na(cor.GRF)) %>% 
  ggplot(aes(x = cor.GRF, y = beta1.sd, color = mod.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = TRUE, aes(fill = mod.type), alpha = 0.3) +
  labs(
    x = "Correlation random",
    y = expression(beta[1]~"SD"),
    fill = "Model Type"
  ) +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_bw() +
  facet_grid(
    mod.type ~ relative.GRF,
    labeller = labeller(
      mod.type = c(
        "m.int.GRF" = "Integrated", 
        "m.PA.GRF" = "Presence-absence",
        "m.PO.GRF" = "Presence-only"
      ),
      relative.GRF = c(
        "low" = "Low GRF",
        "medium" = "Medium GRF",
        "high" = "High GRF"
      )
    )
  ) +
  theme(
    axis.title = element_text(size = 11),    # Increase axis titles
    axis.text = element_text(size = 9),     # Increase axis text
    strip.text = element_text(size = 11),   # Increase facet title size
    strip.background = element_rect(fill = "gray96")
  )


ggsave(plot = plot2, filename = paste0(file.path(result_path),"/Scenario_2_tests/Correlation_random_vs_Beta1SD.png"), w = 21, h = 24, units = "cm", dpi = 400, device = "png")

plot2b <- newdf %>% 
  filter(!is.na(RMSE.global.GRF)) %>% 
  ggplot(aes(x = RMSE.global.GRF, y = beta1.sd, color = mod.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = TRUE, aes(fill = mod.type), alpha = 0.3) +
  labs(
    x = "Mean RMSE random",
    y = expression(beta[1]~"SD"),
    fill = "Model Type"
  ) +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_bw() +
  facet_grid(
    mod.type ~ relative.GRF,
    labeller = labeller(
      mod.type = c(
        "m.int.GRF" = "Integrated", 
        "m.PA.GRF" = "Presence-absence",
        "m.PO.GRF" = "Presence-only"
      ),
      relative.GRF = c(
        "low" = "Low GRF",
        "medium" = "Medium GRF",
        "high" = "High GRF"
      )
    )
  ) +
  theme(
    axis.title = element_text(size = 11),    # Increase axis titles
    axis.text = element_text(size = 9),     # Increase axis text
    strip.text = element_text(size = 11),   # Increase facet title size
    strip.background = element_rect(fill = "gray96")
  )


ggsave(plot = plot2b, filename = paste0(file.path(result_path),"/Scenario_2_tests/RMSE_random_vs_Beta1SD.png"), w = 21, h = 24, units = "cm", dpi = 400, device = "png")

plot3 <- newdf %>% 
  filter(!is.na(cor.GRF)) %>% 
  ggplot(aes(x = cor.GRF, y = beta2.sd, color = mod.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = TRUE, aes(fill = mod.type), alpha = 0.3) +
  labs(
    x = "Correlation random",
    y = expression(beta[2]~"SD"),
    fill = "Model Type"
  ) +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(0, 0.55)) +
  theme_bw() +
  facet_grid(
    mod.type ~ relative.GRF,
    labeller = labeller(
      mod.type = c(
        "m.int.GRF" = "Integrated", 
        "m.PA.GRF" = "Presence-absence",
        "m.PO.GRF" = "Presence-only"
      ),
      relative.GRF = c(
        "low" = "Low GRF",
        "medium" = "Medium GRF",
        "high" = "High GRF"
      )
    )
  ) +
  theme(
    axis.title = element_text(size = 11),    # Increase axis titles
    axis.text = element_text(size = 9),     # Increase axis text
    strip.text = element_text(size = 11),   # Increase facet title size
    strip.background = element_rect(fill = "gray96")
  )

ggsave(plot = plot3, filename = paste0(file.path(result_path),"/Scenario_2_tests/Correlation_random_vs_Beta2SD.png"), w = 21, h = 24, units = "cm", dpi = 400, device = "png")

plot3b <- newdf %>% 
  filter(!is.na(RMSE.global.GRF)) %>% 
  ggplot(aes(x = RMSE.global.GRF, y = beta2.sd, color = mod.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = TRUE, aes(fill = mod.type), alpha = 0.3) +
  labs(
    x = "Mean RMSE random",
    y = expression(beta[2]~"SD"),
    fill = "Model Type"
  ) +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(0, 0.55)) +
  theme_bw() +
  facet_grid(
    mod.type ~ relative.GRF,
    labeller = labeller(
      mod.type = c(
        "m.int.GRF" = "Integrated", 
        "m.PA.GRF" = "Presence-absence",
        "m.PO.GRF" = "Presence-only"
      ),
      relative.GRF = c(
        "low" = "Low GRF",
        "medium" = "Medium GRF",
        "high" = "High GRF"
      )
    )
  ) +
  theme(
    axis.title = element_text(size = 11),    # Increase axis titles
    axis.text = element_text(size = 9),     # Increase axis text
    strip.text = element_text(size = 11),   # Increase facet title size
    strip.background = element_rect(fill = "gray96")
  )

ggsave(plot = plot3b, filename = paste0(file.path(result_path),"/Scenario_2_tests/RMSE_random_vs_Beta2SD.png"), w = 21, h = 24, units = "cm", dpi = 400, device = "png")


plot4 <- newdf %>% 
  filter(!is.na(cor.GRF)) %>% 
  filter(!is.na(PO_intercept)) %>% 
  ggplot(aes(x = cor.GRF, y = PO_intercept, color = mod.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = TRUE, aes(fill = mod.type), alpha = 0.3) +
  geom_hline(yintercept = beta0, linetype = "dashed", color = "red") +
  labs(
    x = "Correlation random",
    y = expression(beta[0]),
    fill = "Model Type"
  ) +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(-5.5, -1)) +
  theme_bw() +
  facet_grid(
    mod.type ~ relative.GRF,
    labeller = labeller(
      mod.type = c(
        "m.int.GRF" = "Integrated", 
        "m.PA.GRF" = "Presence-absence",
        "m.PO.GRF" = "Presence-only"
      ),
      relative.GRF = c(
        "low" = "Low GRF",
        "medium" = "Medium GRF",
        "high" = "High GRF"
      )
    )
  ) +
  theme(
    axis.title = element_text(size = 11),    # Increase axis titles
    axis.text = element_text(size = 9),     # Increase axis text
    strip.text = element_text(size = 11),   # Increase facet title size
    strip.background = element_rect(fill = "gray96")
  ) +
  ggtitle('PO Intercept')

ggsave(plot = plot4, filename = paste0(file.path(result_path),"/Scenario_2_tests/Correlation_random_vs_POINT.png"), w = 21, h = 24, units = "cm", dpi = 400, device = "png")


plot5 <- newdf %>% 
  filter(!is.na(cor.GRF)) %>% 
  filter(!is.na(PA_intercept)) %>% 
  ggplot(aes(x = cor.GRF, y = PA_intercept, color = mod.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = TRUE, aes(fill = mod.type), alpha = 0.3) +
  geom_hline(yintercept = beta0, linetype = "dashed", color = "red") +
  labs(
    x = "Correlation random",
    y = expression(beta[0]),
    fill = "Model Type"
  ) +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(-5.5, 0)) +
  theme_bw() +
  facet_grid(
    mod.type ~ relative.GRF,
    labeller = labeller(
      mod.type = c(
        "m.int.GRF" = "Integrated", 
        "m.PA.GRF" = "Presence-absence",
        "m.PO.GRF" = "Presence-only"
      ),
      relative.GRF = c(
        "low" = "Low GRF",
        "medium" = "Medium GRF",
        "high" = "High GRF"
      )
    )
  ) +
  theme(
    axis.title = element_text(size = 11),    # Increase axis titles
    axis.text = element_text(size = 9),     # Increase axis text
    strip.text = element_text(size = 11),   # Increase facet title size
    strip.background = element_rect(fill = "gray96")
  ) +
  ggtitle('PA Intercept')


ggsave(plot = plot5, filename = paste0(file.path(result_path),"/Scenario_2_tests/Correlation_random_vs_PAINT.png"), w = 21, h = 24, units = "cm", dpi = 400, device = "png")

########## CORRELATION BETWEEN GRF ACCURACY AND RMSE (prediction) ############

# Add in RMSE from prediction site
newdf <- newdf %>% 
  left_join(final.df %>% 
              select(relative.GRF, extrap.type, job_index, mod.type, RMSE.global, correlation),
            by = c("relative.GRF", "extrap.type", "job_index", "mod.type"))


newdfA <- newdf %>% 
  select(-RMSE.global) %>% 
  left_join(final.SiteA.df %>% 
              select(relative.GRF, extrap.type, job_index, mod.type, RMSE.global),
            by = c("relative.GRF", "extrap.type", "job_index", "mod.type"))


plot6 <- newdf %>% 
  filter(!is.na(cor.GRF)) %>% 
  ggplot(aes(x = cor.GRF, y = RMSE.global, color = mod.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = TRUE, aes(fill = mod.type), alpha = 0.3) +
  labs(
    x = "Correlation random",
    y = "Mean RMSE",
    fill = "Model Type"
  ) +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(0, 3.5)) +
  theme_bw() +
  facet_grid(
    mod.type ~ relative.GRF,
    labeller = labeller(
      mod.type = c(
        "m.int.GRF" = "Integrated", 
        "m.PA.GRF" = "Presence-absence",
        "m.PO.GRF" = "Presence-only"
      ),
      relative.GRF = c(
        "low" = "Low GRF",
        "medium" = "Medium GRF",
        "high" = "High GRF"
      )
    )
  ) +
  theme(
    axis.title = element_text(size = 11),    # Increase axis titles
    axis.text = element_text(size = 9),     # Increase axis text
    strip.text = element_text(size = 11),   # Increase facet title size
    strip.background = element_rect(fill = "gray96")
  ) +
  ggtitle('Projection Site')

ggsave(plot = plot6, filename = paste0(file.path(result_path),"/Scenario_2_tests/Correlation_random_vs_ProjectionRMSE.png"), w = 21, h = 24, units = "cm", dpi = 400, device = "png")


plot7 <- newdf %>% 
  filter(!is.na(cor.GRF)) %>% 
  ggplot(aes(x = cor.GRF, y = Mean.Int.Score, color = mod.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = TRUE, aes(fill = mod.type), alpha = 0.3) +
  labs(
    x = "Correlation random",
    y = "Mean interval score",
    fill = "Model Type"
  ) +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(0, 3.5)) +
  theme_bw() +
  facet_grid(
    mod.type ~ relative.GRF,
    labeller = labeller(
      mod.type = c(
        "m.int.GRF" = "Integrated", 
        "m.PA.GRF" = "Presence-absence",
        "m.PO.GRF" = "Presence-only"
      ),
      relative.GRF = c(
        "low" = "Low GRF",
        "medium" = "Medium GRF",
        "high" = "High GRF"
      )
    )
  ) +
  theme(
    axis.title = element_text(size = 11),    # Increase axis titles
    axis.text = element_text(size = 9),     # Increase axis text
    strip.text = element_text(size = 11),   # Increase facet title size
    strip.background = element_rect(fill = "gray96")
  ) +
  ggtitle('Projection Site')

ggsave(plot = plot7, filename = paste0(file.path(result_path),"/Scenario_2_tests/Correlation_random_vs_ProjectionIntScore.png"), w = 21, h = 24, units = "cm", dpi = 400, device = "png")


plot8 <- newdfA %>% 
  filter(!is.na(cor.GRF)) %>% 
  ggplot(aes(x = cor.GRF, y = RMSE.global, color = mod.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = TRUE, aes(fill = mod.type), alpha = 0.3) +
  labs(
    x = "Correlation random",
    y = "Mean RMSE",
    fill = "Model Type"
  ) +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(0, 4.5)) +
  theme_bw() +
  facet_grid(
    mod.type ~ relative.GRF,
    labeller = labeller(
      mod.type = c(
        "m.int.GRF" = "Integrated", 
        "m.PA.GRF" = "Presence-absence",
        "m.PO.GRF" = "Presence-only"
      ),
      relative.GRF = c(
        "low" = "Low GRF",
        "medium" = "Medium GRF",
        "high" = "High GRF"
      )
    )
  ) +
  theme(
    axis.title = element_text(size = 11),    # Increase axis titles
    axis.text = element_text(size = 9),     # Increase axis text
    strip.text = element_text(size = 11),   # Increase facet title size
    strip.background = element_rect(fill = "gray96")
  ) +
  ggtitle('Training Site')

ggsave(plot = plot8, filename = paste0(file.path(result_path),"/Scenario_2_tests/Correlation_random_vs_TrainingRMSE.png"), w = 21, h = 24, units = "cm", dpi = 400, device = "png")


plot9 <- newdfA %>% 
  filter(!is.na(cor.GRF)) %>% 
  ggplot(aes(x = cor.GRF, y = Mean.Int.Score, color = mod.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = TRUE, aes(fill = mod.type), alpha = 0.3) +
  labs(
    x = "Correlation random",
    y = "Mean Interval Score",
    fill = "Model Type"
  ) +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(0, 3)) +
  theme_bw() +
  facet_grid(
    mod.type ~ relative.GRF,
    labeller = labeller(
      mod.type = c(
        "m.int.GRF" = "Integrated", 
        "m.PA.GRF" = "Presence-absence",
        "m.PO.GRF" = "Presence-only"
      ),
      relative.GRF = c(
        "low" = "Low GRF",
        "medium" = "Medium GRF",
        "high" = "High GRF"
      )
    )
  ) +
  theme(
    axis.title = element_text(size = 11),    # Increase axis titles
    axis.text = element_text(size = 9),     # Increase axis text
    strip.text = element_text(size = 11),   # Increase facet title size
    strip.background = element_rect(fill = "gray96")
  ) +
  ggtitle('Training Site')

ggsave(plot = plot9, filename = paste0(file.path(result_path),"/Scenario_2_tests/Correlation_random_vs_TrainingIntscore.png"), w = 21, h = 24, units = "cm", dpi = 400, device = "png")

############################################################
########### 2. CORR BETWEEN SITE A & B GRF CORR AND RESULTS ###### 
############################################################


# Add the fixed and random effects columns to the extrap.scenario dataframe by matching extrap, rep, job, and model type

newdf <- newdf %>% 
  left_join(final.SiteA.df %>% 
              select(relative.GRF, extrap.type, job_index, mod.type, cor.GRFA.GRFB),
            by = c("relative.GRF", "extrap.type", "job_index", "mod.type"))


newdfA <- newdfA %>% 
  left_join(final.SiteA.df %>% 
              select(relative.GRF, extrap.type, job_index, mod.type, cor.GRFA.GRFB),
            by = c("relative.GRF", "extrap.type", "job_index", "mod.type"))

plot10 <- newdf %>% 
  filter(!is.na(cor.GRFA.GRFB)) %>% 
  ggplot(aes(x = cor.GRFA.GRFB, y = RMSE.global, color = mod.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = TRUE, aes(fill = mod.type), alpha = 0.3) +
  labs(
    x = "Correlation random Site A & B",
    y = "Mean RMSE",
    fill = "Model Type"
  ) +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(0, 3.5)) +
  theme_bw() +
  facet_grid(
    mod.type ~ relative.GRF,
    labeller = labeller(
      mod.type = c(
        "m.int.GRF" = "Integrated", 
        "m.PA.GRF" = "Presence-absence",
        "m.PO.GRF" = "Presence-only"
      ),
      relative.GRF = c(
        "low" = "Low GRF",
        "medium" = "Medium GRF",
        "high" = "High GRF"
      )
    )
  ) +
  theme(
    axis.title = element_text(size = 11),    # Increase axis titles
    axis.text = element_text(size = 9),     # Increase axis text
    strip.text = element_text(size = 11),   # Increase facet title size
    strip.background = element_rect(fill = "gray96")
  ) +
  ggtitle('Projection Site')

ggsave(plot = plot10, filename = paste0(file.path(result_path),"/Scenario_2_tests/Correlation_SiteAB_vs_ProjectionRMSE.png"), w = 21, h = 24, units = "cm", dpi = 400, device = "png")


plot11 <- newdf %>% 
  filter(!is.na(cor.GRFA.GRFB)) %>% 
  ggplot(aes(x = cor.GRFA.GRFB, y = Mean.Int.Score, color = mod.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = TRUE, aes(fill = mod.type), alpha = 0.3) +
  labs(
    x = "Correlation random Site A & B",
    y = "Mean Interval Score",
    fill = "Model Type"
  ) +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(0, 3.5)) +
  theme_bw() +
  facet_grid(
    mod.type ~ relative.GRF,
    labeller = labeller(
      mod.type = c(
        "m.int.GRF" = "Integrated", 
        "m.PA.GRF" = "Presence-absence",
        "m.PO.GRF" = "Presence-only"
      ),
      relative.GRF = c(
        "low" = "Low GRF",
        "medium" = "Medium GRF",
        "high" = "High GRF"
      )
    )
  ) +
  theme(
    axis.title = element_text(size = 11),    # Increase axis titles
    axis.text = element_text(size = 9),     # Increase axis text
    strip.text = element_text(size = 11),   # Increase facet title size
    strip.background = element_rect(fill = "gray96")
  ) +
  ggtitle('Training Site')

ggsave(plot = plot11, filename = paste0(file.path(result_path),"/Scenario_2_tests/Correlation_SiteAB_vs_ProjectionMean.Int.Score.png"), w = 21, h = 24, units = "cm", dpi = 400, device = "png")


# Correlation BUT coloured by GRF correlation
newdf %>% 
  filter(!is.na(cor.GRFA.GRFB)) %>% 
  ggplot(aes(x = cor.GRFA.GRFB, y = RMSE.global)) +
  geom_point(alpha = 0.8, size = 2, aes(color = cor.GRF)) +
  labs(
    x = "Correlation random Site A & B",
    y = "Mean RMSE",
    fill = "Model Type"
  ) +
  scale_color_viridis()+
  coord_cartesian(ylim = c(0, 3.5)) +
  theme_bw() +
  facet_grid(
    mod.type ~ relative.GRF,
    labeller = labeller(
      mod.type = c(
        "m.int.GRF" = "Integrated", 
        "m.PA.GRF" = "Presence-absence",
        "m.PO.GRF" = "Presence-only"
      ),
      relative.GRF = c(
        "low" = "Low GRF",
        "medium" = "Medium GRF",
        "high" = "High GRF"
      )
    )
  ) +
  theme(
    axis.title = element_text(size = 11),    # Increase axis titles
    axis.text = element_text(size = 9),     # Increase axis text
    strip.text = element_text(size = 11),   # Increase facet title size
    strip.background = element_rect(fill = "gray96")
  ) +
  ggtitle('Projection Site')


newdf %>% 
  filter(!is.na(cor.GRFA.GRFB)) %>% 
  mutate(cor.GRF.good = ifelse(cor.GRF > 0.8, TRUE, FALSE)) %>% 
  ggplot(aes(x = cor.GRFA.GRFB, y = RMSE.global)) +
  geom_point(alpha = 0.8, size = 2, aes(color = cor.GRF.good)) +
  labs(
    x = "Correlation random Site A & B",
    y = "Mean RMSE",
    fill = "Model Type"
  ) +
  coord_cartesian(ylim = c(0, 3.5)) +
  theme_bw() +
  facet_grid(
    mod.type ~ relative.GRF,
    labeller = labeller(
      mod.type = c(
        "m.int.GRF" = "Integrated", 
        "m.PA.GRF" = "Presence-absence",
        "m.PO.GRF" = "Presence-only"
      ),
      relative.GRF = c(
        "low" = "Low GRF",
        "medium" = "Medium GRF",
        "high" = "High GRF"
      )
    )
  ) +
  theme(
    axis.title = element_text(size = 11),    # Increase axis titles
    axis.text = element_text(size = 9),     # Increase axis text
    strip.text = element_text(size = 11),   # Increase facet title size
    strip.background = element_rect(fill = "gray96")
  ) +
  ggtitle('Projection Site')



newdf %>% 
  filter(!is.na(cor.GRFA.GRFB)) %>% 
  mutate(cor.GRF.good = ifelse(cor.GRF > 0.8, TRUE, FALSE)) %>% 
  ggplot(aes(x = cor.GRFA.GRFB, y = correlation)) +
  geom_point(alpha = 0.8, size = 2, aes(color = cor.GRF.good)) +
  labs(
    x = "Correlation random Site A & B",
    y = "Correlation",
    fill = "Model Type"
  ) +
  coord_cartesian(ylim = c(-0.5, 1.5)) +
  theme_bw() +
  facet_grid(
    mod.type ~ relative.GRF,
    labeller = labeller(
      mod.type = c(
        "m.int.GRF" = "Integrated", 
        "m.PA.GRF" = "Presence-absence",
        "m.PO.GRF" = "Presence-only"
      ),
      relative.GRF = c(
        "low" = "Low GRF",
        "medium" = "Medium GRF",
        "high" = "High GRF"
      )
    )
  ) +
  theme(
    axis.title = element_text(size = 11),    # Increase axis titles
    axis.text = element_text(size = 9),     # Increase axis text
    strip.text = element_text(size = 11),   # Increase facet title size
    strip.background = element_rect(fill = "gray96")
  ) +
  ggtitle('Projection Site')

######################################################
########### CORRELATION BETWEEN SD AND RMSE ##########
######################################################

newdf %>% 
  ggplot(aes(x = RMSE.global, y = Mean.Int.Score, color = mod.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = TRUE, aes(fill = mod.type), alpha = 0.3) +
  labs(
    x = "Mean RMSE",
    y = "Mean Interval Score",
    fill = "Model Type"
  ) +
  scale_color_manual(values = fill.colours, guide = "none") +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(0, 5), xlim = c(0, 5)) +
  theme_bw() +
  facet_grid(
    mod.type ~ relative.GRF,
    labeller = labeller(
      mod.type = c(
        "m.int.GRF" = "Integrated", 
        "m.PA.GRF" = "Presence-absence",
        "m.PO.GRF" = "Presence-only"
      ),
      relative.GRF = c(
        "low" = "Low GRF",
        "medium" = "Medium GRF",
        "high" = "High GRF"
      )
    )
  ) +
  theme(
    axis.title = element_text(size = 11),    # Increase axis titles
    axis.text = element_text(size = 9),     # Increase axis text
    strip.text = element_text(size = 11),   # Increase facet title size
    strip.background = element_rect(fill = "gray96")
  ) +
  ggtitle('Projection Site')


######################################################
########### LOOKING AT RANGE AND SITE DISTANCE ##########
######################################################

final.df <- final.df %>% 
  mutate(scal = ifelse(extrap.type == "Low", 20, ifelse(extrap.type == "Moderate", 100, 200)))


final.df %>%
  filter(Site.distance <= 200) %>% 
  filter(relative.GRF == 5) %>% 
  ggplot(aes(x = extrap.type, y = correlation, fill = bias.type)) +
  geom_violin(alpha = 0.2, trim = TRUE, color = NA) +  # Add a transparent violin plot with no outline
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = x.label, y = "Correlation", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  #coord_cartesian(ylim = c(NA, 4)) +
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



newdf %>% 
  ggplot(aes(x = Site.distance, y = correlation, color = extrap.type)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = TRUE, aes(fill = extrap.type), alpha = 0.3) +
  labs(
    x = "Site distance",
    y = "Mean RMSE",
    fill = "Spatial autocorrelation range"
  ) +
  #coord_cartesian(ylim = c(0, 3.5)) +
  theme_bw() +
  facet_grid(
  ~mod.type,
    labeller = labeller(
      mod.type = c(
        "m.int.GRF" = "Integrated", 
        "m.PA.GRF" = "Presence-absence",
        "m.PO.GRF" = "Presence-only"
      )
    )
  ) +
  theme(
    axis.title = element_text(size = 11),    # Increase axis titles
    axis.text = element_text(size = 9),     # Increase axis text
    strip.text = element_text(size = 11),   # Increase facet title size
    strip.background = element_rect(fill = "gray96")
  ) +
  ggtitle('Training Site')











# SCENARIO 2C VERSION -----------------------------------------------------

###################################################
############## PLOT ALL TOGETHER #################
###################################################

true.validation.df_2C <- true.validation.df_2C %>% 
  mutate(relative.GRF = "1")

true.validation.df_2Cv0.2 <- true.validation.df_2Cv0.2 %>% 
  mutate(relative.GRF = "0.2")

true.validation.df_2Cv5 <- true.validation.df_2Cv5 %>% 
  mutate(relative.GRF = "5")

final.df <- rbind(true.validation.df_2C, true.validation.df_2Cv0.2, true.validation.df_2Cv5)


final.df$bias.type <- factor(final.df$bias.type, levels = c("Without GRF", "With GRF"))
final.df$mod.type2 <- factor(final.df$mod.type2, levels = c("m.PO", "m.PA", "m.int"))

######## Projection site  ############

projection1 <- final.df %>%
  ggplot(aes(x = relative.GRF, y = RMSE.global, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Mean RMSE", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  coord_cartesian(ylim = c(NA, 4)) +
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
  ggplot(aes(x = relative.GRF, y = Mean.Int.Score, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Mean Interval Score", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  coord_cartesian(ylim = c(NA, 4.5)) +
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
  ggplot(aes(x = relative.GRF, y = correlation, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Correlation", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
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
        plot.title = element_text(hjust = 1, size = 15, face = "italic")) 

fig2 <- ggarrange(projection1 + rremove("xlab"), projection2 + rremove("xlab"), projection3, common.legend = TRUE, ncol = 1, nrow = 3, legend = "bottom", labels = c("(a)", "(b)", "(c)"), align = "v")

ggsave(plot = fig2, filename = paste0(file.path(result_path),"/Scenario_2_tests/projection_siteC.png"), w = 21, h = 27, units = "cm", dpi = 400, device = "png")

# Coverage probability
projection4 <-  final.df %>%
  ggplot(aes(x = relative.GRF, y = coverage.rate, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Mean coverage probability", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
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

ggsave(plot = fig2b, filename = paste0(file.path(result_path),"/Scenario_2_tests/projection_siteV2C.png"), w = 21, h = 30, units = "cm", dpi = 400, device = "png")



########### COEFFICIENT RECOVERY ###############


extrap.scenario.df_2C <- extrap.scenario.df_2C %>% 
  mutate(relative.GRF = "1")


extrap.scenario.df_2Cv0.2 <- extrap.scenario.df_2Cv0.2 %>% 
  mutate(relative.GRF = "0.2")


extrap.scenario.df_2Cv5 <- extrap.scenario.df_2Cv5 %>% 
  mutate(relative.GRF = "5")

final.extrap.df <- rbind(extrap.scenario.df_2C, extrap.scenario.df_2Cv0.2, extrap.scenario.df_2Cv5)


final.extrap.df$bias.type <- factor(final.extrap.df$bias.type, levels = c("Without GRF", "With GRF"))
final.extrap.df$mod.type2 <- factor(final.extrap.df$mod.type2, levels = c("m.PO", "m.PA", "m.int"))

coef1 <- final.extrap.df %>%
  ggplot(aes(x = relative.GRF, y = beta1.mean, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = expression(beta[1]), color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  geom_hline(yintercept = beta1, linetype = "dashed", color = "red") +
  coord_cartesian(ylim = c(-1, 1)) +
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
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  geom_hline(yintercept = beta2, linetype = "dashed", color = "red") +
  coord_cartesian(ylim = c(-1, 1)) +
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

ggsave(plot = Supp.Fig.X, filename = paste0(file.path(result_path),"/Scenario_2_tests/CoefficientsC.png"), w = 21, h = 17.5, units = "cm", dpi = 400, device = "png")

PO_int <- final.extrap.df %>%
  filter(PO_intercept != 0 ) %>% 
  ggplot(aes(x = relative.GRF, y = PO_intercept, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = expression(beta[0]), color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
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
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
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

ggsave(plot = Supp.Fig.X, filename = paste0(file.path(result_path),"/Scenario_2_tests/InterceptsC.png"), w = 23.5, h = 15, units = "cm", dpi = 400, device = "png")

coefSD1 <- final.extrap.df %>%
  ggplot(aes(x = relative.GRF, y = beta1.sd, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = expression(beta[1]~"SD"), color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  coord_cartesian(ylim = c(NA, 0.7)) +
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
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  coord_cartesian(ylim = c(0, 0.7)) +
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

ggsave(plot = Supp.Fig.X, filename = paste0(file.path(result_path),"/Scenario_2_tests/Coefficient_SDsC.png"), w = 21, h = 17.5, units = "cm", dpi = 400, device = "png")

#######################################################
################ GRF HYPERPARAMETERS ##################
#######################################################

g1 <- final.extrap.df %>%
  filter(bias.type == "With GRF") %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int"))) %>% 
  mutate(scal = ifelse(extrap.type == "Low", 20, ifelse(extrap.type == "Moderate", 100, 200))) %>%
  ggplot(aes(x = relative.GRF, y = scal - GRF.range.mean, fill = mod.type2)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Bias of GRF range", color = NULL, fill = "Model Type") +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(-300, 200)) +
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

g2 <- final.extrap.df %>%
  filter(bias.type == "With GRF") %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int"))) %>% 
  ggplot(aes(x = relative.GRF, y = GRF.variance - (GRF.sd.mean)^2, fill = mod.type2)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Bias of GRF variance", color = NULL, fill = "Model Type") +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = fill.colours, guide = "none") +
  coord_cartesian(ylim = c(-5, 2)) +
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


Supp.Fig.X <- ggarrange(g1 + rremove("xlab"), g2 + rremove("xlab") , common.legend = T,  ncol = 1, nrow = 2, legend = "bottom", labels = c("(a)", "(b)"))

ggsave(plot = Supp.Fig.X, filename = paste0(file.path(result_path),"/Scenario_2_tests/True_vs_estimated_GRF_paramsC.png"), w = 21, h = 17.5, units = "cm", dpi = 400, device = "png")


#########################################################
################## TRAINING SITE #########################
#########################################################

true.validation.SiteA.df_2C <- true.validation.SiteA.df_2C %>% 
  mutate(relative.GRF = "1")

true.validation.SiteA.df_2Cv0.2 <- true.validation.SiteA.df_2Cv0.2 %>% 
  mutate(relative.GRF = "0.2")

true.validation.SiteA.df_2Cv5 <- true.validation.SiteA.df_2Cv5 %>% 
  mutate(relative.GRF = "5")

final.SiteA.df <- rbind(true.validation.SiteA.df_2C, true.validation.SiteA.df_2Cv0.2, true.validation.SiteA.df_2Cv5)


final.SiteA.df$bias.type <- factor(final.SiteA.df$bias.type, levels = c("Without GRF", "With GRF"))

final.SiteA.df$mod.type2 <- factor(final.SiteA.df$mod.type2, levels = c("m.PO", "m.PA", "m.int"))


training1 <- final.SiteA.df %>%
  ggplot(aes(x = relative.GRF, y = RMSE.global, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Mean RMSE", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  coord_cartesian(ylim = c(NA, 4)) +
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
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
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
        plot.title = element_text(hjust = 1, size = 15, face = "italic"))

training3 <- final.SiteA.df %>%
  ggplot(aes(x = relative.GRF, y = correlation, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Correlation", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
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
        plot.title = element_text(hjust = 1, size = 15, face = "italic"))


training4 <- final.SiteA.df %>%
  ggplot(aes(x = relative.GRF, y = coverage.rate, fill = bias.type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Mean coverage probability", color = NULL) +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
  scale_color_manual(values = c("Without GRF" = "#D55E00", "With GRF" = "#0072B2")) +
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

fig2 <- ggarrange(training1 + rremove("xlab"), training2 + rremove("xlab"), training4 + rremove("xlab"), training3, common.legend = TRUE, ncol = 1, nrow = 4, legend = "bottom", labels = c("(a)", "(b)", "(c)", "(d)"), align = "v")

ggsave(plot = fig2, filename = paste0(file.path(result_path),"/Scenario_2_tests/training_siteC.png"), w = 21, h = 30, units = "cm", dpi = 400, device = "png")

#######################################################
############ FIG 3. TRAINING AND PROJECTION ###########
#######################################################

# 
# ggsave(plot = fig, filename = paste0(file.path(result_path),"/Scenario_2_tests/projection_siteC.png"), w = 21, h = 17.5, units = "cm", dpi = 400, device = "png")



########## CORRELATION BETWEEN ESTIMATED AND TRUE FIXED & GRF #############

cor.GRF <- final.SiteA.df %>%
  filter(bias.type == "With GRF") %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int"))) %>% 
  mutate(scal = ifelse(extrap.type == "Low", 20, ifelse(extrap.type == "Moderate", 100, 200))) %>%
  ggplot(aes(x = relative.GRF, y = cor.GRF, fill = mod.type2)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Correlation random", color = NULL, fill = "Model Type") +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = fill.colours, guide = "none") +
  #coord_cartesian(ylim = c(-300, 200)) +
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


RMSE.GRF <- final.SiteA.df %>%
  filter(bias.type == "With GRF") %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int"))) %>% 
  mutate(scal = ifelse(extrap.type == "Low", 20, ifelse(extrap.type == "Moderate", 100, 200))) %>%
  ggplot(aes(x = relative.GRF, y = RMSE.global.GRF, fill = mod.type2)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Mean RMSE random", color = NULL, fill = "Model Type") +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = fill.colours, guide = "none") +
  #coord_cartesian(ylim = c(-300, 200)) +
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

cor.FIXED <- final.SiteA.df %>%
  filter(bias.type == "With GRF") %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int"))) %>% 
  mutate(scal = ifelse(extrap.type == "Low", 20, ifelse(extrap.type == "Moderate", 100, 200))) %>%
  ggplot(aes(x = relative.GRF, y = cor.FIXED, fill = mod.type2)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Correlation fixed", color = NULL, fill = "Model Type") +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = fill.colours, guide = "none") +
  #coord_cartesian(ylim = c(-300, 200)) +
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

RMSE.FIXED <- final.SiteA.df %>%
  filter(bias.type == "With GRF") %>% 
  mutate(mod.type2 = factor(mod.type2, levels = c("m.PO", "m.PA", "m.int"))) %>% 
  mutate(scal = ifelse(extrap.type == "Low", 20, ifelse(extrap.type == "Moderate", 100, 200))) %>%
  ggplot(aes(x = relative.GRF, y = RMSE.global.FIXED, fill = mod.type2)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +      # Add a boxplot without outliers
  labs(x = "Relative random effect contribution", y = "Mean RMSE fixed", color = NULL, fill = "Model Type") +
  scale_x_discrete(labels = x.discrete.label) +
  scale_fill_manual(values = fill.colours, guide = "none") +
  #coord_cartesian(ylim = c(-300, 200)) +
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

COR_GRF_FIXED_plot <- ggarrange(cor.GRF + rremove("xlab"), cor.FIXED + rremove("xlab"), common.legend = T,  ncol = 2, nrow = 1)

RMSE_GRF_FIXED_plot <- ggarrange(RMSE.GRF + rremove("xlab"), RMSE.FIXED + rremove("xlab"), common.legend = T,  ncol = 2, nrow = 1)

Fig.4 <- ggarrange(COR_GRF_FIXED_plot, g1 + rremove("xlab"), g2, ncol = 1, nrow = 3, labels = c("(a)", "(b)", "(c)"))

ggsave(plot = Fig.4, filename = paste0(file.path(result_path),"/Scenario_2_tests/Correlation_random_fixed_GRF_paramsC.png"), w = 21, h = 24, units = "cm", dpi = 400, device = "png")

Fig.4b <- ggarrange(RMSE_GRF_FIXED_plot, g1 + rremove("xlab"), g2, ncol = 1, nrow = 3, labels = c("(a)", "(b)", "(c)"))

ggsave(plot = Fig.4b, filename = paste0(file.path(result_path),"/Scenario_2_tests/RMSE_random_fixedC.png"), w = 21, h = 24, units = "cm", dpi = 400, device = "png")



