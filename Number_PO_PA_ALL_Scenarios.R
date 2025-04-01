lib_loc = .libPaths() 

library(spatstat)
library(ggplot2)
library(dplyr)
library(ggpubr, lib.loc = lib_loc)
library(viridis)
library(terra)
library(purrr)
library(readr)


outpath <- file.path(dirname(getwd()), "output")

result_path <- file.path(getwd(), "output/RESULTS")

# Make dir if not already there
if(!dir.exists(result__path)) {
  
  dir.create(result_path, recursive = TRUE)
  
}

#############################

# Scenario 1 (env extrap) -------------------------------------------------

############################

####### CHOOSE NUMBER OF REPLICATES TO KEEP ########


replicates <- 300

scenario_name = "1"

# 1. FIRST list all the files with n presences (for each Job)
file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "N_Presences_EACH_rep_Job*", full.names = TRUE, recursive = TRUE) 

# Read all the CSV files and save them to a list
npresence.list <- lapply(file_list, read.csv)

# Combine all dataframes in the list into one dataframe using rbind
replicate_info_ALL.df_1 <- do.call(rbind, npresence.list)

replicate_info_ALL.df_1 <- replicate_info_ALL.df_1 %>% 
  slice(1:replicates)

#############################

# Scenario 1C (env extrap) -------------------------------------------------

############################


scenario_name = "1C"

# 1. FIRST list all the files with n presences (for each Job)
file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "N_Presences_EACH_rep_Job*", full.names = TRUE, recursive = TRUE) 

# Read all the CSV files and save them to a list
npresence.list <- lapply(file_list, read.csv)

# Combine all dataframes in the list into one dataframe using rbind
replicate_info_ALL.df_1C <- do.call(rbind, npresence.list)

replicate_info_ALL.df_1C <- replicate_info_ALL.df_1C %>% 
  slice(1:replicates)


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

# 1. FIRST list all the files with n presences (for each Job)
file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "N_Presences_EACH_rep_Job*", full.names = TRUE, recursive = TRUE) 

# Read all the CSV files and save them to a list
npresence.list <- lapply(file_list, read.csv)

# Combine all dataframes in the list into one dataframe using rbind
replicate_info_ALL.df_5 <- do.call(rbind, npresence.list)

replicate_info_ALL.df_5 <- replicate_info_ALL.df_5 %>% 
  slice(1:replicates)

######################################################################
################ SCENARIO 5C - SPATIAL AUTOCORRELATION C ################
######################################################################

scenario_name = "5C"

# 1. FIRST list all the files with n presences (for each Job)
file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "N_Presences_EACH_rep_Job*", full.names = TRUE, recursive = TRUE) 

# Read all the CSV files and save them to a list
npresence.list <- lapply(file_list, read.csv)

# Combine all dataframes in the list into one dataframe using rbind
replicate_info_ALL.df_5C <- do.call(rbind, npresence.list)

replicate_info_ALL.df_5C <- replicate_info_ALL.df_5C %>% 
  slice(1:replicates)

######################################################################
################ SCENARIO 5v0.2 - SPATIAL AUTOCORRELATION ################
######################################################################

scenario_name = "5v0.2"

# 1. FIRST list all the files with n presences (for each Job)
file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "N_Presences_EACH_rep_Job*", full.names = TRUE, recursive = TRUE) 

# Read all the CSV files and save them to a list
npresence.list <- lapply(file_list, read.csv)

# Combine all dataframes in the list into one dataframe using rbind
replicate_info_ALL.df_5v0.2 <- do.call(rbind, npresence.list)

replicate_info_ALL.df_5v0.2 <- replicate_info_ALL.df_5v0.2 %>% 
  slice(1:replicates)

######################################################################
################ SCENARIO 5Cv0.2 - SPATIAL AUTOCORRELATION C ################
######################################################################

scenario_name = "5Cv0.2"

# 1. FIRST list all the files with n presences (for each Job)
file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "N_Presences_EACH_rep_Job*", full.names = TRUE, recursive = TRUE) 

# Read all the CSV files and save them to a list
npresence.list <- lapply(file_list, read.csv)

# Combine all dataframes in the list into one dataframe using rbind
replicate_info_ALL.df_5Cv0.2 <- do.call(rbind, npresence.list)

replicate_info_ALL.df_5Cv0.2 <- replicate_info_ALL.df_5Cv0.2 %>% 
  slice(1:replicates)

######################################################################
################ SCENARIO 5v5 - SPATIAL AUTOCORRELATION ################
######################################################################

scenario_name = "5v5"

# 1. FIRST list all the files with n presences (for each Job)
file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "N_Presences_EACH_rep_Job*", full.names = TRUE, recursive = TRUE) 

# Read all the CSV files and save them to a list
npresence.list <- lapply(file_list, read.csv)

# Combine all dataframes in the list into one dataframe using rbind
replicate_info_ALL.df_5v5 <- do.call(rbind, npresence.list)

replicate_info_ALL.df_5v5 <- replicate_info_ALL.df_5v5 %>% 
  slice(1:replicates)

######################################################################
################ SCENARIO 5Cv5 - SPATIAL AUTOCORRELATION C ################
######################################################################

scenario_name = "5Cv5"

# 1. FIRST list all the files with n presences (for each Job)
file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "N_Presences_EACH_rep_Job*", full.names = TRUE, recursive = TRUE) 

# Read all the CSV files and save them to a list
npresence.list <- lapply(file_list, read.csv)

# Combine all dataframes in the list into one dataframe using rbind
replicate_info_ALL.df_5Cv5 <- do.call(rbind, npresence.list)

replicate_info_ALL.df_5Cv5 <- replicate_info_ALL.df_5Cv5 %>% 
  slice(1:replicates)


replicate_info_ALL.df_1 <- replicate_info_ALL.df_1 %>%
  mutate(scenario = "1") %>% 
  mutate(record_number = "High")

replicate_info_ALL.df_1C <- replicate_info_ALL.df_1C %>%
  mutate(scenario = "1C") %>% 
  mutate(record_number = "Low")

replicate_info_ALL.df_5 <- replicate_info_ALL.df_5 %>%
  mutate(scenario = "5") %>% 
  mutate(record_number = "High")

replicate_info_ALL.df_5C <- replicate_info_ALL.df_5C %>%
  mutate(scenario = "5C") %>% 
  mutate(record_number = "Low")

replicate_info_ALL.df_5v0.2 <- replicate_info_ALL.df_5v0.2 %>%
  mutate(scenario = "5v0.2") %>% 
  mutate(record_number = "High")

replicate_info_ALL.df_5Cv0.2 <- replicate_info_ALL.df_5Cv0.2 %>%
  mutate(scenario = "5Cv0.2") %>% 
  mutate(record_number = "Low")

replicate_info_ALL.df_5v5 <- replicate_info_ALL.df_5v5 %>%
  mutate(scenario = "5v5") %>% 
  mutate(record_number = "High")

replicate_info_ALL.df_5Cv5 <- replicate_info_ALL.df_5Cv5 %>%
  mutate(scenario = "5Cv5") %>% 
  mutate(record_number = "Low")

all.df <- rbind(replicate_info_ALL.df_1[, c("n_po_gridA", "n_presence_gridA", "n_absence_gridA", "record_number")], 
                replicate_info_ALL.df_1C[, c("n_po_gridA", "n_presence_gridA", "n_absence_gridA", "record_number")], 
                replicate_info_ALL.df_5[, c("n_po_gridA", "n_presence_gridA", "n_absence_gridA", "record_number")], 
                replicate_info_ALL.df_5C[, c("n_po_gridA", "n_presence_gridA", "n_absence_gridA", "record_number")], 
                replicate_info_ALL.df_5v0.2[, c("n_po_gridA", "n_presence_gridA", "n_absence_gridA", "record_number")], 
                replicate_info_ALL.df_5Cv0.2[, c("n_po_gridA", "n_presence_gridA", "n_absence_gridA", "record_number")], 
                replicate_info_ALL.df_5v5[, c("n_po_gridA", "n_presence_gridA", "n_absence_gridA", "record_number")], 
                replicate_info_ALL.df_5Cv5[, c("n_po_gridA", "n_presence_gridA", "n_absence_gridA", "record_number")])


n_presences_all_reps.df <- all.df %>% 
  group_by(record_number) %>% 
  summarise(mean_n_po_gridA = mean(n_po_gridA),
            mean_n_presence_gridA = mean(n_presence_gridA),
            mean_n_absence_gridA = mean(n_absence_gridA),
            median_n_po_gridA = median(n_po_gridA),
            median_n_presence_gridA = median(n_presence_gridA),
            median_n_absence_gridA = median(n_absence_gridA),
            min_n_po_gridA = min(n_po_gridA),
            min_n_presence_gridA = min(n_presence_gridA),
            min_n_absence_gridA = min(n_absence_gridA),
            max_n_po_gridA = max(n_po_gridA),
            max_n_presence_gridA = max(n_presence_gridA),
            max_n_absence_gridA = max(n_absence_gridA),
            sd_n_po_gridA = sd(n_po_gridA),
            sd_n_presence_gridA = sd(n_presence_gridA),
            sd_n_absence_gridA = sd(n_absence_gridA))

write_csv(n_presences_all_reps.df, paste0(file.path(result_path),  "/ALL_Scenarios/N_Presences_ALL_reps.csv"))
