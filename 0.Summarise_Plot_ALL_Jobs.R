

# Load and join replicates together ---------------------------------------

# 1. FIRST list all the files with n presences (for each Job)
file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "N_Presences_each_rep_Job*\\.csv$", full.names = TRUE, recursive = TRUE) 

# Read all the CSV files and save them to a list
npresence.list <- lapply(file_list, read.csv)

# Combine all dataframes in the list into one dataframe using rbind
replicate_info_ALL.df <- do.call(rbind, npresence.list)



# 2. THEN list all the files with validation dataframes (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation.*\\.csv$", full.names = TRUE, recursive = TRUE) 

true.validation.df.list <- lapply(file_list, read.csv)

true.validation.df <- do.call(rbind, true.validation.df.list)



# 3. THEN list all the files with validation dataframes for Site A (for each Job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "True_Validation.*\\.csv$", full.names = TRUE, recursive = TRUE) 

true.validation.SiteA.df.list <- lapply(file_list, read.csv)

true.validation.SiteA.df <- do.call(rbind, true.validation.SiteA.df.list)



# 4. FINALLy list all the files with model outputs (for each job)

file_list <- list.files(path = file.path(outpath, scenario_name), pattern = "Results_Summary.*\\.csv$", full.names = TRUE, recursive = TRUE) 

# Read all the CSV files and save them to a list
extrap.scenario.df.list <- lapply(file_list, read.csv)

# Combine all dataframes in the list into one dataframe using rbind
extrap.scenario.df <- do.call(rbind, extrap.scenario.df.list)


# 6c. Save number of presences all replicates ------------------------------

n_presences_all_reps.df <- replicate_info_ALL.df %>% 
  group_by(extrap.type) %>% 
  summarise(mean_n_po_gridA = mean(n_po_gridA),
            mean_n_presence_gridA = mean(n_presence_gridA),
            mean_n_absence_gridA = mean(n_absence_gridA),
            sd_n_po_gridA = sd(n_po_gridA),
            sd_n_presence_gridA = sd(n_presence_gridA),
            sd_n_absence_gridA = sd(n_absence_gridA))

write_csv(n_presences_all_reps.df, paste0(file.path(outpath, scenario_name),  "/Scenario_", scenario_name, "_N_Presences_ALL_reps.csv"))


# 11. Plot validation true intensity --------------------------------------

source("11.Plot_Validation_True_Intensity.R")

plot_validation_SiteB_func(true.validation.df = true.validation.df,
                           save = TRUE,
                           outpath = outpath,
                           scenario_name = scenario_name,
                           mod.type = mod.type)  


# 12. Plot Model Outputs --------------------------------------------------

source("12.Plot_Model_Outputs.R")

# plot_residuals_func(reps.setup.list = reps.setup.list,
#                     outpath = outpath,
#                     scenario_name = scenario_name)

plot_parameter_recovery_func(extrap.scenario.df = extrap.scenario.df,
                             outpath = outpath,
                             scenario_name = scenario_name,
                             save = TRUE,
                             beta1 = beta1,
                             beta2 = beta2,
                             beta0 = beta0,
                             scal = scal,
                             variance = variance,
                             mod.type = mod.type)



# OPTIONAL - PREDICT TO AND VALIDATE SITE A -------------------------------

plot_validation_SiteA_func(true.validation.df = true.validation.SiteA.df,
                           save = TRUE,
                           outpath = outpath,
                           scenario_name = scenario_name,
                           mod.type = mod.type)


