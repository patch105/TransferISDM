
# Unzip file
# untar(here("Data/rema_mosaic_1km_v2.0_filled_cop30.tar.gz"), list = TRUE)
# untar(here("Data/rema_mosaic_1km_v2.0_filled_cop30.tar.gz"), exdir = here("Data"))
# 
# # Load file
# rema <- terra::rast(here("Data/rema_mosaic_1km_v2.0_filled_cop30_dem.tif"))
# rema <- project(rema, "EPSG:3031")
# writeRaster(rema, here("Data/REMA_v2_elev.tif"), overwrite = T)

# Load rema
elev <- terra::rast("REMA_v2_elev.tif")

plot(elev)

# Load temperature
# AntAirIce <- rast(here("Data/AntAirIce/Annual_mean/AntAir_Yr_mean.tif"))
# AntAirIce_resamp <- resample(AntAirIce, rema, method = "bilinear")
# 
# writeRaster(AntAirIce_resamp, here("Data/AntAir_Yr_mean_resamp.tif"), overwrite = T)

temp <- rast("AntAir_summer_mean_resamp.tif")

test <- c(elev, temp)

filenames <- paste0( system.file("extdata", package="RISDM"),
                     c("/GambaExample_sqrtACC_23Mar28.tif",
                       "/GambaExample_sqrtDEM_23Mar28.tif",
                       "/GambaExample_SMRZ_23Mar28.tif"))
covars <- rast( filenames)

names( covars) <- c("ACC","DEM","SMRZ")


library(geodata)

studyarea <- c(-15, 65, 30, 75) # extent of study area. Default: Europe
studyarea <- c(120, 150, -30, -20)

# Get the WorldClim data for bioclimatic variables
# The 'getData' function is deprecated; use 'geodata' package for worldclim data instead
predictors_global <- geodata::worldclim_global(var='bio', res=10, path='../data/')

# Define the study area (assuming you have an 'extent' for it)
wp <- ext(studyarea)

# Crop the global predictors to the study area
predictors <- crop(predictors_global, wp)

# Create a mask for land area using the first layer
mask <- predictors[[1]]
mask[!is.na(mask)] <- 1

# Plot the mask for visualization (optional)
plot(mask)

plot(predictors)

cov1 <- predictors[[3]]




##############################################
# Choose a random 1000 x 1000 grid
cov1 <- elev
raster_extent <- ext(cov1)

# Generate random origin points for the new extent
x_min <- runif(1, raster_extent[1], raster_extent[2] - 1000)
y_min <- runif(1, raster_extent[3], raster_extent[4] - 1000)

# Define the new extent (1000 x 1000)
new_extent <- ext(x_min, x_min + 1000, y_min, y_min + 1000)

# Crop the raster
cropped_raster <- crop(cov1, new_extent)

# Plot the cropped raster
plot(cropped_raster)
