

library(tidyverse)

packages <- c("sf", "terra", "viridis", "here", "ggpubr", "ENMTools", "mgcv", "RColorBrewer")

walk(packages, require, character.only = T)

here::here() 



# 1. DATA SETUP -----------------------------------------------------------


# Load ice-free land and ACBRs --------------------------------------------

ice_free <- st_read(here("AntarcticISDM/Case_Study/Data/Landsat_8_Derived_Outcrop_Dataset_2016.shp"))
ice_free <- st_transform(ice_free, crs = 3031)
ice_freeSPVE <- vect(ice_free)

# Load the Antarctic Conservation Biogeographic Regions, filter to East Antarctica
ACBRS <- st_read(here("AntarcticISDM/Case_Study/Data/ACBRs_v2_2016.shp"), crs = 3031) %>% filter(ACBR_Name == "East Antarctica")
ACBRS_SPVE <- vect(ACBRS)

# Load the North East Antarctica extent box
north_east_ant <- st_read(here("AntarcticISDM/Case_Study/Data/North_East_Ant_Box.shp"), crs = 3031)

# # Also trim ice-free land to East Antarctica
# ice_freeSPVE.EastAnt <- terra::crop(ice_freeSPVE, ACBRS_SPVE)

# Also trim ice-free land to just NORTH East Antarctica
ice_freeSPVE.NorthEastAnt <- terra::crop(ice_freeSPVE, north_east_ant)
# Do the same for the ACBRs for the mesh creation
ACBRS.NorthEastAnt <- terra::crop(ACBRS_SPVE, north_east_ant)

# Load 100m rema layer ----------------------------------------------------

rema <- terra::rast(here("AntarcticISDM/Case_Study/Data/rema_mosaic_100m_v2.0_filled_cop30_dem.tif"))


# Trim rema to ACBR East Ant buffered -------------------------------------

ACBRS_buffer <- buffer(ACBRS_SPVE, 10000) # 10 km Buffer around East Ant

ACBRS_buffer.proj <- terra::project(ACBRS_buffer, crs(rema))

elev <- terra::crop(rema, ext(ACBRS_buffer.proj))


# Calculate slope and aspect ----------------------------------------------

slope <- rast(here("AntarcticISDM/Case_Study/Data/REMA_East_Ant_v2_100m_slope.tif"))
aspect <- rast(here("AntarcticISDM/Case_Study/Data/REMA_East_Ant_v2_100m_aspect.tif"))


# Stack predictors --------------------------------------------------------

predictors <- c(elev, slope, aspect)
names(predictors) <- c("elev", "slope", "aspect")



# Load and prepare 10 km covariates ---------------------------------------

wind <- rast(here("AntarcticISDM/Case_Study/Data/David_ERA5_Land_wind_speed_mean.tif"))
names(wind) <- "wind"

# Change crs of ERA5-Land to match predictors
# if you don't use `res = ` then the temp is resampled
wind_prj <- terra::project(wind, y = crs(predictors), res = 11000)

# Aggregate predictors to resolution of ERA5-Land variables
predictors_agg <- terra::aggregate(predictors, fact = 110)



# Crop ERA5-Land variables to East Antarctica ---------------------------------------

# Also resample to match extent
wind_prj <- terra::crop(wind_prj, ext(ACBRS_buffer.proj))
wind_prj <- terra::resample(wind_prj, predictors_agg)


# Create single stack and propagate NA values
stck <- c(wind_prj, predictors_agg)
stck <- ENMTools::check.env(stck)


# Create data frame of values for GAM --------------------------------------

coarse_field <- data.frame(long = crds(stck)[,1],
                           lat = crds(stck)[,2],
                           w.speed = as.vector(values(stck[[1]], na.rm = T)),
                           elev = as.vector(values(stck[[2]], na.rm = T)),
                           slope = as.vector(values(stck[[3]], na.rm = T)),
                           aspect = as.vector(values(stck[[4]], na.rm = T)))


coarse_field <- coarse_field %>% 
  mutate(w.speed.sq = w.speed^2)

coarse_field <- coarse_field[, c(1,2,4,5,6,3,7)]


# Fit a thin plate regression spline model --------------------------------

kk <- 5000
gam.5000 <- gam(coarse_field$w.speed ~ elev + aspect + slope +
                                      s(long, lat, bs="tp", k = kk),
                                    method = "REML",
                                    data = coarse_field,
                                    family = gaussian())



# Checking the spline -----------------------------------------------------

output.path <- here("AntarcticISDM/Case_Study/Figures")

png(paste0(output.path, "/gam.check_kk_5000.png"), width = 10, height = 10, units = "in", res = 300)

gam.check(gam.5000)

dev.off()

summary(gam.5000)

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

resid <- data.frame(lon = coarse_field$long,
                    lat = coarse_field$lat,
                    res = gam.5000$residuals)

p <- ggplot(data = resid, aes(x = lon, y = lat, fill = res)) +
  geom_raster(interpolate = T) +
  scale_fill_gradient(colours = myPalette(100)) +
  labs(fill = "", x = "Easting", y = "Northing") + #, title = "Coarse Field") +
  theme_bw()

ggsave(plot = p, filename = paste0(output.path, "/gam.residuals.kk.5000.png"), width = 10, height = 10, units = "in", dpi = 300)


# Create a dataframe of values for the GAM prediction ---------------------

# Reproject ice_freeSPVE
ice_freeSPVE.proj <- terra::project(ice_freeSPVE, crs(rema))

# Trim prediction grid to ice-free land
predictors.icefree <- terra::mask(predictors, ice_freeSPVE.proj)

# Set all predictors to NA if NA in any
predictors.icefree <- ENMTools::check.env(predictors.icefree)

fine_grid <- data.frame(long = crds(predictors.icefree)[,1],
                        lat = crds(predictors.icefree)[,2],
                        elev = as.vector(values(predictors.icefree[[1]], na.rm = T)),
                        slope = as.vector(values(predictors.icefree[[2]], na.rm = T)),
                        aspect = as.vector(values(predictors.icefree[[3]], na.rm = T)))


# Predict at 100 m resolution and create raster layer ---------------------


downscale <- function(mod, fine_grid, to_disk, rast_path){
  
  pred_list <- list()
  
  print("downscaling...")
  
  pred <- predict(mod, fine_grid, se.fit = T)
  
    fine_field <- data.frame(long = fine_grid$long,
                             lat = fine_grid$lat,
                             zz = pred$fit,
                             se = pred$se.fit)
    
    pred_list[[i]] <- fine_field
    
    
    
    if(to_disk == TRUE){
      
      print("writing raster to disk")
      
      ras <- rast(pred_list[[i]],
                  type = "xyz",
                  crs = crs(predictors.icefree))
      
      writeRaster(ras,
                  here(rast_path,
                       paste0("mean_w.speed_100.tif")),
                  overwrite = T)
    }
  }
  
  return(pred_list)
}


#Create downscaled predictions
predictions <- downscale(mod = gam.5000,
                         fine_grid = fine_grid,
                         to_disk = T,
                         rast_path = here("AntarcticISDM/Case_Study/Outputs/"))


# ##############################################################################
# ################## TEMPERATURE ###############################################
# #############################################################################
# 
# 
# 
# # Load and prepare 1 km TEMP covariate ---------------------------------------
# 
# temp <- rast(here("AntarcticISDM/Case_Study/Data/AntAir_summer_mean_resamp.tif"))
# names(temp) <- "temp"
# 
# # Change crs of ERA5-Land to match predictors
# # if you don't use `res = ` then the temp is resampled
# temp_prj <- terra::project(temp, y = crs(predictors), res = 11000)
# 
# 
# # Crop Temperature variable to East Antarctica --------------------------------
# 
# # Also resample to match extent
# temp_prj <- terra::crop(temp_prj, ext(ACBRS_buffer.proj))
# temp_prj <- terra::resample(temp_prj, predictors_agg)
# 
# 
# # Create single stack and propagate NA values
# stck <- c(temp_prj, predictors_agg)
# stck <- ENMTools::check.env(stck)
# 
# 
# # Create data frame of values for GAM --------------------------------------
# 
# coarse_field <- data.frame(long = crds(stck)[,1],
#                            lat = crds(stck)[,2],
#                            temp = as.vector(values(stck[[1]], na.rm = T)),
#                            elev = as.vector(values(stck[[2]], na.rm = T)),
#                            slope = as.vector(values(stck[[3]], na.rm = T)),
#                            aspect = as.vector(values(stck[[4]], na.rm = T)))
# 
# 
# coarse_field <- coarse_field[, c(1,2,4,5,6,3,7)]
# 
# 
# # Fit a thin plate regression spline model --------------------------------
# 
# kk <- 5000
# gam.5000 <- gam(coarse_field$w.speed ~ elev + aspect + slope +
#                   s(long, lat, bs="tp", k = kk),
#                 method = "REML",
#                 data = coarse_field,
#                 family = gaussian())
# 
# 
# 
# # Checking the spline -----------------------------------------------------
# 
# output.path <- here("AntarcticISDM/Case_Study/Figures")
# 
# png(paste0(output.path, "/gam.check_kk_5000.png"), width = 10, height = 10, units = "in", res = 300)
# 
# gam.check(gam.5000)
# 
# dev.off()
# 
# summary(gam.5000)
# 
# myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
# 
# resid <- data.frame(lon = coarse_field$long,
#                     lat = coarse_field$lat,
#                     res = gam.5000$residuals)
# 
# p <- ggplot(data = resid, aes(x = lon, y = lat, fill = res)) +
#   geom_raster(interpolate = T) +
#   scale_fill_gradient(colours = myPalette(100)) +
#   labs(fill = "", x = "Easting", y = "Northing") + #, title = "Coarse Field") +
#   theme_bw()
# 
# ggsave(plot = p, filename = paste0(output.path, "/gam.residuals.kk.5000.png"), width = 10, height = 10, units = "in", dpi = 300)
# 
# 
# # Create a dataframe of values for the GAM prediction ---------------------
# 
# # Reproject ice_freeSPVE
# ice_freeSPVE.proj <- terra::project(ice_freeSPVE, crs(rema))
# 
# # Trim prediction grid to ice-free land
# predictors.icefree <- terra::mask(predictors, ice_freeSPVE.proj)
# 
# # Set all predictors to NA if NA in any
# predictors.icefree <- ENMTools::check.env(predictors.icefree)
# 
# fine_grid <- data.frame(long = crds(predictors.icefree)[,1],
#                         lat = crds(predictors.icefree)[,2],
#                         elev = as.vector(values(predictors.icefree[[1]], na.rm = T)),
#                         slope = as.vector(values(predictors.icefree[[2]], na.rm = T)),
#                         aspect = as.vector(values(predictors.icefree[[3]], na.rm = T)))
# 
# 
# # Predict at 100 m resolution and create raster layer ---------------------
# 
# 
# downscale <- function(mod, fine_grid, to_disk, rast_path){
#   
#   pred_list <- list()
#   
#   print("downscaling...")
#   
#   pred <- predict(mod, fine_grid, se.fit = T)
#   
#   fine_field <- data.frame(long = fine_grid$long,
#                            lat = fine_grid$lat,
#                            zz = pred$fit,
#                            se = pred$se.fit)
#   
#   pred_list[[i]] <- fine_field
#   
#   
#   
#   if(to_disk == TRUE){
#     
#     print("writing raster to disk")
#     
#     ras <- rast(pred_list[[i]],
#                 type = "xyz",
#                 crs = crs(predictors.icefree))
#     
#     writeRaster(ras,
#                 here(rast_path,
#                      paste0("mean_w.speed_100.tif")),
#                 overwrite = T)
#   }
# }
# 
# return(pred_list)
# }
# 
# 
# #Create downscaled predictions
# predictions <- downscale(mod = gam.5000,
#                          fine_grid = fine_grid,
#                          to_disk = T,
#                          rast_path = here("AntarcticISDM/Case_Study/Outputs/"))



