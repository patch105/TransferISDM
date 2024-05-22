
library(terra)
library(ggplot2)
library(tidyverse)
library(viridis)


# Load ice-free land ------------------------------------------------------

ice_free <- st_read(here("Data/add_rock_outcrop_medium_res_polygon_v7.3.shp"), crs = 3031)


# Load habitat covariates -------------------------------------------------




# Calculate topographic wetness index -------------------------------------


