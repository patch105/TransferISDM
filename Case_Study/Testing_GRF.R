

library(tidyverse)

packages <- c("sf", "terra", "viridis", "here", "ggpubr", "ENMTools", "RColorBrewer", "RISDM", "inlabru")

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


# Trim predictors to ice-free land ----------------------------------------

predictors <- terra::project(predictors, "EPSG:3031")

predictors.icefree <- terra::mask(predictors, ice_freeSPVE)

# # Also masking predictors by a 200m buffered ice-free area for mesh creation
# ice_freeSPVE.EastAnt.buffered <- buffer(ice_freeSPVE.EastAnt, 200)
# ice_freeSPVE.NorthEastAnt.buffered <- buffer(ice_freeSPVE.NorthEastAnt, 200)
# 
# predictors.icefree.buffered <- terra::mask(predictors, ice_freeSPVE.EastAnt.buffered)

predictors.icefree.NorthEastAnt <- terra::mask(predictors, ice_freeSPVE.NorthEastAnt)
predictors.icefree.NorthEastAnt <- terra::crop(predictors.icefree.NorthEastAnt, ext(north_east_ant))


# Set all predictors to NA if NA in any
predictors.icefree.NorthEastAnt <- ENMTools::check.env(predictors.icefree.NorthEastAnt)

# Load Vestfold Hills polygon
Vestfold.landsat <- st_read(here("AntarcticISDM/Case_Study/Data/Vestfold_Landsat_Polygon.shp")) %>% 
  st_transform(3031) %>% 
  vect()

Vestfold.landsat.sf <- st_read(here("AntarcticISDM/Case_Study/Data/Vestfold_Landsat_Polygon.shp")) %>% 
  st_transform(3031) 

# Load Bunger Hills polygon
Bunger.landsat <- st_read(here("AntarcticISDM/Case_Study/Data/Bunger_Landsat_Polygon.shp")) %>% 
  st_transform(3031) %>% 
  vect()

predictors.icefree.Bunger <- terra::mask(predictors.icefree, Bunger.landsat)
predictors.icefree.Bunger.crop <- terra::crop(predictors.icefree.Bunger, ext(Bunger.landsat))

predictors.icefree.Vestfold <-terra::mask(predictors.icefree, Vestfold.landsat)
predictors.icefree.Vestfold.crop <- terra::crop(predictors.icefree.Vestfold, ext(Vestfold.landsat))


# Load biodiversity data --------------------------------------------------

# Load SCAR Biodiversity Database
Ant_biodf <- read.csv(here("AntarcticISDM/Case_Study/Data/SCAR_Ant_Terr_Bio_DataBase_MASTER_16-11-2023.csv"), header = T)

Ant_bio <- st_as_sf(Ant_biodf, 
                    coords = c("decimalLongitude", "decimalLatitude"), 
                    crs = 4326) %>% 
  filter(class %in% c("Lecanoromycetes", "Lichinomycetes", "Arthonionmycetes", "Leotiomycetes", "Dothideomycetes",
                      "Bryopsida",
                      "Andreaeopsida"))

# Removed PTM that identified non-lichenised fungi
Ant_bio <- Ant_bio %>% 
  filter(!PTM_ID %in% c("?", "29", "26 & 29", "75")) %>% 
  dplyr::select(scientificName, class, order, family, genus, coordinateUncertaintyInMetres)

Ant_bio <- st_transform(Ant_bio, 3031) #project to WGS_1984 Antarctic Polar Stereographic

# Load GBIF data
GBIF_df <- read.csv(here("AntarcticISDM/Case_Study/Data/GBIF_Lichen_Moss.csv"), header = T)

GBIF_sf <- st_as_sf(GBIF_df, 
                    coords = c("x", "y"), 
                    crs = 3031) %>% 
  dplyr::select(scientificNameClean, class, order, familyClean, genusClean, coordinateUncertaintyInMeters) %>% 
  rename(coordinateUncertaintyInMetres = coordinateUncertaintyInMeters, 
         family = familyClean, 
         genus = genusClean,
         scientificName = scientificNameClean)

# Combine datasets
SCAR_GBIF_bio.vect <- vect(rbind(Ant_bio, GBIF_sf))


# bio_east_ant_df <- terra::mask(SCAR_GBIF_bio.vect, ice_freeSPVE.EastAnt) %>% as.data.frame(geom = "XY")
# bio_east_ant_sf <- terra::mask(SCAR_GBIF_bio.vect, ice_freeSPVE.EastAnt) %>% st_as_sf()
# 
# bio_east_ant <- terra::mask(SCAR_GBIF_bio.vect, ice_freeSPVE.EastAnt)
# 
# bio_east_ant.no.Bunger.df <- terra::mask(bio_east_ant, Bunger.landsat, inverse = T) %>% as.data.frame(geom = "XY")
# bio_east_ant.no.Bunger.sf <- terra::mask(bio_east_ant, Bunger.landsat, inverse = T) %>% st_as_sf()

# TAKE 2 WITH NORTH EAST ANT
bio_north_east_ant.df <- terra::mask(SCAR_GBIF_bio.vect, ice_freeSPVE.NorthEastAnt) %>% as.data.frame(geom = "XY")
bio_north_east_ant.sf <- terra::mask(SCAR_GBIF_bio.vect, ice_freeSPVE.NorthEastAnt) %>% st_as_sf()

bio_north_east_ant <- terra::mask(SCAR_GBIF_bio.vect, ice_freeSPVE.NorthEastAnt)

# bio_east_ant.no.Bunger.df <- terra::mask(bio_east_ant, Bunger.landsat, inverse = T) %>% as.data.frame(geom = "XY")
# bio_east_ant.no.Bunger.sf <- terra::mask(bio_east_ant, Bunger.landsat, inverse = T) %>% st_as_sf()


count(bio_north_east_ant.df, class)

# Load presence-absence data Bunger ---------------------------------------

leishman <- read.csv(here("AntarcticISDM/Case_Study/Data/leishman_data.csv"))
leishman$easting <- as.character(leishman$easting)
leishman$northing_new <- as.character(leishman$northing_new)

leishman <- leishman %>% mutate(easting_final = paste0("5", easting, "00"))
leishman <- leishman %>% mutate(northing_final = paste0("26", northing_new, "00"))

leishman$easting_final <- as.numeric(leishman$easting_final)
leishman$northing_final <- as.numeric(leishman$northing_final)

leishman_sf <- st_as_sf(leishman,
                        coords = c("easting_final", "northing_final"),
                        crs = 32747) # UTM Zone 47S

# Project to WGS 84 Antarctic Polar Stereographic
leishman_sf <- st_transform(leishman_sf, 3031) 

leishman_df <- leishman_sf %>% 
  st_coordinates() %>%
  as.data.frame() %>% 
  bind_cols(st_drop_geometry(leishman_sf)) %>% 
  rename(x = X, y = Y) 


# Load presence-absence data Vestfold -------------------------------------

travers <- read.csv(here("AntarcticISDM/Case_Study/Data/Travers_Vestfold_PA_Survey.csv"))

travers_sf <- st_as_sf(travers,
                       coords = c("x", "y"),
                       crs = 4326) # WGS 84 geographic coordinates

travers_sf <- st_transform(travers_sf, 3031) #project to WGS_1984 Antarctic Polar Stereographic

travers_df <- travers_sf %>% 
  st_coordinates() %>%
  as.data.frame() %>% 
  bind_cols(st_drop_geometry(travers_sf)) %>% 
  rename(x = X, y = Y) 


# Format  -----------------------------------------------------------------

PO <- bio_north_east_ant.df %>% 
  filter(class == "Bryopsida") %>% 
  dplyr::select(x, y) 

PO.sf <- bio_north_east_ant.sf %>% 
  filter(class == "Bryopsida")

PA_fit <- travers_df %>% 
  dplyr::select(x, y, surface_moss) %>% 
  rename(presence = surface_moss)

PA_fit.sf <- travers_sf %>% 
  rename(presence = surface_moss)

PA_val <- leishman_df %>% 
  filter(species == "Moss") %>% 
  dplyr::select(x, y, presence)



# 1. MODEL FITTING --------------------------------------------------------

# Model 1. Comparison ISDM with no GRF vs. PA / PO with no GRF

## Model specification

East.Ant.covs.stk <- predictors.icefree.NorthEastAnt

my.control.GRF <- list(coord.names = c("x", "y"),
                       prior.mean = 0,
                       int.sd = 1000, # Intercept standard deviation
                       other.sd = 10, # Covariate effect standard deviation
                       prior.range = c(10, 0.1), # Prior chance 10% that parameter falls below range of 1km
                       prior.space.sigma = c(50, 0.1), # Prior chance 10% that parameter falls above SD of 5
                       addRandom = TRUE) # With random effect

PA_fit <- PA_fit %>% mutate(area = 80)
PA_val <- PA_val %>% mutate(area = 1000)


# Integrated Model Fitting

boundary <- fmesher::fm_as_segm(st_as_sf(ACBRS.NorthEastAnt))

dep.range <- 10000 # 10km Set the range based on biology

mesh.range.10km.cutoff.50 <- fmesher::fm_mesh_2d_inla(loc = st_coordinates(PO.sf),
                                                      boundary = boundary,
                                                      max.edge = c(0.2, 0.5)*dep.range,
                                                      cutoff = 50,
                                                      crs=3031)

# Integrated Model Fitting ------------------------------------------------
# 
# m.int.no.GRF <- isdm(observationList = list(POdat = PO,
#                                             PAdat = PA_fit),
#                      covars = East.Ant.covs.stk,
#                      mesh = mesh.range.10km.cutoff.50,
#                      responseNames = c(PO = NULL, PA = "presence"),
#                      sampleAreaNames = c(PO = NULL, PA = "area"),
#                      distributionFormula = ~0 + elev + slope + aspect, # Linear w covs
#                      biasFormula = ~1, #Intercept only
#                      artefactFormulas = list(PA = ~1), # Intercept only
#                      control = my.control)    


m.int.GRF <- isdm(observationList = list(POdat = PO,
                                         PAdat = PA_fit),
                  covars = East.Ant.covs.stk,
                  mesh = mesh.range.10km.cutoff.50,
                  responseNames = c(PO = NULL, PA = "presence"),
                  sampleAreaNames = c(PO = NULL, PA = "area"),
                  distributionFormula = ~0 + elev + slope + aspect, # Linear w covs
                  biasFormula = ~1, #Intercept only
                  artefactFormulas = list(PA = ~1), # Intercept only
                  control = my.control.GRF)    


