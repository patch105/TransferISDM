

library(tidyverse)

packages <- c("sf", "terra", "viridis", "here", "ggpubr", "ENMTools", "mgcv", "RColorBrewer", "RISDM", "inlabru", "flexsdm")

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

# Priors
my.control <- list(coord.names = c("x", "y"),
                   prior.mean = 0,
                   int.sd = 1000, # Intercept standard deviation
                   other.sd = 10, # Covariate effect standard deviation
                   addRandom = FALSE) # No random effect

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

# You still must fit a mesh even when using a model with no random effect!

# Use the ACBRs layer for making mesh, since it's less complex

# East.Ant.covs.stk.buffer <- buffer(East.Ant.covs.stk)

# mesh.default <- makeMesh(predictors.icefree.NorthEastAnt,
#                          max.n = c(500,200), # Default c(500,200)
#                          dep.range = NULL, # In raster projection units, default is 1/3 diagonal length of raster extent
#                          expans.mult = 1.5, # Default, 1.5 x dep.range
#                          max.edge = NULL, # Default c(0.2, 0.5)*dep.range
#                          cutoff = NULL, # Default 0.2*max.edge1
#                          offset = NULL, # Default is dep.range
#                          doPlot = TRUE)

boundary <- fmesher::fm_as_segm(st_as_sf(ACBRS.NorthEastAnt))

dep.range <- 10000 # 10km Set the range based on biology

mesh.range.10km.cutoff.50 <- fmesher::fm_mesh_2d_inla(loc = st_coordinates(PO.sf),
                                                      boundary = boundary,
                                                      max.edge = c(0.2, 0.5)*dep.range,
                                                      cutoff = 50,
                                                      crs=3031)

# p1 <- ggplot() +
#   inlabru::gg(mesh.range.10km.cutoff.50) +
#   # gg(mesh.vrt, color = "red") +
#   coord_sf(
#     xlim = c(st_bbox(Vestfold.landsat.sf)$xmin, st_bbox(Vestfold.landsat.sf)$xmax), 
#     ylim = c(st_bbox(Vestfold.landsat.sf)$ymin, st_bbox(Vestfold.landsat.sf)$ymax))
# 
# p2 <- ggplot() +
#   inlabru::gg(mesh.range.10km.cutoff.50)

# ggsave(plot = p1, filename = here("AntarcticISDM/Case_Study/Figures/mesh_range_10km_cutoff_50.png"), width = 10, height = 10, dpi = 300)
# ggsave(plot = p2, filename = here("AntarcticISDM/Case_Study/Figures/mesh_range_10km_cutoff_50_ALL.png"), width = 10, height = 10, dpi = 300)

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

print("Integrated Model Fitted")

# Presence-Only Model Fitting ---------------------------------------------

# 
# m.PO.no.GRF <- isdm(observationList = list(POdat = PO), 
#                     covars = East.Ant.covs.stk,
#                     mesh = mesh.range.10km.cutoff.50,
#                     responseNames = NULL,
#                     sampleAreaNames = NULL,
#                     distributionFormula = ~0 + elev + slope + aspect, # Linear w one cov
#                     biasFormula = ~1, #Intercept only
#                     artefactFormulas = NULL,
#                     control = my.control)

m.PO.GRF <- isdm(observationList = list(POdat = PO), 
                 covars = East.Ant.covs.stk,
                 mesh = mesh.range.10km.cutoff.50,
                 responseNames = NULL,
                 sampleAreaNames = NULL,
                 distributionFormula = ~0 + elev + slope + aspect, # Linear w one cov
                 biasFormula = ~1, #Intercept only
                 artefactFormulas = NULL,
                 control = my.control.GRF)

print("PO Model Fitted")

# Presence-Absence Model Fitting ------------------------------------------

#Intercept only
# 
# m.PA.no.GRF <- isdm(observationList = list(PAdat = PA_fit),
#                     covars = East.Ant.covs.stk,
#                     mesh = mesh.range.10km.cutoff.50,
#                     responseNames = c(PA = "presence"),
#                     sampleAreaNames = c(PA = "area"),
#                     distributionFormula = ~0 + elev + slope + aspect, # Linear w one cov
#                     biasFormula = NULL, #Intercept only
#                     artefactFormulas = list(PA = ~1), # Intercept only
#                     control = my.control)

# m.PA.GRF <- isdm(observationList = list(PAdat = PA_fit),
#                  covars = East.Ant.covs.stk,
#                  mesh = mesh.range.10km.cutoff.50,
#                  responseNames = c(PA = "presence"),
#                  sampleAreaNames = c(PA = "area"),
#                  distributionFormula = ~0 + elev + slope + aspect, # Linear w one cov
#                  biasFormula = NULL, #Intercept only
#                  artefactFormulas = list(PA = ~1), # Intercept only
#                  control = my.control)

# Stack models as a list
# mod.list <- list(integrated.no.GRF = m.int.no.GRF,
#                  integrated.GRF = m.int.GRF,
#                  PO.no.GRF = m.PO.no.GRF,
#                  PO.GRF = m.PO.GRF,
#                  PA.no.GRF = m.PA.no.GRF)

# mod.list <- list(integrated.GRF = m.int.GRF,
#                  PO.GRF = m.PO.GRF,
#                  PA.GRF = m.PA.GRF)

mod.list <- list(integrated.GRF = m.int.GRF,
                 PO.GRF = m.PO.GRF)
                 

# 3. MODEL EVALUATION -----------------------------------------------------

## Summary
map(mod.list, function(x) { summary(x)})
  
output.path <- here("AntarcticISDM/Case_Study/Figures")

## Residual plots
for(i in seq_along(mod.list)) {
  
  png(paste0(output.path, "/plot.isdm_", names(mod.list)[i], ".png"), width = 10, height = 10, units = "in", res = 300)
  
  plot(mod.list[[i]], nFigRow = 2, ask = FALSE)
  
  dev.off()
  
}

# 4. PREDICTION VESTFOLD -----------------------------------------------------


# Run for intensity predictions first

for(i in seq_along(mod.list)) {
  
  if(names(mod.list)[i] == "PA.no.GRF" || names(mod.list)[i] == "PA.GRF") {
    
    mod.list[[i]]$preds.INT.VESTFOLD <- predict(mod.list[[i]],
                                                covars = East.Ant.covs.stk,
                                                S = 50, 
                                                intercept.terms = "PA_Intercept",
                                                type = "intensity",
                                                includeRandom = T)
    
    png(paste0(output.path, "/plot.pred.int.vestfold_", names(mod.list)[i], ".png"), width = 10, height = 10, units = "in", res = 300)
    
    plot(mod.list[[i]]$preds.INT.VESTFOLD$field[[2]])                           
    
    dev.off()
    
    # Run for probability next 
    
    
    mod.list[[i]]$preds.PROB.VESTFOLD <- predict(mod.list[[i]],
                                                 covars = East.Ant.covs.stk,
                                                 S = 50, 
                                                 intercept.terms = "PA_Intercept",
                                                 type = "probability",
                                                 includeRandom = T)
    
    png(paste0(output.path, "/plot.pred.prob.vestfold_", names(mod.list)[i], ".png"), width = 10, height = 10, units = "in", res = 300)
    
    plot(mod.list[[i]]$preds.PROB.VESTFOLD$field[[2]])                           
    
    dev.off()
    
    
  } else {
    
    mod.list[[i]]$preds.INT.VESTFOLD <- predict(mod.list[[i]],
                                                covars = East.Ant.covs.stk,
                                                S = 50, 
                                                intercept.terms = "PO_Intercept",
                                                type = "intensity",
                                                includeRandom = T)
    
    png(paste0(output.path, "/plot.pred.int.vestfold_", names(mod.list)[i], ".png"), width = 10, height = 10, units = "in", res = 300)
    
    plot(mod.list[[i]]$preds.INT.VESTFOLD$field[[2]])                           
    
    dev.off()
    
    
    # Run for probability next 
    mod.list[[i]]$preds.PROB.VESTFOLD <- predict(mod.list[[i]],
                                                 covars = East.Ant.covs.stk,
                                                 S = 50, 
                                                 intercept.terms = "PO_Intercept",
                                                 type = "probability",
                                                 includeRandom = T)
    
    png(paste0(output.path, "/plot.pred.prob.vestfold_", names(mod.list)[i], ".png"), width = 10, height = 10, units = "in", res = 300)
    
    plot(mod.list[[i]]$preds.PROB.VESTFOLD$field[[2]])                           
    
    dev.off()
  }}




# Now plot random field ---------------------------------------------------

m.int.GRF$preds.GRF <- predict(m.int.GRF, 
                               covars = East.Ant.covs.stk,
                               S = 30,
                               intercept.terms = "PO_Intercept",
                               type = "link",
                               includeRandom = T,
                               includeFixed = F)

png(paste0(output.path, "/GRF.plot_m.int.GRF.png"), width = 10, height = 10, units = "in", res = 300)
plot(m.int.GRF$preds.GRF$field[[1:3]], nc = 3)
dev.off()

m.PO.GRF$preds.GRF <- predict(m.PO.GRF, 
                              covars = East.Ant.covs.stk,
                              S = 30,
                              intercept.terms = "PO_Intercept",
                              type = "link",
                              includeRandom = T,
                              includeFixed = F)

png(paste0(output.path, "/GRF.plot_m.PO.GRF.png"))
plot(m.PO.GRF$preds.GRF$field[[1:3]], nc = 3)
dev.off()

# m.PA.GRF$preds.GRF <- predict(m.PA.GRF, 
#                               covars = East.Ant.covs.stk,
#                               S = 30,
#                               intercept.terms = "PA_Intercept",
#                               type = "link",
#                               includeRandom = T,
#                               includeFixed = F)
# 
# png(paste0(output.path, "/GRF.plot_m.PA.GRF.png"),width = 10, height = 10, units = "in", res = 300)
# plot(m.PO.GRF$preds.GRF$field[[1:3]], nc = 3)
# dev.off()

## Another set of plots for GRF 
png(here("output", "GRF.plot.ZOOMED.m.int.GRF.png"), width = 10, height = 10, units = "in", res = 300)

pred.GRF.df <- as.data.frame(m.int.GRF$preds.GRF$field$Median, xy = T)

ggplot() +
  geom_tile(data = pred.GRF.df, aes(x = x, y = y, fill = Median)) +
  scale_fill_viridis() +
  coord_sf(
    xlim = c(st_bbox(Vestfold.landsat.sf)$xmin, st_bbox(Vestfold.landsat.sf)$xmax), 
    ylim = c(st_bbox(Vestfold.landsat.sf)$ymin, st_bbox(Vestfold.landsat.sf)$ymax)) +
  theme_bw()

dev.off()

png(here("output", "GRF.plot.ZOOMED.m.PO.GRF.png"), width = 10, height = 10, units = "in", res = 300)

pred.GRF.df <- as.data.frame(m.PO.GRF$preds.GRF$field$Median, xy = T)

ggplot() +
  geom_tile(data = pred.GRF.df, aes(x = x, y = y, fill = Median)) +
  scale_fill_viridis() +
  coord_sf(
    xlim = c(st_bbox(Vestfold.landsat.sf)$xmin, st_bbox(Vestfold.landsat.sf)$xmax), 
    ylim = c(st_bbox(Vestfold.landsat.sf)$ymin, st_bbox(Vestfold.landsat.sf)$ymax)) +
  theme_bw()

dev.off()

# png(here("output", "GRF.plot.ZOOMED.m.PA.GRF.png"), width = 10, height = 10, units = "in", res = 300)
# 
# pred.GRF.df <- as.data.frame(m.PA.GRF$preds.GRF$field$Median, xy = T)
# 
# ggplot() +
#   geom_tile(data = pred.GRF.df, aes(x = x, y = y, fill = Median)) +
#   scale_fill_viridis() +
#   coord_sf(
#     xlim = c(st_bbox(Vestfold.landsat.sf)$xmin, st_bbox(Vestfold.landsat.sf)$xmax), 
#     ylim = c(st_bbox(Vestfold.landsat.sf)$ymin, st_bbox(Vestfold.landsat.sf)$ymax)) +
#   theme_bw()
# 
# dev.off()


# 4. PREDICTION BUNGER -----------------------------------------------------

gc()

for(i in seq_along(mod.list)) {
  
  if(names(mod.list)[i] == "PA.no.GRF" || names(mod.list)[i] == "PA.GRF") { 
    
    mod.list[[i]]$preds.INT.BUNGER <- predict(mod.list[[i]],
                                              covars = predictors.icefree.Bunger.crop,
                                              S = 50, 
                                              intercept.terms = "PA_Intercept",
                                              type = "intensity",
                                              includeRandom = F)
    
    png(paste0(output.path, "/plot.pred.int.bunger_", names(mod.list)[i], ".png"), width = 10, height = 10, units = "in", res = 300)
    
    plot(mod.list[[i]]$preds.INT.BUNGER$field[[2]])                           
    
    dev.off()
    
    
    # Run for probability next 
    mod.list[[i]]$preds.PROB.BUNGER <- predict(mod.list[[i]],
                                               covars = predictors.icefree.Bunger.crop,
                                               S = 50, 
                                               intercept.terms = "PA_Intercept",
                                               type = "probability",
                                               includeRandom = F)
    
    png(paste0(output.path, "/plot.pred.prob.bunger_", names(mod.list)[i], ".png"), width = 10, height = 10, units = "in", res = 300)
    
    plot(mod.list[[i]]$preds.PROB.BUNGER$field[[2]])                           
    
    dev.off() 
    
    
  } else {
    
    mod.list[[i]]$preds.INT.BUNGER <- predict(mod.list[[i]],
                                              covars = predictors.icefree.Bunger.crop,
                                              S = 50, 
                                              intercept.terms = "PO_Intercept",
                                              type = "intensity",
                                              includeRandom = F)
    
    png(paste0(output.path, "/plot.pred.int.bunger_", names(mod.list)[i], ".png"), width = 10, height = 10, units = "in", res = 300)
    
    plot(mod.list[[i]]$preds.INT.BUNGER$field[[2]])                           
    
    dev.off()
    
    
    # Run for probability next 
    mod.list[[i]]$preds.PROB.BUNGER <- predict(mod.list[[i]],
                                               covars = predictors.icefree.Bunger.crop,
                                               S = 50, 
                                               intercept.terms = "PO_Intercept",
                                               type = "probability",
                                               includeRandom = F)
    
    png(paste0(output.path, "/plot.pred.prob.bunger_", names(mod.list)[i], ".png"), width = 10, height = 10, units = "in", res = 300)
    
    plot(mod.list[[i]]$preds.PROB.BUNGER$field[[2]])                           
    
    dev.off()
  }}




# Inference ---------------------------------------------------------------

# # Adding a temporary cell area layer
# cov_inter <- c(East.Ant.covs.stk, East.Ant.covs.stk[[1]]) 
# names(cov_inter) <- c(names(East.Ant.covs.stk), "tmp.habiArea") # Rename the new covariate
# values(cov_inter$tmp.habiArea) <- 1
# 
# # ELEVATION
# 
# posterior_plots <- map(mod.list, function(x) {
#   
#   interpPreds <- predict(x, 
#                          covars=cov_inter,
#                          habitatArea= "tmp.habiArea", S=50,
#                          includeFixed="elev",# Include fixed effect
#                          includeRandom=FALSE, 
#                          type="link") # Difference is you use type = "link"
#   
#   # compile covariate and prediction
#   pred.df <- as.data.frame(cbind(elev = values(East.Ant.covs.stk$elev),
#                                  values(interpPreds$field[[c("Median", "Lower", "Upper")]]))) 
#   
#   # Plot
#   pred.df <- pred.df[!is.na(pred.df$elev),]
#   pred.df <- pred.df[order(pred.df$elev),]
#   
#   matplot(pred.df[,1], pred.df[,2:4], pch = "", xlab = "cov", ylab = "Effect",
#           main = "Effect plot for elevation")
#   
#   polygon( x=c( pred.df$elev, rev( pred.df$elev)),
#            c(pred.df$Upper, rev(pred.df$Lower)),
#            col=grey(0.95), bor=NA)
#   
#   lines( pred.df[,c("Elevation","Median")], type='l', lwd=2)
#   
#   
# })
# 
# # SLOPE
# 
# posterior_plots <- map(mod.list, function(x) {
#   
#   interpPreds <- predict(x, 
#                          covars=cov_inter,
#                          habitatArea= "tmp.habiArea", S=50,
#                          includeFixed="slope",# Include fixed effect
#                          includeRandom=FALSE, 
#                          type="link") # Difference is you use type = "link"
#   
#   # compile covariate and prediction
#   pred.df <- as.data.frame(cbind(slope = values(East.Ant.covs.stk$slope),
#                                  values(interpPreds$field[[c("Median", "Lower", "Upper")]]))) 
#   
#   # Plot
#   pred.df <- pred.df[!is.na(pred.df$slope),]
#   pred.df <- pred.df[order(pred.df$slope),]
#   
#   matplot(pred.df[,1], pred.df[,2:4], pch = "", xlab = "cov", ylab = "Effect",
#           main = "Effect plot for slope")
#   
#   polygon( x=c( pred.df$slope, rev( pred.df$slope)),
#            c(pred.df$Upper, rev(pred.df$Lower)),
#            col=grey(0.95), bor=NA)
#   
#   lines( pred.df[,c("Slope","Median")], type='l', lwd=2)
#   
#   
# })
# 
# # ASPECT
# 
# posterior_plots <- map(mod.list, function(x) {
#   
#   interpPreds <- predict(x, 
#                          covars=cov_inter,
#                          habitatArea= "tmp.habiArea", S=50,
#                          includeFixed="aspect",# Include fixed effect
#                          includeRandom=FALSE, 
#                          type="link") # Difference is you use type = "link"
#   
#   # compile covariate and prediction
#   pred.df <- as.data.frame(cbind(aspect = values(East.Ant.covs.stk$aspect),
#                                  values(interpPreds$field[[c("Median", "Lower", "Upper")]]))) 
#   
#   # Plot
#   pred.df <- pred.df[!is.na(pred.df$aspect),]
#   pred.df <- pred.df[order(pred.df$aspect),]
#   
#   matplot(pred.df[,1], pred.df[,2:4], pch = "", xlab = "cov", ylab = "Effect",
#           main = "Effect plot for aspect")
#   
#   polygon( x=c( pred.df$aspect, rev( pred.df$aspect)),
#            c(pred.df$Upper, rev(pred.df$Lower)),
#            col=grey(0.95), bor=NA)
#   
#   lines( pred.df[,c("Aspect","Median")], type='l', lwd=2)
#   
#   
# })

# 
# # 5. VALIDATION -----------------------------------------------------------
# 
# # Validate with independent presence/absence data
# 
# # First, extract intensity predictions from locations of validation data 
# # Then, calculate prediction accuracy with the Brier Score
# 
# # Using the median posterior prediction of probability of presence per cell. Using the Brier Score via the package 'DescTools'
# 
library(DescTools)

for(i in seq_along(mod.list)) {
  
  # Extract the median prediction for each cell that has validation data
  val.med <- extract(mod.list[[i]]$preds.PROB.BUNGER$field$Median, PA_val[,1:2], xy = T)
  
  # Add the validation data P/A into the dataframe
  val.med <- val.med %>% 
    mutate(presence = PA_val$presence) %>% 
    filter(!is.na(Median))  
  
  print(paste0("Brier Score for ", names(mod.list)[i], ": ", DescTools::BrierScore(resp = val.med$presence,
                                                                                   pred = val.med$Median)))
  
  
}



# 6. FINAL PLOTS

# Final Plots -------------------------------------------------------------

for(i in seq_along(mod.list)) {
  
  pred.prob.df <- as.data.frame(mod.list[[i]]$preds.PROB.VESTFOLD$field$Median, xy = T)
  
  p <- ggplot() +
    geom_tile(data = pred.prob.df, aes(x = x, y = y, fill = Median)) +
    scale_fill_viridis() +
    geom_point(data = PA_fit, aes(x = x, y = y, color = as.factor(presence)), size = 2) +
    coord_sf(
      xlim = c(st_bbox(Vestfold.landsat.sf)$xmin, st_bbox(Vestfold.landsat.sf)$xmax), 
      ylim = c(st_bbox(Vestfold.landsat.sf)$ymin, st_bbox(Vestfold.landsat.sf)$ymax)) +
    theme_bw()
  
  ggsave(plot = p, filename = paste0(output.path, "/plot.prob.vs.true.vestfold_", names(mod.list)[i], ".png"), width = 10, height = 10, units = "in", dpi = 300)
  
  
}


for(i in seq_along(mod.list)) {
  
  pred.prob.df <- as.data.frame(mod.list[[i]]$preds.PROB.BUNGER$field$Median, xy = T)
  
  p <- ggplot() +
    geom_tile(data = pred.prob.df, aes(x = x, y = y, fill = Median)) +
    scale_fill_viridis() +
    geom_point(data = PA_val, aes(x = x, y = y, color = as.factor(presence)), size = 2) +
    theme_bw()
  
  ggsave(plot = p, filename = paste0(output.path, "/plot.prob.vs.true.bunger_", names(mod.list)[i], ".png"), width = 10, height = 10, units = "in", dpi = 300)
  
}

