

library(tidyverse)

packages <- c("sf", "terra", "viridis", "here", "ggpubr", "ENMTools", "mgcv", "RColorBrewer", "RISDM", "inlabru", "flexsdm")

walk(packages, require, character.only = T)

here::here() 



# 1. DATA SETUP -----------------------------------------------------------



# Load ice-free land and ACBRs --------------------------------------------

ice_free <- st_read(here("Data/Landsat_8_Derived_Outcrop_Dataset_2016.shp"))
ice_free <- st_transform(ice_free, crs = 3031)
ice_freeSPVE <- vect(ice_free)

# Load the Antarctic Conservation Biogeographic Regions, filter to East Antarctica
ACBRS <- st_read(here("Data/ACBRs_v2_2016.shp"), crs = 3031) %>% filter(ACBR_Name == "East Antarctica")
ACBRS_SPVE <- vect(ACBRS)

# Also trim ice-free land to East Antarctica
ice_freeSPVE.EastAnt <- terra::crop(ice_freeSPVE, ACBRS_SPVE)


# Load 100m rema layer ----------------------------------------------------

rema <- terra::rast(here("Data/rema_mosaic_100m_v2.0_filled_cop30_dem.tif"))


# Trim rema to ACBR East Ant buffered -------------------------------------

ACBRS_buffer <- buffer(ACBRS_SPVE, 10000) # 10 km Buffer around East Ant

ACBRS_buffer.proj <- terra::project(ACBRS_buffer, crs(rema))

elev <- terra::crop(rema, ext(ACBRS_buffer.proj))


# Calculate slope and aspect ----------------------------------------------

slope <- rast(here("Data/REMA_East_Ant_v2_100m_slope.tif"))
aspect <- rast(here("Data/REMA_East_Ant_v2_100m_aspect.tif"))


# Stack predictors --------------------------------------------------------

predictors <- c(elev, slope, aspect)
names(predictors) <- c("elev", "slope", "aspect")


# Trim predictors to ice-free land ----------------------------------------

predictors.icefree <- terra::project(predictors, "EPSG:3031")

predictors.icefree <- terra::mask(predictors, ice_freeSPVE)

# Set all predictors to NA if NA in any
predictors.icefree <- ENMTools::check.env(predictors.icefree)

# Load Vestfold Hills polygon
Vestfold.landsat <- st_read(here("Data/Vestfold_Landsat_Polygon.shp")) %>% 
  st_transform(3031) %>% 
  vect()

# Load Bunger Hills polygon
Bunger.landsat <- st_read(here("Data/Bunger_Landsat_Polygon.shp")) %>% 
  st_transform(3031) %>% 
  vect()

predictors.icefree.Bunger <- terra::mask(predictors.icefree, Bunger.landsat)
predictors.icefree.Bunger.crop <- terra::crop(predictors.icefree.Bunger, ext(Bunger.landsat))

predictors.icefree.Vestfold <-terra::mask(predictors.icefree, Vestfold.landsat)
predictors.icefree.Vestfold.crop <- terra::crop(predictors.icefree.Vestfold, ext(Vestfold.landsat))



# Load biodiversity data --------------------------------------------------

# Load SCAR Biodiversity Database
Ant_biodf <- read.csv(here("Data/SCAR_Ant_Terr_Bio_DataBase_MASTER_16-11-2023.csv"), header = T)

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
GBIF_df <- read.csv(here("Data/GBIF_Lichen_Moss.csv"), header = T)

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


bio_east_ant_df <- terra::mask(SCAR_GBIF_bio.vect, ice_freeSPVE.EastAnt) %>% as.data.frame(geom = "XY")
bio_east_ant_sf <- terra::mask(SCAR_GBIF_bio.vect, ice_freeSPVE.EastAnt) %>% st_as_sf()

bio_east_ant <- terra::mask(SCAR_GBIF_bio.vect, ice_freeSPVE.EastAnt)

bio_east_ant.no.Bunger.df <- terra::mask(bio_east_ant, Bunger.landsat, inverse = T) %>% as.data.frame(geom = "XY")
bio_east_ant.no.Bunger.sf <- terra::mask(bio_east_ant, Bunger.landsat, inverse = T) %>% st_as_sf()


# Load presence-absence data Bunger ---------------------------------------

leishman <- read.csv(here("Data/leishman_data.csv"))
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

travers <- read.csv(here("Data/Travers_Vestfold_PA_Survey.csv"))

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

PO <- bio_east_ant.no.Bunger.df %>% 
  filter(class == "Bryopsida") %>% 
  dplyr::select(x, y) 

PA_fit <- leishman_df %>% 
  filter(species == "Moss") %>% 
  dplyr::select(x, y, presence)

PA_val <- travers_df %>% 
  dplyr::select(x, y, surface_moss) %>% 
  rename(presence = surface_moss)


# Calculating extrapolation -----------------------------------------------

# Convert covarites to a dataframe
Bunger.covs.df <- as.data.frame(predictors.icefree.Bunger.crop, xy = T)
Vestfold.covs.df <- as.data.frame(predictors.icefree.Vestfold.crop, xy = T)

# Adding presence column due to extra_eval requirements
# Trimming so just the covariates
training <- Vestfold.covs.df %>%
  mutate(Presence = 1) %>%
  .[,c("elev", "slope", "aspect", "Presence")]

projection <- Bunger.covs.df %>%
  .[,c("elev", "slope", "aspect")]


## NOTE - TAKES A WHILE
shape_extrap <- extra_eval(training_data = training,
                           pr_ab = "Presence",
                           projection_data = projection,
                           metric = "mahalanobis",
                           univar_comb = F)

shape_extrap <- cbind(shape_extrap, Bunger.covs.df[, c("x", "y")])

extrap.plot <- shape_extrap %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = extrapolation)) +
  scale_fill_viridis() +
  coord_fixed() +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank()) +
  ggtitle('Extrapolation - Vestfold to Bunger')

mean(shape_extrap$extrapolation, na.rm = T)
median(shape_extrap$extrapolation, na.rm = T)
min(shape_extrap$extrapolation, na.rm = T)
max(shape_extrap$extrapolation, na.rm = T)

# Save plot
ggsave(here("Figures/Extrapolation_Vestfold_to_Bunger.png"), extrap.plot, width = 10, height = 10, units = "in", dpi = 300)


# 1. MODEL FITTING --------------------------------------------------------

# Model 1. Comparison ISDM with no GRF vs. PA / PO with no GRF

## Model specification

East.Ant.covs.stk <- predictors.icefree

# Priors
my.control <- list(coord.names = c("x", "y"),
                   prior.mean = 0,
                   int.sd = 1000, # Intercept standard deviation
                   other.sd = 10, # Covariate effect standard deviation
                   # prior.range = c(1, 0.1), # Prior chance 10% that parameter falls below range of 1km
                   # prior.space.sigma = c(5, 0.1), # Prior chance 10% that parameter falls above SD of 5
                   addRandom = FALSE) # No random effect


PA_fit <- PA_fit %>% mutate(area = 80)
PA_val <- PA_val %>% mutate(area = 1000)


# Integrated Model Fitting

# You still must fit a mesh even when using a model with no random effect!

mesh.default <- makeMesh(East.Ant.covs.stk,
                         max.n = c(500,200), # Default c(500,200)
                         dep.range = NULL, # In raster projection units, default is 1/3 diagonal length of raster extent
                         expans.mult = 1.5, # Default, 1.5 x dep.range
                         max.edge = NULL, # Default c(0.2, 0.5)*dep.range
                         cutoff = NULL, # Default 0.2*max.edge1
                         offset = NULL, # Default is dep.range
                         doPlot = TRUE
)


# Extract and save mesh
mesh.vrt <- fmesher::fm_vertices(mesh.default)

st_write(mesh.vrt, here("Outputs/mesh_vrt.shp"))

mesh.plot <- ggplot() +
  inlabru::gg(mesh.default) +
  gg(mesh.vrt, color = "red")

ggsave(here("Figures/mesh_plot.png"), mesh.plot, width = 10, height = 10, units = "in", dpi = 300)


m.int.no.GRF <- isdm(observationList = list(POdat = PO,
                                            PAdat = PA_fit),
                     covars = East.Ant.covs.stk,
                     mesh = mesh.default,
                     responseNames = c(PO = NULL, PA = "presence"),
                     sampleAreaNames = c(PO = NULL, PA = "area"),
                     distributionFormula = ~0 + elev + slope + aspect, # Linear w covs
                     biasFormula = ~1, #Intercept only
                     artefactFormulas = list(PA = ~1), # Intercept only
                     control = my.control)    


# Presence-Only Model Fitting ---------------------------------------------


m.PO <- isdm(observationList = list(POdat = PO), 
             covars = East.Ant.covs.stk,
             mesh = mesh.default,
             responseNames = NULL,
             sampleAreaNames = NULL,
             distributionFormula = ~0 + elev + slope + aspect, # Linear w one cov
             biasFormula = ~1, #Intercept only
             artefactFormulas = NULL,
             control = my.control)


# Presence-Absence Model Fitting ------------------------------------------

# fm.PA <- isdm(observationList = list(PAdat = PA_fit),
#               covars = cov,
#               mesh = mesh.default,
#               responseNames = c(PA = "presence"),
#               sampleAreaNames = c(PA = "area"),
#               distributionFormula = dist.form,
#               biasFormula = NULL,
#               artefactFormulas = artefact.form,
#               control = my.control)

m.PA <- isdm(observationList = list(PAdat = PA_fit), 
             covars = East.Ant.covs.stk,
             mesh = mesh.default,
             responseNames = c(PA = "presence"),
             sampleAreaNames = c(PA = "area"),
             distributionFormula = ~0 + elev + slope + aspect, # Linear w one cov
             biasFormula = ~1, #Intercept only
             artefactFormulas = NULL,
             control = my.control)

# Stack models as a list
mod.list <- list(integrated = m.int,
                 PO = m.PO)


# 3. MODEL EVALUATION -----------------------------------------------------

## Summary
map(mod.list, function(x) {summary(x)})

## Residual plots
map(mod.list, function(x) {
  
  plot(x, nFigRow = 2, ask = FALSE)
  
})



# 4. PREDICTION -----------------------------------------------------------



# Run for intensity prediction first
for(i in 1:length(mod.list)) {
  
  mod.list[[i]]$preds <- predict(mod.list[[i]],
                                 covars = cov,
                                 S = 50, 
                                 intercept.terms = "PO_Intercept",
                                 type = "link")
  
  
  plot(mod.list[[i]]$preds$field[[1:3]], nc = 3)                           
  
}


# Now plot random field ---------------------------------------------------
# 
# for(i in 1:length(mod.list)) {
#   
#   mod.list[[i]]$preds.GRF <- predict(mod.list[[i]],
#                                      covars = cov,
#                                      S = 30, 
#                                      intercept.terms = "PO_Intercept",
#                                      type = "link",
#                                      includeRandom = TRUE,
#                                      includeFixed = FALSE)
#   
#   
#   plot(mod.list[[i]]$preds.GRF$field[[1:3]], nc = 3)                           
#   
# }


# Inference ---------------------------------------------------------------

# Adding a temporary cell area layer
cov_inter <- c(East.Ant.covs.stk, East.Ant.covs.stk[[1]]) 
names(cov_inter) <- c(names(East.Ant.covs.stk), "tmp.habiArea") # Rename the new covariate
values(cov_inter$tmp.habiArea) <- 1

# ELEVATION

posterior_plots <- map(mod.list, function(x) {
  
  interpPreds <- predict(x, 
                         covars=cov_inter,
                         habitatArea= "tmp.habiArea", S=50,
                         includeFixed="elev",# Include fixed effect
                         includeRandom=FALSE, 
                         type="link") # Difference is you use type = "link"
  
  # compile covariate and prediction
  pred.df <- as.data.frame(cbind(elev = values(East.Ant.covs.stk$elev),
                                 values(interpPreds$field[[c("Median", "Lower", "Upper")]]))) 
  
  # Plot
  pred.df <- pred.df[!is.na(pred.df$elev),]
  pred.df <- pred.df[order(pred.df$elev),]
  
  matplot(pred.df[,1], pred.df[,2:4], pch = "", xlab = "cov", ylab = "Effect",
          main = "Effect plot for elevation")
  
  polygon( x=c( pred.df$elev, rev( pred.df$elev)),
           c(pred.df$Upper, rev(pred.df$Lower)),
           col=grey(0.95), bor=NA)
  
  lines( pred.df[,c("Elevation","Median")], type='l', lwd=2)
  
  
})

# SLOPE

posterior_plots <- map(mod.list, function(x) {
  
  interpPreds <- predict(x, 
                         covars=cov_inter,
                         habitatArea= "tmp.habiArea", S=50,
                         includeFixed="slope",# Include fixed effect
                         includeRandom=FALSE, 
                         type="link") # Difference is you use type = "link"
  
  # compile covariate and prediction
  pred.df <- as.data.frame(cbind(slope = values(East.Ant.covs.stk$slope),
                                 values(interpPreds$field[[c("Median", "Lower", "Upper")]]))) 
  
  # Plot
  pred.df <- pred.df[!is.na(pred.df$slope),]
  pred.df <- pred.df[order(pred.df$slope),]
  
  matplot(pred.df[,1], pred.df[,2:4], pch = "", xlab = "cov", ylab = "Effect",
          main = "Effect plot for slope")
  
  polygon( x=c( pred.df$slope, rev( pred.df$slope)),
           c(pred.df$Upper, rev(pred.df$Lower)),
           col=grey(0.95), bor=NA)
  
  lines( pred.df[,c("Slope","Median")], type='l', lwd=2)
  
  
})

# ASPECT

posterior_plots <- map(mod.list, function(x) {
  
  interpPreds <- predict(x, 
                         covars=cov_inter,
                         habitatArea= "tmp.habiArea", S=50,
                         includeFixed="aspect",# Include fixed effect
                         includeRandom=FALSE, 
                         type="link") # Difference is you use type = "link"
  
  # compile covariate and prediction
  pred.df <- as.data.frame(cbind(aspect = values(East.Ant.covs.stk$aspect),
                                 values(interpPreds$field[[c("Median", "Lower", "Upper")]]))) 
  
  # Plot
  pred.df <- pred.df[!is.na(pred.df$aspect),]
  pred.df <- pred.df[order(pred.df$aspect),]
  
  matplot(pred.df[,1], pred.df[,2:4], pch = "", xlab = "cov", ylab = "Effect",
          main = "Effect plot for aspect")
  
  polygon( x=c( pred.df$aspect, rev( pred.df$aspect)),
           c(pred.df$Upper, rev(pred.df$Lower)),
           col=grey(0.95), bor=NA)
  
  lines( pred.df[,c("Aspect","Median")], type='l', lwd=2)
  
  
})


# 5. VALIDATION -----------------------------------------------------------

# Validate with independent presence/absence data

# First, extract intensity predictions from locations of validation data 
# Then, calculate prediction accuracy with the Brier Score

# Using the median posterior prediction of probability of presence per cell. Using the Brier Score via the package 'DescTools'

imap(mod.list, function(x, y) {
  
  
  # Extract the median prediction for each cell that has validation data
  val.med <- extract(x$preds.probs$field$Median, PA_val[,1:2], xy = T)
  
  # Add the validation data P/A into the dataframe
  val.med <- val.med %>% 
    mutate(presence = PA_val$presence)  
  
  print(paste0("Brier Score for ", y, ": ", DescTools::BrierScore(resp = val.med$presence,
                                                                  pred = val.med$Median)))
  
})



