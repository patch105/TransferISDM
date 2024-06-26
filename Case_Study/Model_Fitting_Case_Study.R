

library(tidyverse)

packages <- c("sf", "terra", "viridis", "here", "ggpubr", "ENMTools", "mgcv", "RColorBrewer", "RISDM", "inlabru")

walk(packages, require, character.only = T)

here::here() 


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
                      
p1 <- ggplot() +
  inlabru::gg(mesh.range.10km.cutoff.50) +
  # gg(mesh.vrt, color = "red") +
  coord_sf(
    xlim = c(st_bbox(Vestfold.landsat.sf)$xmin, st_bbox(Vestfold.landsat.sf)$xmax), 
    ylim = c(st_bbox(Vestfold.landsat.sf)$ymin, st_bbox(Vestfold.landsat.sf)$ymax))

p2 <- ggplot() +
  inlabru::gg(mesh.range.10km.cutoff.50)

# ggsave(plot = p1, filename = here("Case_Study/Figures/mesh_range_10km_cutoff_50.png"), width = 10, height = 10, dpi = 300)
# ggsave(plot = p2, filename = here("Case_Study/Figures/mesh_range_10km_cutoff_50_ALL.png"), width = 10, height = 10, dpi = 300)


rm(ice_free)
rm(ice_freeSPVE)
gc()

# Integrated Model Fitting ------------------------------------------------

m.int.no.GRF <- isdm(observationList = list(POdat = PO,
                                            PAdat = PA_fit),
                     covars = East.Ant.covs.stk,
                     mesh = mesh.range.10km.cutoff.50,
                     responseNames = c(PO = NULL, PA = "presence"),
                     sampleAreaNames = c(PO = NULL, PA = "area"),
                     distributionFormula = ~0 + elev + slope + aspect, # Linear w covs
                     biasFormula = ~1, #Intercept only
                     artefactFormulas = list(PA = ~1), # Intercept only
                     control = my.control)    

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

# Presence-Only Model Fitting ---------------------------------------------


m.PO.no.GRF <- isdm(observationList = list(POdat = PO), 
             covars = East.Ant.covs.stk,
             mesh = mesh.range.10km.cutoff.50,
             responseNames = NULL,
             sampleAreaNames = NULL,
             distributionFormula = ~0 + elev + slope + aspect, # Linear w one cov
             biasFormula = ~1, #Intercept only
             artefactFormulas = NULL,
             control = my.control)


# Presence-Absence Model Fitting ------------------------------------------

m.PA.no.GRF <- isdm(observationList = list(PAdat = PA_fit), 
             covars = East.Ant.covs.stk,
             mesh = mesh.range.10km.cutoff.50,
             responseNames = c(PA = "presence"),
             sampleAreaNames = c(PA = "area"),
             distributionFormula = ~0 + elev + slope + aspect, # Linear w one cov
             biasFormula = NULL, #Intercept only
             artefactFormulas = list(PA = ~1), # Intercept only
             control = my.control)

# Stack models as a list
mod.list <- list(integrated.no.GRF = m.int.no.GRF,
                 integrated.GRF = m.int.GRF,
                 PO.no.GRF = m.PO.no.GRF,
                 PO.GRF = m.PO.GRF,
                 PA.no.GRF = m.PA.no.GRF)

mod.list <- list(integrated.no.GRF= m.int.no.GRF,
                 PO.no.GRF = m.PO.no.GRF,
                 PA.no.GRF = m.PA.no.GRF)


