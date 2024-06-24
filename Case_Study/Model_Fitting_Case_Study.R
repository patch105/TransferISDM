

library(tidyverse)

packages <- c("sf", "terra", "viridis", "here", "ggpubr", "ENMTools", "mgcv", "RColorBrewer", "RISDM", "inlabru")

walk(packages, require, character.only = T)

here::here() 


# Model 1. Comparison ISDM with no GRF vs. PA / PO with no GRF

## Model specification


East.Ant.covs.stk <- predictors.icefree

# Priors
my.control <- list(coord.names = c("x", "y"),
                   prior.mean = 0,
                   int.sd = 1000, # Intercept standard deviation
                   other.sd = 10, # Covariate effect standard deviation
                   prior.range = c(1, 0.1), # Prior chance 10% that parameter falls below range of 1km
                   prior.space.sigma = c(5, 0.1), # Prior chance 10% that parameter falls above SD of 5
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

ggplot() +
  inlabru::gg(mesh.default) +
  gg(mesh.vrt, color = "red")
  



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



