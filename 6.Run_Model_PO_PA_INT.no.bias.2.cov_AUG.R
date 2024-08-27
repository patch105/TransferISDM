library(tidyverse)

packages <- c("sf", "terra", "ggpubr", "RISDM", "DescTools", "spatstat", "Metrics", "scoringutils")

walk(packages, require, character.only = T)


######## PO model random covariate ----------------------------------------

# spp_process <- cbind(x = lg.s$x, y = lg.s$y)
# colnames(spp_process) <- c("x", "y")
# 

# Model specification -----------------------------------------------------

# Priors
my.control <- list(coord.names = c("x", "y"),
                   prior.mean = 0,
                   int.sd = 1000, # Intercept standard deviation
                   other.sd = 10, # Covariate effect standard deviation
                   prior.range = c(1, 0.1), # Prior chance 10% that parameter falls below range of 1km
                   prior.space.sigma = c(5, 0.1)) # Prior chance 10% that parameter falls above SD of 5


# Load PA data and run models for each extrap type and replicate

mods.reps <- map(extrap.reps.out.PA, function(extrap.type) {
  
  map(extrap.type, function(rep) {
    
    # Load PA occurrence data -------------------------------------------------
    
    PA_fit <- rep$pa_a_df
    
    PO <- rep$PO_GridA
    names(PO) <- c("x", "y")
    
    # Load covariates ---------------------------------------------------------

    cov.rep <- rep$covs.SiteA.rast
    
    # Mesh default ------------------------------------------------------------
    
    mesh.default <- makeMesh(cov.rep,
                             max.n = c(5000, 2500), # Default c(500,200)
                             dep.range = NULL, # In raster projection units, default is 1/3 diagonal length of raster extent
                             expans.mult = 1.5, # Default, 1.5 x dep.range
                             max.edge = NULL, # Default c(0.2, 0.5)*dep.range
                             cutoff = NULL, # Default 0.2*max.edge1
                             offset = NULL, # Default is dep.range
                             doPlot = TRUE
    )
    
    # Integrated Model Fitting
    
    m.int <- isdm(observationList = list(POdat = PO,
                                         PAdat = PA_fit),
                  covars = cov.rep,
                  mesh = mesh.default,
                  responseNames = c(PO = NULL, PA = "presence"),
                  sampleAreaNames = c(PO = NULL, PA = "area"),
                  distributionFormula = ~0 + cov1 + cov2, # Linear w one cov
                  biasFormula = ~1, #Intercept only
                  artefactFormulas = list(PA = ~1), # Intercept only
                  control = my.control)
    
    # Presence-Only Model Fitting
    
    m.PO <- isdm(observationList = list(POdat = PO), 
                  covars = cov.rep,
                  mesh = mesh.default,
                  responseNames = NULL,
                  sampleAreaNames = NULL,
                  distributionFormula = ~0 + cov1 + cov2, # Linear w one cov
                  biasFormula = ~1, #Intercept only
                  artefactFormulas = NULL,
                  control = my.control)
    
    # Presence-Absence Model Fitting
    
    # m.PA <- list()
    m.PA <- isdm(observationList = list(PAdat = PA_fit), 
                        covars = cov.rep,
                        mesh = mesh.default,
                        responseNames = c(PA = "presence"),
                        sampleAreaNames = c(PA = "area"),
                        distributionFormula = ~0 + cov1 + cov2, # Linear w one cov
                        biasFormula = NULL, #Intercept only
                        artefactFormulas = list(PA = ~1), # Intercept only
                        control = my.control)
    
    # Create a data frame for this replicate
    list(models = tibble(
      Mod.type = c("Integrated", "PO", "PA"),
      Model = list(m.int, m.PO, m.PA),
      Summary = list(summary(m.int), summary(m.PO), NULL) # Update PA summary when available
      
    ))
    
  })
  
  
})



# Add models to list of extrap type replicates -----------------------------

extrap.reps.out.mods <- map2(extrap.reps.out.PA, mods.reps, ~map2(.x, .y, c))

rm(extrap.reps.out.PA)
rm(mods.reps)


##### ARCHIVE #####
# Covariates add together
# cov <- c(rast(rand.cov1, type = "xyz"),
#          rast(rand.cov2,  type = "xyz"))
    
    
    
    





