## TO DO -WANT TO HAVE SO I CAN TURN ON AND OFF MODELS WITH A CHARACTER LIST (e.g. c("Integrated", "PO", "PA"))
## TO DO: FIX ERROR IN 6. RUN MODEL CODE 


# 6. Run Model ------------------------------------------------------------

library(sf)
library(RISDM)

run_model_func <- function(prior.mean,
                           int.sd,
                           other.sd,
                           prior.range,
                           prior.space.sigma,
                           reps.setup.list,
                           max.n,
                           dep.range,
                           expans.mult,
                           max.edge,
                           cutoff,
                           offset,
                           doPlot,
                           distributionFormula) {
  
  # Model specification -----------------------------------------------------
  
  # Priors
  my.control <- list(coord.names = c("x", "y"),
                     prior.mean = prior.mean,
                     int.sd = int.sd, # Intercept standard deviation
                     other.sd = other.sd, # Covariate effect standard deviation
                     prior.range = prior.range, # Prior chance 10% that parameter falls below range of 1km
                     prior.space.sigma = prior.space.sigma, # Prior chance 10% that parameter falls above SD of 5
                     addRandom = FALSE) # No random effect
                     
  my.control.GRF <- list(coord.names = c("x", "y"),
                         prior.mean = prior.mean,
                         int.sd = int.sd, # Intercept standard deviation
                         other.sd = other.sd, # Covariate effect standard deviation
                         prior.range = prior.range, # Prior chance 10% that parameter falls below range of 1km
                         prior.space.sigma = prior.space.sigma, # Prior chance 10% that parameter falls above SD of 5
                         addRandom = TRUE) # With random effect
  
  # Load PA data and run models for each extrap type and replicate
  
  mods.reps <- map(reps.setup.list, function(extrap.type) {
    
    map(extrap.type, function(rep) {
      
  # Load PA occurrence data -------------------------------------------------
      
      PA_fit <- rep$pa_a_df
      
      PO <- rep$PO_GridA
      names(PO) <- c("x", "y")
      
  # Load covariates ---------------------------------------------------------
      
      cov.rep <- rep$extrap.reps.out$covs.SiteA.rast
      
  # Mesh default ------------------------------------------------------------
      
      mesh.default <- makeMesh(cov.rep,
                               max.n = max.n, # Default c(500,200)
                               dep.range = dep.range, # In raster projection units, default is 1/3 diagonal length of raster extent
                               expans.mult = expans.mult, # Default, 1.5 x dep.range
                               max.edge = max.edge, # Default c(0.2, 0.5)*dep.range
                               cutoff = cutoff, # Default 0.2*max.edge1
                               offset = offset, # Default is dep.range
                               doPlot = doPlot
      )
      

# Integrated Model Fitting - no GRF ---------------------------------------
      
      m.int.no.GRF <- isdm(observationList = list(POdat = PO,
                                                  PAdat = PA_fit),
                           covars = cov.rep,
                           mesh = mesh.default,
                           responseNames = c(PO = NULL, PA = "presence"),
                           sampleAreaNames = c(PO = NULL, PA = "area"),
                           distributionFormula = distributionFormula,
                           biasFormula = ~1, #Intercept only
                           artefactFormulas = list(PA = ~1), # Intercept only
                           control = my.control)
      

# Integrated Model Fitting - with GRF -------------------------------------

      
      # m.int.GRF <- isdm(observationList = list(POdat = PO,
      #                                          PAdat = PA_fit),
      #                   covars = cov.rep,
      #                   mesh = mesh.default,
      #                   responseNames = c(PO = NULL, PA = "presence"),
      #                   sampleAreaNames = c(PO = NULL, PA = "area"),
      #                   distributionFormula = distributionFormula, # Linear w one cov
      #                   biasFormula = ~1, #Intercept only
      #                   artefactFormulas = list(PA = ~1), # Intercept only
      #                   control = my.control.GRF)
      
      
# Presence-Only Model Fitting - no GRF ----------------------------------
      
      
      m.PO.no.GRF <- isdm(observationList = list(POdat = PO), 
                   covars = cov.rep,
                   mesh = mesh.default,
                   responseNames = NULL,
                   sampleAreaNames = NULL,
                   distributionFormula = distributionFormula, # Linear w one cov
                   biasFormula = ~1, #Intercept only
                   artefactFormulas = NULL,
                   control = my.control)      

# Presence-Only Model Fitting - with GRF ----------------------------------

      
      # m.PO <- isdm(observationList = list(POdat = PO), 
      #              covars = cov.rep,
      #              mesh = mesh.default,
      #              responseNames = NULL,
      #              sampleAreaNames = NULL,
      #              distributionFormula = distributionFormula, # Linear w one cov
      #              biasFormula = ~1, #Intercept only
      #              artefactFormulas = NULL,
      #              control = my.control.GRF)
      

# Presence-Absence Model Fitting - no GRF ---------------------------------
      
      m.PA.no.GRF <- isdm(observationList = list(PAdat = PA_fit), 
                   covars = cov.rep,
                   mesh = mesh.default,
                   responseNames = c(PA = "presence"),
                   sampleAreaNames = c(PA = "area"),
                   distributionFormula = distributionFormula, # Linear w one cov
                   biasFormula = NULL, #Intercept only
                   artefactFormulas = list(PA = ~1), # Intercept only
                   control = my.control)
      
# Presence-Absence Model Fitting - with GRF ---------------------------------
      
      # m.PA <- isdm(observationList = list(PAdat = PA_fit), 
      #              covars = cov.rep,
      #              mesh = mesh.default,
      #              responseNames = c(PA = "presence"),
      #              sampleAreaNames = c(PA = "area"),
      #              distributionFormula = distributionFormula, # Linear w one cov
      #              biasFormula = NULL, #Intercept only
      #              artefactFormulas = list(PA = ~1), # Intercept only
      #              control = my.control.GRF)
      
      
      # Create a data frame for this replicate
      return(list(models = tibble(
        Mod.type = c("Integrated.no.GRF", "PO.no.GRF", "PA.no.GRF"),
        Model = list(m.int.no.GRF, m.PO.no.GRF, m.PA.no.GRF),
        Summary = list(summary(m.int.no.GRF), summary(m.PO.no.GRF), summary(m.PA.no.GRF))) 
        
      ))
      
    })
    
    
  })
 
  # Add models to list of extrap type replicates -----------------------------
  
  reps.setup.list <- map2(reps.setup.list, mods.reps, ~map2(.x, .y, c))
   
  return(reps.setup.list)
}
