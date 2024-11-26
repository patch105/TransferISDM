## TO DO -WANT TO HAVE SO I CAN TURN ON AND OFF MODELS WITH A CHARACTER LIST (e.g. c("Integrated", "PO", "PA"))


# 6. Run Model ------------------------------------------------------------

# library(devtools)
# devtools::install_github( repo="Scott-Foster/RISDM", build_vignettes=FALSE) # Had to do force = T when I wanted to re-load
                          
library(sf, lib.loc=lib_loc)
# library(INLA)
# INLA:::inla.binary.install(os="CentOS Linux-7")

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
                           distributionFormula,
                           mod.type,
                           bias) {
                           
  
  
  # Model specification -----------------------------------------------------
  
  # Priors
  my.control <- list(coord.names = c("x", "y"),
                     prior.mean = prior.mean,
                     int.sd = int.sd, # Intercept standard deviation
                     other.sd = other.sd, # Covariate effect standard deviation
                     prior.range = prior.range, # Prior chance 10% that parameter falls below range of 1km
                     prior.space.sigma = prior.space.sigma, # Prior chance 10% that parameter falls above SD of 5
                     addRandom = FALSE, # No random effect
                     standardiseCovariates = FALSE) 
  
  my.control.GRF <- list(coord.names = c("x", "y"),
                         prior.mean = prior.mean,
                         int.sd = int.sd, # Intercept standard deviation
                         other.sd = other.sd, # Covariate effect standard deviation
                         prior.range = prior.range, # Prior chance 10% that parameter falls below range of 1km
                         prior.space.sigma = prior.space.sigma, # Prior chance 10% that parameter falls above SD of 5
                         addRandom = TRUE, # With random effect
                         standardiseCovariates = FALSE) 
  

  # Load PA data and run models for each extrap type and replicate
  
  mods.reps <- map(reps.setup.list, function(extrap.type) {
    
    map(extrap.type, function(rep) {
      
      # Load PA occurrence data -------------------------------------------------
      
      PA_fit <- rep$pa_a_df
      PA_fit <- PA_fit %>% mutate(presence = ifelse(presence == 1, TRUE, FALSE))
      
      PO <- rep$PO_GridA
      PO <- as.data.frame(PO)
     
      # Load covariates ---------------------------------------------------------
      
      cov.rep <- rep$extrap.reps.out$covs.SiteA.rast
      

      # Load bias covariate if relevant -----------------------------------------

      # If there's bias and you're going to model it
      if(bias == T & sum(grepl("bias", mod.type, fixed = T) > 0)) {
        
        # Add bias covariate to covariate stack
        Bias.rast <- rep$Bias.rast
        cov.rep <- c(cov.rep, Bias.rast)

      }
      
      # Mesh default ------------------------------------------------------------
      
      mesh.default <- makeMesh(cov.rep,
                               max.n = max.n, # Default c(500,200)
                               dep.range = dep.range, # In raster projection units, default is 1/3 diagonal length of raster extent
                               expans.mult = expans.mult, # Default, 1.5 x dep.range
                               max.edge = max.edge, # Default c(0.2, 0.5)*dep.range
                               cutoff = cutoff, # Default 0.2*max.edge1
                               offset = offset, # Default is dep.range
                               doPlot = doPlot)
      
      
      Model <- list()
      
      if(sum(grepl("no-GRF", mod.type, fixed = T)) > 0) { # If non-spatial model in list of models to run
        
        # Integrated Model Fitting - no GRF ---------------------------------------
        
        Model$m.int <- isdm(observationList = list(POdat = PO,
                                                    PAdat = PA_fit),
                             covars = cov.rep,
                             mesh = mesh.default,
                             responseNames = c(PO = NULL, PA = "presence"),
                             sampleAreaNames = c(PO = NULL, PA = "area"),
                             distributionFormula = distributionFormula,
                             biasFormula = ~1, # Intercept only
                             artefactFormulas = list(PA = ~1), # Intercept only
                             control = my.control)
        
        # Presence-Only Model Fitting - no GRF ----------------------------------
        
        
        Model$m.PO <- isdm(observationList = list(POdat = PO), 
                            covars = cov.rep,
                            mesh = mesh.default,
                            responseNames = NULL,
                            sampleAreaNames = NULL,
                            distributionFormula = distributionFormula, # Linear w one cov
                            biasFormula = ~1, # Intercept only
                            artefactFormulas = NULL,
                            control = my.control)     
        
        # Presence-Absence Model Fitting - no GRF ---------------------------------
        
        Model$m.PA <- isdm(observationList = list(PAdat = PA_fit), 
                            covars = cov.rep,
                            mesh = mesh.default,
                            responseNames = c(PA = "presence"),
                            sampleAreaNames = c(PA = "area"),
                            distributionFormula = distributionFormula, # Linear w one cov
                            biasFormula = NULL, #Not applicable to PA
                            artefactFormulas = list(PA = ~1), # Intercept only
                            control = my.control)
        
      } 
      
      if(sum(grepl("spatial", mod.type, fixed = T)) > 0) { # If spatial model in list of models to run
        
        # Integrated Model Fitting - with GRF -------------------------------------
        
        
        Model$m.int.GRF <- isdm(observationList = list(POdat = PO,
                                                        PAdat = PA_fit),
                                 covars = cov.rep,
                                 mesh = mesh.default,
                                 responseNames = c(PO = NULL, PA = "presence"),
                                 sampleAreaNames = c(PO = NULL, PA = "area"),
                                 distributionFormula = distributionFormula, # Linear w one cov
                                 biasFormula = ~1, #Intercept only
                                 artefactFormulas = list(PA = ~1), # Intercept only
                                 control = my.control.GRF)
        
        # Presence-Only Model Fitting - with GRF ----------------------------------
        
        
        Model$m.PO.GRF <- isdm(observationList = list(POdat = PO),
                                covars = cov.rep,
                                mesh = mesh.default,
                                responseNames = NULL,
                                sampleAreaNames = NULL,
                                distributionFormula = distributionFormula, # Linear w one cov
                                biasFormula = ~1, #Intercept only
                                artefactFormulas = NULL,
                                control = my.control.GRF)
        
        # Presence-Absence Model Fitting - with GRF ---------------------------------
        
        Model$m.PA.GRF <- isdm(observationList = list(PAdat = PA_fit), 
                                covars = cov.rep,
                                mesh = mesh.default,
                                responseNames = c(PA = "presence"),
                                sampleAreaNames = c(PA = "area"),
                                distributionFormula = distributionFormula, # Linear w one cov
                                biasFormula = NULL, #Intercept only
                                artefactFormulas = list(PA = ~1), # Intercept only
                                control = my.control.GRF)
        
        }
        
      if(sum(grepl("no-GRF.bias", mod.type, fixed = T)) > 0) { # If non-spatial model with bias
        
        # Integrated Model Fitting - no GRF ---------------------------------------
        
        Model$m.int.bias <- isdm(observationList = list(POdat = PO,
                                                        PAdat = PA_fit),
                                 covars = cov.rep,
                                 mesh = mesh.default,
                                 responseNames = c(PO = NULL, PA = "presence"),
                                 sampleAreaNames = c(PO = NULL, PA = "area"),
                                 distributionFormula = distributionFormula,
                                 biasFormula = ~1 + bias, 
                                 artefactFormulas = list(PA = ~1), # Intercept only
                                 control = my.control)
        
        # Presence-Only Model Fitting - no GRF ----------------------------------
        
        
        Model$m.PO.bias <- isdm(observationList = list(POdat = PO), 
                                covars = cov.rep,
                                mesh = mesh.default,
                                responseNames = NULL,
                                sampleAreaNames = NULL,
                                distributionFormula = distributionFormula, # Linear w one cov
                                biasFormula = ~1 + bias, 
                                artefactFormulas = NULL,
                                control = my.control)     
        
        
      } 
      
      if(sum(grepl("spatial.bias", mod.type, fixed = T)) > 0) { # If spatial model with bias
        
        # Integrated Model Fitting - with GRF -------------------------------------
        
        Model$m.int.GRF.bias <- isdm(observationList = list(POdat = PO,
                                                            PAdat = PA_fit),
                                     covars = cov.rep,
                                     mesh = mesh.default,
                                     responseNames = c(PO = NULL, PA = "presence"),
                                     sampleAreaNames = c(PO = NULL, PA = "area"),
                                     distributionFormula = distributionFormula, # Linear w one cov
                                     biasFormula = ~1 + bias, 
                                     artefactFormulas = list(PA = ~1), # Intercept only
                                     control = my.control.GRF)
        
        # Presence-Only Model Fitting - with GRF ----------------------------------
        
        
        Model$m.PO.GRF.bias <- isdm(observationList = list(POdat = PO),
                                    covars = cov.rep,
                                    mesh = mesh.default,
                                    responseNames = NULL,
                                    sampleAreaNames = NULL,
                                    distributionFormula = distributionFormula, # Linear w one cov
                                    biasFormula = ~1 + bias, 
                                    artefactFormulas = NULL,
                                    control = my.control.GRF)
        
      }

      # List and summarise all models run ---------------------------------------

      Mod.type <- names(Model)
      
      Summary <- lapply(Model, summary) # Summarise all model objects
    
      # Create a data frame for this replicate
      return(list(models = tibble(Mod.type = Mod.type,
                                   Model = Model,
                                   Summary = Summary)))
      

    })
    
    
  })
  
  # Add models to list of extrap type replicates -----------------------------
  
  reps.setup.list <- map2(reps.setup.list, mods.reps, ~map2(.x, .y, c))
  
  return(reps.setup.list)

  }
