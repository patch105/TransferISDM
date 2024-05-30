ibrary(tidyverse)

packages <- c("sf", "terra", "ggpubr", "RISDM", "DescTools", "spatstat", "Metrics", "scoringutils")

walk(packages, require, character.only = T)


######## PO model random covariate ----------------------------------------

spp_process <- cbind(x = lg.s$x, y = lg.s$y)
colnames(spp_process) <- c("x", "y")

# Load PO occurrence data ----------------------------------------------------

PO <- spp_process

# Load enviro. covs -------------------------------------------------------

# Covariates add together
# cov <- c(rast(rand.cov1, type = "xyz"),
#          rast(rand.cov2,  type = "xyz"))

cov <- c(rast(GRF.cov1, type = "xyz"),
         rast(GRF.cov2,  type = "xyz"))

crs(cov) <- "epsg:3031"

names(cov) <- c("cov1", "cov2")

# Mesh default ------------------------------------------------------------

mesh.default <- makeMesh(cov,
                         max.n = c(5000, 2500), # Default c(500,200)
                         dep.range = NULL, # In raster projection units, default is 1/3 diagonal length of raster extent
                         expans.mult = 1.5, # Default, 1.5 x dep.range
                         max.edge = NULL, # Default c(0.2, 0.5)*dep.range
                         cutoff = NULL, # Default 0.2*max.edge1
                         offset = NULL, # Default is dep.range
                         doPlot = TRUE
)


# Model specification -----------------------------------------------------

# Priors
my.control <- list(coord.names = c("x", "y"),
                   prior.mean = 0,
                   int.sd = 1000, # Intercept standard deviation
                   other.sd = 10, # Covariate effect standard deviation
                   prior.range = c(1, 0.1), # Prior chance 10% that parameter falls below range of 1km
                   prior.space.sigma = c(5, 0.1)) # Prior chance 10% that parameter falls above SD of 5

# Load PA data and run models for each extrap type and replicate

mods.reps <- map(reps.merged, function(extrap.type) {
  
  map(extrap.type, function(rep) {
    
    # Load PA occurrence data -------------------------------------------------
    
    PA_fit <- rep$pa_a_df
    PA_fit <- PA_fit %>% mutate(area = 0.02)
    
    PA_val <- rep$pa_b_df
    PA_val <- PA_val %>% mutate(area = 0.02)
    
    # Integrated Model Fitting
    
    m.int <- isdm(observationList = list(POdat = PO,
                                         PAdat = PA_fit),
                  covars = cov,
                  mesh = mesh.default,
                  responseNames = c(PO = NULL, PA = "presence"),
                  sampleAreaNames = c(PO = NULL, PA = "area"),
                  distributionFormula = ~0 + cov1 + cov2, # Linear w one cov
                  biasFormula = ~1, #Intercept only
                  artefactFormulas = list(PA = ~1), # Intercept only
                  control = my.control)
    
    # Presence-Only Model Fitting
    
    m.PO <- isdm(observationList = list(POdat = PO), 
                  covars = cov,
                  mesh = mesh.default,
                  responseNames = NULL,
                  sampleAreaNames = NULL,
                  distributionFormula = ~0 + cov1 + cov2, # Linear w one cov
                  biasFormula = ~1, #Intercept only
                  artefactFormulas = NULL,
                  control = my.control)
    
    # Presence-Absence Model Fitting
    
    m.PA <- list()
    
    return(list(models = list(integrated = m.int,
                integrated.summary = summary(m.int),
                PO = m.PO,
                PO.summary = summary(m.PO),
                PA = m.PA))) ## ADD PA SUMMARY LATER WHEN NOT NULL
    
  })
  
  
})


# Add models to list of extrap type replicates -----------------------------

reps.merged.mods <- map2(reps.merged, mods.reps, ~map2(.x, .y, c))


# Extract and save summary results ----------------------------------------

# Make a dataframe of the format I want
extrap.scenario.df <- data.frame(extrap.type = character(),
                                 rep = numeric(),
                                 mod.type = character(),
                                 beta1 = numeric(),
                                 beta2 = numeric(),
                                 beta1_25 = numeric(),
                                 beta1_975 = numeric(),
                                 beta2_25 = numeric(),
                                 beta2_975 = numeric())

for(i in seq_along(reps.merged.mods)) {
  
  for(rep in seq_along(i)) {
    
    print(rep)}
  
  }
  
  
  
test <- reps.merged.mods$Low

for(rep in seq_along(test)) {
  
  print(test[[rep]]$models)
  
}

for(extrap.type in names(reps.merged.mods)) {
  
  for(rep in seq_along(extrap.type)) {
    
    models_list <- reps.merged.mods[extrap.type][[rep]]
      
    # Reason I have this is because when you do this, it doesn't extract rep, it keeps it as list[[1]]$models
    
     for(mod.type in names(models_list[[1]]$models)) {
       
       print(mod.type)
     } 
      
    }}
    
    
    
    

    extrap.scenario.df <- extrap_scenario.df %>% add_row(extrap.type = names(reps.merged.mods)[extrap.type],
                                                         rep = rep,
                                                         mod.type =  )
    
    
      
      
      
      
    
    
    
    





