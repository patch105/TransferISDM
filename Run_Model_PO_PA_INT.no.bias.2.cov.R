library(tidyverse)

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

mods.reps <- map(extrap.reps.out.PA, function(extrap.type) {
  
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

# Extract and save summary results ----------------------------------------

# Make a dataframe of the format I want to save the results in
extrap.scenario.df <- data.frame(extrap.type = character(),
                                 rep = numeric(),
                                 mod.type = character(),
                                 beta1 = numeric(),
                                 beta2 = numeric(),
                                 beta1_25 = numeric(),
                                 beta1_975 = numeric(),
                                 beta2_25 = numeric(),
                                 beta2_975 = numeric())


# Get the names of the extrap types for indexing
extrap_names <- names(extrap.reps.out.mods)

# For every level of extrap type (Low, Mod, High)
for(extrap.type in seq_along(extrap.reps.out.mods)) {
  
  # Extract the name ("Low") for indexing from the names list
  name <- extrap_names[extrap.type] 
  
  # For every replicate
  for(rep in seq_along(extrap.type)) {
    
    # Extract the models dataframe [[name]] double brackets for list extract
    models_df <- extrap.reps.out.mods[[name]][[rep]]$models
      
    for (i in 1:2) { # NEED TO ADD BACK IN nrow(models_df) ONCE HAVE PA WORKING
      
      mod.summary <- models_df[i, "Summary"][[i]]
      
      
      extrap.scenario.df <<- extrap.scenario.df %>% 
        add_row(extrap.type = name,
                rep = rep,
                mod.type = as.character(models_df[i, "Mod.type"]),
                beta1 = mod.summary[[i]]$DISTRIBUTION$mean[1],
                beta2 = mod.summary[[i]]$DISTRIBUTION$mean[2],
                beta1_25 = mod.summary[[i]]$DISTRIBUTION[[3]][1],
                beta1_975 = mod.summary[[i]]$DISTRIBUTION[[5]][1],
                beta2_25 = mod.summary[[i]]$DISTRIBUTION[[3]][2],
                beta2_975 = mod.summary[[i]]$DISTRIBUTION[[5]][2]
                )
      
    }
    
    
         
      
    }}
    
    
  

# ARCHIVE -----------------------------------------------------------------

    
    return(list(models = list(integrated = m.int,
                              integrated.summary = summary(m.int),
                              PO = m.PO,
                              PO.summary = summary(m.PO),
                              PA = m.PA))) ## ADD PA SUMMARY LATER WHEN NOT NULL        
      
    
    ########################################################
    
    # Example input list structure
    example_data <- list(
      group1 = list(
        rep1 = data.frame(a = rnorm(5), b = runif(5)),
        rep2 = data.frame(a = rnorm(5), b = runif(5))
      ),
      group2 = list(
        rep1 = data.frame(a = rnorm(5), b = runif(5)),
        rep2 = data.frame(a = rnorm(5), b = runif(5))
      )
    )
    
    mods.reps <- map(example_data, function(extrap.type){
      
      map(extrap.type, function(rep) {
        
        # Create a data frame for this replicate
        list(models = tibble(
          Mod.type = c("Integrated", "PO"),
          Model = list("test", "test2"),
          Summary = list("test3", "test4") # Update PA summary when available
          
        ))
        
      })
      
    })
    
    ######################################################################
    
      
      
    
    
    
    





