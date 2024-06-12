
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

