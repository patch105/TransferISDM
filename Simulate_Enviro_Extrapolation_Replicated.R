

# Version 2. Sample grid spatially varying covariates -----------------

# Specify number of replicates per extrapolation type
nreps <- 2

extrap_out <- list()
extrap.reps.out <- list(Low = list(), Moderate = list(), High = list())

# Function for generating Site A and Site B and calculating extrapolation
extrap_func <- function() {
  
  gridcov1.rast <- cov1
  gridcov2.rast <- cov2
  
  crs(gridcov1.rast) <- "epsg:3031" # Arbitrarily setting to a polar projection for later functions requiring a CRS
  
  crs(gridcov2.rast) <- "epsg:3031" # Arbitrarily setting to a polar projection for later functions requiring a CRS
  
  
  # Set size of grid for Site A (Reference)
  rast_sizeA <- c(100,50)
  # Set size of grid for Site B (Target)
  rast_sizeB <- c(100,50)
  
  # Get coords of overall grid domain boundary
  xmin <- min(x0)
  xmax <- max(x0)
  ymin <- min(y0)
  ymax <- max(y0)
  
  # Set the limit for x and y coord so box is completely inside the domain
  rand.limA <- c(xmax - rast_sizeA[1], ymax - rast_sizeA[2])
  rand.limB <- c(xmax - rast_sizeB[1], ymax - rast_sizeB[2])
  
  # Create random coordinate index for top corner of subgrid within grid domain
  # Then use this index on x0 to get the coordinate
  xmin.randA <- x0[round(runif(1, min = xmin, max = rand.limA[1]))]
  ymin.randA <- y0[round(runif(1, min = ymin, max = rand.limA[2]))]
  xmax.randA <- x0[xmin.randA + rast_sizeA[1]]
  ymax.randA <- y0[ymin.randA + rast_sizeA[2]]
  
  xmin.randB <- x0[round(runif(1, min = xmin, max = rand.limB[1]))]
  ymin.randB <- y0[round(runif(1, min = ymin, max = rand.limB[2]))]
  xmax.randB <- x0[xmin.randB + rast_sizeB[1]]
  ymax.randB <- y0[ymin.randB + rast_sizeB[2]]
  
  rand.gridA <- rast(xmin = xmin.randA, 
                     xmax = xmax.randA, 
                     ymin = ymin.randA, 
                     ymax = ymax.randA, 
                     nrows = rast_sizeA[1], 
                     ncols = rast_sizeA[2],
                     vals = 1:rast_sizeA[2]) # Just setting values for plotting and for converting to a dataframe
  
  rand.gridB <- rast(xmin = xmin.randB, 
                     xmax = xmax.randB, 
                     ymin = ymin.randB, 
                     ymax = ymax.randB, 
                     nrows = rast_sizeB[1], 
                     ncols = rast_sizeB[2],
                     vals = 1:rast_sizeB[2]) # Just setting values for plotting and for converting to a dataframe
  
  plot(gridcov1.rast)
  lines(ext(rand.gridA), lwd = 2, col = "red")
  lines(ext(rand.gridB), lwd = 2, col = "blue")
  
  extrap_out <- c(SiteA.rast = rand.gridA,
                  SiteB.rast = rand.gridB)
  
  # Extract covariates for the random grid Site A  --------------------------
  
  rand.grid.df <- as.data.frame(rand.gridA, xy = T)[,c("x", "y")]
  
  cov1.SiteA <- terra::extract(gridcov1.rast, rand.grid.df, xy = T) %>% rename(cov1 = layer)
  cov2.SiteA <- terra::extract(gridcov2.rast, rand.grid.df, xy = T) %>% rename(cov2 = layer)
  
  # Join to create one dataframe
  covs.SiteA <- left_join(cov1.SiteA, cov2.SiteA, by = join_by(ID, x, y))
  
  
  # Extract covariates for the random grid Site B ---------------------------
  
  rand.grid.df <- as.data.frame(rand.gridB, xy = T)[,c("x", "y")]
  
  cov1.SiteB <- terra::extract(gridcov1.rast, rand.grid.df, xy = T) %>% rename(cov1 = layer)
  cov2.SiteB <- terra::extract(gridcov2.rast, rand.grid.df, xy = T) %>% rename(cov2 = layer)
  
  # Join to create one dataframe
  covs.SiteB <- left_join(cov1.SiteB, cov2.SiteB, by = join_by(ID, x, y))
  
  
  # Plotting location of data in cov space ----------------------------------
  
  # ggplot() + 
  #   geom_point(data = covs.SiteA, aes(x = cov1, y = cov2), color = "grey") +
  #   geom_point(data = covs.SiteB, aes(x = cov1, y = cov2), color = "purple4") +
  #   theme_bw()
  
  
  
  # Calculating Overlap of Environment - Whole Grid -------------------------
  
  # Shape approach ----------------------------------------------------------
  
  # # Install flexsdm
  # remotes::install_github("sjevelazco/flexsdm")
  library(flexsdm)
  
  # Adding presence column due to extra_eval requirements
  # Trimming so just the covariates
  training <- covs.SiteA %>% 
    mutate(Presence = 1) %>% 
    .[,c("cov1", "cov2", "Presence")]
  
  projection <- covs.SiteB %>% 
    .[,c("cov1", "cov2")]
  
  shape_extrap <- extra_eval(training_data = training,
                             pr_ab = "Presence",
                             projection_data = projection,
                             metric = "mahalanobis",
                             univar_comb = F)
  
  shape_extrap <- cbind(shape_extrap, covs.SiteB[, c("x", "y")])
  
  # shape_extrap %>% 
  #   ggplot() + 
  #   geom_tile(aes(x = x, y = y, fill = extrapolation)) + 
  #   scale_fill_viridis() +
  #   coord_fixed() + 
  #   theme_bw() + 
  #   theme(axis.title.x = element_blank(),
  #         axis.title.y = element_blank(),
  #         legend.ticks = element_blank(),
  #         legend.title = element_blank()) +
  #   ggtitle('Extrapolation')
  
  summary.extrap = data.frame(mean = mean(shape_extrap$extrapolation),
                              median = median(shape_extrap$extrapolation),
                              min = min(shape_extrap$extrapolation),
                              max = max(shape_extrap$extrapolation))
  
  # Classify extrapolation type
  # If median extrap is less than or = to 50, low
  # If median extrap is not less than or = to 50, but is less than or = to 100, moderate
  # If median extrap is not less than or = to 100, high
  
  extrap.type <- ifelse(summary.extrap$median <= 10 , "Low", 
                        ifelse(summary.extrap$median <= 50, "Moderate", "High"))
  
  # Plotting data in covariate space with extrapolation  ------------------------
  
  extrap.plot <- ggplot() + 
    geom_point(data = covs.SiteA, aes(x = cov1, y = cov2), color = "grey") +
    geom_point(data = shape_extrap, aes(x = cov1, y = cov2, color = extrapolation)) +
    scale_color_viridis(option = "magma", direction = -1) +
    theme_bw() +
    theme(legend.ticks = element_blank())
  
  extrap_out <- c(extrap_out, list(SiteA.rast = rand.gridA,
                                   SiteB.rast = rand.gridB,
                                   covs.SiteA = covs.SiteA,
                                   covs.SiteB = covs.SiteB,
                                   extrap.plot = extrap.plot,
                                   extrap.df = shape_extrap,
                                   summary.extrap = summary.extrap))
  
  # If the output adds a required replicate to meet nreps, keep it
  if (length(extrap.reps.out[[extrap.type]]) < nreps) { 
    
     # Add extrap_out to the appropriate sublist in extrap.reps.out 
     extrap.reps.out[[extrap.type]] <- c(extrap.reps.out[[extrap.type]], list(extrap_out))
     
     
     }
  
  return(extrap.reps.out)
  
}

# Iterate over the function until you get to the desired nreps
while(length(extrap.reps.out$Low) < nreps | length(extrap.reps.out$Moderate) < nreps | length(extrap.reps.out$High) < nreps) {
  extrap.reps.out <- extrap_func()
}


