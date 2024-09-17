

# 3. Simulate Environmental Extrapolation ---------------------------------

## NOTE - I CHANGED TO 50,50 FOR SPEED. BUT I NEED TO CHANGE BACK TO 100, 100 WHEN DOING PROPERLY

# # Install flexsdm
# remotes::install_github("sjevelazco/flexsdm")
library(flexsdm)

run_extrap_func <- function(ncores,
                            nreps,
                            rast_cellsA,
                            rast_cellsB,
                            bau_east_step,
                            bau_north_step,
                            eastings,
                            northings,
                            cov1,
                            cov2,
                            covs,
                            reps.setup.list,
                            extrap.reps.out) {
  
  extrap_func <- function() {
    
    # Set size of grid (number of cells) for Site A (Reference)
    # NOTE - must be smaller than total cell number in x y directions
    rast_sizeA <- c(rast_cellsA[1]*bau_east_step, rast_cellsA[2]*bau_north_step)
    # Set size of grid (number of cells) for Site B (Target)
    rast_sizeB <- c(rast_cellsB[1]*bau_east_step, rast_cellsB[2]*bau_north_step)
    
    # Get coords of overall grid domain boundary
    xmin <- min(eastings)
    xmax <- max(eastings)
    ymin <- min(northings)
    ymax <- max(northings)
    
    # Set the limit for x and y coord so box is completely inside the domain
    rand.limA <- c(xmax - rast_sizeA[1], ymax - rast_sizeA[2])
    rand.limB <- c(xmax - rast_sizeB[1], ymax - rast_sizeB[2])
    
    ##### GRID A ###### 
    # Create random coordinate index for (bottom left?) corner of subgrid within grid domain
    # Do this by generating a random number and finding the nearest eastings/northings value
    # Then use this index on x0 to get the coordinate
    # Had to take 0.01 off at the end to make it 50 cells
    
    xmin.randA <- eastings[which.min(abs(eastings - runif(1, min = round(xmin,2), max = round(rand.limA[1],2))))] 
    xmax.randA <- xmin.randA + rast_sizeA[1]
    
    xmin.randA <- xmin.randA - bau_east_step/2
    xmax.randA <- xmax.randA + bau_east_step/2 - bau_east_step
    
    ymin.randA <- northings[which.min(abs(northings - runif(1, min = round(ymin,2), max = round(rand.limA[2],2))))]
    ymax.randA <- ymin.randA + rast_sizeA[2]
    
    ymin.randA <- ymin.randA - bau_north_step/2
    ymax.randA <- ymax.randA + bau_north_step/2 - bau_east_step
    
    #### GRID B ########
    
    Generate_Grid_B <- function() {
      
      xmin.randB <<- eastings[which.min(abs(eastings - (runif(1, min = round(xmin,2), max = round(rand.limB[1],2)))))]
      xmax.randB <<- xmin.randB + rast_sizeB[1]
      
      xmin.randB <<- xmin.randB - bau_east_step/2
      xmax.randB <<- xmax.randB + bau_east_step/2 - bau_east_step
      
      ymin.randB <<- northings[which.min(abs(northings - (runif(1, min = round(ymin,2), max = round(rand.limB[2],2)))))]
      ymax.randB <<- ymin.randB + rast_sizeB[2]
      
      ymin.randB <<- ymin.randB - bau_north_step/2
      ymax.randB <<- ymax.randB + bau_north_step/2 - bau_east_step
      
      
    }
    
    # Run function to generate GRID B
    Generate_Grid_B()
    
    # Must be checked to see if overlaps with GRID A  
    # If overlap occurs, run the generate function again
    while(xmin.randB < xmax.randA & xmax.randB > xmin.randA & ymin.randB < ymax.randA & ymax.randB > ymin.randA) {
      
      Generate_Grid_B()
      
    } 
    
    ### Make the grids into rasters
    rand.gridA <- rast(xmin = xmin.randA, 
                       xmax = xmax.randA, 
                       ymin = ymin.randA, 
                       ymax = ymax.randA, 
                       nrows = rast_cellsA[1], 
                       ncols = rast_cellsA[2],
                       vals = 1:rast_cellsA[2]) # Just setting values for plotting and for converting to a dataframe
    
    
    
    crs(rand.gridA) <- "epsg:3857"
    
    
    rand.gridB <- rast(xmin = xmin.randB, 
                       xmax = xmax.randB, 
                       ymin = ymin.randB, 
                       ymax = ymax.randB, 
                       nrows = rast_cellsB[1], 
                       ncols = rast_cellsB[2],
                       vals = 1:rast_cellsB[2]) # Just setting values for plotting and for converting to a dataframe
    
    
    
    crs(rand.gridB) <- "epsg:3857"
    
    # Extract covariates for the random grid Site A  --------------------------
    
    rand.grid.df <- as.data.frame(rand.gridA, xy = T)[,c("x", "y")]
    
    cov1.SiteA <- terra::extract(cov1, rand.grid.df, xy = T) %>% rename(cov1 = cov)
    cov2.SiteA <- terra::extract(cov2, rand.grid.df, xy = T) %>% rename(cov2 = cov)
    
    # Join to create one dataframe
    covs.SiteA <- left_join(cov1.SiteA, cov2.SiteA, by = join_by(ID, x, y))
    
    covs.SiteA.rast <- crop(covs, ext(rand.gridA))
    
    # Extract covariates for the random grid Site B ---------------------------
    
    rand.grid.df <- as.data.frame(rand.gridB, xy = T)[,c("x", "y")]
    
    cov1.SiteB <- terra::extract(cov1, rand.grid.df, xy = T) %>% rename(cov1 = cov)
    cov2.SiteB <- terra::extract(cov2, rand.grid.df, xy = T) %>% rename(cov2 = cov)
    
    # Join to create one dataframe
    covs.SiteB <- left_join(cov1.SiteB, cov2.SiteB, by = join_by(ID, x, y))
    
    covs.SiteB.rast <- crop(covs, ext(rand.gridB))
    
    
    # Calculating Overlap of Environment - Whole Grid -------------------------
    
    # Shape approach ----------------------------------------------------------
    
    # Adding presence column due to extra_eval requirements
    # Trimming so just the covariates
    training <- covs.SiteA %>% 
      mutate(Presence = 1) %>% 
      .[,c("cov1", "cov2", "Presence")]
    
    projection <- covs.SiteB %>% 
      .[,c("cov1", "cov2")]
    
    shape_extrap <- flexsdm::extra_eval(training_data = training,
                                        pr_ab = "Presence",
                                        projection_data = projection,
                                        metric = "mahalanobis",
                                        univar_comb = F,
                                        n_cores = n_cores)
    
    shape_extrap <- cbind(shape_extrap, covs.SiteB[, c("x", "y")])
    
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
    
    extrap.reps.out <-list(rand.gridA = rand.gridA,
                           rand.gridB = rand.gridB,
                           covs.SiteA = covs.SiteA,
                           covs.SiteB = covs.SiteB,
                           covs.SiteA.rast = covs.SiteA.rast,
                           covs.SiteB.rast = covs.SiteB.rast,
                           extrap.plot = extrap.plot,
                           extrap.df = shape_extrap,
                           summary.extrap = summary.extrap)
    
    
    # If the output adds a required replicate to meet nreps, keep it
    # (x3 because there's a cov.list, latent.list, and extrap.reps.out)
    if (length(reps.setup.list[[extrap.type]]) < nreps) { 
      
      # Note that an output has been saved
      saved <- 1
      
      
    } else {
      saved <- 0
      
    }
    
    return(list(extrap.reps.out = extrap.reps.out, saved = saved, extrap.type = extrap.type))
  }
  
  ## Now run the extrapolation function and temporarily save
  temp <- extrap_func()
  
  # If the output from this hasn't been saved, keep running the function
  while(temp$saved != 1) {
    
    temp <- extrap_func()
    "Trying again to get a extrapolation type"
    
  }
  
  # Finally, return just the output which will be saved as extrap_reps_out
  return(list(extrap.reps.out = temp$extrap.reps.out, extrap.type = temp$extrap.type))
  
  
}




