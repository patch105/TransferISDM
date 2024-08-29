## NOTE - I CHANGED TO 50,50 FOR SPEED. BUT I NEED TO CHANGE BACK TO 100, 100 WHEN DOING PROPERLY

##TO CONSIDER:
# I removed this code because then the Site A and B were saved twice as SiteA.rast and rand.gridA
# extrap_out <- c(SiteA.rast = rand.gridA,
#                 SiteB.rast = rand.gridB)

# Version 2a. Sample grid GRF covariates - XZ code -----------------

# # Install flexsdm
# remotes::install_github("sjevelazco/flexsdm")
library(flexsdm)

# Specify number of replicates per extrapolation type
nreps <- 10

extrap_out <- list()
extrap.reps.out <- list(Low = list(), Moderate = list(), High = list())

plot(cov1)

# Function for generating Site A and Site B and calculating extrapolation
extrap_func <- function() {
  
  # Set size of grid (number of cells) for Site A (Reference)
  # NOTE - must be smaller than total cell number in x y directions
  rast_cellsA <- c(50, 50)
  rast_sizeA <- c(rast_cellsA[1]*bau_east_step, rast_cellsA[2]*bau_north_step)
  # Set size of grid (number of cells) for Site B (Target)
  rast_cellsB <- c(50, 50)
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
  

  lines(ext(rand.gridA), lwd = 2, col = "red")
  lines(ext(rand.gridB), lwd = 2, col = "blue")
  
  # Extract covariates for the random grid Site A  --------------------------
  
  rand.grid.df <- as.data.frame(rand.gridA, xy = T)[,c("x", "y")]
  
  # # Add 0.005 to grid coordinates because of weird problem with raster coords
  # rand.grid.df$x <- rand.grid.df$x + bau_east_step/2
  # rand.grid.df$y <- rand.grid.df$y + bau_north_step/2
  # 
  cov1.SiteA <- terra::extract(cov1, rand.grid.df, xy = T) %>% rename(cov1 = cov)
  cov2.SiteA <- terra::extract(cov2, rand.grid.df, xy = T) %>% rename(cov2 = cov)
  
  # Join to create one dataframe
  covs.SiteA <- left_join(cov1.SiteA, cov2.SiteA, by = join_by(ID, x, y))
  
  covs.SiteA.rast <- crop(covs, ext(rand.gridA))
  
  # Plot covariates for checking --------------------------------------------
  
   ggplot() +
    geom_tile(data = covs.SiteA, aes(x = x, y = y, fill = cov1))  
  ggplot() +
    geom_tile(data = covs.SiteA, aes(x = x, y = y, fill = cov2)) 
  
  # Extract covariates for the random grid Site B ---------------------------
  
  rand.grid.df <- as.data.frame(rand.gridB, xy = T)[,c("x", "y")]
  
  # # Add 0.005 to grid coordinates because of weird problem with raster coords
  # rand.grid.df$x <- rand.grid.df$x
  # rand.grid.df$y <- rand.grid.df$y
  # 
  cov1.SiteB <- terra::extract(cov1, rand.grid.df, xy = T) %>% rename(cov1 = cov)
  cov2.SiteB <- terra::extract(cov2, rand.grid.df, xy = T) %>% rename(cov2 = cov)
  
  # Join to create one dataframe
  covs.SiteB <- left_join(cov1.SiteB, cov2.SiteB, by = join_by(ID, x, y))
  
  covs.SiteB.rast <- crop(covs, ext(rand.gridB))

# Plot covariates for checking --------------------------------------------
  
  ggplot() +
    geom_tile(data = covs.SiteB, aes(x = x, y = y, fill = cov1))  
  ggplot() +
    geom_tile(data = covs.SiteB, aes(x = x, y = y, fill = cov2)) 
  
  # Plotting location of data in cov space ----------------------------------
  
  # ggplot() + 
  #   geom_point(data = covs.SiteA, aes(x = cov1, y = cov2), color = "grey") +
  #   geom_point(data = covs.SiteB, aes(x = cov1, y = cov2), color = "purple4") +
  #   theme_bw()
  
  
  
  # Calculating Overlap of Environment - Whole Grid -------------------------
  
  # Shape approach ----------------------------------------------------------
  
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
  
  extrap_out <- c(extrap_out, list(rand.gridA = rand.gridA,
                                   rand.gridB = rand.gridB,
                                   covs.SiteA = covs.SiteA,
                                   covs.SiteB = covs.SiteB,
                                   covs.SiteA.rast = covs.SiteA.rast,
                                   covs.SiteB.rast = covs.SiteB.rast,
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


# Iterate over the function until you get to the desired nreps for low, moderate, high
while(length(extrap.reps.out$Low) < nreps | length(extrap.reps.out$Moderate) < nreps | length(extrap.reps.out$High) < nreps) {
  extrap.reps.out <- extrap_func()
}

print(extrap.reps.out$Low[[1]]$extrap.plot)
print(extrap.reps.out$Moderate[[1]]$extrap.plot)
print(extrap.reps.out$High[[1]]$extrap.plot)







#### ARCHIVE ####
# Define possible regions for the new grid 
regions <- c("left", "right", "above", "below")

# # Randomly select a region
# region <- sample(regions, 1)
# 
# if (region == "left") {
#   xmin.randB <- eastings[which.min(abs(eastings - runif(1, min = round(xmin,2), max = round(xmin.randA - rast_sizeB[1],2))))]
#   ymin.randB <- northings[which.min(abs(northings - runif(1, min = round(ymin,2), max = round(rand.limB[2],2))))]
#   xmax.randB <- eastings[which.min(abs(eastings - (xmin.randB + rast_sizeB[1])))]
#   ymax.randB <- northings[which.min(abs(northings - (ymin.randB + rast_sizeB[2])))]
#   
# } else if (region == "right") {
#   xmin_new <- runif(1, round(xmax + 1, 2), round(xmax + 1 + width_new, 2))
#   xmax_new <- xmin_new + width_new
#   ymin_new <- runif(1, round(ymin - height_new, 2), round(ymax + 1, 2))
#   ymax_new <- ymin_new + height_new
# } else if (region == "above") {
#   ymin_new <- runif(1, round(ymax + 1, 2), round(ymax + 1 + height_new, 2))
#   ymax_new <- ymin_new + height_new
#   xmin_new <- runif(1, xmin - width_new, xmax + 1)
#   xmax_new <- xmin_new + width_new
# } else if (region == "below") {
#   ymax_new <- runif(1, ymin - height_new - 1, ymin - height_new)
#   ymin_new <- ymax_new - height_new
#   xmin_new <- runif(1, xmin - width_new, xmax + 1)
#   xmax_new <- xmin_new + width_new
# }


### ANOTHER ARCHIVE - OLD WAY OF CREATING RANDOM GRID A AND B

# Then use this index on x0 to get the coordinate
xmin.randA <- eastings[which.min(abs(eastings - runif(1, min = xmin, max = rand.limA[1])))] 
# xmin.randA <- xmin.randA - bau_east_step/2
ymin.randA <- northings[which.min(abs(northings - runif(1, min = ymin, max = rand.limA[2])))]
# ymin.randA <- ymin.randA - bau_north_step/2

xmax.randA <- eastings[which.min(abs(eastings - (xmin.randA + (rast_sizeA[1]))))]
# xmax.randA <- xmax.randA + bau_east_step/2
ymax.randA <- northings[which.min(abs(northings - (ymin.randA + (rast_sizeA[2]))))]
# ymax.randA <- ymax.randA + bau_north_step/2

rand.gridA <- rast(xmin = xmin.randA, 
                   xmax = xmax.randA, 
                   ymin = ymin.randA, 
                   ymax = ymax.randA, 
                   nrows = rast_cellsA[1], 
                   ncols = rast_cellsA[2],
                   vals = 1:rast_cellsA[2]) # Just setting values for plotting and for converting to a dataframe


# Generate grid *centroid* coordinates
# We generate a sequence of centroid values and then remove the last value to ensure that it is 50 x 50 in size
# This is because otherwise it would create a centroid for the last cell, which should be edge of the grid

eastings.randA <- seq(xmin.randA, xmax.randA, by = bau_east_step)
eastings.randA <- eastings.randA[-length(eastings.randA)] 

northings.randA <- seq(ymin.randA, ymax.randA, by = bau_north_step)
northings.randA <- northings.randA[-length(northings.randA)]

coords.randA <- as.matrix(expand.grid(eastings.randA, northings.randA))
coords.randA <-  1:rast_cellsA[2]
colnames(coords.randA) <- c("x", "y")


# Generate grid *centroid* coordinates for Site B
# We generate a sequence of centroid values and then remove the last value to ensure that it is 50 x 50 in size
# This is because otherwise it would create a centroid for the last cell, which should be edge of the grid

eastings.randB <- seq(xmin.randB, xmax.randB, by = bau_east_step)
eastings.randB <- eastings.randB[-length(eastings.randB)] 

northings.randB <- seq(ymin.randB, ymax.randB, by = bau_north_step)
northings.randB <- northings.randB[-length(northings.randB)]

coords.randB <- as.matrix(expand.grid(eastings.randB, northings.randB))
colnames(coords.randB) <- c("x", "y")

Generate_Grid_B <- function() {
  
xmin.randB <<- eastings[which.min(abs(eastings - (runif(1, min = xmin, max = rand.limB[1]))))]
  xmin.randB <<- xmin.randB - bau_east_step/2
  ymin.randB <<- northings[which.min(abs(northings - (runif(1, min = ymin, max = rand.limB[2]))))]
  ymin.randB <<- ymin.randB - bau_north_step/2
  
  xmax.randB <<- eastings[which.min(abs(eastings - (xmin.randB + rast_sizeB[1])))]
  xmax.randB <<- xmax.randB + bau_east_step/2
  ymax.randB <<- northings[which.min(abs(northings - (ymin.randB + rast_sizeB[2])))]
  ymax.randB <<- ymax.randB + bau_north_step/2
  
}

rand.gridB <- rast(xmin = xmin.randB, 
                   xmax = xmax.randB, 
                   ymin = ymin.randB, 
                   ymax = ymax.randB, 
                   nrows = rast_cellsB[1], 
                   ncols = rast_cellsB[2],
                   vals = 1:rast_cellsB[2]) # Just setting values for plotting and for converting to a dataframe



rand.gridA <- rast(coords.randA,
                   type = "xyz")

rand.gridB <- rast(coords.randB,
                   type = "xyz")

values(rand.gridB) <- 1:rast_cellsB[2]
# Just setting values for plotting and for converting to a dataframe
