

# 3. Simulate Environmental Extrapolation ---------------------------------

# # Install flexsdm
# remotes::install_github("sjevelazco/flexsdm")
# library(flexsdm, lib.loc = lib_loc)
library(ks)

run_extrap_func <- function(n_cores,
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
    
    
    # Calculate the distance between the centre of the Site A and B -----------
    
    # Get centre of Site A & turn into point
    centreA <- data.frame(x = mean(c(xmin.randA, xmax.randA)), 
                          y = mean(c(ymin.randA, ymax.randA))) %>% 
      vect(geom=c("x", "y"), crs = "epsg:3857")
    
    # Get centre of Site B & turn into point
    centreB <- data.frame(x = mean(c(xmin.randB, xmax.randB)), 
                          y = mean(c(ymin.randB, ymax.randB))) %>% 
      vect(geom=c("x", "y"), crs = "epsg:3857")
    
    Site.distance <- terra::distance(centreA, centreB)
    
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
    
    
    # Batthacharyya's Affinity ------------------------------------------------
    
    training <- covs.SiteA %>% 
      .[,c("cov1", "cov2")]
    
    projection <- covs.SiteB %>% 
      .[,c("cov1", "cov2")]
    
    xmin <- min(c(training$cov1, projection$cov1)) - 0.5
    xmax <- max(c(training$cov1, projection$cov1)) + 0.5
    ymin <- min(c(training$cov2, projection$cov2)) - 0.5
    ymax <- max(c(training$cov2, projection$cov2)) + 0.5
    
    xmin <- min(c(training$cov1, projection$cov1))
    xmax <- max(c(training$cov1, projection$cov1)) 
    ymin <- min(c(training$cov2, projection$cov2)) 
    ymax <- max(c(training$cov2, projection$cov2))
    
    # How many grid points to evaluate against?
    eval.grid.res <- 150
    
    # Set up a shared grid domain
    domain <- rast(xmin = xmin, 
                   xmax = xmax, 
                   ymin = ymin, 
                   ymax = ymax, 
                   ncols = eval.grid.res, 
                   nrows = eval.grid.res,
                   vals = 1) %>% 
      as.data.frame(xy = T) %>% 
      .[,c("x", "y")]
    
    
    Ref_density <- ks::kde(x = training, eval.points = domain)
    
    Target_density <- ks::kde(x = projection, eval.points = domain)
    
    Ref_data <- data.frame(
      cov1 = Ref_density$eval.points[[1]],
      cov2 = Ref_density$eval.points[[2]],
      density = as.vector(Ref_density$estimate)
    ) %>% rast()
    
    
    Target_data <- data.frame(
      cov1 = Target_density$eval.points[[1]],
      cov2 = Target_density$eval.points[[2]],
      density = as.vector(Target_density$estimate)
    ) %>% rast()
    
    
    # Normalise the densities to sum to 1
    tot <- global(Ref_data, "sum")$sum
    Ref_data <- Ref_data / tot
    global(Ref_data, "sum")
    
    tot <- global(Target_data, "sum")$sum
    Target_data <- Target_data / tot
    global(Target_data, "sum")
    
    # Calculate the Bhattacharyya Affinity  ------------------------------------
    
    BA <- sum(sqrt(abs(values(Ref_data))) * 
                sqrt(abs(values(Target_data))))
    
    # Calculate the Bhattacharyya Distance  ------------------------------------
    BD <- -log(BA)
    
    summary.extrap = data.frame(BA = BA,
                                BD = BD)
    
    # Classify extrapolation type
    # If median extrap is less than or = to 50, low
    # If median extrap is not less than or = to 50, but is less than or = to 100, moderate
    # If median extrap is not less than or = to 100, high
    
    extrap.type <- ifelse(summary.extrap$BA <= 1/3 , "Low", 
                          ifelse(summary.extrap$BA <= 2/3, "Moderate", 
                                 ifelse(summary.extrap$BA <= 1, "High", "Very High")))
    
    # Plotting data in covariate space with extrapolation  ------------------------
    
    extrap.reps.out <-list(rand.gridA = rand.gridA,
                           rand.gridB = rand.gridB,
                           covs.SiteA = covs.SiteA,
                           covs.SiteB = covs.SiteB,
                           covs.SiteA.rast = covs.SiteA.rast,
                           covs.SiteB.rast = covs.SiteB.rast,
                           summary.extrap = summary.extrap,
                           Site.distance = Site.distance)
    
    
    # If the output adds a required replicate to meet nreps, keep it
    # (x3 because there's a cov.list, latent.list, and extrap.reps.out)
    
    if (extrap.type == "Very High") { # If above high threshold, don't save
      
      saved <- 0
      
    } else if(length(reps.setup.list[[extrap.type]]) < nreps) {
      
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



#############################################################################

# 3. Simulate Environmental Extrapolation for SPAT AUTO scenario --------------

run_extrap_func_Spat_Auto <- function(n_cores,
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
                                      extrap.reps.out,
                                      extrap.type) {



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


        # Calculate the distance between the centre of the Site A and B -----------

        # Get centre of Site A & turn into point
        centreA <- data.frame(x = mean(c(xmin.randA, xmax.randA)),
                              y = mean(c(ymin.randA, ymax.randA))) %>%
          vect(geom=c("x", "y"), crs = "epsg:3857")

        # Get centre of Site B & turn into point
        centreB <- data.frame(x = mean(c(xmin.randB, xmax.randB)),
                              y = mean(c(ymin.randB, ymax.randB))) %>%
          vect(geom=c("x", "y"), crs = "epsg:3857")

        Site.distance <- terra::distance(centreA, centreB)

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
        
        
        # Batthacharyya's Affinity ------------------------------------------------
        
        training <- covs.SiteA %>% 
          .[,c("cov1", "cov2")]
        
        projection <- covs.SiteB %>% 
          .[,c("cov1", "cov2")]

        xmin <- min(c(training$cov1, projection$cov1)) - 0.5
        xmax <- max(c(training$cov1, projection$cov1)) + 0.5
        ymin <- min(c(training$cov2, projection$cov2)) - 0.5
        ymax <- max(c(training$cov2, projection$cov2)) + 0.5
        
        xmin <- min(c(training$cov1, projection$cov1))
        xmax <- max(c(training$cov1, projection$cov1)) 
        ymin <- min(c(training$cov2, projection$cov2)) 
        ymax <- max(c(training$cov2, projection$cov2))
        
        # How many grid points to evaluate against?
        eval.grid.res <- 150
        
        # Set up a shared grid domain
        domain <- rast(xmin = xmin, 
                       xmax = xmax, 
                       ymin = ymin, 
                       ymax = ymax, 
                       ncols = eval.grid.res, 
                       nrows = eval.grid.res,
                       vals = 1) %>% 
          as.data.frame(xy = T) %>% 
          .[,c("x", "y")]

        Ref_density <- ks::kde(x = training, eval.points = domain)
        
        Target_density <- ks::kde(x = projection, eval.points = domain)
        
        Ref_data <- data.frame(
          cov1 = Ref_density$eval.points[[1]],
          cov2 = Ref_density$eval.points[[2]],
          density = as.vector(Ref_density$estimate)
        ) %>% rast()
        
        
        Target_data <- data.frame(
          cov1 = Target_density$eval.points[[1]],
          cov2 = Target_density$eval.points[[2]],
          density = as.vector(Target_density$estimate)
        ) %>% rast()
        
        
        # Normalise the densities to sum to 1
        tot <- global(Ref_data, "sum")$sum
        Ref_data <- Ref_data / tot
        global(Ref_data, "sum")
        
        tot <- global(Target_data, "sum")$sum
        Target_data <- Target_data / tot
        global(Target_data, "sum")
        

        # Calculate the Bhattacharyya Affinity  ------------------------------------
        
        BA <- sum(sqrt(abs(values(Ref_data))) * 
                    sqrt(abs(values(Target_data))))
        
        # Calculate the Bhattacharyya Distance  ------------------------------------
        BD <- -log(BA)
        
        summary.extrap = data.frame(BA = BA,
                                    BD = BD)
        
        # Classify extrapolation type
        # If median extrap is less than or = to 50, low
        # If median extrap is not less than or = to 50, but is less than or = to 100, moderate
        # If median extrap is not less than or = to 100, high
        
        env.extrap <- ifelse(summary.extrap$BA <= 1/3 , "Low", "Very High")

        extrap.reps.out <-list(rand.gridA = rand.gridA,
                               rand.gridB = rand.gridB,
                               covs.SiteA = covs.SiteA,
                               covs.SiteB = covs.SiteB,
                               covs.SiteA.rast = covs.SiteA.rast,
                               covs.SiteB.rast = covs.SiteB.rast,
                               summary.extrap = summary.extrap,
                               Site.distance = Site.distance)

        # If the output adds a required replicate to meet nreps, keep it
        # (x3 because there's a cov.list, latent.list, and extrap.reps.out)
        
        if (env.extrap == "Very High") { # If above high threshold, don't save
          
          saved <- 0
          
        } else if(length(reps.setup.list[[extrap.type]]) < nreps) {
          
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







##### ARCHIVE !!! ######


# #############################################################################
# 
# # 3. Simulate Environmental Extrapolation for SPAT AUTO scenario --------------
# 
# run_extrap_func_Spat_Auto <- function(n_cores,
#                                       nreps,
#                                       rast_cellsA,
#                                       rast_cellsB,
#                                       bau_east_step,
#                                       bau_north_step,
#                                       eastings,
#                                       northings,
#                                       cov1,
#                                       cov2,
#                                       covs,
#                                       reps.setup.list,
#                                       extrap.reps.out) {
#   
#   # Get the names of the extrap types for indexing
#   extrap_names <- names(reps.setup.list)                            
#   
#   # For extrap type (low range, mod range, high range)
#   for(extrap.type in seq_along(reps.setup.list)) { 
#     
#     # Extract the name ("Low") for indexing from the names list
#     name <- extrap_names[extrap.type]
#     
#     for(rep in seq_along(reps.setup.list[[name]])) {
#       
#       extrap_func <- function() {
#         
#         # Set size of grid (number of cells) for Site A (Reference)
#         # NOTE - must be smaller than total cell number in x y directions
#         rast_sizeA <- c(rast_cellsA[1]*bau_east_step, rast_cellsA[2]*bau_north_step)
#         # Set size of grid (number of cells) for Site B (Target)
#         rast_sizeB <- c(rast_cellsB[1]*bau_east_step, rast_cellsB[2]*bau_north_step)
#         
#         # Get coords of overall grid domain boundary
#         xmin <- min(eastings)
#         xmax <- max(eastings)
#         ymin <- min(northings)
#         ymax <- max(northings)
#         
#         # Set the limit for x and y coord so box is completely inside the domain
#         rand.limA <- c(xmax - rast_sizeA[1], ymax - rast_sizeA[2])
#         rand.limB <- c(xmax - rast_sizeB[1], ymax - rast_sizeB[2])
#         
#         ##### GRID A ###### 
#         # Create random coordinate index for (bottom left?) corner of subgrid within grid domain
#         # Do this by generating a random number and finding the nearest eastings/northings value
#         # Then use this index on x0 to get the coordinate
#         # Had to take 0.01 off at the end to make it 50 cells
#         
#         xmin.randA <- eastings[which.min(abs(eastings - runif(1, min = round(xmin,2), max = round(rand.limA[1],2))))] 
#         xmax.randA <- xmin.randA + rast_sizeA[1]
#         
#         xmin.randA <- xmin.randA - bau_east_step/2
#         xmax.randA <- xmax.randA + bau_east_step/2 - bau_east_step
#         
#         ymin.randA <- northings[which.min(abs(northings - runif(1, min = round(ymin,2), max = round(rand.limA[2],2))))]
#         ymax.randA <- ymin.randA + rast_sizeA[2]
#         
#         ymin.randA <- ymin.randA - bau_north_step/2
#         ymax.randA <- ymax.randA + bau_north_step/2 - bau_east_step
#         
#         #### GRID B ########
#         
#         Generate_Grid_B <- function() {
#           
#           xmin.randB <<- eastings[which.min(abs(eastings - (runif(1, min = round(xmin,2), max = round(rand.limB[1],2)))))]
#           xmax.randB <<- xmin.randB + rast_sizeB[1]
#           
#           xmin.randB <<- xmin.randB - bau_east_step/2
#           xmax.randB <<- xmax.randB + bau_east_step/2 - bau_east_step
#           
#           ymin.randB <<- northings[which.min(abs(northings - (runif(1, min = round(ymin,2), max = round(rand.limB[2],2)))))]
#           ymax.randB <<- ymin.randB + rast_sizeB[2]
#           
#           ymin.randB <<- ymin.randB - bau_north_step/2
#           ymax.randB <<- ymax.randB + bau_north_step/2 - bau_east_step
#           
#           
#         }
#         
#         # Run function to generate GRID B
#         Generate_Grid_B()
#         
#         # Must be checked to see if overlaps with GRID A  
#         # If overlap occurs, run the generate function again
#         while(xmin.randB < xmax.randA & xmax.randB > xmin.randA & ymin.randB < ymax.randA & ymax.randB > ymin.randA) {
#           
#           Generate_Grid_B()
#           
#         } 
#         
#         ### Make the grids into rasters
#         rand.gridA <- rast(xmin = xmin.randA, 
#                            xmax = xmax.randA, 
#                            ymin = ymin.randA, 
#                            ymax = ymax.randA, 
#                            nrows = rast_cellsA[1], 
#                            ncols = rast_cellsA[2],
#                            vals = 1:rast_cellsA[2]) # Just setting values for plotting and for converting to a dataframe
#         
#         
#         
#         crs(rand.gridA) <- "epsg:3857"
#         
#         
#         rand.gridB <- rast(xmin = xmin.randB, 
#                            xmax = xmax.randB, 
#                            ymin = ymin.randB, 
#                            ymax = ymax.randB, 
#                            nrows = rast_cellsB[1], 
#                            ncols = rast_cellsB[2],
#                            vals = 1:rast_cellsB[2]) # Just setting values for plotting and for converting to a dataframe
#         
#         
#         
#         crs(rand.gridB) <- "epsg:3857"
#         
#         
#         # Calculate the distance between the centre of the Site A and B -----------
#         
#         # Get centre of Site A & turn into point
#         centreA <- data.frame(x = mean(c(xmin.randA, xmax.randA)), 
#                               y = mean(c(ymin.randA, ymax.randA))) %>% 
#           vect(geom=c("x", "y"), crs = "epsg:3857")
#         
#         # Get centre of Site B & turn into point
#         centreB <- data.frame(x = mean(c(xmin.randB, xmax.randB)), 
#                               y = mean(c(ymin.randB, ymax.randB))) %>% 
#           vect(geom=c("x", "y"), crs = "epsg:3857")
#         
#         Site.distance <- terra::distance(centreA, centreB)
#         
#         # Extract covariates for the random grid Site A  --------------------------
#         
#         rand.grid.df <- as.data.frame(rand.gridA, xy = T)[,c("x", "y")]
#         
#         cov1.SiteA <- terra::extract(cov1, rand.grid.df, xy = T) %>% rename(cov1 = cov)
#         cov2.SiteA <- terra::extract(cov2, rand.grid.df, xy = T) %>% rename(cov2 = cov)
#         
#         # Join to create one dataframe
#         covs.SiteA <- left_join(cov1.SiteA, cov2.SiteA, by = join_by(ID, x, y))
#         
#         covs.SiteA.rast <- crop(covs, ext(rand.gridA))
#         
#         # Extract covariates for the random grid Site B ---------------------------
#         
#         rand.grid.df <- as.data.frame(rand.gridB, xy = T)[,c("x", "y")]
#         
#         cov1.SiteB <- terra::extract(cov1, rand.grid.df, xy = T) %>% rename(cov1 = cov)
#         cov2.SiteB <- terra::extract(cov2, rand.grid.df, xy = T) %>% rename(cov2 = cov)
#         
#         # Join to create one dataframe
#         covs.SiteB <- left_join(cov1.SiteB, cov2.SiteB, by = join_by(ID, x, y))
#         
#         covs.SiteB.rast <- crop(covs, ext(rand.gridB))
#         
#         
#         # Calculating Overlap of Environment - Whole Grid -------------------------
#         
#         # Shape approach ----------------------------------------------------------
#         
#         # Adding presence column due to extra_eval requirements
#         # Trimming so just the covariates
#         training <- covs.SiteA %>% 
#           mutate(Presence = 1) %>% 
#           .[,c("cov1", "cov2", "Presence")]
#         
#         projection <- covs.SiteB %>% 
#           .[,c("cov1", "cov2")]
#         
#         shape_extrap <- flexsdm::extra_eval(training_data = training,
#                                             pr_ab = "Presence",
#                                             projection_data = projection,
#                                             metric = "mahalanobis",
#                                             univar_comb = F,
#                                             n_cores = n_cores)
#         
#         shape_extrap <- cbind(shape_extrap, covs.SiteB[, c("x", "y")])
#         
#         summary.extrap = data.frame(mean = mean(shape_extrap$extrapolation),
#                                     median = median(shape_extrap$extrapolation),
#                                     min = min(shape_extrap$extrapolation),
#                                     max = max(shape_extrap$extrapolation))
#         
#         print(median(shape_extrap$extrapolation))
#         
#         # Classify extrapolation type
#         
#         env.extrap <- ifelse(summary.extrap$median <= 5 , "Low", "Very High")
#         
#         
#         # Plotting data in covariate space with extrapolation  ------------------------
#         
#         extrap.plot <- ggplot() + 
#           geom_point(data = covs.SiteA, aes(x = cov1, y = cov2), color = "grey") +
#           geom_point(data = shape_extrap, aes(x = cov1, y = cov2, color = extrapolation)) +
#           scale_color_viridis(option = "magma", direction = -1) +
#           theme_bw() +
#           theme(legend.ticks = element_blank())
#         
#         extrap.reps.out <-list(rand.gridA = rand.gridA,
#                                rand.gridB = rand.gridB,
#                                covs.SiteA = covs.SiteA,
#                                covs.SiteB = covs.SiteB,
#                                covs.SiteA.rast = covs.SiteA.rast,
#                                covs.SiteB.rast = covs.SiteB.rast,
#                                extrap.plot = extrap.plot,
#                                extrap.df = shape_extrap,
#                                summary.extrap = summary.extrap,
#                                Site.distance = Site.distance)
#         
#         # If the output adds a required replicate to meet nreps, keep it
#         # (x3 because there's a cov.list, latent.list, and extrap.reps.out)
#         
#         if (env.extrap == "Very High") { # If above high threshold, don't save
#           
#           saved <- 0
#           
#         } else if(length(reps.setup.list[[extrap.type]]) < nreps) {
#           
#           # Note that an output has been saved
#           saved <- 1
#           
#         } else {
#           saved <- 0
#           
#         }
#         
#         return(list(extrap.reps.out = extrap.reps.out, saved = saved, extrap.type = extrap.type))
#         
#       }
#       
#       ## Now run the extrapolation function and temporarily save
#       temp <- extrap_func()
#       
#       # If the output from this hasn't been saved, keep running the function
#       while(temp$saved != 1) {
#         
#         temp <- extrap_func()
#         "Trying again to get a extrapolation type"
#         
#       }
#       
#       
#       reps.setup.list[[name]][[rep]] <- list(extrap.reps.out = temp$extrap.reps.out, extrap.type = temp$extrap.type)
#       
#     }
#     
#   }
#   
#   return(reps.setup.list)
# }
           


  
 


