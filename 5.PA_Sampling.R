

########################################################################
########################## 5. PA sampling   ##########################
########################################################################

# This script simulates presence-absence (PA) records at the training site (Site A). 

# It does so by:
# Randomly placing a 50x50 grid within the Site A domain 
# Assigning 10x10 strata within this grid
# Placing a random 1x1 quadrat in each stratum
# Checking whether the 1x1 quadrat overlaps with a species presence from the true species distribution process
# Assigning each quadrat as a presence or absence

# It also calculates the 'realised' environmental extrapolation for the PA and PO data. This is done by estimating the Shape value for the PA and PO records.

# Inputs: 

# The output from step 4. 

# n_cores: The number of cores for the Shape metric calculation. The flexsdm::extra_eval function can utilise parallel processing. If available, this can speed up the calculation of the Shape metric for environmental dissimilarity.Specify the number of cores to use here.

# Outputs:

# A list that contains the PA records for eac replicate, as well as summaries of the number of presences and absences and the realised extrapolation of the records


########################################################################

pa_sampling_func <- function(reps.setup.list,
                             n_cores) {
                             
  
  PA.data <- imap(reps.setup.list, function(extrap.type, extrap.name) {
    
    
    imap(extrap.type, function(rep, rep_index) {
      
      # Obtain the Site A (training site) grid
      rand.gridA <- rep$extrap.reps.out$rand.gridA

      # Obtain the species process for Site A
      spp_process.rand.gridA <- rep$spp_process.rand.gridA
      
      
      #### 1. SELECTING A RANDOM GRID FOR SITE A
      
      # Set size of grid (number of cells) for PA grid in Site A (Reference)
      # NOTE - must be smaller than total cell number in x y directions
      rast_cellsA <- c(50, 50)
      rast_sizeA <- c(rast_cellsA[1]*res(rand.gridA)[1], rast_cellsA[2]*res(rand.gridA)[2])
      
      # Get coords of overall grid domain boundary
      xmin <- xmin(rand.gridA)
      xmax <- xmax(rand.gridA)
      ymin <- ymin(rand.gridA)
      ymax <- ymax(rand.gridA)
      
      eastingsSITE <- crds(rand.gridA)[,1]
      northingsSITE <- crds(rand.gridA)[,2]
      
      # Set the limit for x and y coord so box is completely inside the domain
      rand.limA <- c(xmax - rast_sizeA[1], ymax - rast_sizeA[2])
      
      # Create random coordinate index for (bottom left) corner of subgrid within grid domain
      # Do this by generating a random number and finding the nearest eastings/northings value
      # Then use this index on x0 to get the coordinate
      # Had to round because there was a problem if xmin and rand.limA[1] were slightly different by a tiny decimal place
      xmin.randA <- eastingsSITE[which.min(abs(eastingsSITE - runif(1, min = round(xmin,2), max = round(rand.limA[1],2))))]
      ymin.randA <- northingsSITE[which.min(abs(northingsSITE - runif(1, min = round(ymin,2), max = round(rand.limA[2],2))))]
      
      xmax.randA <- eastingsSITE[which.min(abs(eastingsSITE - (xmin.randA + rast_sizeA[1])))]
      ymax.randA <- northingsSITE[which.min(abs(northingsSITE - (ymin.randA + rast_sizeA[2])))]
      
      PA.rand.gridA <- rast(xmin = xmin.randA, 
                            xmax = xmax.randA, 
                            ymin = ymin.randA, 
                            ymax = ymax.randA, 
                            nrows = rast_cellsA[1], 
                            ncols = rast_cellsA[2],
                            vals = 1:rast_cellsA[2]) # Just setting values for plotting and for converting to a dataframe
      
      crs(PA.rand.gridA) <- "epsg:3857"
      
      ##### 2. SAMPLING PA DATA FROM RANDOM GRID A
      
      # Get the domain of random grid A
      dom_a_bbox <- c(east_min = xmin(PA.rand.gridA), east_max = xmax(PA.rand.gridA), north_min = ymin(PA.rand.gridA), north_max = ymax(PA.rand.gridA))
      
      # Choose a grid size number of rows (for PA sampling)
      PA_a_res <- 10
      dom_a_resE <- (dom_a_bbox["east_max"] - dom_a_bbox["east_min"]) / PA_a_res
      dom_a_resN <- (dom_a_bbox["north_max"] - dom_a_bbox["north_min"]) / PA_a_res
      
      
      # Set centroids of PA sampling grids
      east_seq <- seq(dom_a_bbox["east_min"] + dom_a_resE/2, 
                      dom_a_bbox["east_max"] - dom_a_resE/2, 
                      by = dom_a_resE)
      north_seq <- seq(dom_a_bbox["north_min"] + dom_a_resN/2, 
                       dom_a_bbox["north_max"] - dom_a_resN/2, 
                       by = dom_a_resN)
      
      
      # Create a blank PA dataset at Site A (all zeros), located on grids cells defined by random grid domain and our PA sampling grid size
      grid_a <- expand.grid(east_seq, north_seq)
      pa_a_df.temp <- cbind(grid_a, 0)
      colnames(pa_a_df.temp) <- c("x", "y", "presence")
      pa_a <- terra::rast(pa_a_df.temp)
      
      
      print(paste0("length of spp process rand grid A is: ",nrow(spp_process.rand.gridA)))
      
      # Random stratified sampling ----------------------------------------------
      
      # Create a reference grid for the 10x10 raster to identify each cell with a unique number
      ref_grid <- pa_a
      values(ref_grid) <- 1:ncell(ref_grid)  # Assign unique values (1 to 100) to each cell
      
      # Resample the reference grid to the resolution of the 50x50 raster
      # This doesn't change the resolution of the 50x50 raster but assigns the corresponding values from the 10x10 cells
      alignment_layer <- resample(ref_grid, PA.rand.gridA, method="near")
      
      names(alignment_layer) <- "strata"
      
      # Assign strata to PA.rand.grid.A cells
      PA.Strata <- c(PA.rand.gridA, alignment_layer)
      
      # Create a third layer in PA.Strata to store quadrats
      quadrats_layer <- PA.rand.gridA
      values(quadrats_layer) <- NA  # Initialize with NA
      
      # Loop through each unique stratum
      for (i in unique(values(PA.Strata$strata))) {
        
        # Find cells in PA.Strata that belong to the current stratum
        strata_cells <- which(values(PA.Strata$strata) == i)
        
        # Randomly sample two cells from the strata
        selected_cells <- sample(strata_cells, 1, replace = FALSE)
        
        # Assign a unique value to the selected cells (e.g., i or 1 for selected)
        values(quadrats_layer)[selected_cells] <- 1  
        
      }
      
      # Add quadrats layer to PA.Strata
      PA.Strata <- c(PA.Strata, quadrats_layer)
      names(PA.Strata)[nlyr(PA.Strata)] <- "quadrats"
      
      # Change spp_process to a spatvector for extract
      spp_process.rand.gridA.vect <- vect(spp_process.rand.gridA)
      
      match <- terra::extract(PA.Strata, spp_process.rand.gridA.vect) %>% 
        filter(quadrats == 1)
      
      
      # If there's no presences in the quadrats:
      if(nrow(match) == 0) {
        
        print("No presences in quadrats")
        
        PA.Strata[["quadrats"]][!is.na(PA.Strata[["quadrats"]])] <- 0 # set all quadrats to 0
        po_a <- PA.Strata[["quadrats"]]
        names(po_a) <- "presence"
        
        pa_a_df <- as.data.frame(po_a, xy = T)
        
        
      } else {
        
        # Set strata without presence to NA & update others to 1
        values(PA.Strata$strata)[!values(PA.Strata$strata) %in% match$strata] <- NA
        values(PA.Strata$strata)[values(PA.Strata$strata) > 1] <- 1
        
        # Mask the quadrats that have presence by saying, where strata == NA, quadrats stay as 1, otherwise set to 0
        temp <- mask(PA.Strata[["quadrats"]], PA.Strata[["strata"]], maskvalue = NA, updatevalue = 0)
        po_a <- mask(temp, PA.Strata[["quadrats"]], maskvalue = NA, updatevalue = NA)
        names(po_a) <- "presence"
        
        pa_a_df <- as.data.frame(po_a, xy = T)
        
      }
      
      # PA - region a
      pa_a_df <- pa_a_df %>% 
        mutate(area = 1)
      
      # Finally calculate the 'realised' environmental extrapolation  --------
      
      cov1 <- rep$extrap.reps.out$covs.SiteA.rast["cov1"]
      cov2 <- rep$extrap.reps.out$covs.SiteA.rast["cov2"]
      
      # Extract covariates for PA grid A
      
      pa_xy <- pa_a_df[,c("x", "y")]
      
      cov.1PA <- terra::extract(cov1, pa_xy, xy = T) 
      cov.2PA <- terra::extract(cov2, pa_xy, xy = T) 
      
      covsPA <- left_join(cov.1PA, cov.2PA, by = join_by(ID, x, y)) %>% 
        mutate(Presence = 1) %>% 
        .[,c("cov1", "cov2", "Presence")]
      
      # Extract covariates for PO grid A
      
      PO_GridA <- rep$PO_GridA
      PO_GridA <- as.data.frame(PO_GridA)
      
      cov.1PO <- terra::extract(cov1, PO_GridA, xy = T)
      cov.2PO <- terra::extract(cov2, PO_GridA, xy = T) 
      
      covsPO <- left_join(cov.1PO, cov.2PO, by = join_by(ID, x, y)) %>% 
        mutate(Presence = 1) %>% 
        .[,c("cov1", "cov2", "Presence")]
      
      # Extract covariates for PA + PO grid A
    
      PA_PO <- rbind(pa_xy, PO_GridA) 
      
      cov.1PAPO <- terra::extract(cov1, PA_PO, xy = T) 
      cov.2PAPO <- terra::extract(cov2, PA_PO, xy = T) 
      
      covsPAPO <- left_join(cov.1PAPO, cov.2PAPO, by = join_by(ID, x, y)) %>% 
        mutate(Presence = 1) %>% 
        .[,c("cov1", "cov2", "Presence")]
      
      # Covariates for site B (projection)
      
      covs.SiteB <- rep$extrap.reps.out$covs.SiteB
      
      projection <- covs.SiteB %>% 
        .[,c("cov1", "cov2")]
      
      
      # Shape approach ----------------------------------------------------------
      
      extrap_PA <- flexsdm::extra_eval(training_data = covsPA,
                                       pr_ab = "Presence",
                                       projection_data = projection,
                                       metric = "mahalanobis",
                                       univar_comb = F,
                                       n_cores = n_cores)
      
      extrap_PA <- cbind(extrap_PA, covs.SiteB[, c("x", "y")])
      
      extrap_PO <- flexsdm::extra_eval(training_data = covsPO,
                                       pr_ab = "Presence",
                                       projection_data = projection,
                                       metric = "mahalanobis",
                                       univar_comb = F,
                                       n_cores = n_cores)
      
      extrap_PO <- cbind(extrap_PO, covs.SiteB[, c("x", "y")])
      
      extrap_PAPO <- flexsdm::extra_eval(training_data = covsPAPO,
                                       pr_ab = "Presence",
                                       projection_data = projection,
                                       metric = "mahalanobis",
                                       univar_comb = F,
                                       n_cores = n_cores)
      
      extrap_PAPO <- cbind(extrap_PAPO, covs.SiteB[, c("x", "y")])
      
      summary.realised.extrap <- data.frame(meanPA = mean(extrap_PA$extrapolation),
                                   meanPO = mean(extrap_PO$extrapolation),
                                   meanPAPO = mean(extrap_PAPO$extrapolation),
                                   medianPA = median(extrap_PA$extrapolation),
                                   medianPO = median(extrap_PO$extrapolation),
                                   medianPAPO = median(extrap_PAPO$extrapolation))
      
      
      return(list(pa_a_df = pa_a_df, 
                  PA_grid_size = rast_cellsA, 
                  PA_a_res = PA_a_res,
                  n_presence_gridA = sum(pa_a_df$presence == 1),
                  n_absence_gridA = sum(pa_a_df$presence == 0),
                  PA.rand.gridA = PA.rand.gridA, # Need these last two for plotting
                  summary.realised.extrap = summary.realised.extrap)) 
      
    })
    
  })
  
  # Add new PA grids to list
  reps.setup.list <- map2(reps.setup.list, PA.data, ~map2(.x, .y, c))
  
  return(reps.setup.list)
  
  
}




