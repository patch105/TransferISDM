

# 5. PA Sampling ----------------------------------------------------------

pa_sampling_func <- function(reps.setup.list) {
                             
  
  PA.data <- imap(reps.setup.list, function(extrap.type, extrap.name) {
    
    
    imap(extrap.type, function(rep, rep_index) {
      
      print(paste("Processing extrap.type:", extrap.name))
      
      print(paste("Processing rep:", rep_index))
      
      rand.gridA <- rep$extrap.reps.out$rand.gridA

      spp_process.rand.gridA <- rep$spp_process.rand.gridA
      
      #-------------------------------------------------------------------------------
      # Site A
      #-------------------------------------------------------------------------------
      
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
      
      # Create random coordinate index for (bottom left?) corner of subgrid within grid domain
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
      
      # Get the domain of region a
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
      pa_a <- cbind(grid_a, 0)
      colnames(pa_a) <- c("x", "y", "presence")
      pa_a <- terra::rast(pa_a)
      
      
      print(paste0("length of spp process rand grid A is: ",nrow(spp_process.rand.gridA)))
      
      # If there are no presences at all in the presence-only data
      # Save an empty dataframe
      if(length(spp_process.rand.gridA) < 2) {
        
        print("No presences in PA grid A")
        pa_a_df <- data.frame(x = NA, y = NA, presence = NA)
        
      } else {
        
      
        # Random stratified sampling ----------------------------------------------
        
        
        # Create a reference grid for the 10x10 raster to identify each cell with a unique number
        ref_grid <- pa_a
        values(ref_grid) <- 1:ncell(ref_grid)  # Assign unique values (1 to 100) to each cell
        
        # Resample the reference grid to the resolution of the 50x30 raster
        # This doesn't change the resolution of the 50x30 raster but assigns the corresponding values from the 10x10 cells
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
          
          print("No presences in PA grid A")
          pa_a_df <- data.frame(x = NA, y = NA, presence = NA)
          
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
        
      }
      
      # Debugging output for pa_a_df
      print("pa_a_df column names:")
      print(colnames(pa_a_df))
      print(head(pa_a_df))
      
      
      # pa - region a
      pa_a_df <- pa_a_df %>% 
        mutate(area = 1)
      
      return(list(pa_a_df = pa_a_df, 
                  PA_grid_size = rast_cellsA, 
                  PA_a_res = PA_a_res,
                  n_presence_gridA = sum(pa_a_df$presence == 1),
                  n_absence_gridA = sum(pa_a_df$presence == 0),
                  PA.rand.gridA = PA.rand.gridA)) # Need these last two for plotting
      
    })
    
  })
  
  # Add new PA grids to list
  reps.setup.list <- map2(reps.setup.list, PA.data, ~map2(.x, .y, c))
  
  return(reps.setup.list)
  
  
}



# # CHECK IF PO DATA IN PA SITE A GRID --------------------------------------
# 
# po_pa_checking_func <- function(reps.setup.list) {
#   
#   # Initialise a counter for removed reps
#   removed_counts_pa <- c(Low = 0, Moderate = 0, High = 0)
#   
#   # Iterate through each Extrap type
#   for (extrap.type in c("Low", "Moderate", "High")) {
#     
#     # Get the corresponding sub-list (Low, Moderate, or High)
#     reps_list <- reps.setup.list[[extrap.type]]
#     
#     remove_index <- c()
#     
#     # Iterate through each rep within the sub-list
#     for (i in seq_along(reps_list)) {
#       
#       # Check if PO_grid in the rep has no PO data or if there's any PO data in the PA sampling grid A
#       if (length(reps_list[[i]]$PO_GridA) < 2 | reps_list[[i]]$n_presence_gridA == 0) {
#         
#         # Save index
#         remove_index <- c(remove_index, i)
#         
#       }
#     }
#     
#     # Remove the reps with 0 length PO_GridA or PO_GridB
#     if (length(remove_index) > 0) {
#       
#       reps_list <- reps_list[-remove_index]
#       
#       # Count the number of removed reps for this extrap.type
#       removed_counts_pa[extrap.type] <- length(remove_index)
#     }
#     
#     # Assign the updated list back to the original structure
#     reps.setup.list[[extrap.type]] <- reps_list
#   }
#   
#   # Print the number of removed reps for each extrap.type
#   for (extrap.type in names(removed_counts_pa)) {
#     cat(removed_counts_pa[extrap.type], "reps removed from", extrap.type, "\n")
#   }
#   
#   return(list(reps.setup.list = reps.setup.list, removed_counts_pa = removed_counts_pa)) 
#   
# }


