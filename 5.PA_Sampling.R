
### TO DO: ADD BACK IN PLOTTING FOR RANDOM GRID

# 5. PA Sampling ----------------------------------------------------------

pa_sampling_func <- function(reps.setup.list) {
  
  PA.data <- imap(reps.setup.list, function(extrap.type, extrap.name) {
    
    
    imap(extrap.type, function(rep, rep_index) {
      
      print(paste("Processing extrap.type:", extrap.name))
      
      print(paste("Processing rep:", rep_index))
      
      rand.gridA <- rep$extrap.reps.out$rand.gridA
      rand.gridB <- rep$extrap.reps.out$rand.gridB
      
      # Make new realisation of presences from LGCP for PA ----------------------
      
      presenceforPA <- rpoispp(lambda = attr(rep$latent.list$lg.s, 'Lambda') )
      
      poforPA <- cbind(x = presenceforPA$x, y = presenceforPA$y)
      
      # Trim NEW po to only include points in Site A or B
      poforPA.rand.gridA <- poforPA[
        poforPA[,1] >= xmin(ext(rand.gridA)) & poforPA[,1] <= xmax(ext(rand.gridA)) & 
          poforPA[,2] >= ymin(ext(rand.gridA)) & poforPA[,2] <= ymax(ext(rand.gridA)), 
      ]
      
      
      #-------------------------------------------------------------------------------
      # Site A
      #-------------------------------------------------------------------------------
      
      #### 1. SELECTING A RANDOM GRID FOR SITE A
      
      # Set size of grid (number of cells) for PA grid in Site A (Reference)
      # NOTE - must be smaller than total cell number in x y directions
      rast_cellsA <- c(50, 30)
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
      
      # find species coordinates from underlying LGCP IN GRID A that are in region a
      inbox_idx_a <- which(poforPA.rand.gridA[, "x"] >= dom_a_bbox["east_min"] &
                             poforPA.rand.gridA[, "x"] <= dom_a_bbox["east_max"] &
                             poforPA.rand.gridA[, "y"] >= dom_a_bbox["north_min"] &
                             poforPA.rand.gridA[, "y"] <= dom_a_bbox["north_max"])
      
      
      po_a <- poforPA.rand.gridA[inbox_idx_a, ]
      
      # If there are no presences from the presence-only data in the PA grid
      # Save an empty dataframe
      if(length(po_a) == 0) {
        print("No PO in PA grid A")
        po_a_df <- data.frame(x = NA, y = NA)
        
      }
      
      # If there's only one PO location, have to adjust so that it formats into dataframe correctly
      if(length(po_a) == 2) {
        po_a_df <- data.frame(x = po_a[[1]], y = po_a[[2]])
        
      } else {
        
        po_a_df <- as.data.frame(po_a)
        
      }
      
      # Debugging output for po_a_df
      print("po_a_df column names:")
      print(colnames(po_a_df))
      print(head(po_a_df))
      
      # Now assuming perfect detection
      po_a_df$presence <- 1
      
      # Get cell indices of the species coordinates
      cell_idx <- terra::cellFromXY(pa_a, po_a_df[, c("x", "y")])
      
      # Fill in the raster with 1 from the cell indices
      pres_idx <- as.numeric(names(table(cell_idx)))
      pa_a[pres_idx] <- 1
      
      # pa - region a
      pa_a_df <- as.data.frame(pa_a, xy = TRUE) %>% 
        mutate(area = dom_a_resE * dom_a_resN)
      
      return(list(pa_a_df = pa_a_df))
      
    })
    
  })
  
  # Add new PA grids to list
  reps.setup.list <- map2(reps.setup.list, PA.data, ~map2(.x, .y, c))
  
  return(reps.setup.list)
  
  
}
