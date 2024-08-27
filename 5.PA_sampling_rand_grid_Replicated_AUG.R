# TO DO:
# Fix the PA plot and add back to final list
# FIX ERROR CODE
# At the moment assuming complete survey coverage of a grid site 
# The PA grid is a subset of the Site A area

library(purrr)

rep <- extrap.reps.out$Low[[1]]

PA.data <- imap(extrap.reps.out, function(extrap.type, extrap.name) {
  
  
  imap(extrap.type, function(rep, rep_index) {
    
    print(paste("Processing extrap.type:", extrap.name))
    
    print(paste("Processing rep:", rep_index))
    
    rand.gridA <- rep$rand.gridA
    rand.gridB <- rep$rand.gridB
    PO_GridA <- rep$PO_GridA
    PO_GridB <- rep$PO_GridB
    
    #-------------------------------------------------------------------------------
    # Site A
    #-------------------------------------------------------------------------------
    
    #### 1. SELECTING A RANDOM GRID FOR SITE A
    
    # Set size of grid (number of cells) for PA grid in Site A (Reference)
    # NOTE - must be smaller than total cell number in x y directions
    rast_cellsA <- c(20, 15)
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
    xmin.randA <- eastingsSITE[which.min(abs(eastings - runif(1, min = xmin, max = rand.limA[1])))]
    ymin.randA <- northingsSITE[which.min(abs(northingsSITE - runif(1, min = ymin, max = rand.limA[2])))]
    
    xmax.randA <- eastingsSITE[which.min(abs(eastings - (xmin.randA + rast_sizeA[1])))]
    ymax.randA <- northingsSITE[which.min(abs(northingsSITE - (ymin.randA + rast_sizeA[2])))]
    
    PA.rand.gridA <- rast(xmin = xmin.randA, 
                       xmax = xmax.randA, 
                       ymin = ymin.randA, 
                       ymax = ymax.randA, 
                       nrows = rast_cellsA[1], 
                       ncols = rast_cellsA[2],
                       vals = 1:rast_cellsA[2]) # Just setting values for plotting and for converting to a dataframe
    

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
    inbox_idx_a <- which(po.rand.gridA[, "x"] >= dom_a_bbox["east_min"] &
                           po.rand.gridA[, "x"] <= dom_a_bbox["east_max"] &
                           po.rand.gridA[, "y"] >= dom_a_bbox["north_min"] &
                           po.rand.gridA[, "y"] <= dom_a_bbox["north_max"])
    
    
    po_a <- po.rand.gridA[inbox_idx_a, ]
    
    # If there are no presences from the presence-only data in the PA grid
    # Halt the computation and move to next replicate
    if(length(po_a) == 0) {
      print("No PO in PA grid A")
      
      
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
    
    # ggplot() +
    #   geom_tile(data = bias.df, aes(x = x, y = y, fill = bias)) +
    #   scale_fill_viridis() +
    #   geom_point(data = po_a_df, aes(x = x, y = y), color = "white", alpha = 0.5)+
    #   theme_bw()
    
    # ## RANDOM SUBSET OF THESE * HAVE TO REMOVE THIS (JUST FOR REDUCING NUMBER OF PRESENCES)
    # # Thin points using the detection probability
    # # This was me trying to incorporate imperfect detection. So even if the species is present, it may not be detected
    # po_a_df$presence <- rbinom(nrow(po_a_df), 1, prob = 0.005)
    
    # # make it presence only data
    # po_a_df <- po_a_df[po_a_df$presence == 1,]
    
    # Now assuming perfect detection
    po_a_df$presence <- 1
    
    # Get cell indices of the species coordinates
    cell_idx <- terra::cellFromXY(pa_a, po_a_df[, c("x", "y")])
    
    # Fill in the raster with 1 from the cell indices
    pres_idx <- as.numeric(names(table(cell_idx)))
    pa_a[pres_idx] <- 1
    
    # # plot the data
    # plot(pa_a)
    
    # pa - region a
    pa_a_df <- as.data.frame(pa_a, xy = TRUE) %>% 
      mutate(area = dom_a_res^2)
    
   
    #-------------------------------------------------------------------------------
    # Plot the site with all data types
    #-------------------------------------------------------------------------------
    
    # PA_plot <- ggplot() +
    #   # geom_tile(data = bias.df, aes(x = x, y = y), fill = "white") +
    #   # scale_fill_viridis() +
    #   geom_point(data = PO_GridA, aes(x = x, y = y), color = "black", alpha = 0.1) +
    #   geom_point(data = PO_GridB, aes(x = x, y = y), color = "black", alpha = 0.1) +
    #   geom_point(data = pa_a_df, aes(x = x, y = y, color = as.factor(presence)), size = 2) +
    #   labs(color =  "Presence / Absence") +
    #   theme_bw() +
    #   theme(legend.ticks = element_blank()) +
    #   scale_color_manual(values = c("purple4", "green3"))
    
    
    return(list(pa_a_df = pa_a_df, 
                pa_b_df = pa_b_df))
    
    # return(list(PA_plot = PA_plot, 
    #             pa_a_df = pa_a_df, 
    #             pa_b_df = pa_b_df))
   
    
    
  })
  
})

# Add new PA grids to list
extrap.reps.out.PA <- map2(extrap.reps.out, PA.data, ~map2(.x, .y, c))


############ TO DO

# Need to set up so that if there are no presences in the PA grid, it doesn't run the rest of the code
# Then it returns and re-runs the extrapolation so that there's enough reps.

########### ARCHIVE
