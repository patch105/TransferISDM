
spp_process <- cbind(x = lg.s$x, y = lg.s$y)

po <- thinpp[, c("x", "y")]



PA.data <- map(extrap.reps.out, function(extrap.type) {
  
  map(extrap.type, function(rep) {
    
    rand.gridA <- rep$rand.gridA
    rand.gridB <- rep$rand.gridB
    
    
    #-------------------------------------------------------------------------------
    # Site A
    #-------------------------------------------------------------------------------
    
    # Get the domain of region a
    dom_a_bbox <- c(east_min = xmin(rand.gridA), east_max = xmax(rand.gridA), north_min = ymin(rand.gridA), north_max = ymax(rand.gridA))
    
    # Choose a grid size number of rows (for PA sampling)
    PA_a_res <- 30
    dom_a_res <- (dom_a_bbox["east_max"] - dom_a_bbox["east_min"]) / PA_a_res
    
    # Set centroids of PA sampling grids
    east_seq <- seq(dom_a_bbox["east_min"] + dom_a_res/2, 
                    dom_a_bbox["east_max"] - dom_a_res/2, 
                    by = dom_a_res)
    north_seq <- seq(dom_a_bbox["north_min"] + dom_a_res/2, 
                     dom_a_bbox["north_max"] - dom_a_res/2, 
                     by = dom_a_res)
    
    
    # Create a blank PA dataset at Site A (all zeros), located on grids cells defined by random grid domain and our PA sampling grid size
    grid_a <- expand.grid(east_seq, north_seq)
    pa_a <- cbind(grid_a, 0)
    colnames(pa_a) <- c("x", "y", "presence")
    pa_a <- terra::rast(pa_a)
    
    # find species coordinates from underlying LGCP that are in region a
    inbox_idx_a <- which(spp_process[, "x"] >= dom_a_bbox["east_min"] &
                           spp_process[, "x"] <= dom_a_bbox["east_max"] & 
                           spp_process[, "y"] >= dom_a_bbox["north_min"] &
                           spp_process[, "y"] <= dom_a_bbox["north_max"])
    
    
    po_a <- spp_process[inbox_idx_a, ]
    po_a_df <- as.data.frame(po_a)
    
    
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
    
    
    #-------------------------------------------------------------------------------
    # Site B
    #-------------------------------------------------------------------------------
    
    # Get the domain of region a
    dom_b_bbox <- c(east_min = xmin(rand.gridB), east_max = xmax(rand.gridB), north_min = ymin(rand.gridB), north_max = ymax(rand.gridB))
    
    # Choose a grid size (for PA sampling)
    PA_b_res <- 30
    dom_b_res <- (dom_b_bbox["east_max"] - dom_b_bbox["east_min"]) / PA_b_res
    
    # Set centroids of PA sampling grids
    east_seq <- seq(dom_b_bbox["east_min"] + dom_b_res/2, 
                    dom_b_bbox["east_max"] - dom_b_res/2, 
                    by = dom_b_res)
    north_seq <- seq(dom_b_bbox["north_min"] + dom_b_res/2, 
                     dom_b_bbox["north_max"] - dom_b_res/2, 
                     by = dom_b_res)
    
    
    # Create a blank PA dataset at Site B (all zeros), located on grids cells defined by random grid domain and our PA sampling grid size
    grid_b <- expand.grid(east_seq, north_seq)
    pa_b <- cbind(grid_b, 0)
    colnames(pa_b) <- c("x", "y", "presence")
    pa_b <- terra::rast(pa_b)
    
    # find species coordinates from underlying LGCP that are in region a
    inbox_idx_b <- which(spp_process[, "x"] >= dom_b_bbox["east_min"] &
                           spp_process[, "x"] <= dom_b_bbox["east_max"] & 
                           spp_process[, "y"] >= dom_b_bbox["north_min"] &
                           spp_process[, "y"] <= dom_b_bbox["north_max"])
    
    
    po_b <- spp_process[inbox_idx_b, ]
    po_b_df <- as.data.frame(po_b)
    
    
    # ggplot() +
    #   geom_tile(data = bias.df, aes(x = x, y = y, fill = bias)) +
    #   scale_fill_viridis() +
    #   geom_point(data = po_b_df, aes(x = x, y = y), color = "white", alpha = 0.5)+
    #   theme_bw()
    
    # ## RANDOM SUBSET OF THESE
    # # Thin points using the detection probability
    # # This was me trying to incorporate imperfect detection. So even if the species is present, it may not be detected
    # po_b_df$presence <- rbinom(nrow(po_b_df), 1, prob = 0.005)
    # # 
    # # # make it presence only data
    # po_b_df <- po_b_df[po_b_df$presence == 1,]
    
    # Now assuming perfect detection
    po_b_df$presence <- 1
    
    # Get cell indices of the species coordinates
    cell_idx <- terra::cellFromXY(pa_b, po_b_df[, c("x", "y")])
    
    # Fill in the raster with 1 from the cell indices
    pres_idx <- as.numeric(names(table(cell_idx)))
    pa_b[pres_idx] <- 1
    
    # # plot the data
    # plot(pa_b)
    
    # pa - region a
    pa_a_df <- as.data.frame(pa_a, xy = TRUE)
    
    # pa - region b
    pa_b_df <- as.data.frame(pa_b, xy = TRUE)
    
    
    
    #-------------------------------------------------------------------------------
    # Plot the site with all data types
    #-------------------------------------------------------------------------------
    
    PA_plot <- ggplot() +
      # geom_tile(data = bias.df, aes(x = x, y = y), fill = "white") +
      scale_fill_viridis() +
      geom_point(data = thinpp, aes(x = x, y = y), color = "black", alpha = 0.1) +
      geom_point(data = pa_a_df, aes(x = x, y = y, color = as.factor(presence)), size = 2) +
      geom_point(data = pa_b_df, aes(x = x, y = y, color = as.factor(presence)), size = 2) +
      labs(color =  "Presence / Absence") +
      theme_bw() +
      theme(legend.ticks = element_blank()) +
      scale_color_manual(values = c("purple4", "green3"))
    
    
    return(list(PA_plot = PA_plot, 
                pa_a_df = pa_a_df, 
                pa_b_df = pa_b_df))
    
   
    
    
  })
  
})

# Add new PA grids to list
reps.merged <- map2(extrap.reps.out, PA.data, ~map2(.x, .y, c))


