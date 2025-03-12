
########################################################################
####################### 8. Make Truth  ####################
########################################################################

# This script makes a raster of the 'true' log intensity of the latent species distribution across the landscape

# Used for comparing with the predicted intensity



# Inputs:

# Output from step 6


# Output:

# A list with the true log intensity as a raster


########################################################################


make_truth_func <- function(reps.setup.list) {
  
  truth <- imap(reps.setup.list, function(extrap.type, extrap.name) {
    
    imap(extrap.type, function(rep, rep_index) {
      
      # Get the log intensity values (expected number of points per unit area)
      # NOTE because log(intensity) = mu (when using rpoispp), the true_log_int = mu
      true_log_int <- rep$latent.list$mu
      
      true_log_int.rast <- rast(true_log_int)
      
      covs <- rep$cov.list$covs
      
      crs(true_log_int.rast) <- crs(covs) 
      
      names(true_log_int.rast) <- "int"
      
      # Extract cell size because RISDM predictions are with reference to cell area
      log.cell_size <- log(cellSize(covs))
      
      # Add intensity + log(cell area)
      true_log_int.rast <- true_log_int.rast+log.cell_size
      
      true.int.plot <- true_log_int.rast %>% 
        as.data.frame(xy = T) %>% 
        ggplot() + 
        geom_tile(aes(x = x, y = y, fill = int)) + 
        scale_fill_viridis() +
        coord_fixed() + 
        theme_bw() + 
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.ticks = element_blank(),
              legend.title = element_blank()) +
        ggtitle('True log intensity')
      
      return(list(true_log_int.rast = true_log_int.rast,
                  true.int.plot = true.int.plot))
      
    })
    
  })
  
  # Add true log intensity to list
  reps.setup.list <- map2(reps.setup.list, truth, ~map2(.x, .y, c))  
  
  return(reps.setup.list)
  
}
