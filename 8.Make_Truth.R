

# 8. Make Truth -----------------------------------------------------------

# Extract the 'true' intensity (lambda) values from the LGCP model so that they can be compared with the estimated intensity

# The rLGCP function has produced a realisation of the LGCP but also saved the intensity values 

# Code modified from Simmonds et al. (2020).
# NOTE - Need to fix so I extract the median intensity, right now I think it's the mean

make_truth_func <- function(reps.setup.list) {
  
  truth <- imap(reps.setup.list, function(extrap.type, extrap.name) {
    
    imap(extrap.type, function(rep, rep_index) {
      
      # Get the (v) log intensity values (expected number of points per unit area)
      # NOTE - the format is from the lgcp package, so I need to reverse the order if want to plot
      # NOTE because log(intensity) = mu (when using rpoispp), the true_log_int = mu
      true_log_int <- rep$latent.list$mu
      
      # Reverse the row order
      true_log_int <- apply(true_log_int, 2, rev)
      
      # Transpose the matrix to match the raster layout
      true_log_int <- t(true_log_int)
      
      # Melt into xy dataframe
      true_log_int.melt <- true_log_int %>% 
        reshape2::melt(c("x", "y"), value.name = "int") 
      
      # Create a raster  
      true_log_int.rast <- cbind(x = coords[,1], y = coords[,2], true.int = true_log_int.melt["int"]) %>% rast(.)
      
      # Get covariates
      covs <- rep$cov.list$covs
      
      crs(true_log_int.rast) <- crs(covs) 
      
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
