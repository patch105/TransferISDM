
########################################################################
########################## 4. PO sampling   ##########################
########################################################################

# This script simulates presence-only (PO) records at the training site (Site A). 

# It does so by thinning the latent species process either randomly (if bias == F) or according to a bias field (if bias == T).

# For modelling the bias with a bias covariate later, a ~0.99 correlated bias field is generated and saved as a raster (Bias.rast = log(bias_noisy)).

# *** NOTE *** if you want a bias covariate of a different correlation or you make a different original bias field, you must manually change the 'noise' object below to reflect this.

# Inputs: 

# The output from step 3.

# Is there bias in the PO sampling? bias = T or F

# detect.prob is the probability of detecting a species at a site and controls how many PO records there will be (along with the intercept of the latent species distribution). If you want bias and no bias scenarios to have equivalent numbers of PO records, you should set detect.prob to be the mean of the bias field.

# maxprob is the maximum probability of detection in the bias field (if bias == T)

# rast_cellsA are dimensions of the Training site


# Outputs: A list that contains the species process at the Grid A (training site), the PO records and count of them from Grid A, as well as the bias covariate Bias.rast


## ADDITIONAL FUNCTION:

# po_checking_func() 

# This function is run after po_sampling_func() to check if there are any reps with no PO data in Grid A. If there are, the rep is removed from the output list and the number of removed reps is printed. The code is set up in 0.Run_Replicates.R to re-run the reps with no PO data in Grid A.

########################################################################

po_sampling_func <- function(reps.setup.list,
                             bias = FALSE,
                             detect.prob,
                             maxprob,
                             rast_cellsA) {
  
  # This function locates the presence-only data in the Grid A and B and saves it as an object
  PO.data <- imap(reps.setup.list, function(extrap.type, extrap.name) { 
    
    imap(extrap.type, function(rep, rep_index) { 
      
      print(paste("Processing extrap.type:", extrap.name))
      
      print(paste("Processing rep:", rep_index))
      
      if(length(rep) < 4) { # If a replicate has no PO currently 
        
        print("no PO")
        
        # Get the original species process
        spp_process <- cbind(x = rep$latent.list$lg.s$x, y = rep$latent.list$lg.s$y)
        
        rand.gridA <- rep$extrap.reps.out$rand.gridA
        rand.gridB <- rep$extrap.reps.out$rand.gridB
        
        # Trim species process to only include points in Site A
        spp_process.rand.gridA <- spp_process[
          spp_process[,1] >= xmin(ext(rand.gridA)) & spp_process[,1] <= xmax(ext(rand.gridA)) & 
            spp_process[,2] >= ymin(ext(rand.gridA)) & spp_process[,2] <= ymax(ext(rand.gridA)), ]
        
        # If no bias, random thinning applied based on detect.prob
        if(bias == FALSE) {
          
          # Thin the process by the probability
          po.rand.gridA <- cbind(spp_process.rand.gridA, presence = rbinom(nrow(spp_process.rand.gridA), 1, detect.prob))
          po.rand.gridA <- po.rand.gridA[po.rand.gridA[, "presence"] == 1, ]
          
          # Trim to just xy
          po.rand.gridA <- po.rand.gridA[, c("x", "y")]
          
        }
        
        # If bias field applied 
        if(bias == TRUE) {
          
          # Thin the process by the bias field
          minprob <- maxprob/10 # Keep the relative difference between maximum and minimum probabilities the same across different scenarios of maxprob (i.e. strength of bias is the same)
          
          probseq <-  exp(seq(log(minprob), log(maxprob), length.out = rast_cellsA[1]))
          
          # Generate a matrix of continuous values from minprob to maxprob, going left to right
          bias_matrix <- matrix(rep(probseq, each = rast_cellsA[1]), nrow = rast_cellsA[1], ncol = rast_cellsA[2], byrow = TRUE)
          
          # Flatten the matrix into a vector to match the expected input format for 'vals'
          bias_vals <- as.vector(bias_matrix)
          
          # Make the true bias cov
          
          bias.true <- rast(nrows = rast_cellsA[1],
                       ncols = rast_cellsA[2],
                       xmin = xmin(rand.gridA),
                       xmax = xmax(rand.gridA),
                       ymin = ymin(rand.gridA),
                       ymax = ymax(rand.gridA),
                       resolution = res,
                       vals = bias_vals,
                       names = c("bias")
          )
          
          crs(bias.true) <- "epsg:3857" # Setting to WGS 84 / Pseudo-Mercator projection for later functions requiring cell size
          names(bias.true) <- "bias"
          
          # Add random noise to make correlated bias variable (~0.99 correlation)
          noise <- rnorm(length(bias_vals), mean = 0, sd = 0.035 * max(bias_vals)) # Adjust 'sd' for desired correlation
          
          bias_vals_noisy <- bias_vals + noise
          bias_vals_noisy <- pmax(pmin(bias_vals_noisy, maxprob), minprob) # Clip to valid range
          
          bias_noisy <- rast(nrows = rast_cellsA[1],
                             ncols = rast_cellsA[2],
                             xmin = xmin(rand.gridA),
                             xmax = xmax(rand.gridA),
                             ymin = ymin(rand.gridA),
                             ymax = ymax(rand.gridA),
                             resolution = res,
                             vals = bias_vals_noisy,
                             names = c("bias")
          )
          
          crs(bias_noisy) <- "epsg:3857"
          names(bias_noisy) <- "bias"
          
          # Check correlation between original and noisy bias (this is how you can adjust your noise object)
          correlation <- cor(bias_vals, bias_vals_noisy)
          print(paste("Correlation between original and noisy bias:", round(correlation, 4)))
          
          # Add spatial bias info to your spp. process at grid A data
          po.rand.gridA <- cbind(spp_process.rand.gridA, bias = terra::extract(bias.true, spp_process.rand.gridA[,1:2]))
          
          # Thin the process by the bias field
          po.rand.gridA <- cbind(po.rand.gridA, presence = rbinom(nrow(po.rand.gridA), 1, po.rand.gridA[, "bias"]))
          
          po.rand.gridA <- po.rand.gridA[po.rand.gridA$presence == 1, ]
          
          # Turn back into a matrix
          po.rand.gridA <- as.matrix(po.rand.gridA)
          
          # Trim to just xy
          po.rand.gridA <- po.rand.gridA[, c("x", "y")]
          
        }

        po.rand.gridB <- spp_process[
          spp_process[,1] >= xmin(ext(rand.gridB)) & spp_process[,1] <= xmax(ext(rand.gridB)) &
            spp_process[,2] >= ymin(ext(rand.gridB)) & spp_process[,2] <= ymax(ext(rand.gridB)),
        ]
        
        # Formatting if there's only one PO point in Grid A or B so that they become dataframes
        
        if(length(po.rand.gridA) == 2) {
          
          po.rand.gridA <- matrix(po.rand.gridA, ncol = 2, byrow = T)
          dimnames(po.rand.gridA) <- list(NULL, c("x", "y"))
        }
        
        if(length(po.rand.gridB) == 2) {
          
          po.rand.gridB <- matrix(po.rand.gridB, ncol = 2, byrow = T)
          dimnames(po.rand.gridB) <- list(NULL, c("x", "y"))
        }
        
        # If bias field applied 
        if(bias == TRUE) { 
          
          return(list(spp_process.rand.gridA = spp_process.rand.gridA,
                                       PO_GridA = po.rand.gridA,
                                       PO_GridB = po.rand.gridB,
                                       n_po_gridA = nrow(po.rand.gridA),
                                       n_po_gridB = nrow(po.rand.gridB),
                                       Bias.rast = log(bias_noisy))) # Log the bias raster
        }
        
        # If bias field not applied 
        if(bias == FALSE) { 
          
          return(list(spp_process.rand.gridA = spp_process.rand.gridA,
                                        PO_GridA = po.rand.gridA,
                                        PO_GridB = po.rand.gridB,
                                        n_po_gridA = nrow(po.rand.gridA),
                                        n_po_gridB = nrow(po.rand.gridB)))
                                        
          }
        
        
        
        }
       

      })
    
    })
                  
  # Add PO data to list
  reps.setup.list <- map2(reps.setup.list, PO.data, ~map2(.x, .y, c))                
  
  return(reps.setup.list)
  
}




# CHECK if PO data in both Sites ------------------------------------------

# If none, need to re-run for that replicate
po_checking_func <- function(reps.setup.list) {

  # Initialise a counter for removed reps
  removed_counts <- c(Low = 0, Moderate = 0, High = 0)

  # Iterate through each Extrap type
  for (extrap.type in c("Low", "Moderate", "High")) {

    # Get the corresponding sub-list (Low, Moderate, or High)
    reps_list <- reps.setup.list[[extrap.type]]

    remove_index <- c()

    # Iterate through each rep within the sub-list
    for (i in seq_along(reps_list)) {

      # Check if PO_grid in the rep has no PO data
      if (length(reps_list[[i]]$PO_GridA) < 2) {

        # Save index
        remove_index <- c(remove_index, i)

      }
    }

    # Remove the reps with 0 length PO_GridA
    if (length(remove_index) > 0) {

      reps_list <- reps_list[-remove_index]

      # Count the number of removed reps for this extrap.type
      removed_counts[extrap.type] <- length(remove_index)
    }

    # Assign the updated list back to the original structure
    reps.setup.list[[extrap.type]] <- reps_list
  }

  # Print the number of removed reps for each extrap.type
  for (extrap.type in names(removed_counts)) {
    cat(removed_counts[extrap.type], "reps removed from", extrap.type, "\n")
  }

  return(list(reps.setup.list = reps.setup.list, removed_counts = removed_counts))

}



