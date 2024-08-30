

# 4. PO sampling ----------------------------------------------------------

po_sampling_func <- function(reps.setup.list) {
  
  # This function locates the presence-only data in the Grid A and B and saves it as an object
  PO.data <- imap(reps.setup.list, function(extrap.type, extrap.name) { 
    
    imap(extrap.type, function(rep, rep_index) { 
      
      print(paste("Processing extrap.type:", extrap.name))
      
      print(paste("Processing rep:", rep_index))
      
      if(length(rep) < 4) { # If a replicate has no PO currently 
        
        print("no PO")
        
        po <- cbind(x = rep$latent.list$lg.s$x, y = rep$latent.list$lg.s$y)
        
        rand.gridA <- rep$extrap.reps.out$rand.gridA
        rand.gridB <- rep$extrap.reps.out$rand.gridB
        
        # Trim po to only include points in Site A or B
        po.rand.gridA <- po[
          po[,1] >= xmin(ext(rand.gridA)) & po[,1] <= xmax(ext(rand.gridA)) & 
            po[,2] >= ymin(ext(rand.gridA)) & po[,2] <= ymax(ext(rand.gridA)), 
        ]
        
        po.rand.gridB <- po[
          po[,1] >= xmin(ext(rand.gridB)) & po[,1] <= xmax(ext(rand.gridB)) & 
            po[,2] >= ymin(ext(rand.gridB)) & po[,2] <= ymax(ext(rand.gridB)), 
        ]
        
        return(list(PO_GridA = po.rand.gridA, 
                    PO_GridB = po.rand.gridB))
        
        }
       

      })
    
    })
                  
  # Add PO data to list
  extrap.reps.out <- map2(reps.setup.list, PO.data, ~map2(.x, .y, c))                
  
  return(extrap.reps.out)
  
}




# CHECK if PO data in both Sites ------------------------------------------

# If none, need to re-run for that rep.
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
      
      # Check if PO_grid in the rep has length 0
      if (length(reps_list[[i]]$PO_GridA) == 0 | length(reps_list[[i]]$PO_GridB) == 0) {
        
        # Save index
        remove_index <- c(remove_index, i)
        
      }
    }
    
    # Remove the reps with 0 length PO_GridA or PO_GridB
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



