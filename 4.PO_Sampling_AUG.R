## TO DO: 
# Need to think about case when there's no PO data in a grid. Need to set some sort of minimum number of PO data points in a grid

library(purrr)

# Non-thinned version -----------------------------------------------------
# Have to rep over all replicates, extract the Grid A and B keep the spp PO records from just those grids

spp_process <- cbind(x = lg.s$x, y = lg.s$y)

# po <- thinpp[, c("x", "y")]

# For random covariate case
po <- spp_process

# This function locates the presence-only data in the Grid A and B and saves it as an object
PO.data <- imap(extrap.reps.out, function(extrap.type, extrap.name)
  
  {
  
  imap(extrap.type, function(rep, rep_index) {
    
    print(paste("Processing extrap.type:", extrap.name))
    
    print(paste("Processing rep:", rep_index))
    
    rand.gridA <- rep$rand.gridA
    rand.gridB <- rep$rand.gridB
    
    # Trim po to only include points in Site A or B
    po.rand.gridA <- po[
      po[,1] >= xmin(ext(rand.gridA)) & po[,1] <= xmax(ext(rand.gridA)) & 
        po[,2] >= ymin(ext(rand.gridA)) & po[,2] <= ymax(ext(rand.gridA)), 
    ]
    
    po.rand.gridB <- po[
      po[,1] >= xmin(ext(rand.gridB)) & po[,1] <= xmax(ext(rand.gridB)) & 
        po[,2] >= ymin(ext(rand.gridB)) & po[,2] <= ymax(ext(rand.gridB)), 
    ]
    
    # If there are no presences from the presence-only data in the PA grid
    # Halt the computation and move to next replicate
    if(length(po.rand.gridA) == 0) {
      print(paste0("No PO in Grid A, Extrap.type: ", extrap.name, ", Rep:", rep_index))
    }
    
    if(length(po.rand.gridB) == 0) {
      print(paste0("No PO in Grid A, Extrap.type: ", extrap.name, ", Rep:", rep_index))
    }
    
      
          return(list(PO_GridA = po.rand.gridA, 
                PO_GridB = po.rand.gridB)) 
     
  })
})

# Add PO data to list
extrap.reps.out <- map2(extrap.reps.out, PO.data, ~map2(.x, .y, c))

# Plot one grid to check
ggplot() +
  geom_tile(data = extrap.reps.out$Low[[1]]$covs.SiteA, aes(x = x, y = y, fill = cov1)) +
  geom_point(data = extrap.reps.out$Low[[1]]$PO_GridA, aes(x = x, y = y), color = "red") +
  theme_bw()

# Plot other grid to check
ggplot() +
  geom_tile(data = extrap.reps.out$Low[[1]]$covs.SiteB, aes(x = x, y = y, fill = cov1)) +
  geom_point(data = extrap.reps.out$Low[[1]]$PO_GridB, aes(x = x, y = y), color = "red") +
  theme_bw()


