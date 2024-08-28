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
    
    # rep <- extrap.reps.out$Low[[1]]
    
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
    

# ADDED - PLOTTING PO DATA ON GRID  ---------------------------------------

    # # Extract the extents
    # extA <- ext(rand.gridA)
    # extB <- ext(rand.gridB)
    # 
    # df_extA <- data.frame(
    #   xmin = extA[1],
    #   xmax = extA[2],
    #   ymin = extA[3],
    #   ymax = extA[4],
    #   color = "black",
    #   label = "Site A",
    #   label_x = (extA[1] + extA[2]) / 2,  # Center of the extent for text placement
    #   label_y = extA[4] + (extA[4] - extA[3]) * 0.1  # Slightly above the top of the extent
    # )
    # 
    # df_extB <- data.frame(
    #   xmin = extB[1],
    #   xmax = extB[2],
    #   ymin = extB[3],
    #   ymax = extB[4],
    #   color = "blue",
    #   label = "Site B",
    #   label_x = (extB[1] + extB[2]) / 2,  # Center of the extent for text placement
    #   label_y = extB[4] + (extB[4] - extB[3]) * 0.1  # Slightly above the top of the extent
    # )
    # 
    # # Combine the extents into one data frame
    # df_extents <- rbind(df_extA, df_extB)
    # 
    # # Plot PO data on grid A and B
    # ggplot() +
    #   geom_tile(data = rep$covs.SiteA, aes(x = x, y = y, fill = cov1)) +
    #   scale_fill_viridis() +
    #   coord_fixed() +
    #   geom_point(data = po.rand.gridA, aes(x = x, y = y), color = "red") +
    #   theme_bw() +
    #   theme(axis.title.x = element_blank(),
    #         axis.title.y = element_blank(),
    #         legend.ticks = element_blank(),
    #         legend.title = element_blank()) +
    #   ggtitle('Site A')
    # 
    # ggplot() +
    #   geom_tile(data = rep$covs.SiteB, aes(x = x, y = y, fill = cov1)) +
    #   scale_fill_viridis() +
    #   coord_fixed() +
    #   geom_point(data = po.rand.gridB, aes(x = x, y = y), color = "red") +
    #   theme_bw() +
    #   theme(axis.title.x = element_blank(),
    #         axis.title.y = element_blank(),
    #         legend.ticks = element_blank(),
    #         legend.title = element_blank()) +
    #   ggtitle('Site B')
    # 
    # ggplot() +
    #   geom_tile(data = cov1.df, aes(x = x, y = y, fill = cov)) +
    #   geom_point(data = po, aes(x = x, y = y), color = "red") +
    #   geom_rect(data = df_extents, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, color = color), 
    #             fill = NA, linetype = "solid", linewidth = 1) +
    #   scale_color_identity() +
    #   theme_bw()
    
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


