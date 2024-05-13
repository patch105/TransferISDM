

# Version 1. Sample grid continuous covariates ----------------------------


# Turn original domain grid into a raster
domain.grid <- gridcov1 %>% 
  reshape2::melt(c("y", "x"), value.name = "cov") %>% 
  rast()

# Set size of grid 
rast_size <- c(100,50)

# Get coords of overall grid domain boundary
xmin <- min(x0)
xmax <- max(x0)
ymin <- min(y0)
ymax <- max(y0)


rand.x.lim <- c(xmax - rast_size[1], ymax - rast_size[2])
# Create random coordinate index for top corner of subgrid within grid domain
# Then use this index on x0 to get the coordinate
xmin.rand <- x0[round(runif(1, min = xmin, max = rand.x.lim[1]))]
ymin.rand <- y0[round(runif(1, min = ymin, max = rand.x.lim[2]))]
xmax.rand <- x0[xmin.rand + rast_size[1]]
ymax.rand <- y0[ymin.rand + rast_size[2]]

rand.grid <- rast(xmin = xmin.rand, 
             xmax = xmax.rand, 
             ymin = ymin.rand, 
             ymax = ymax.rand, 
             nrows = rast_size[1], 
             ncols = rast_size[2],
             vals = 1:50)



plot(domain.grid)
lines(ext(rand.grid), lwd = 2, col = "red")



