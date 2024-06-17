
# Extract the 'true' intensity (lambda) values from the LGCP model so that they can be compared with the estimated intensity

# The rLGCP function has produced a realisation of the LGCP but also saved the intensity values 

# Code modified from Simmonds et al. (2020).
# NOTE - Need to fix so I extract the median intensity, right now I think it's the mean

###################################################

# True intensity
Lam <- attr(lg.s, "Lambda")

# Get the (v) log intensity values (expected number of points per unit area)
# NOTE - the format is from the lgcp package, so I need to reverse the order if want to plot
true_log_int <- log(Lam$v) 

# Reverse the row order
true_log_int <- apply(true_log_int, 2, rev)

# Transpose the matrix to match the raster layout
true_log_int <- t(true_log_int)

# Melt into xy dataframe
true_log_int.melt <- true_log_int %>% 
  reshape2::melt(c("x", "y"), value.name = "int") 

# Create a raster  
true_log_int.rast <- cbind(x = coords[,1], y = coords[,2], true.int = true_log_int.melt["int"]) %>% rast(.)

# Extract cell size 
log.cell_size <- log(cellSize(cov))

true_log_int.rast <- true_log_int.rast*log.cell_size

# Plot the true log intensity
true_log_int.rast %>% 
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




############## ARCHIVE ##############################

# Extract abundance values by point for truth
# Set up a blank grid shape of the covariate domain
grid <- rast(ext(cov),
             resolution = res(cov),
             crs = crs(cov))

# Extract raster cell coordinates
xy <- xyFromCell(grid, 1:ncell(grid))

# Create a data frame with the coordinates
grid_expand <- data.frame(x = xy[,1], y = xy[,2])

# Now find the nearest pixel for every lambda in the LGCP
grid_expand$abundance <- true_log_int[Reduce('cbind', nearest.pixel(
  grid_expand[,1], grid_expand[,2],
  im(true_log_int)))] # Converting to an image pixel so it can be processed by the nearest.pixel function

truth_grid <- rast(grid_expand, crs = crs(cov))

plot(truth_grid)

### TO FIX: THE grid_expand section isn't working, I think because of the differences in resolution vs. number of cols/rows 

###################################################
# Test code
test <- matrix(c("A", "B", 
                 "C", "D"), 
               nrow = 2, 
               ncol = 2, 
               byrow = TRUE)

plot(rast(test))

test <- as.data.frame(test, xy  = T)

# Reverse the row order
test2 <- apply(test, 2, rev)

# Transpose the matrix to match the raster layout
test3 <- t(test2)

test4 <- test3 %>% 
  reshape2::melt(c("x", "y"), value.name = "int") 

true_log_int.rast <- cbind(x = coords[,1], y = coords[,2], true.int = true_log_int.melt["int"]) %>% rast(.)

true_log_int.rast <- cbind(x = coords[,1], y = coords[,2]) %>% rast(.)

