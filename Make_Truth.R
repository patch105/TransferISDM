
# Extract the 'true' intensity (lambda) values from the LGCP model so that they can be compared with the estimated intensity

# The rLGCP function has produced a realisation of the LGCP but also saved the intensity values 

# Code modified from Simmonds et al. (2020).
# NOTE - Need to fix so I extract the median intensity, right now I think it's the mean

# True intensity
Lam <- attr(lg.s, "Lambda")

# Get the (v) log intensity values (expected number of points per unit area)
# NOTE - the format is from the lgcp package, so I need to reverse the order if want to plot
true_log_int<- log(Lam$v) 

# This code basically reverses the row order from 100 to 1 due to layout in lgcp output
# For plotting
true_log_int.rast <- true_log_int %>% 
  reshape2::melt(c("y", "x"), value.name = "int") %>% rast(.)

true_log_int %>% 
  reshape2::melt(c("y", "x"), value.name = "int") %>% 
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
grid_expand$abundance <- data[Reduce('cbind', nearest.pixel(
  grid_expand[,1], grid_expand[,2],
  im(true_log_int)))] # Converting to an image pixel so it can be processed by the nearest.pixel function

truth_grid <- rast(grid_expand, crs = crs(cov))

### TO FINISH: GET THE FINAL BIT WORKING AND CHECK THAT IT'S DOING THE RIGHT MATCHING OF THE SPATSTAT MATRIX FORMAT TO THE 
# RIGHT FORMAT FOR THE COVARIATE RASTER EXTENT

# Then we want to compare with our median expectation of the log intensity

