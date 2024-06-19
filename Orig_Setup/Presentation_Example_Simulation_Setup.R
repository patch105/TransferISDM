
library(spatstat)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(viridis)
library(terra)

set.seed(50)

outpath <- getwd()

# Version 1. Simulate Continuous Covariates ------------------------------------------

# Create a bounded domain on [0, 300] x [0, 300]

dim <- c(300, 300)

win <- owin(c(0,dim[1]), c(0,dim[2])) 

# set number of pixels
spatstat.options(npixel=c(dim[1],dim[2]))

# Create an artificial covariate
# create y0 and x0 separately as rectangle
y0 <- seq(win$yrange[1], win$yrange[2],
          length=spatstat.options()$npixel[2])
x0 <- seq(win$xrange[1], win$xrange[2],
          length=spatstat.options()$npixel[1])

# Covariate is a continuous gradient from the bottom to the top with values from 0 to 1
multiplier <- 1/dim[2]

gridcov1 <- outer(y0,x0, function (x,y) multiplier*x + 0*y)

gridcov2 <- outer(y0,x0, function (x,y) multiplier*y + 0*x)


c1 <- gridcov1 %>% 
  reshape2::melt(c("x", "y"), value.name = "cov") %>% 
  ggplot() + 
  geom_tile(aes(x = x, y = y, fill = cov)) + 
  scale_fill_viridis() +
  coord_fixed() + 
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank()) +
  ggtitle('Covariate 1')

c1

cov1.rast <- gridcov1 %>% 
  reshape2::melt(c("y", "x"), value.name = "cov1") %>% 
  rast() 

crs(cov1.rast) <- crs("epsg:3857")

# Set Parameters Data Generation ----------------------------------------------------------

beta0 <- 5 # Intercept
beta1 <- 0.5 # Coefficient for cov 1
beta2 <- 0.1 # Coefficient for cov 2
# beta3 <- 5 # Coefficient for cov 1*2 interaction
var <- 1 # Variance of the Gaussian field (changed  from 0.5)  
scal <- 0.2 # Scale parameter 
nu <- 1 # Smoothness parameter - ONLY FOR MATERN
seed <- 3L


# Version 1. Mean of process dependent on continuous covariates ----------------------------------

# Note to self - the im inputs the matrix and outputs a pixel image that is the correct xy values (transposes the matrix)

mu_gcov1 <- im(beta0 + beta1*gridcov1, xcol=x0, yrow=y0)

plot(mu_gcov1)

# Set seed 
set.seed(seed)

# Create LGCP with environmental covariate
lg.s <- rLGCP('matern', mu = mu_gcov1,
              var=var, scale=scal, nu = nu)

# plot(lg.s)

######## TEST ###############
# Convert the pixel image to a data frame for ggplot
intensity_df <- as.data.frame(as.im(mu_gcov1), xy=TRUE)

# Plot the true intensity surface using ggplot
intensity_plot <- ggplot(intensity_df) +
  geom_tile(aes(x=x, y=y, fill=value)) +
  scale_fill_viridis(name="Intensity") +
  coord_fixed() +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank()) +
  ggtitle('True Intensity Surface')

# Step 2: Overlay the point pattern
# Convert the point pattern to a data frame for ggplot
points_df <- as.data.frame(lg.s)

# Add the points to the intensity plot
intensity_with_points_plot <- intensity_plot +
  geom_point(data=points_df, aes(x=x, y=y), color='red', size=0.5) +
  ggtitle('True Intensity Surface with LGCP Realization')

# Display the plot
print(intensity_with_points_plot)

##### EXTRACT AND PLOT TRUE INTENSITY #######

# Get coords of original raster
coords <- xyFromCell(cov1.rast, 1:ncell(cov1.rast))

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

crs(true_log_int.rast) <- crs("epsg:3857")

# Extract cell size because RISDM predictions are with reference to cell area
log.cell_size <- log(cellSize(cov1.rast))

# Add intensity + log(cell area)
true_log_int.rast <- true_log_int.rast+log.cell_size

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


