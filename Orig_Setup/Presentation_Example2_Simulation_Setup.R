library(spatstat)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(viridis)
library(terra)

set.seed(50)

outpath <- getwd()



# Version 2. Simulate Spatial Covariates - XZ code ---------------------------------

# START with set up of resolution and north/east step length for later Site A and B grid creation.

# Set ncol
ncol <- 100
nrow <- 100
res <- 0.01

# Create a bounded domain on [0, 1] x [0, 1]

east_min <- 0
east_max <- 1
north_min <- 0
north_max <- 1

# We generate the grid resolution from min, max dimensions and the number of pixels

# Set number of pixels (100 x 100)
n_bau_east <- ncol
n_bau_north <- nrow
# so now we have n_bau_est x n_bau_north grid cells

# Obtain the cell resolution
bau_east_step <- (east_max - east_min) / n_bau_east
bau_north_step <- (north_max - north_min) / n_bau_north 

# Generate grid centroid coordinates
# We do this so that our centroid begins in the centre of a cell (hence, bau_east_step/2))

eastings <- seq(east_min + bau_east_step/2, east_max - bau_east_step/2, by = bau_east_step)
northings <- seq(north_min + bau_north_step/2, north_max - bau_north_step/2, by = bau_north_step)

coords <- as.matrix(expand.grid(eastings, northings))
colnames(coords) <- c("eastings", "northings")


# library(geoR)
# 
# # cov.model is the covariance model, cov.pars is the parameters (partial sill, range parameter)
# # Currently using partial sill and range values from Muff et al., 2019 and default kappa / nugget
# cov1 <- grf(n = ncell(r), grid = "reg", nx = 300, ny = 300, xlims = c(0,300), ylims = c(0,300), cov.model = "matern", cov.pars=c(50,0.1), kappa = 0.5, nugget = 0)


# Following the approach of Fletcher Jr. et al. (2023) and Grimmett et al. (2021), we use the NLMR package.
# Create simulated habitat using Gaussian random fields within a 100 x 100 grid


# First install NLMR
# remotes::install_github("ropensci/NLMR")
# install.packages("landscapetools")

library(NLMR)
library(landscapetools)
library(RandomFields) # Note that RandomFields is no longer on CRAN. Downloaded archived file.
library(terra)

cov1 <- nlm_gaussianfield(ncol = ncol,
                          nrow = nrow,
                          resolution = res,
                          autocorr_range = 50, # Maximum range (raster units) of spatial autocorrelation
                          mag_var = 1, # Magnitude of variation over the landscape
                          nug = 0.01, # Magnitude of variation in the scale of the autocorr_range (smaller values = more homogenous)
                          mean = 0.5, # Mean value over the field
                          user_seed = 2L, # Set random seed for the simulation
                          rescale = T # If T, the values are rescaled between 0 and 1
) %>% 
  rast()

crs(cov1) <- "epsg:3857" # Setting to WGS 84 / Pseudo-Mercator projection for later functions requiring cell size

c1 <- cov1 %>% 
  as.data.frame(xy = T) %>%  
  ggplot() + 
  geom_tile(aes(x = x, y = y, fill = layer)) + 
  scale_fill_viridis() +
  coord_fixed() + 
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank()) +
  ggtitle('Covariate 1')

c1

# Set Parameters Data Generation ----------------------------------------------------------

beta0 <- 5 # Intercept
beta1 <- 0.5 # Coefficient for cov 1
beta2 <- 0.1 # Coefficient for cov 2
# beta3 <- 5 # Coefficient for cov 1*2 interaction
var <- 1 # Variance of the Gaussian field (changed  from 0.5)  
scal <- 0.2 # Scale parameter 
nu <- 1 # Smoothness parameter - ONLY FOR MATERN
seed <- 3L


# Version 2. Mean of process dependent on TWO spatially-varying covar - XZ code --------

# Note to self - the im function requires a matrix of a certain input order
# Terra::as.matrix just converts all cells to a matrix in the same order as they appear in the raster

# The code below therefore: converts the raster to a matrix, retaining the terra ordering
# We then reshape into long (x, y) format 
# We extract the coords from the original raster and bind to the covariate matrix

# Get coords of original raster
coords <- xyFromCell(cov1, 1:ncell(cov1))

# Convert raster to matrix object
cov1.mat <- terra::as.matrix(cov1, wide = T) 


cov1.mat2 <- cov1.mat %>% 
  reshape2::melt(c("x", "y"), value.name = "cov") 

GRF.cov1 <- cbind(x = coords[,1], y = coords[,2], cov = cov1.mat2["cov"]) 

# Can do with one or two covariates
fe <- beta0 + beta1*GRF.cov1[, "cov"] 
# fe <- beta0 + beta2*GRF.cov2[, "cov"]

mu <- data.frame(x = coords[,1], y = coords[, 2], z = fe)
mu <- spatstat.geom::as.im(mu, W = win)

plot(mu)


# Set seed 
set.seed(seed)

# Create LGCP with environmental covariate
lg.s <- rLGCP('exp', mu = mu,
              var=var, scale=scal)

lg.s <- rpoispp(lambda <- mu)

plot(lg.s)

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

crs(true_log_int.rast) <- crs(cov1)

# Extract cell size because RISDM predictions are with reference to cell area
log.cell_size <- log(cellSize(cov1))

# Add intensity + log(cell area)
true_log_int.rast <- true_log_int.rast+log.cell_size

true_int_plot <- true_log_int.rast %>% 
  as.data.frame(xy = T) %>% 
  ggplot() + 
  geom_tile(aes(x = x, y = y, fill = exp(int))) + 
  scale_fill_viridis() +
  coord_fixed() + 
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank()) +
  ggtitle('True log intensity')


# Convert the point pattern to a data frame for ggplot
points_df <- as.data.frame(cbind(lg.s$x, lg.s$y)[, 2:1]) %>% 
  rename(x = V1, y = V2)

# Add the points to the intensity plot
true_int_plot +
  geom_point(data=points_df, aes(x=x, y=y), color='black', size=1.5) 


