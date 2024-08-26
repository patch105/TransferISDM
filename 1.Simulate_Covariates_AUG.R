
library(spatstat)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(viridis)

# Install RandomFields package (required for covariate simulation). No longer on CRAN. 
# Downloaded archived RandomFields utils and RandomFields with code below, accessed from: https://cran.r-project.org/src/contrib/Archive/RandomFieldsUtils/ and https://cran.r-project.org/src/contrib/Archive/RandomFields/ 

# install.packages("C:/Users/n11222026/OneDrive - Queensland University of Technology/Code/Downloaded_packages/RandomFieldsUtils_1.2.5.tar.gz", repos = NULL, type="source")

# install.packages("C:/Users/n11222026/OneDrive - Queensland University of Technology/Code/Downloaded_packages/RandomFields_3.3.14.tar.gz", repos = NULL, type="source")

set.seed(50)

outpath <- getwd()


# Version 2. Simulate Spatial Covariates - XZ code ---------------------------------

# START with set up of resolution and north/east step length for later Site A and B grid creation.

# Set ncol
ncol <- 1000
nrow <- 1000
res <- 0.01

# Create a bounded domain on [0, 1] x [0, 1]

east_min <- 0
east_max <- 10
north_min <- 0
north_max <- 10
# dom <- spatstat.geom::owin(c(east_min, east_max), c(north_min, north_max))

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
library(RandomFields) # See above for download from archived files. 
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

names(cov1) <- "cov"

cov2 <- nlm_gaussianfield(ncol = ncol,
                          nrow = nrow,
                          resolution = res,
                          autocorr_range = 50, # Maximum range (raster units) of spatial autocorrelation
                          mag_var = 1, # Magnitude of variation over the landscape
                          nug = 0.01, # Magnitude of variation in the scale of the autocorr_range (smaller values = more homogenous)
                          mean = 0.5, # Mean value over the field
                          user_seed = 3L, # Set random seed for the simulation
                          rescale = T # If T, the values are rescaled between 0 and 1
) %>% 
  rast()

crs(cov2) <- "epsg:3857" # Setting to WGS 84 / Pseudo-Mercator projection for later functions requiring cell size

names(cov2) <- "cov"

c1 <- cov1 %>% 
  as.data.frame(xy = T) %>%  
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

c2 <- cov2 %>% 
  as.data.frame(xy = T) %>%  
  ggplot() + 
  geom_tile(aes(x = x, y = y, fill = cov)) + 
  scale_fill_viridis() +
  coord_fixed() + 
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank()) +
  ggtitle('Covariate 2')

plot2 <- ggarrange(c1, c2)
# plot2 
ggsave(plot = plot2, filename = paste0(outpath, "/output/Covariates_GRF.png"), w = 21.5, h = 18, units = "cm", dpi = 400, device = "png")



