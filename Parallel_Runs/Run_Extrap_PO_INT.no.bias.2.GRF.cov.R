
# extract the arguments provided in the command line
args <- commandArgs(trailingOnly = TRUE)
# The first argument is now the job index
job_index <- as.integer(args[1])

library(spatstat)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(viridis)
library(NLMR)
library(landscapetools)
library(RandomFields) # Note that RandomFields is no longer on CRAN. Downloaded archived file.
library(terra)

# # Install flexsdm
# remotes::install_github("sjevelazco/flexsdm")
library(flexsdm)

outpath <- getwd()


############################################################################
################# Simulate Covariates ######################################
###########################################################################

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

cov1 <- nlm_gaussianfield(ncol = ncol,
                          nrow = nrow,
                          resolution = res,
                          autocorr_range = 50, # Maximum range (raster units) of spatial autocorrelation
                          mag_var = 1, # Magnitude of variation over the landscape
                          nug = 0.01, # Magnitude of variation in the scale of the autocorr_range (smaller values = more homogenous)
                          mean = 0.5, # Mean value over the field
                          user_seed = NULL, # Set random seed for the simulation
                          rescale = T # If T, the values are rescaled between 0 and 1
) %>% 
  rast()

crs(cov1) <- "epsg:3857" # Setting to WGS 84 / Pseudo-Mercator projection for later functions requiring cell size

cov2 <- nlm_gaussianfield(ncol = ncol,
                          nrow = nrow,
                          resolution = res,
                          autocorr_range = 50, # Maximum range (raster units) of spatial autocorrelation
                          mag_var = 1, # Magnitude of variation over the landscape
                          nug = 0.01, # Magnitude of variation in the scale of the autocorr_range (smaller values = more homogenous)
                          mean = 0.5, # Mean value over the field
                          user_seed = NULL, # Set random seed for the simulation
                          rescale = T # If T, the values are rescaled between 0 and 1
) %>% 
  rast()

crs(cov2) <- "epsg:3857" # Setting to WGS 84 / Pseudo-Mercator projection for later functions requiring cell size

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

c2 <- cov2 %>% 
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
  ggtitle('Covariate 2')

plot2 <- ggarrange(c1, c2)

ggsave(plot = plot2, filename = paste0(outpath, "/output/", job_index,"_Plot_Covariates_GRF.png"), w = 21.5, h = 18, units = "cm", dpi = 400, device = "png")


#########################################################################
##################### SIMULATE LATENT DIST ##############################
#########################################################################

# Set Parameters Data Generation ----------------------------------------------------------

beta0 <- 5 # Intercept
beta1 <- 0.5 # Coefficient for cov 1
beta2 <- 0.1 # Coefficient for cov 2
# beta3 <- 5 # Coefficient for cov 1*2 interaction
var <- 1 # Variance of the Gaussian field (changed  from 0.5)  
scal <- 0.2 # Scale parameter 
nu <- 1 # Smoothness parameter - ONLY FOR MATERN

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

cov2.mat <- terra::as.matrix(cov2, wide = T) 

cov1.mat2 <- cov1.mat %>% 
  reshape2::melt(c("x", "y"), value.name = "cov") 

GRF.cov1 <- cbind(x = coords[,1], y = coords[,2], cov = cov1.mat2["cov"]) 

cov2.mat2 <- cov2.mat %>% 
  reshape2::melt(c("x", "y"), value.name = "cov")

GRF.cov2 <- cbind(x = coords[,1], y = coords[,2], cov = cov2.mat2["cov"])

# Can do with one or two covariates
fe <- beta0 + beta1*GRF.cov1[, "cov"] + beta2*GRF.cov2[, "cov"]
# fe <- beta0 + beta2*GRF.cov2[, "cov"]

mu <- data.frame(x = coords[,1], y = coords[, 2], z = fe)
mu <- spatstat.geom::as.im(mu, W = win)

# Create LGCP with environmental covariate
lg.s <- rLGCP('exp', mu = mu,
              var=var, scale=scal)

#####################################################################
################# RUN EXTRAPOLATION #################################
#####################################################################

# Version 2a. Sample grid GRF covariates - XZ code -----------------

gridcov1.rast <- rast(GRF.cov1, type = "xyz") 
gridcov2.rast <- rast(GRF.cov2, type = "xyz")

crs(gridcov1.rast) <- "epsg:3857" # Setting to WGS 84 / Pseudo-Mercator projection for later functions requiring cell size
crs(gridcov2.rast) <- "epsg:3857" # Setting to WGS 84 / Pseudo-Mercator projection for later functions requiring cell size

# Specify number of replicates per extrapolation type
nreps <- 1

extrap_out <- list()
extrap.reps.out <- list(Low = list(), Moderate = list(), High = list())

# Function for generating Site A and Site B and calculating extrapolation
extrap_func <- function() {
  
  # Set size of grid (number of cells) for Site A (Reference)
  # NOTE - must be smaller than total cell number in x y directions
  rast_cellsA <- c(30, 20)
  rast_sizeA <- c(rast_cellsA[1]*bau_east_step, rast_cellsA[2]*bau_north_step)
  # Set size of grid (number of cells) for Site B (Target)
  rast_cellsB <- c(30, 20)
  rast_sizeB <- c(rast_cellsB[1]*bau_east_step, rast_cellsB[2]*bau_north_step)
  
  # Get coords of overall grid domain boundary
  xmin <- min(eastings)
  xmax <- max(eastings)
  ymin <- min(northings)
  ymax <- max(northings)
  
  # Set the limit for x and y coord so box is completely inside the domain
  rand.limA <- c(xmax - rast_sizeA[1], ymax - rast_sizeA[2])
  rand.limB <- c(xmax - rast_sizeB[1], ymax - rast_sizeB[2])
  
  # Create random coordinate index for top corner of subgrid within grid domain
  # Do this by generating a random number and finding the nearest eastings/northings value
  # Then use this index on x0 to get the coordinate
  xmin.randA <- eastings[which.min(abs(eastings - runif(1, min = xmin, max = rand.limA[1])))]
  ymin.randA <- northings[which.min(abs(northings - runif(1, min = ymin, max = rand.limA[2])))]
  
  xmax.randA <- eastings[which.min(abs(eastings - (xmin.randA + rast_sizeA[1])))]
  ymax.randA <- northings[which.min(abs(northings - (ymin.randA + rast_sizeA[2])))]
  
  xmin.randB <- eastings[which.min(abs(eastings - (runif(1, min = xmin, max = rand.limB[1]))))]
  ymin.randB <- northings[which.min(abs(northings - (runif(1, min = ymin, max = rand.limB[2]))))]
  
  xmax.randB <- eastings[which.min(abs(eastings - (xmin.randB + rast_sizeB[1])))]
  ymax.randB <- northings[which.min(abs(northings - (ymin.randB + rast_sizeB[2])))]
  
  
  rand.gridA <- rast(xmin = xmin.randA, 
                     xmax = xmax.randA, 
                     ymin = ymin.randA, 
                     ymax = ymax.randA, 
                     nrows = rast_cellsA[1], 
                     ncols = rast_cellsA[2],
                     vals = 1:rast_sizeA[2]) # Just setting values for plotting and for converting to a dataframe
  
  rand.gridB <- rast(xmin = xmin.randB, 
                     xmax = xmax.randB, 
                     ymin = ymin.randB, 
                     ymax = ymax.randB, 
                     nrows = rast_cellsB[1], 
                     ncols = rast_cellsB[2],
                     vals = 1:rast_sizeB[2]) # Just setting values for plotting and for converting to a dataframe
  
  
  plot(gridcov1.rast)
  lines(ext(rand.gridA), lwd = 2, col = "red")
  lines(ext(rand.gridB), lwd = 2, col = "blue")
  
  extrap_out <- c(SiteA.rast = rand.gridA,
                  SiteB.rast = rand.gridB)
  
  # Extract covariates for the random grid Site A  --------------------------
  
  rand.grid.df <- as.data.frame(rand.gridA, xy = T)[,c("x", "y")]
  
  cov1.SiteA <- terra::extract(gridcov1.rast, rand.grid.df, xy = T) %>% rename(cov1 = cov)
  cov2.SiteA <- terra::extract(gridcov2.rast, rand.grid.df, xy = T) %>% rename(cov2 = cov)
  
  # Join to create one dataframe
  covs.SiteA <- left_join(cov1.SiteA, cov2.SiteA, by = join_by(ID, x, y))
  
  
  # Extract covariates for the random grid Site B ---------------------------
  
  rand.grid.df <- as.data.frame(rand.gridB, xy = T)[,c("x", "y")]
  
  cov1.SiteB <- terra::extract(gridcov1.rast, rand.grid.df, xy = T) %>% rename(cov1 = cov)
  cov2.SiteB <- terra::extract(gridcov2.rast, rand.grid.df, xy = T) %>% rename(cov2 = cov)
  
  # Join to create one dataframe
  covs.SiteB <- left_join(cov1.SiteB, cov2.SiteB, by = join_by(ID, x, y))
  
  
  # Plotting location of data in cov space ----------------------------------
  
  # ggplot() + 
  #   geom_point(data = covs.SiteA, aes(x = cov1, y = cov2), color = "grey") +
  #   geom_point(data = covs.SiteB, aes(x = cov1, y = cov2), color = "purple4") +
  #   theme_bw()
  
  
  
  # Calculating Overlap of Environment - Whole Grid -------------------------
  
  # Shape approach ----------------------------------------------------------
  
  # Adding presence column due to extra_eval requirements
  # Trimming so just the covariates
  training <- covs.SiteA %>% 
    mutate(Presence = 1) %>% 
    .[,c("cov1", "cov2", "Presence")]
  
  projection <- covs.SiteB %>% 
    .[,c("cov1", "cov2")]
  
  shape_extrap <- extra_eval(training_data = training,
                             pr_ab = "Presence",
                             projection_data = projection,
                             metric = "mahalanobis",
                             univar_comb = F)
  
  shape_extrap <- cbind(shape_extrap, covs.SiteB[, c("x", "y")])
  
  # shape_extrap %>% 
  #   ggplot() + 
  #   geom_tile(aes(x = x, y = y, fill = extrapolation)) + 
  #   scale_fill_viridis() +
  #   coord_fixed() + 
  #   theme_bw() + 
  #   theme(axis.title.x = element_blank(),
  #         axis.title.y = element_blank(),
  #         legend.ticks = element_blank(),
  #         legend.title = element_blank()) +
  #   ggtitle('Extrapolation')
  
  summary.extrap = data.frame(mean = mean(shape_extrap$extrapolation),
                              median = median(shape_extrap$extrapolation),
                              min = min(shape_extrap$extrapolation),
                              max = max(shape_extrap$extrapolation))
  
  # Classify extrapolation type
  # If median extrap is less than or = to 50, low
  # If median extrap is not less than or = to 50, but is less than or = to 100, moderate
  # If median extrap is not less than or = to 100, high
  
  extrap.type <- ifelse(summary.extrap$median <= 10 , "Low", 
                        ifelse(summary.extrap$median <= 50, "Moderate", "High"))
  
  # Plotting data in covariate space with extrapolation  ------------------------
  
  extrap.plot <- ggplot() + 
    geom_point(data = covs.SiteA, aes(x = cov1, y = cov2), color = "grey") +
    geom_point(data = shape_extrap, aes(x = cov1, y = cov2, color = extrapolation)) +
    scale_color_viridis(option = "magma", direction = -1) +
    theme_bw() +
    theme(legend.ticks = element_blank())
  
  extrap_out <- c(extrap_out, list(rand.gridA = rand.gridA,
                                   rand.gridB = rand.gridB,
                                   covs.SiteA = covs.SiteA,
                                   covs.SiteB = covs.SiteB,
                                   extrap.plot = extrap.plot,
                                   extrap.df = shape_extrap,
                                   summary.extrap = summary.extrap))
  
  # If the output adds a required replicate to meet nreps, keep it
  if (length(extrap.reps.out[[extrap.type]]) < nreps) { 
    
    # Add extrap_out to the appropriate sublist in extrap.reps.out 
    extrap.reps.out[[extrap.type]] <- c(extrap.reps.out[[extrap.type]], list(extrap_out))
    
    
  }
  
  return(extrap.reps.out)
  
}


# Iterate over the function until you get to the desired nreps for low, moderate, high
while(length(extrap.reps.out$Low) < nreps | length(extrap.reps.out$Moderate) < nreps | length(extrap.reps.out$High) < nreps) {
  extrap.reps.out <- extrap_func()
}