
# extract the arguments provided in the command line
args <- commandArgs(trailingOnly = TRUE)
# The first argument is now the job index
job_index <- as.integer(args[1])

# First install NLMR
# remotes::install_github("ropensci/NLMR")
# install.packages("landscapetools")

# library(devtools)
# 
# devtools::install_github(repo = "Scott-Foster/RISDM", build_vignettes = T)

library(tidyverse)

packages <- c("sf", "terra", "ggpubr", "RISDM", "DescTools", "spatstat", "Metrics", "scoringutils")

walk(packages, require, character.only = T)

library(spatstat)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(viridis)
library(NLMR)
library(landscapetools)
library(RandomFields) # Note that RandomFields is no longer on CRAN. Downloaded archived file.
library(terra)
library(purrr)
library(Metrics)

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

names(cov1) <- "cov"

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

plot1 <- ggarrange(c1, c2)

ggsave(plot = plot1, filename = paste0(outpath, "/output/", job_index,"_Plot_Covariates_GRF.png"), w = 21.5, h = 18, units = "cm", dpi = 400, device = "png")


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

# # Get coords of original raster
coords <- xyFromCell(cov1, 1:ncell(cov1))

# Convert raster to matrix object
cov1.df <- as.data.frame(cov1, xy = T)
cov1.mat <- as.matrix(cov1.df)
colnames(cov1.df) <- c("x", "y", "cov")
colnames(cov1.mat) <- c("x", "y", "cov")

cov2.df <- as.data.frame(cov2, xy = T)
cov2.mat <- as.matrix(cov2.df)
colnames(cov2.df) <- c("x", "y", "cov")
colnames(cov2.mat) <- c("x", "y", "cov")

# Can do with one or two covariates

fe <- beta0 + beta1*cov1.mat[, "cov"] + beta2*cov2.mat[, "cov"]

mu <- cov1.df %>% mutate(cov = fe)
mu <- spatstat.geom::as.im(mu)

# Set seed 
set.seed(seed)

# Create LGCP with environmental covariate
lg.s <- rLGCP('exp', mu = mu,
              var=var, scale=scal)

#####################################################################
################# RUN EXTRAPOLATION #################################
#####################################################################

# Version 2a. Sample grid GRF covariates - XZ code -----------------

# Specify number of replicates per extrapolation type
nreps <- 1

extrap_out <- list()
extrap.reps.out <- list(Low = list())

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
  
  
  extrap_out <- c(SiteA.rast = rand.gridA,
                  SiteB.rast = rand.gridB)
  
  # Extract covariates for the random grid Site A  --------------------------
  
  rand.grid.df <- as.data.frame(rand.gridA, xy = T)[,c("x", "y")]
  
  cov1.SiteA <- terra::extract(cov1, rand.grid.df, xy = T) %>% rename(cov1 = cov)
  cov2.SiteA <- terra::extract(cov2, rand.grid.df, xy = T) %>% rename(cov2 = cov)
  
  # Join to create one dataframe
  covs.SiteA <- left_join(cov1.SiteA, cov2.SiteA, by = join_by(ID, x, y))
  
  
  # Extract covariates for the random grid Site B ---------------------------
  
  rand.grid.df <- as.data.frame(rand.gridB, xy = T)[,c("x", "y")]
  
  cov1.SiteB <- terra::extract(cov1, rand.grid.df, xy = T) %>% rename(cov1 = cov)
  cov2.SiteB <- terra::extract(cov2, rand.grid.df, xy = T) %>% rename(cov2 = cov)
  
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


# Iterate over the function until you get to the desired nreps 
while(length(extrap.reps.out$Low) < nreps) {
  extrap.reps.out <- extrap_func()
}

plot2 <- extrap.reps.out$Low[[1]]$extrap.plot

ggsave(plot = plot2, filename = paste0(outpath, "/output/", job_index,"_Plot_Extrap_Low.png"), w = 21.5, h = 18, units = "cm", dpi = 400, device = "png")

#############################################################################
##################### SAMPLING RANDOM GRID #################################
###########################################################################


spp_process <- cbind(x = lg.s$x, y = lg.s$y)

# po <- thinpp[, c("x", "y")]

# For random covariate case
po <- spp_process


PA.data <- map(extrap.reps.out, function(extrap.type) {
  
  map(extrap.type, function(rep) {
    
    rand.gridA <- rep$rand.gridA
    rand.gridB <- rep$rand.gridB
    
    
    #-------------------------------------------------------------------------------
    # Site A
    #-------------------------------------------------------------------------------
    
    # Get the domain of region a
    dom_a_bbox <- c(east_min = xmin(rand.gridA), east_max = xmax(rand.gridA), north_min = ymin(rand.gridA), north_max = ymax(rand.gridA))
    
    # Choose a grid size number of rows (for PA sampling)
    PA_a_res <- 30
    dom_a_res <- (dom_a_bbox["east_max"] - dom_a_bbox["east_min"]) / PA_a_res
    
    # Set centroids of PA sampling grids
    east_seq <- seq(dom_a_bbox["east_min"] + dom_a_res/2, 
                    dom_a_bbox["east_max"] - dom_a_res/2, 
                    by = dom_a_res)
    north_seq <- seq(dom_a_bbox["north_min"] + dom_a_res/2, 
                     dom_a_bbox["north_max"] - dom_a_res/2, 
                     by = dom_a_res)
    
    
    # Create a blank PA dataset at Site A (all zeros), located on grids cells defined by random grid domain and our PA sampling grid size
    grid_a <- expand.grid(east_seq, north_seq)
    pa_a <- cbind(grid_a, 0)
    colnames(pa_a) <- c("x", "y", "presence")
    pa_a <- terra::rast(pa_a)
    
    # find species coordinates from underlying LGCP that are in region a
    inbox_idx_a <- which(spp_process[, "x"] >= dom_a_bbox["east_min"] &
                           spp_process[, "x"] <= dom_a_bbox["east_max"] & 
                           spp_process[, "y"] >= dom_a_bbox["north_min"] &
                           spp_process[, "y"] <= dom_a_bbox["north_max"])
    
    
    po_a <- spp_process[inbox_idx_a, ]
    po_a_df <- as.data.frame(po_a)
    
    
    # ggplot() +
    #   geom_tile(data = bias.df, aes(x = x, y = y, fill = bias)) +
    #   scale_fill_viridis() +
    #   geom_point(data = po_a_df, aes(x = x, y = y), color = "white", alpha = 0.5)+
    #   theme_bw()
    
    # ## RANDOM SUBSET OF THESE * HAVE TO REMOVE THIS (JUST FOR REDUCING NUMBER OF PRESENCES)
    # # Thin points using the detection probability
    # # This was me trying to incorporate imperfect detection. So even if the species is present, it may not be detected
    # po_a_df$presence <- rbinom(nrow(po_a_df), 1, prob = 0.005)
    
    # # make it presence only data
    # po_a_df <- po_a_df[po_a_df$presence == 1,]
    
    # Now assuming perfect detection
    po_a_df$presence <- 1
    
    # Get cell indices of the species coordinates
    cell_idx <- terra::cellFromXY(pa_a, po_a_df[, c("x", "y")])
    
    # Fill in the raster with 1 from the cell indices
    pres_idx <- as.numeric(names(table(cell_idx)))
    pa_a[pres_idx] <- 1
    
    # # plot the data
    # plot(pa_a)
    
    
    #-------------------------------------------------------------------------------
    # Site B
    #-------------------------------------------------------------------------------
    
    # Get the domain of region a
    dom_b_bbox <- c(east_min = xmin(rand.gridB), east_max = xmax(rand.gridB), north_min = ymin(rand.gridB), north_max = ymax(rand.gridB))
    
    # Choose a grid size (for PA sampling)
    PA_b_res <- 30
    dom_b_res <- (dom_b_bbox["east_max"] - dom_b_bbox["east_min"]) / PA_b_res
    
    # Set centroids of PA sampling grids
    east_seq <- seq(dom_b_bbox["east_min"] + dom_b_res/2, 
                    dom_b_bbox["east_max"] - dom_b_res/2, 
                    by = dom_b_res)
    north_seq <- seq(dom_b_bbox["north_min"] + dom_b_res/2, 
                     dom_b_bbox["north_max"] - dom_b_res/2, 
                     by = dom_b_res)
    
    
    # Create a blank PA dataset at Site B (all zeros), located on grids cells defined by random grid domain and our PA sampling grid size
    grid_b <- expand.grid(east_seq, north_seq)
    pa_b <- cbind(grid_b, 0)
    colnames(pa_b) <- c("x", "y", "presence")
    pa_b <- terra::rast(pa_b)
    
    # find species coordinates from underlying LGCP that are in region a
    inbox_idx_b <- which(spp_process[, "x"] >= dom_b_bbox["east_min"] &
                           spp_process[, "x"] <= dom_b_bbox["east_max"] & 
                           spp_process[, "y"] >= dom_b_bbox["north_min"] &
                           spp_process[, "y"] <= dom_b_bbox["north_max"])
    
    
    po_b <- spp_process[inbox_idx_b, ]
    po_b_df <- as.data.frame(po_b)
    
    
    # ggplot() +
    #   geom_tile(data = bias.df, aes(x = x, y = y, fill = bias)) +
    #   scale_fill_viridis() +
    #   geom_point(data = po_b_df, aes(x = x, y = y), color = "white", alpha = 0.5)+
    #   theme_bw()
    
    # ## RANDOM SUBSET OF THESE
    # # Thin points using the detection probability
    # # This was me trying to incorporate imperfect detection. So even if the species is present, it may not be detected
    # po_b_df$presence <- rbinom(nrow(po_b_df), 1, prob = 0.005)
    # # 
    # # # make it presence only data
    # po_b_df <- po_b_df[po_b_df$presence == 1,]
    
    # Now assuming perfect detection
    po_b_df$presence <- 1
    
    # Get cell indices of the species coordinates
    cell_idx <- terra::cellFromXY(pa_b, po_b_df[, c("x", "y")])
    
    # Fill in the raster with 1 from the cell indices
    pres_idx <- as.numeric(names(table(cell_idx)))
    pa_b[pres_idx] <- 1
    
    # # plot the data
    # plot(pa_b)
    
    # pa - region a
    pa_a_df <- as.data.frame(pa_a, xy = TRUE) %>% 
      mutate(area = dom_a_res^2)
    
    # pa - region b
    pa_b_df <- as.data.frame(pa_b, xy = TRUE) %>% 
      mutate(area = dom_b_res^2)
    
    
    
    #-------------------------------------------------------------------------------
    # Plot the site with all data types
    #-------------------------------------------------------------------------------
    
    PA_plot <- ggplot() +
      # geom_tile(data = bias.df, aes(x = x, y = y), fill = "white") +
      scale_fill_viridis() +
      geom_point(data = po, aes(x = x, y = y), color = "black", alpha = 0.1) +
      geom_point(data = pa_a_df, aes(x = x, y = y, color = as.factor(presence)), size = 1.8) +
      geom_point(data = pa_b_df, aes(x = x, y = y, color = as.factor(presence)), size = 1.8) +
      labs(color =  "Presence / Absence") +
      theme_bw() +
      theme(legend.ticks = element_blank()) +
      scale_color_manual(values = c("purple4", "green3"))
    
    
    return(list(PA_plot = PA_plot, 
                pa_a_df = pa_a_df, 
                pa_b_df = pa_b_df))
    
    
    
    
  })
  
})

# Add new PA grids to list
extrap.reps.out.PA <- map2(extrap.reps.out, PA.data, ~map2(.x, .y, c))

plot3 <- extrap.reps.out.PA$Low[[1]]$PA_plot

ggsave(plot = plot3, filename = paste0(outpath, "/output/", job_index,"_Plot_Data_Low.png"), w = 21.5, h = 18, units = "cm", dpi = 400, device = "png")

#########################################################################
########################### RUN MODELS ####################################
#########################################################################


######## PO model random covariate ----------------------------------------

spp_process <- cbind(x = lg.s$x, y = lg.s$y)
colnames(spp_process) <- c("x", "y")

# Load PO occurrence data ----------------------------------------------------

PO <- spp_process

# Load enviro. covs -------------------------------------------------------

# Covariates add together
# cov <- c(rast(rand.cov1, type = "xyz"),
#          rast(rand.cov2,  type = "xyz"))

cov <- c(cov1, cov2)

names(cov) <- c("cov1", "cov2")

# Mesh default ------------------------------------------------------------

mesh.default <- makeMesh(cov,
                         max.n = c(5000, 2500), # Default c(500,200)
                         dep.range = NULL, # In raster projection units, default is 1/3 diagonal length of raster extent
                         expans.mult = 1.5, # Default, 1.5 x dep.range
                         max.edge = NULL, # Default c(0.2, 0.5)*dep.range
                         cutoff = NULL, # Default 0.2*max.edge1
                         offset = NULL, # Default is dep.range
                         doPlot = TRUE
)


# Model specification -----------------------------------------------------

# Priors
my.control <- list(coord.names = c("x", "y"),
                   prior.mean = 0,
                   int.sd = 1000, # Intercept standard deviation
                   other.sd = 10, # Covariate effect standard deviation
                   prior.range = c(1, 0.1), # Prior chance 10% that parameter falls below range of 1km
                   prior.space.sigma = c(5, 0.1)) # Prior chance 10% that parameter falls above SD of 5


# Load PA data and run models for each extrap type and replicate

mods.reps <- map(extrap.reps.out.PA, function(extrap.type) {
  
  map(extrap.type, function(rep) {
    
    # Load PA occurrence data -------------------------------------------------
    
    PA_fit <- rep$pa_a_df
    
    PA_val <- rep$pa_b_df
    
    # Integrated Model Fitting
    
    m.int <- isdm(observationList = list(POdat = PO,
                                         PAdat = PA_fit),
                  covars = cov,
                  mesh = mesh.default,
                  responseNames = c(PO = NULL, PA = "presence"),
                  sampleAreaNames = c(PO = NULL, PA = "area"),
                  distributionFormula = ~0 + cov1 + cov2, # Linear w one cov
                  biasFormula = ~1, #Intercept only
                  artefactFormulas = list(PA = ~1), # Intercept only
                  control = my.control)
    
    # Presence-Only Model Fitting
    
    m.PO <- isdm(observationList = list(POdat = PO), 
                 covars = cov,
                 mesh = mesh.default,
                 responseNames = NULL,
                 sampleAreaNames = NULL,
                 distributionFormula = ~0 + cov1 + cov2, # Linear w one cov
                 biasFormula = ~1, #Intercept only
                 artefactFormulas = NULL,
                 control = my.control)
    
    # Presence-Absence Model Fitting
    
    m.PA <- list()
    
    # Create a data frame for this replicate
    list(models = tibble(
      Mod.type = c("Integrated", "PO", "PA"),
      Model = list(m.int, m.PO, m.PA),
      Summary = list(summary(m.int), summary(m.PO), NULL) # Update PA summary when available
      
    ))
    
  })
  
  
})



# Add models to list of extrap type replicates -----------------------------

extrap.reps.out.mods <- map2(extrap.reps.out.PA, mods.reps, ~map2(.x, .y, c))

rm(extrap.reps.out.PA)
rm(mods.reps)


#########################################################################
###################### EXTRACT MODEL RESULTS ############################
#########################################################################

# Extract and save summary results ----------------------------------------

# Make a dataframe of the format I want to save the results in
extrap.scenario.df <- data.frame(extrap.type = character(),
                                 rep = numeric(),
                                 job.id = numeric(),
                                 mod.type = character(),
                                 beta1.mean = numeric(),
                                 beta2.mean = numeric(),
                                 beta1.median = numeric(),
                                 beta2.median = numeric(),
                                 beta1_25 = numeric(),
                                 beta1_975 = numeric(),
                                 beta2_25 = numeric(),
                                 beta2_975 = numeric())

# Get the names of the extrap types for indexing
extrap_names <- names(extrap.reps.out.mods)

# For every level of extrap type (Low, Mod, High)
for(extrap.type in seq_along(extrap.reps.out.mods)) {
  
  # Extract the name ("Low") for indexing from the names list
  name <- extrap_names[extrap.type] 
  
  # For every replicate
  for(rep in seq_along(extrap.reps.out.mods[[name]])) {
    
    # Extract the models dataframe [[name]] double brackets for list extract
    models_df <- extrap.reps.out.mods[[name]][[rep]]$models
    
    for (i in 1:2) { # NEED TO ADD BACK IN nrow(models_df) ONCE HAVE PA WORKING
      
      mod.summary <- models_df[[i, "Summary"]]
      
      # Had to add the [[1]] here because the summary is always list of length 1
      extrap.scenario.df <<- extrap.scenario.df %>% 
        add_row(extrap.type = name,
                rep = rep,
                job.id = job_index,
                mod.type = as.character(models_df[i, "Mod.type"]),
                beta1.mean = mod.summary[[1]]$DISTRIBUTION$mean[1],
                beta2.mean = mod.summary[[1]]$DISTRIBUTION$mean[2],
                beta1.median = mod.summary[[1]]$DISTRIBUTION[[4]][1],
                beta2.median = mod.summary[[1]]$DISTRIBUTION[[4]][2],
                beta1_25 = mod.summary[[1]]$DISTRIBUTION[[3]][1],
                beta1_975 = mod.summary[[1]]$DISTRIBUTION[[5]][1],
                beta2_25 = mod.summary[[1]]$DISTRIBUTION[[3]][2],
                beta2_975 = mod.summary[[1]]$DISTRIBUTION[[5]][2])
      
    }
    
  }}

write_csv(extrap.scenario.df, paste0(outpath, "/output/Summary_Extrap_Low_PO_INT.no.bias.2.GRF.cov_REP_", job_index,".csv"))

#########################################################################
###################### MAKE TRUTH GRID ###############################
#########################################################################


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

crs(true_log_int.rast) <- crs(cov)

# Extract cell size because RISDM predictions are with reference to cell area
log.cell_size <- log(cellSize(cov))

# Add intensity + log(cell area)
true_log_int.rast <- true_log_int.rast+log.cell_size


#########################################################################
###################### PREDICT FROM FITTED ###############################
#########################################################################

# Get the names of the extrap types for indexing
extrap_names <- names(extrap.reps.out.mods)

# For every level of extrap type (Low, Mod, High)
for(extrap.type in seq_along(extrap.reps.out.mods)) {
  
  # Extract the name ("Low") for indexing from the names list
  name <- extrap_names[extrap.type] 
  
  # For every replicate
  for(rep in seq_along(extrap.reps.out.mods[[name]])) {
    
    # Extract the models dataframe [[name]] double brackets for list extract
    models_df <- extrap.reps.out.mods[[name]][[rep]]$models
    
    for (i in 1:2) { # NEED TO ADD BACK IN nrow(models_df) ONCE HAVE PA WORKING
      
      mod <- models_df[[i, "Model"]]
      
      # Had to add the [[1]] here because the summary is always list of length 1
      mod[[1]]$preds <- predict(mod[[1]],
                                covars = cov,
                                S = 1, 
                                intercept.terms = "PO_Intercept",
                                type = "link")
      
      # Save the updated model back to the dataframe
      models_df[[i, "Model"]] <- mod
      
    }
    
    # Save the updated models dataframe back to the original list
    extrap.reps.out.mods[[name]][[rep]]$models <- models_df
    
    
  }
  
}


########################################################################
###################### VALIDATION TRUE INTENSITY #######################
########################################################################


# Make a dataframe of the format I want to save the results in
true.validation.df <- data.frame(extrap.type = character(),
                                 rep = numeric(),
                                 job.id = numeric(),
                                 mod.type = character(),
                                 correlation = numeric(),
                                 MAE = numeric(),
                                 RMSE = numeric(),
                                 Sum.Int.Score = numeric(),
                                 Mean.Int.Score = numeric())


# Get the names of the extrap types for indexing
extrap_names <- names(extrap.reps.out.mods)

# For every level of extrap type (Low, Mod, High)
for(extrap.type in seq_along(extrap.reps.out.mods)) {
  
  # Extract the name ("Low") for indexing from the names list
  name <- extrap_names[extrap.type] 
  
  # For every replicate
  for(rep in seq_along(extrap.reps.out.mods[[name]])) {
    
    # Extract the models dataframe [[name]] double brackets for list extract
    models_df <- extrap.reps.out.mods[[name]][[rep]]$models
    
    for (i in 1:2) { # NEED TO ADD BACK IN nrow(models_df) ONCE HAVE PA WORKING
      
      mod <- models_df[[i, "Model"]]
      
      # Pull out the mean intensity prediction for each cell
      mean.int.pred <- mod[[1]]$preds$field$Mean
      
      # Pull out the lower and upper bounds of the prediction
      lower.int.pred <- mod[[1]]$preds$field$Lower
      
      upper.int.pred <- mod[[1]]$preds$field$Upper
      
      # Metrics from Simmonds et al. 
      # Compare the predicted intensity to the true intensity 
      cor <- cor(as.vector(mean.int.pred), as.vector(true_log_int.rast))
      
      MAE <- mean(abs(as.vector(mean.int.pred - true_log_int.rast)))
      
      RMSE <- Metrics::rmse(actual = as.vector(true_log_int.rast), 
                            predicted = as.vector(mean.int.pred))
      
      ### Calculating the Interval Score ###
      
      interval_score <- interval_score(true_values = as.vector(true_log_int.rast),
                                       lower = as.vector(lower.int.pred), 
                                       upper = as.vector(upper.int.pred),
                                       interval_range = 95,
                                       weigh = TRUE)
      
      Sum.Int.Score <- sum(interval_score)
      
      Mean.Int.Score <- mean(interval_score)
      
      # Save results to dataframe
      true.validation.df <<- true.validation.df %>% 
        add_row(extrap.type = name,
                rep = rep,
                job.id = job_index,
                mod.type = as.character(models_df[i, "Mod.type"]),
                correlation = cor,
                MAE = MAE,
                RMSE = RMSE,
                Sum.Int.Score = Sum.Int.Score,
                Mean.Int.Score = Mean.Int.Score)
      
      
    }
    
  }
  
}

write_csv(true.validation.df, paste0(outpath, "/output/Validation_Extrap_Low_PO_INT.no.bias.2.GRF.cov_REP_", job_index,".csv"))


