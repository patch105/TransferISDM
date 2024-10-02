
library(ggplot2)
library(dplyr)
library(terra)
library(purrr)
#library(RISDM,lib.loc=lib_loc)
library(RISDM)
library(flexsdm)


# Number of cores ---------------------------------------------------------

n_cores <- 3

# PARAMETERS --------------------------------------------------------------

# Set the seed for all
# seed <- 24
# set.seed(50)

# DOMAIN SETUP ------------------------------------------------------------

# START with set up of resolution and north/east step length for later Site A and B grid creation.

# Set ncol
ncol <- 1000
nrow <- 1000
res <- 1

# Create a bounded domain on [0, 10] x [0, 10]

east_min <- 0
east_max <- 1000
north_min <- 0
north_max <- 1000


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


# Run setup for replicates ------------------------------------------------

variance <- 0.5 # Variance of the Gaussian field at distance zero (changed  from 0.5)

# Size of the two Reference and Target sites
rast_cellsA <- c(50, 50)
rast_cellsB <- c(50,50)

# Number of replicates per range
nreps <- 10

# 1. Simulate Covariates -------------------------------------------------

# Set autocorrelation range to test Cov1
scal.list <- list(10, 25, 50, 75, 100, 125, 150)  # Maximum range (raster units) of spatial autocorrelation


landscape.rast <- terra::rast(xmin = east_min, 
                              xmax = east_max, 
                              ymin = north_min,  
                              ymax = north_max, 
                              nrows = nrow,
                              ncols = ncol,
                              vals = 1:1000)

crs(landscape.rast) <- "epsg:3857" # Setting to WGS 84 / Pseudo-Mercator projection for later functions requiring cell size

xSeq <- terra::xFromCol(landscape.rast)
ySeq <- terra::yFromRow(landscape.rast)




cov.list <- list()

# Run over all range values and for each rep, simulate a covariate
cov.list <- imap(scal.list, function(scal, i) {
  
  temp.list <- list()
  
  for(rep in 1:nreps) {
    
    temp.list[[rep]] <- RISDM:::fftGPsim2( x=xSeq, y=ySeq, sig2 = 1 , rho = scal, nu = 1/2) %>% rast()
  }
  
  return(temp.list)
  
})

## This function creates two grids and calulcates the extrapolation metric (mean, median) between them

# For every range level in cov.list
extrap.reps.out <- imap(cov.list ,  function(cov.reps, i) {
  
  range_cov <- scal.list[i]
  
  # For every replicate in the range level
  map(cov.reps, function(cov1) {
    
    names(cov1) <- "cov"
    
    crs(cov1) <- "epsg:3857"
    
    # Get coords of original raster
    coords <- xyFromCell(cov1, 1:ncell(cov1))
    
    # Convert raster to matrix object
    cov1.df <- as.data.frame(cov1, xy = T)
    cov1.mat <- as.matrix(cov1.df)
    colnames(cov1.df) <- c("x", "y", "cov")
    colnames(cov1.mat) <- c("x", "y", "cov")
    
    # Set size of grid (number of cells) for Site A (Reference)
    # NOTE - must be smaller than total cell number in x y directions
    rast_sizeA <- c(rast_cellsA[1]*bau_east_step, rast_cellsA[2]*bau_north_step)
    # Set size of grid (number of cells) for Site B (Target)
    rast_sizeB <- c(rast_cellsB[1]*bau_east_step, rast_cellsB[2]*bau_north_step)
    
    # Get coords of overall grid domain boundary
    xmin <- min(eastings)
    xmax <- max(eastings)
    ymin <- min(northings)
    ymax <- max(northings)
    
    ##### GRID A ###### 
    # Create a grid A that is in the centre left of the domain
    # Then use this index on x0 to get the coordinate
    # Had to take 0.01 off at the end to make it 50 cells
    
    xmin.randA <- eastings[which.min(abs(eastings - (xmin + 4*(rast_sizeA[1]))))] 
    xmax.randA <- xmin.randA + rast_sizeA[1]
    
    xmin.randA <- xmin.randA - bau_east_step/2
    xmax.randA <- xmax.randA + bau_east_step/2 - bau_east_step
    
    ymin.randA <- northings[which.min(abs(northings - ((ymax - ymin)/2 - (rast_sizeA[2]/2))))]
    ymax.randA <- ymin.randA + rast_sizeA[2]
    
    ymin.randA <- ymin.randA - bau_north_step/2
    ymax.randA <- ymax.randA + bau_north_step/2 - bau_east_step
    
    #### GRID B ########
    # Create a grid B which is in the centre right of the domain
    
    xmin.randB <<- eastings[which.min(abs(eastings - (xmax - (rast_sizeB[1]*2))))]
    xmax.randB <<- xmin.randB + rast_sizeB[1]
    
    xmin.randB <<- xmin.randB - bau_east_step/2
    xmax.randB <<- xmax.randB + bau_east_step/2 - bau_east_step
    
    ymin.randB <<- northings[which.min(abs(northings - ((ymax - ymin)/2 - (rast_sizeB[2]/2))))]
    ymax.randB <<- ymin.randB + rast_sizeB[2]
    
    ymin.randB <<- ymin.randB - bau_north_step/2
    ymax.randB <<- ymax.randB + bau_north_step/2 - bau_east_step
    
    
    
    ### Make the grids into rasters
    rand.gridA <- rast(xmin = xmin.randA, 
                       xmax = xmax.randA, 
                       ymin = ymin.randA, 
                       ymax = ymax.randA, 
                       nrows = rast_cellsA[1], 
                       ncols = rast_cellsA[2],
                       vals = 1:rast_cellsA[2]) # Just setting values for plotting and for converting to a dataframe
    
    
    
    crs(rand.gridA) <- "epsg:3857"
    
    
    rand.gridB <- rast(xmin = xmin.randB, 
                       xmax = xmax.randB, 
                       ymin = ymin.randB, 
                       ymax = ymax.randB, 
                       nrows = rast_cellsB[1], 
                       ncols = rast_cellsB[2],
                       vals = 1:rast_cellsB[2]) # Just setting values for plotting and for converting to a dataframe
    
    
    
    crs(rand.gridB) <- "epsg:3857"
    
    # Extract covariates for the random grid Site A  --------------------------
    
    rand.grid.df <- as.data.frame(rand.gridA, xy = T)[,c("x", "y")]
    
    covs.SiteA <- terra::extract(cov1, rand.grid.df, xy = T) %>% rename(cov1 = cov)
    
    covs.SiteA.rast <- crop(cov1, ext(rand.gridA))
    
    # Extract covariates for the random grid Site B ---------------------------
    
    rand.grid.df <- as.data.frame(rand.gridB, xy = T)[,c("x", "y")]
    
    covs.SiteB <- terra::extract(cov1, rand.grid.df, xy = T) %>% rename(cov1 = cov)
    
    covs.SiteB.rast <- crop(cov1, ext(rand.gridB))
    
    # Calculating Overlap of Environment - Whole Grid -------------------------
    
    # Shape approach ----------------------------------------------------------
    
    # Adding presence column due to extra_eval requirements
    # Trimming so just the covariates
    training <- covs.SiteA %>% 
      mutate(Presence = 1) %>% 
      .[,c("cov1", "Presence")]
    
    projection <- covs.SiteB[, "cov1", drop = F]
    
    
    
    shape_extrap <- flexsdm::extra_eval(training_data = training,
                                        pr_ab = "Presence",
                                        projection_data = projection,
                                        metric = "mahalanobis",
                                        univar_comb = F,
                                        n_cores = n_cores)
    
    shape_extrap <- cbind(shape_extrap, covs.SiteB[, c("x", "y")])
    
    summary.extrap = data.frame(mean = mean(shape_extrap$extrapolation),
                                median = median(shape_extrap$extrapolation),
                                min = min(shape_extrap$extrapolation),
                                max = max(shape_extrap$extrapolation),
                                range_cov_val = range_cov)
    
    # extrap.plot <- ggplot() + 
    #   geom_point(data = covs.SiteA, aes(x = cov1), color = "grey") +
    #   geom_point(data = shape_extrap, aes(x = cov1, y = cov2, color = extrapolation)) +
    #   scale_color_viridis(option = "magma", direction = -1) +
    #   theme_bw() +
    #   theme(legend.ticks = element_blank())
    
    extrap.reps.out <-list(rand.gridA = rand.gridA,
                           rand.gridB = rand.gridB,
                           covs.SiteA = covs.SiteA,
                           covs.SiteB = covs.SiteB,
                           covs.SiteA.rast = covs.SiteA.rast,
                           covs.SiteB.rast = covs.SiteB.rast,
                           extrap.df = shape_extrap,
                           summary.extrap = summary.extrap)
    
    return(list(extrap.reps.out = extrap.reps.out))
    
    
    
  }) 
})
  

# Set up saving dataframe
merged.df <- data.frame(range_cov = unlist(map(scal.list, function(x) rep(x, each = nreps))),
                 median = rep(NA, length(scal.list)*nreps)) 
  

# Save the median extrapolation score per range value into one dataframe
for(i in seq_along(scal.list)) {
  
  merged <- do.call(rbind, lapply(extrap.reps.out[[i]], function(x) x$extrap.reps.out$summary.extrap[, 1:5]))
  
  indx1 <- nreps*i
  indx2 <- indx1 - (nreps - 1)
  
  merged.df$median[indx2:indx1] <- merged$median
  
}

write.csv(merged.df, "range_comparison_v_2_df.csv")

# Plot the range vs. the median extrapolation score
ggplot() +
  geom_point(data = merged.df, aes(x = range_cov, y = median)) +
  theme_bw()










