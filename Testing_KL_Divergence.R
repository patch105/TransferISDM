########################
# TWO COVARIATE VERSION ---------------------------------------------------
#######################

library(ggplot2)
library(dplyr)
library(terra)
library(purrr)
#library(RISDM,lib.loc=lib_loc)
library(RISDM)
library(flexsdm)
library(ggpubr)
library(viridis)
library(tidyterra)
library(sf)
library(fmesher)
library(INLA)
library(geoR)
library(bayesmeta)
library(fpc)


# Number of cores ---------------------------------------------------------

n_cores <- 3

# PARAMETERS --------------------------------------------------------------

# Set the seed for all
# seed <- 24
# set.seed(50)

# DOMAIN SETUP ------------------------------------------------------------

# START with set up of resolution and north/east step length for later Site A and B grid creation.

# Set ncol
ncol <- 300
nrow <- 300
res <- 1

# Create a bounded domain on [0, 10] x [0, 10]

east_min <- 0
east_max <- 300
north_min <- 0
north_max <- 300

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
nreps <- 3


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

range_cov1 <- 10
range_cov2 <- 100

cov1 <- RISDM:::fftGPsim2( x=xSeq, y=ySeq, sig2 = variance , rho = range_cov1, nu = 1/2) %>% rast()
cov2 <- RISDM:::fftGPsim2( x=xSeq, y=ySeq, sig2 = variance , rho = range_cov2, nu = 1/2) %>% rast()

#plot(cov1)

names(cov1) <- "cov"
names(cov2) <- "cov"

crs(cov1) <- "epsg:3857"
crs(cov2) <- "epsg:3857"

covs <- c(cov1, cov2)
names(covs) <- c("cov1", "cov2")

# Get coords of original raster
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

# Set the limit for x and y coord so box is completely inside the domain
rand.limA <- c(xmax - rast_sizeA[1], ymax - rast_sizeA[2])
rand.limB <- c(xmax - rast_sizeB[1], ymax - rast_sizeB[2])

##### GRID A ###### 
# Create random coordinate index for (bottom left?) corner of subgrid within grid domain
# Do this by generating a random number and finding the nearest eastings/northings value
# Then use this index on x0 to get the coordinate
# Had to take 0.01 off at the end to make it 50 cells

xmin.randA <- eastings[which.min(abs(eastings - runif(1, min = round(xmin,2), max = round(rand.limA[1],2))))] 
xmax.randA <- xmin.randA + rast_sizeA[1]

xmin.randA <- xmin.randA - bau_east_step/2
xmax.randA <- xmax.randA + bau_east_step/2 - bau_east_step

ymin.randA <- northings[which.min(abs(northings - runif(1, min = round(ymin,2), max = round(rand.limA[2],2))))]
ymax.randA <- ymin.randA + rast_sizeA[2]

ymin.randA <- ymin.randA - bau_north_step/2
ymax.randA <- ymax.randA + bau_north_step/2 - bau_east_step

#### GRID B ########

Generate_Grid_B <- function() {
  
  xmin.randB <<- eastings[which.min(abs(eastings - (runif(1, min = round(xmin,2), max = round(rand.limB[1],2)))))]
  xmax.randB <<- xmin.randB + rast_sizeB[1]
  
  xmin.randB <<- xmin.randB - bau_east_step/2
  xmax.randB <<- xmax.randB + bau_east_step/2 - bau_east_step
  
  ymin.randB <<- northings[which.min(abs(northings - (runif(1, min = round(ymin,2), max = round(rand.limB[2],2)))))]
  ymax.randB <<- ymin.randB + rast_sizeB[2]
  
  ymin.randB <<- ymin.randB - bau_north_step/2
  ymax.randB <<- ymax.randB + bau_north_step/2 - bau_east_step
  
  
}

# Run function to generate GRID B
Generate_Grid_B()

# Must be checked to see if overlaps with GRID A  
# If overlap occurs, run the generate function again
while(xmin.randB < xmax.randA & xmax.randB > xmin.randA & ymin.randB < ymax.randA & ymax.randB > ymin.randA) {
  
  Generate_Grid_B()
  
} 

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

# Extract the extents
extA <- ext(rand.gridA)
extB <- ext(rand.gridB)

df_extA <- data.frame(
  xmin = extA[1],
  xmax = extA[2],
  ymin = extA[3],
  ymax = extA[4],
  color = "grey",
  label = "Reference Site",
  label_x = (extA[1] + extA[2]) / 2,  # Center of the extent for text placement
  label_y = extA[4] + (extA[4] - extA[3]) * 0.1  # Slightly above the top of the extent
)

df_extB <- data.frame(
  xmin = extB[1],
  xmax = extB[2],
  ymin = extB[3],
  ymax = extB[4],
  color = "purple",
  label = "Target Site",
  label_x = (extB[1] + extB[2]) / 2,  # Center of the extent for text placement
  label_y = extB[4] + (extB[4] - extB[3]) * 0.1  # Slightly above the top of the extent
)

# Combine the extents into one data frame
df_extents <- rbind(df_extA, df_extB)

# Create ggplot
C <- cov1 %>% 
  as.data.frame(xy = TRUE) %>% 
  ggplot() + 
  geom_tile(aes(x = x, y = y, fill = cov)) + 
  scale_fill_viridis(guide = guide_colorbar(barwidth = 0.5)) +
  coord_fixed() + 
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  geom_rect(data = df_extents, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, color = color), 
            fill = NA, linetype = "solid", linewidth = 1) +
  scale_color_identity() +
  geom_text(data = df_extents, aes(x = label_x, y = label_y, label = label), 
            color = "black", size = 5, vjust = 0) +
  ggtitle('Covariate 1')

# Create ggplot
D <- cov2 %>% 
  as.data.frame(xy = TRUE) %>% 
  ggplot() + 
  geom_tile(aes(x = x, y = y, fill = cov)) + 
  scale_fill_viridis(guide = guide_colorbar(barwidth = 0.5)) +
  coord_fixed() + 
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  geom_rect(data = df_extents, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, color = color), 
            fill = NA, linetype = "solid", linewidth = 1) +
  scale_color_identity() +
  geom_text(data = df_extents, aes(x = label_x, y = label_y, label = label), 
            color = "black", size = 5, vjust = 0) +
  ggtitle('Covariate 2')

ggarrange(C, D, ncol = 2, nrow = 1)



# Extract covariates for the random grid Site A  --------------------------

rand.grid.df <- as.data.frame(rand.gridA, xy = T)[,c("x", "y")]

cov1.SiteA <- terra::extract(cov1, rand.grid.df, xy = T) %>% rename(cov1 = cov)
cov2.SiteA <- terra::extract(cov2, rand.grid.df, xy = T) %>% rename(cov2 = cov)

# Join to create one dataframe
covs.SiteA <- left_join(cov1.SiteA, cov2.SiteA, by = join_by(ID, x, y))

covs.SiteA.rast <- crop(covs, ext(rand.gridA))

# Extract covariates for the random grid Site B ---------------------------

rand.grid.df <- as.data.frame(rand.gridB, xy = T)[,c("x", "y")]

cov1.SiteB <- terra::extract(cov1, rand.grid.df, xy = T) %>% rename(cov1 = cov)
cov2.SiteB <- terra::extract(cov2, rand.grid.df, xy = T) %>% rename(cov2 = cov)

# Join to create one dataframe
covs.SiteB <- left_join(cov1.SiteB, cov2.SiteB, by = join_by(ID, x, y))

covs.SiteB.rast <- crop(covs, ext(rand.gridB))


# Calculate the Kullback-Leibler Divergence -------------------------------

cov1.SiteA <- covs.SiteA %>% 
  .[,c("cov1", "x","y")]

cov2.SiteA <- covs.SiteA %>% 
  .[,c("cov2", "x","y")]

cov1.SiteB <- covs.SiteB %>% 
  .[,c("cov1", "x","y")]

cov2.SiteB <- covs.SiteB %>% 
  .[,c("cov1", "x","y")]


KL_divergence_function <- function(cov.SiteA, cov.SiteB) {
  
  ## SITE A.
  
  cov.SiteA.sf <- cov.SiteA %>% 
    st_as_sf(coords = c("x", "y"), crs = st_crs(3857))
  
  
  max.edge <- 1
  mesh <- fm_mesh_2d(loc = cov.SiteA.sf,
                     offset = c(0.5, 1.5),
                     max.edge = c(max.edge, max.edge*3),
                     cutoff = 0.2*max.edge)
  
  #plot(mesh)
  
  
  A.cov.SiteA <- inla.spde.make.A(mesh = mesh, loc = cov.SiteA.sf)
  
  cov.SiteA.spde <- inla.spde2.pcmatern(mesh = mesh,
                                        alpha= 2,
                                        prior.range = c(1, 0.01),  ## P(range < 1) = 0.01
                                        prior.sigma = c(1, 0.5)) ## P(sigma > 1) = 0.5
  
  s.index <- inla.spde.make.index(name = "spatial.field",
                                  n.spde = cov.SiteA.spde$n.spde)
  
  stk <- inla.stack(
    data = list(y = cov.SiteA$cov),
    A = list(A.cov.SiteA),
    effects = list(c(s.index, list(Intercept = 1))),
    tag = "estimation"
    
  )
  
  
  formula <- y ~ f(spatial.field, model = cov.SiteA.spde)
  
  mod <- inla(formula,
              family = "gaussian",
              data = inla.stack.data(stk),
              control.compute = list(return.marginals.predictor = T),
              control.predictor = list(compute = T, A = inla.stack.A(stk)))
  
  
  summary(mod)
  
  # Extract mean, range and std. dev
  cov.SiteA.mean <- mod$summary.fixed$mean
  cov.SiteA.mean <- mod$summary.random$spatial.field$mean
  
  range <- mod$summary.hyperpar["Range for spatial.field", "mean"]
  std.dev <- mod$summary.hyperpar["Stdev for spatial.field", "mean"]
  var <- std.dev^2
  
  # Get pairwise distance of Site A
  cov.SiteA.vect <- vect(cov.SiteA.sf)
  dist <- terra::distance(cov.SiteA.vect)
  dist.mat <- as.matrix(dist)
  
  
  cov.mat.cov.SiteA <- geoR::cov.spatial(dist.mat,
                                         cov.model = "matern",
                                         cov.pars = c(var, range), 
                                         kappa = 1/2)
  
  cov.SiteA.mean <- rep(cov.SiteA.mean, length = nrow(cov.mat.cov.SiteA))
  
  ## SITE B.
  
  cov.SiteB.sf <- cov.SiteB %>% 
    st_as_sf(coords = c("x", "y"), crs = st_crs(3857))
  
  max.edge <- 1
  mesh <- fm_mesh_2d(loc = cov.SiteB.sf,
                     offset = c(0.5, 1.5),
                     max.edge = c(max.edge, max.edge*3),
                     cutoff = 0.2*max.edge)
  
  #plot(mesh)
  
  
  A.cov.SiteB <- inla.spde.make.A(mesh = mesh, loc = cov.SiteB.sf)
  
  cov.SiteB.spde <- inla.spde2.pcmatern(mesh = mesh,
                                        alpha= 2,
                                        prior.range = c(1, 0.01),  ## P(range < 1) = 0.01
                                        prior.sigma = c(1, 0.5)) ## P(sigma > 1) = 0.5
  
  s.index <- inla.spde.make.index(name = "spatial.field",
                                  n.spde = cov.SiteB.spde$n.spde)
  
  stk <- inla.stack(
    data = list(y = cov.SiteB$cov1),
    A = list(A.cov.SiteB),
    effects = list(c(s.index, list(Intercept = 1))),
    tag = "estimation"
    
  )
  
  
  formula <- y ~ f(spatial.field, model = cov.SiteB.spde)
  
  mod <- inla(formula,
              family = "gaussian",
              data = inla.stack.data(stk),
              control.compute = list(return.marginals.predictor = T),
              control.predictor = list(compute = T, A = inla.stack.A(stk)))
  
  
  summary(mod)
  
  # Extract mean, range and std. dev
  cov.SiteB.mean <- mod$summary.fixed$mean
  cov.SiteB.mean <- mod$summary.random$spatial.field$mean
  range <- mod$summary.hyperpar["Range for spatial.field", "mean"]
  std.dev <- mod$summary.hyperpar["Stdev for spatial.field", "mean"]
  var <- std.dev^2
  
  # Get pairwise distance of Site A
  cov.SiteB.vect <- vect(cov.SiteB.sf)
  dist <- terra::distance(cov.SiteB.vect)
  dist.mat <- as.matrix(dist)
  
  cov.mat.cov.SiteB <- geoR::cov.spatial(dist.mat,
                                         cov.model = "matern",
                                         cov.pars = c(var, range), 
                                         kappa = 1/2)
  cov.SiteB.mean <- rep(cov.SiteB.mean, length = nrow(cov.mat.cov.SiteB))
  
  
  KL <- kldiv(mu1 = cov.SiteB.mean,
              mu2 = cov.SiteA.mean,
              sigma1 = cov.mat.cov.SiteB,
              sigma2 = cov.mat.cov.SiteA,
              symmetrized = F)
  
  BD <- fpc::bhattacharyya.dist(mu1 = cov.SiteB.mean,
                                mu2 = cov.SiteA.mean,
                                Sigma1 = cov.mat.cov.SiteB,
                                Sigma2 = cov.mat.cov.SiteA)
  
  results <- c(KL, BD)
  return(results)
  
}

results.Cov1 <- KL_divergence_function(cov1.SiteA, cov1.SiteB)
results.Cov2 <- KL_divergence_function(cov2.SiteA, cov2.SiteB)

KL <- results.Cov1[1] + results.Cov2[1]
BA <- results.Cov1[2] + results.Cov2[2]

print(KL)
print(BA)

ggplot() +
  geom_density(data = covs.SiteA, aes(x = cov1), color = "grey", fill = "grey", alpha = 0.2) +
  geom_density(data = covs.SiteB, aes(x = cov1), color = "purple", fill = "purple", alpha = 0.2)+
  theme_classic()

ggplot() +
  geom_density(data = covs.SiteA, aes(x = cov2), color = "grey", fill = "grey", alpha = 0.2) +
  geom_density(data = covs.SiteB, aes(x = cov2), color = "purple", fill = "purple", alpha = 0.2)+
  theme_classic()