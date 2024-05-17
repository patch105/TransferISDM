

# Version 1. Sample grid continuous covariates ----------------------------
library(terra)


# Turn original covariate domain into rasters
gridcov1.rast <- gridcov1 %>% 
  reshape2::melt(c("y", "x"), value.name = "cov1") %>% 
  rast() 

crs(gridcov1.rast) <- "epsg:3031" # Arbitrarily setting to a polar projection for later functions requiring a CRS

gridcov2.rast <- gridcov2 %>% 
  reshape2::melt(c("y", "x"), value.name = "cov2") %>% 
  rast()

crs(gridcov2.rast) <- "epsg:3031" # Arbitrarily setting to a polar projection for later functions requiring a CRS


# Set size of grid for Site A (Reference)
rast_sizeA <- c(100,50)
# Set size of grid for Site B (Target)
rast_sizeB <- c(100,50)

# Get coords of overall grid domain boundary
xmin <- min(x0)
xmax <- max(x0)
ymin <- min(y0)
ymax <- max(y0)

# Set the limit for x and y coord so box is completely inside the domain
rand.limA <- c(xmax - rast_sizeA[1], ymax - rast_sizeA[2])
rand.limB <- c(xmax - rast_sizeB[1], ymax - rast_sizeB[2])

# Create random coordinate index for top corner of subgrid within grid domain
# Then use this index on x0 to get the coordinate
xmin.randA <- x0[round(runif(1, min = xmin, max = rand.limA[1]))]
ymin.randA <- y0[round(runif(1, min = ymin, max = rand.limA[2]))]
xmax.randA <- x0[xmin.randA + rast_sizeA[1]]
ymax.randA <- y0[ymin.randA + rast_sizeA[2]]

xmin.randB <- x0[round(runif(1, min = xmin, max = rand.limB[1]))]
ymin.randB <- y0[round(runif(1, min = ymin, max = rand.limB[2]))]
xmax.randB <- x0[xmin.randB + rast_sizeB[1]]
ymax.randB <- y0[ymin.randB + rast_sizeB[2]]

rand.gridA <- rast(xmin = xmin.randA, 
             xmax = xmax.randA, 
             ymin = ymin.randA, 
             ymax = ymax.randA, 
             nrows = rast_sizeA[1], 
             ncols = rast_sizeA[2],
             vals = 1:rast_sizeA[2]) # Just setting values for plotting and for converting to a dataframe

rand.gridB <- rast(xmin = xmin.randB, 
                   xmax = xmax.randB, 
                   ymin = ymin.randB, 
                   ymax = ymax.randB, 
                   nrows = rast_sizeB[1], 
                   ncols = rast_sizeB[2],
                   vals = 1:rast_sizeB[2]) # Just setting values for plotting and for converting to a dataframe

plot(gridcov1.rast)
lines(ext(rand.gridA), lwd = 2, col = "red")
lines(ext(rand.gridB), lwd = 2, col = "blue")


# Extract covariates for the random grid Site A  --------------------------

rand.grid.df <- as.data.frame(rand.gridA, xy = T)[,c("x", "y")]

cov1.SiteA <- terra::extract(gridcov1.rast, rand.grid.df, xy = T) 
cov2.SiteA <- terra::extract(gridcov2.rast, rand.grid.df, xy = T) 

# Join to create one dataframe
covs.SiteA <- left_join(cov1.SiteA, cov2.SiteA, by = join_by(ID, x, y))


# Extract covariates for the random grid Site B ---------------------------

rand.grid.df <- as.data.frame(rand.gridB, xy = T)[,c("x", "y")]

cov1.SiteB <- terra::extract(gridcov1.rast, rand.grid.df, xy = T)
cov2.SiteB <- terra::extract(gridcov2.rast, rand.grid.df, xy = T) 

# Join to create one dataframe
covs.SiteB <- left_join(cov1.SiteB, cov2.SiteB, by = join_by(ID, x, y))



# Plotting location of data in cov space ----------------------------------

ggplot() + 
  geom_point(data = covs.SiteA, aes(x = cov1, y = cov2), color = "grey") +
  geom_point(data = covs.SiteB, aes(x = cov1, y = cov2), color = "purple4") +
  theme_bw()


# Calculating Overlap of Environment - Whole Grid -------------------------

# Shape approach ----------------------------------------------------------

# # Install flexsdm
# remotes::install_github("sjevelazco/flexsdm")
library(flexsdm)

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
                           univar_comb = T)

shape_extrap <- cbind(shape_extrap, covs.SiteB[, c("x", "y")])

shape_extrap %>% 
  ggplot() + 
  geom_tile(aes(x = x, y = y, fill = extrapolation)) + 
  scale_fill_viridis() +
  coord_fixed() + 
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank()) +
  ggtitle('Extrapolation')

mean(shape_extrap$extrapolation)
median(shape_extrap$extrapolation)
min(shape_extrap$extrapolation)
max(shape_extrap$extrapolation)

# Plotting data in covariate space with extrapolation  ------------------------

ggplot() + 
  geom_point(data = covs.SiteA, aes(x = cov1, y = cov2), color = "grey") +
  geom_point(data = shape_extrap, aes(x = cov1, y = cov2, color = extrapolation)) +
  scale_color_viridis(option = "magma", direction = -1) +
  theme_bw() +
  theme(legend.ticks = element_blank())



# Version 2. Sample grid spatially varying covariates -----------------

# Just testing out with other habitat covariates now
gridcov1.rast <- cov1
gridcov2.rast <- cov2

crs(gridcov1.rast) <- "epsg:3031" # Arbitrarily setting to a polar projection for later functions requiring a CRS

crs(gridcov2.rast) <- "epsg:3031" # Arbitrarily setting to a polar projection for later functions requiring a CRS


# Set size of grid for Site A (Reference)
rast_sizeA <- c(100,50)
# Set size of grid for Site B (Target)
rast_sizeB <- c(100,50)

# Get coords of overall grid domain boundary
xmin <- min(x0)
xmax <- max(x0)
ymin <- min(y0)
ymax <- max(y0)

# Set the limit for x and y coord so box is completely inside the domain
rand.limA <- c(xmax - rast_sizeA[1], ymax - rast_sizeA[2])
rand.limB <- c(xmax - rast_sizeB[1], ymax - rast_sizeB[2])

# Create random coordinate index for top corner of subgrid within grid domain
# Then use this index on x0 to get the coordinate
xmin.randA <- x0[round(runif(1, min = xmin, max = rand.limA[1]))]
ymin.randA <- y0[round(runif(1, min = ymin, max = rand.limA[2]))]
xmax.randA <- x0[xmin.randA + rast_sizeA[1]]
ymax.randA <- y0[ymin.randA + rast_sizeA[2]]

xmin.randB <- x0[round(runif(1, min = xmin, max = rand.limB[1]))]
ymin.randB <- y0[round(runif(1, min = ymin, max = rand.limB[2]))]
xmax.randB <- x0[xmin.randB + rast_sizeB[1]]
ymax.randB <- y0[ymin.randB + rast_sizeB[2]]

rand.gridA <- rast(xmin = xmin.randA, 
                   xmax = xmax.randA, 
                   ymin = ymin.randA, 
                   ymax = ymax.randA, 
                   nrows = rast_sizeA[1], 
                   ncols = rast_sizeA[2],
                   vals = 1:rast_sizeA[2]) # Just setting values for plotting and for converting to a dataframe

rand.gridB <- rast(xmin = xmin.randB, 
                   xmax = xmax.randB, 
                   ymin = ymin.randB, 
                   ymax = ymax.randB, 
                   nrows = rast_sizeB[1], 
                   ncols = rast_sizeB[2],
                   vals = 1:rast_sizeB[2]) # Just setting values for plotting and for converting to a dataframe

plot(gridcov1.rast)
lines(ext(rand.gridA), lwd = 2, col = "red")
lines(ext(rand.gridB), lwd = 2, col = "blue")


# Extract covariates for the random grid Site A  --------------------------

rand.grid.df <- as.data.frame(rand.gridA, xy = T)[,c("x", "y")]

cov1.SiteA <- terra::extract(gridcov1.rast, rand.grid.df, xy = T) %>% rename(cov1 = layer)
cov2.SiteA <- terra::extract(gridcov2.rast, rand.grid.df, xy = T) %>% rename(cov2 = layer)

# Join to create one dataframe
covs.SiteA <- left_join(cov1.SiteA, cov2.SiteA, by = join_by(ID, x, y))


# Extract covariates for the random grid Site B ---------------------------

rand.grid.df <- as.data.frame(rand.gridB, xy = T)[,c("x", "y")]

cov1.SiteB <- terra::extract(gridcov1.rast, rand.grid.df, xy = T) %>% rename(cov1 = layer)
cov2.SiteB <- terra::extract(gridcov2.rast, rand.grid.df, xy = T) %>% rename(cov2 = layer)

# Join to create one dataframe
covs.SiteB <- left_join(cov1.SiteB, cov2.SiteB, by = join_by(ID, x, y))



# Plotting location of data in cov space ----------------------------------

ggplot() + 
  geom_point(data = covs.SiteA, aes(x = cov1, y = cov2), color = "grey") +
  geom_point(data = covs.SiteB, aes(x = cov1, y = cov2), color = "purple4") +
  theme_bw()


# Calculating Overlap of Environment - Whole Grid -------------------------

# Shape approach ----------------------------------------------------------

# # Install flexsdm
# remotes::install_github("sjevelazco/flexsdm")
library(flexsdm)

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
                           univar_comb = T)

shape_extrap <- cbind(shape_extrap, covs.SiteB[, c("x", "y")])

shape_extrap %>% 
  ggplot() + 
  geom_tile(aes(x = x, y = y, fill = extrapolation)) + 
  scale_fill_viridis() +
  coord_fixed() + 
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank()) +
  ggtitle('Extrapolation')

mean(shape_extrap$extrapolation)
median(shape_extrap$extrapolation)
min(shape_extrap$extrapolation)
max(shape_extrap$extrapolation)

# Plotting data in covariate space with extrapolation  ------------------------

ggplot() + 
  geom_point(data = covs.SiteA, aes(x = cov1, y = cov2), color = "grey") +
  geom_point(data = shape_extrap, aes(x = cov1, y = cov2, color = extrapolation)) +
  scale_color_viridis(option = "magma", direction = -1) +
  theme_bw() +
  theme(legend.ticks = element_blank())



##### Extrapolation for PA datasets -----------------------------------------------

# Get covariate coverage for Site A PA

cov1.SiteA.PA <- terra::extract(gridcov1.rast, pa_a_df[c("x","y")], xy = T) %>% rename(cov1 = layer)
cov2.SiteA.PA <- terra::extract(gridcov2.rast, pa_a_df[c("x","y")], xy = T) %>% rename(cov2 = layer)

# Join to create one dataframe
covs.SiteA.PA <- left_join(cov1.SiteA.PA, cov2.SiteA.PA, by = join_by(ID, x, y))

# Get covariate coverage for Site B PA

cov1.SiteB.PA <- terra::extract(gridcov1.rast, pa_b_df[c("x","y")], xy = T) %>% rename(cov1 = layer)
cov2.SiteB.PA <- terra::extract(gridcov2.rast, pa_b_df[c("x","y")], xy = T) %>% rename(cov2 = layer)

# Join to create one dataframe
covs.SiteB.PA <- left_join(cov1.SiteB.PA, cov2.SiteB.PA, by = join_by(ID, x, y))

# Plotting location of data in cov space ----------------------------------

ggplot() + 
  geom_point(data = covs.SiteA.PA, aes(x = cov1, y = cov2), color = "grey") +
  geom_point(data = covs.SiteB.PA, aes(x = cov1, y = cov2), color = "purple4") +
  theme_bw()


# Calculating Overlap of Environment - PA Site A to Site B-----------------

# Shape approach ----------------------------------------------------------

# # Install flexsdm
# remotes::install_github("sjevelazco/flexsdm")
library(flexsdm)

# Adding presence column due to extra_eval requirements
# Trimming so just the covariates
training <- covs.SiteA.PA %>% 
  mutate(Presence = 1) %>% 
  .[,c("cov1", "cov2", "Presence")]

projection <- covs.SiteB.PA %>% 
  .[,c("cov1", "cov2")]

shape_extrap <- extra_eval(training_data = training,
                           pr_ab = "Presence",
                           projection_data = projection,
                           metric = "mahalanobis",
                           univar_comb = T)

shape_extrap <- cbind(shape_extrap, covs.SiteB.PA[, c("x", "y")])

shape_extrap %>% 
  ggplot() + 
  geom_tile(aes(x = x, y = y, fill = extrapolation)) + 
  scale_fill_viridis() +
  coord_fixed() + 
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank()) +
  ggtitle('Extrapolation at Site B')

mean(shape_extrap$extrapolation)
median(shape_extrap$extrapolation)
min(shape_extrap$extrapolation)
max(shape_extrap$extrapolation)

# Plotting data in covariate space with extrapolation  ------------------------

plot_extrap.PA.PA <- ggplot() + 
  geom_point(data = covs.SiteA.PA, aes(x = cov1, y = cov2), color = "grey") +
  geom_point(data = shape_extrap, aes(x = cov1, y = cov2, color = extrapolation)) +
  scale_color_viridis(option = "magma", direction = -1) +
  theme_bw() +
  theme(legend.ticks = element_blank())




###### Extrapolation for PO + PA datasets --------------------------------------

# Get covariate coverage for PO data
# ***TO REMOVE: For illustration we will thin the PO to make it more realistic
thinpp$presence1 <- rbinom(nrow(thinpp), 1, prob = 0.09)
thinpp <- thinpp[thinpp$presence1 == 1,]

# Get covariate coverage for PO data

cov1.PO <- terra::extract(gridcov1.rast, thinpp[c("x","y")], xy = T) %>% rename(cov1 = layer)
cov2.PO <- terra::extract(gridcov2.rast, thinpp[c("x","y")], xy = T) %>% rename(cov2 = layer)

# Join to create one dataframe
covs.PO <- left_join(cov1.PO, cov2.PO, by = join_by(ID, x, y))

# Calculating Overlap of Environment - PA Site A + PO all to Site B--------

# Shape approach ----------------------------------------------------------


# Adding presence column due to extra_eval requirements
# Trimming so just the covariates
covs.PO.PA <- rbind(covs.SiteA.PA, covs.PO)

training <- covs.PO.PA %>% 
  mutate(Presence = 1) %>% 
  .[,c("cov1", "cov2", "Presence")]


projection <- covs.SiteB.PA %>% 
  .[,c("cov1", "cov2")]

shape_extrap <- extra_eval(training_data = training,
                           pr_ab = "Presence",
                           projection_data = projection,
                           metric = "mahalanobis",
                           univar_comb = T)

shape_extrap <- cbind(shape_extrap, covs.SiteB.PA[, c("x", "y")])

shape_extrap %>% 
  ggplot() + 
  geom_tile(aes(x = x, y = y, fill = extrapolation)) + 
  scale_fill_viridis() +
  coord_fixed() + 
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank()) +
  ggtitle('Extrapolation at Site B')

mean(shape_extrap$extrapolation)
median(shape_extrap$extrapolation)
min(shape_extrap$extrapolation)
max(shape_extrap$extrapolation)

# Plotting data in covariate space with extrapolation  ------------------------

plot_extrap.PA_PO.PA <- ggplot() + 
  geom_point(data = covs.PO.PA, aes(x = cov1, y = cov2), color = "grey") +
  geom_point(data = shape_extrap, aes(x = cov1, y = cov2, color = extrapolation)) +
  scale_color_viridis(option = "magma", direction = -1) +
  theme_bw() +
  theme(legend.ticks = element_blank())




# ARCHIVE -----------------------------------------------------------------

# EXDET approach ----------------------------------------------------------

library(dsmextra)

# Format for dsmextra
covs.SiteA <- covs.SiteA[, c("cov1", "cov2")]

# Define projected coordinate system
crs <- sp::CRS("+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

extrap <- compute_extrapolation(
  samples = covs.SiteA,
  covariate.names = c("cov1", "cov2"),
  prediction.grid = covs.SiteB,
  coordinate.system = crs
)


plot(extrap$rasters$ExDet$all)


