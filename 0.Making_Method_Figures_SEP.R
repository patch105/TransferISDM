
### FIGURE TO DOS:
# Update so that all the inputs match the one we have done
# Adding in plot for thinning process & bias variable
# I have code in the "Trying_scenarios_simple_cov.R" file to make these plots



# FIGURE 1A. ---------------------------------------------------------------


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

# START with set up of resolution and north/east step length for later Site A and B grid creation.

# Set ncol
ncol <- 1000
nrow <- 1000
res <- 1

# Create a bounded domain on [0, 1] x [0, 1]

east_min <- 0
east_max <- 1000
north_min <- 0
north_max <- 1000
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


library(terra)
library(RISDM)

landscape.rast <- terra::rast(xmin = east_min, 
                              xmax = east_max, 
                              ymin = north_min,  
                              ymax = north_max, 
                              nrows = nrow,
                              ncols = ncol,
                              vals = 1:1000)

crs(landscape.rast) <- "epsg:3857" # Setting to WGS 84 / Pseudo-Mercator projection for later functions requiring cell size

#### Make a simple Cov 1 #####

# Generate a matrix of continuous values from 0 to 1, going left to right
covariate_matrix <- matrix(seq(1, 0, length.out = ncol), nrow = nrow, ncol = ncol, byrow = TRUE)

# # Flatten the matrix into a vector to match the expected input format for 'vals'
covariate_vals <- as.vector(covariate_matrix)

cov1 <- rast(nrows = nrow,
             ncols = ncol,
             xmin = east_min,
             xmax = east_max,
             ymin = north_min,
             ymax = north_max,
             resolution = res,
             vals = covariate_vals,
             names = c("cov")
)
crs(cov1) <- "epsg:3857" # Setting to WGS 84 / Pseudo-Mercator projection for later functions requiring cell size

names(cov1) <- "cov"


#### Make a spatially structured Cov 2 #####

range_cov2 = 300

xSeq <- terra::xFromCol(landscape.rast)
ySeq <- terra::yFromRow(landscape.rast)

cov2 <- RISDM:::fftGPsim2( x=xSeq, y=ySeq, sig2 = 1 , rho = range_cov2, nu = 1/2) 

cov2 <- rast(cov2)

crs(cov2) <- "epsg:3857" # Setting to WGS 84 / Pseudo-Mercator projection for later functions requiring cell size

names(cov2) <- "cov"

c1 <- cov1 %>% 
  as.data.frame(xy = T) %>%  
  ggplot() + 
  geom_tile(aes(x = x, y = y, fill = cov)) + 
  scale_fill_viridis(guide = guide_colorbar(barwidth = 0.4)) +
  coord_fixed() + 
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle('Covariate 1')

c2 <- cov2 %>% 
  as.data.frame(xy = T) %>%  
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
  ggtitle('Covariate 2')


Fig.1a <- ggarrange(c1, c2, ncol = 2, nrow = 1, align = "v", widths = c(0.5, 0.5))
# fig.1a 
ggsave(plot = Fig.1a, filename = paste0("output/Figures/Covariates_plot.png"), w = 21.5, h = 18, units = "cm", dpi = 400, device = "png")

ggsave(plot = c1, filename = paste0("output/Figures/Covariate1_plot.png"), w = 21.5, h = 18, units = "cm", dpi = 400, device = "png")

ggsave(plot = c2, filename = paste0("output/Figures/Covariate2_plot.png"), w = 21.5, h = 18, units = "cm", dpi = 400, device = "png")



# FIGURE 1B. --------------------------------------------------------------


# Set Parameters Data Generation ----------------------------------------------------------

beta0 <- -2 # Intercept
beta1 <- 2 # Coefficient for cov 1
beta2 <- 0.1 # Coefficient for cov 2
# beta3 <- 5 # Coefficient for cov 1*2 interaction
var <- 0.5 # Variance of the Gaussian field (changed  from 0.5)  
scal <- 20 # Scale parameter 
nu <- 1/2 # Smoothness parameter - ONLY FOR MATERN
seed <- 3L


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

# plot(mu)

# Set seed 
set.seed(seed)

# Create LGCP with environmental covariate
lg.s <- rLGCP('exp', mu = mu,
              var=var, scale=scal)


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

Fig.1b <- true_log_int.rast %>% 
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


# # Convert the point pattern to a data frame for ggplot
# points_df <- as.data.frame(cbind(lg.s$x, lg.s$y)[, 1:2]) %>% 
#   rename(x = V1, y = V2)

# Add the points to the intensity plot
# true.int.plot +
#   geom_point(data=points_df, aes(x=x, y=y), color='black', size=1.5) 

# Save the plot
# ggsave(paste0(outpath, "/output/True_log_intensity.png"), true.int.plot, width = 6, height = 6, units = "in")

# FIGURE 1C. ---------------------------------------------------------------

rast_cellsA <- c(100, 100)
rast_sizeA <- c(rast_cellsA[1]*bau_east_step, rast_cellsA[2]*bau_north_step)
# Set size of grid (number of cells) for Site B (Target)
rast_cellsB <- c(100, 100)
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

xmin.randA <- eastings[which.min(abs(eastings - runif(1, min = xmin, max = rand.limA[1])))] 
xmax.randA <- xmin.randA + rast_sizeA[1]

xmin.randA <- xmin.randA - bau_east_step/2
xmax.randA <- xmax.randA + bau_east_step/2 - bau_east_step

ymin.randA <- northings[which.min(abs(northings - runif(1, min = ymin, max = rand.limA[2])))]
ymax.randA <- ymin.randA + rast_sizeA[2]

ymin.randA <- ymin.randA - bau_north_step/2
ymax.randA <- ymax.randA + bau_north_step/2 - bau_east_step


#### GRID B ########

Generate_Grid_B <- function() {
  
  xmin.randB <<- eastings[which.min(abs(eastings - (runif(1, min = xmin, max = rand.limB[1]))))]
  xmax.randB <<- xmin.randB + rast_sizeB[1]
  
  xmin.randB <<- xmin.randB - bau_east_step/2
  xmax.randB <<- xmax.randB + bau_east_step/2 - bau_east_step
  
  ymin.randB <<- northings[which.min(abs(northings - (runif(1, min = ymin, max = rand.limB[2]))))]
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
  color = "red",
  label = "Reference Site",
  label_x = (extA[1] + extA[2]) / 2,  # Center of the extent for text placement
  label_y = extA[4] + (extA[4] - extA[3]) * 0.1  # Slightly above the top of the extent
)

df_extB <- data.frame(
  xmin = extB[1],
  xmax = extB[2],
  ymin = extB[3],
  ymax = extB[4],
  color = "blue",
  label = "Target Site",
  label_x = (extB[1] + extB[2]) / 2,  # Center of the extent for text placement
  label_y = extB[4] + (extB[4] - extB[3]) * 0.1  # Slightly above the top of the extent
)

# Combine the extents into one data frame
df_extents <- rbind(df_extA, df_extB)

# Create ggplot
Fig.1c <- true_log_int.rast %>% 
  as.data.frame(xy = TRUE) %>% 
  ggplot() + 
  geom_tile(aes(x = x, y = y, fill = int)) + 
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
  ggtitle('True log intensity')


# FIGURE 1D. -----------------------------------------------------------------

# Crop true intensity to Site A andb B
true_log_int.rast.SiteA <- crop(true_log_int.rast, ext(rand.gridA))
true_log_int.rast.SiteB <- crop(true_log_int.rast, ext(rand.gridB))

spp_process <- cbind(x = lg.s$x, y = lg.s$y)

# po <- thinpp[, c("x", "y")]

# For random covariate case
po <- spp_process

# Trim po to only include points in Site A or B
po.rand.gridA <- po[
  po[,1] >= xmin(ext(rand.gridA)) & po[,1] <= xmax(ext(rand.gridA)) & 
    po[,2] >= ymin(ext(rand.gridA)) & po[,2] <= ymax(ext(rand.gridA)), 
]

po.rand.gridB <- po[
  po[,1] >= xmin(ext(rand.gridB)) & po[,1] <= xmax(ext(rand.gridB)) & 
    po[,2] >= ymin(ext(rand.gridB)) & po[,2] <= ymax(ext(rand.gridB)), 
]

## ADDING IN RANDOM PRESENCE-ABSENCE GRID

# Set size of grid (number of cells) for PA grid in Site A (Reference)
# NOTE - must be smaller than total cell number in x y directions
rast_cellsA <- c(50, 30)
rast_sizeA <- c(rast_cellsA[1]*res(rand.gridA)[1], rast_cellsA[2]*res(rand.gridA)[2])

# Get coords of overall grid domain boundary
xmin <- xmin(rand.gridA)
xmax <- xmax(rand.gridA)
ymin <- ymin(rand.gridA)
ymax <- ymax(rand.gridA)

eastingsSITE <- crds(rand.gridA)[,1]
northingsSITE <- crds(rand.gridA)[,2]

# Set the limit for x and y coord so box is completely inside the domain
rand.limA <- c(xmax - rast_sizeA[1], ymax - rast_sizeA[2])

# Create random coordinate index for (bottom left?) corner of subgrid within grid domain
# Do this by generating a random number and finding the nearest eastings/northings value
# Then use this index on x0 to get the coordinate
xmin.randA <- eastingsSITE[which.min(abs(eastingsSITE - runif(1, min = xmin, max = rand.limA[1])))]
ymin.randA <- northingsSITE[which.min(abs(northingsSITE - runif(1, min = ymin, max = rand.limA[2])))]

xmax.randA <- eastingsSITE[which.min(abs(eastingsSITE - (xmin.randA + rast_sizeA[1])))]
ymax.randA <- northingsSITE[which.min(abs(northingsSITE - (ymin.randA + rast_sizeA[2])))]

PA.rand.gridA <- rast(xmin = xmin.randA, 
                      xmax = xmax.randA, 
                      ymin = ymin.randA, 
                      ymax = ymax.randA, 
                      nrows = rast_cellsA[1], 
                      ncols = rast_cellsA[2],
                      vals = 1:rast_cellsA[2]) # Just setting values for plotting and for converting to a dataframe

crs(PA.rand.gridA) <- "epsg:3857"

# Get extent for plotting
extPA <- ext(PA.rand.gridA) 

df_extPA <- data.frame(
  xmin = extPA[1],
  xmax = extPA[2],
  ymin = extPA[3],
  ymax = extPA[4],
  color = "black",
  label = "PA Grid",
  label_x = (extPA[1] + extPA[2]) / 2,  # Center of the extent for text placement
  label_y = extPA[4] + (extPA[4] - extPA[3]) * 0.1  # Slightly above the top of the extent
)

#### NOW DO PA SAMPLING ####


# Get the domain of region a
dom_a_bbox <- c(east_min = xmin(PA.rand.gridA), east_max = xmax(PA.rand.gridA), north_min = ymin(PA.rand.gridA), north_max = ymax(PA.rand.gridA))

# Choose a grid size number of rows (for PA sampling)
PA_a_res <- 10
dom_a_resE <- (dom_a_bbox["east_max"] - dom_a_bbox["east_min"]) / PA_a_res
dom_a_resN <- (dom_a_bbox["north_max"] - dom_a_bbox["north_min"]) / PA_a_res


# Set centroids of PA sampling grids
east_seq <- seq(dom_a_bbox["east_min"] + dom_a_resE/2, 
                dom_a_bbox["east_max"] - dom_a_resE/2, 
                by = dom_a_resE)
north_seq <- seq(dom_a_bbox["north_min"] + dom_a_resN/2, 
                 dom_a_bbox["north_max"] - dom_a_resN/2, 
                 by = dom_a_resN)


# Create a blank PA dataset at Site A (all zeros), located on grids cells defined by random grid domain and our PA sampling grid size
grid_a <- expand.grid(east_seq, north_seq)
pa_a <- cbind(grid_a, 0)
colnames(pa_a) <- c("x", "y", "presence")
pa_a <- terra::rast(pa_a)

# find species coordinates from underlying LGCP IN GRID A that are in region a
inbox_idx_a <- which(po.rand.gridA[, "x"] >= dom_a_bbox["east_min"] &
                       po.rand.gridA[, "x"] <= dom_a_bbox["east_max"] &
                       po.rand.gridA[, "y"] >= dom_a_bbox["north_min"] &
                       po.rand.gridA[, "y"] <= dom_a_bbox["north_max"])


po_a <- po.rand.gridA[inbox_idx_a, ]


# If there's only one PO location, have to adjust so that it formats into dataframe correctly
if(length(po_a) == 2) {
  po_a_df <- data.frame(x = po_a[[1]], y = po_a[[2]])
  
} else {
  
  po_a_df <- as.data.frame(po_a)
  
}


# Now assuming perfect detection
po_a_df$presence <- 1

# Get cell indices of the species coordinates
cell_idx <- terra::cellFromXY(pa_a, po_a_df[, c("x", "y")])

# Fill in the raster with 1 from the cell indices
pres_idx <- as.numeric(names(table(cell_idx)))
pa_a[pres_idx] <- 1


# pa - region a
pa_a_df <- as.data.frame(pa_a, xy = TRUE) %>% 
  mutate(area = dom_a_resE * dom_a_resN) 

### NOW PLOT

# Plot PO data on grid A and B
Fig.1d <- true_log_int.rast.SiteA %>% 
  as.data.frame(xy = T) %>% 
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = int)) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 0.5)) +
  coord_fixed() +
  geom_rect(data = df_extPA, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, color = color), 
            fill = NA, linetype = "solid", linewidth = 1) +
  geom_point(data = po.rand.gridA, aes(x = x, y = y), color = "red", size = 0.8) +
  geom_point(data = pa_a_df, aes(x=x, y=y, shape = factor(presence, levels = c(1,0))), size = 0.9) +
  scale_shape_manual(values = c("0" = 4, "1" = 16),
                     label = c("0" = "Absence", "1" = "Presence")) +
  scale_color_identity() +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank(),
        plot.margin = unit(c(0.5, 0.1, 0.5, 0.1), "lines"),
        plot.title = element_text(hjust = 0.5)) +  # Reduce margins
  ggtitle('Reference Site')

Fig.1e <- true_log_int.rast.SiteB %>% 
  as.data.frame(xy = T) %>% 
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = int)) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 0.5)) +
  coord_fixed() +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank(),
        plot.margin = unit(c(0.5, 0.1, 0.5, 0.1), "lines"),
        plot.title = element_text(hjust = 0.5)) +  # Reduce margins
  ggtitle('Target Site')

Fig.1de <- ggarrange(Fig.1d, Fig.1e, ncol = 2, nrow = 1, align = "v", widths = c(0.5, 0.5))



# ARRANGE PLOT ------------------------------------------------------------

Fig.1 <- ggarrange(Fig.1a, Fig.1c, Fig.1de,  ncol = 1, nrow = 3, heights = c(0.4, 0.4, 0.4), labels = c("(a)", "(b)", "(c)"), label.x = 0)

ggsave(paste0(outpath, "/output/Fig1.png"), Fig.1, width = 21, height = 28, units = "cm", dpi = 400, device = "png")

