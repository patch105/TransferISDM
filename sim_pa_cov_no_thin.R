################################################################################
# Get presence-absence data within two regions.
################################################################################

rm(list = ls())

library(terra)
library(readr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

mypath <- getwd()

load(file = paste0(mypath, "/output/nhpp.rdata"))


#-------------------------------------------------------------------------------
# presence only data
#-------------------------------------------------------------------------------

spp_process <- cbind(x = lg.s$x, y = lg.s$y)

#-------------------------------------------------------------------------------
# region a
#-------------------------------------------------------------------------------
# set the domain of region a
dom_a_bbox <- c(east_min = 150, east_max = 300, north_min = 150, north_max = 300)

# set resolution (e.g., grid cell size )
dom_a_res <- 6

# grid cell centroids
east_seq <- seq(dom_a_bbox["east_min"] + dom_a_res/2, 
                dom_a_bbox["east_max"] - dom_a_res/2, 
                by = dom_a_res)
north_seq <- seq(dom_a_bbox["north_min"] + dom_a_res/2, 
                 dom_a_bbox["north_max"] - dom_a_res/2, 
                 by = dom_a_res)

# create a grid, assuming no species present now
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

## RANDOM SUBSET OF THESE
# Thin points using the detection probability
# Reduces it to presence, absence, not abundance
po_a_df$presence <- rbinom(nrow(po_a_df), 1, prob = 0.015) 

# make it presence only data
po_a_df <- po_a_df[po_a_df$presence == 1,]


# get cell indices of the species coordinates
cell_idx <- terra::cellFromXY(pa_a, po_a_df[,1:2])

# fill in the raster with 1 from the cell indices
pres_idx <- as.numeric(names(table(cell_idx)))
pa_a[pres_idx] <- 1

# plot the data
plot(pa_a)


#-------------------------------------------------------------------------------
# region b
#-------------------------------------------------------------------------------
# set the domain of region b
dom_b_bbox <- c(east_min = 150, east_max = 300, north_min = 0, north_max = 120)

# set resolution (e.g., grid cell size )
dom_b_res <- 6


# grid cell centroids
east_seq <- seq(dom_b_bbox["east_min"] + dom_b_res/2, 
                dom_b_bbox["east_max"] - dom_b_res/2, 
                by = dom_a_res)
north_seq <- seq(dom_b_bbox["north_min"] + dom_b_res/2, 
                 dom_b_bbox["north_max"] - dom_b_res/2, 
                 by = dom_b_res)

# create a grid, assuming no species present now
grid_b <- expand.grid(east_seq, north_seq)
pa_b <- cbind(grid_b, 0)
colnames(pa_b) <- c("x", "y", "presence")
pa_b <- terra::rast(pa_b)

# find species coordinates that are in region b
inbox_idx_b <- which(spp_process[, "x"] >= dom_b_bbox["east_min"] &
                       spp_process[, "x"] <= dom_b_bbox["east_max"] & 
                       spp_process[, "y"] >= dom_b_bbox["north_min"] &
                       spp_process[, "y"] <= dom_b_bbox["north_max"])

po_b <- spp_process[inbox_idx_b, ]
po_b_df <- as.data.frame(po_b)

## RANDOM SUBSET OF THESE
# Thin points using the detection probability
# Reduce it to presence, absence, not abundance
po_b_df$presence <- rbinom(nrow(po_b_df), 1, prob = 0.015)

# make it presence only data
po_b_df <- po_b_df[po_b_df$presence == 1,]

# get cell indices of the species coordinates
cell_idx <- terra::cellFromXY(pa_b, po_b_df[,1:2])

# fill in the raster with 1 from the cell indices
pres_idx <- as.numeric(names(table(cell_idx)))
pa_b[pres_idx] <- 1

# plot the data
plot(pa_b)


#-------------------------------------------------------------------------------
# write the po (excluding those in regions a and b) and pa data to csv
#-------------------------------------------------------------------------------
# po (we may sub-sample the data here)
readr::write_csv(as.data.frame(spp_process), file = paste0(mypath, "/output/po_all_no_thin.csv"))

# pa - region a
pa_a_df <- as.data.frame(pa_a, xy = TRUE)
pa_a_df <- round(pa_a_df, 4)
readr::write_csv(pa_a_df, file = paste0(mypath, "/output/pa_a.csv"))

# pa - region b
pa_b_df <- as.data.frame(pa_b, xy = TRUE)
pa_b_df <- round(pa_b_df, 4)
readr::write_csv(pa_b_df, file = paste0(mypath, "/output/pa_b.csv"))


#-------------------------------------------------------------------------------
# Plot the site with all data types
#-------------------------------------------------------------------------------

library(ggplot2)

ggplot() +
  geom_point(data = data.frame(spp_process), aes(x = x, y = y), color = "black") +
  geom_point(data = pa_a_df, aes(x = x, y = y), color = "red", size = 2) +
  geom_point(data = pa_b_df, aes(x = x, y = y), color = "blue", size = 2) +
  theme_classic()
  
ggsave(paste0(mypath, "/output/po_pa.png"), width = 10, height = 10, units = "in")
