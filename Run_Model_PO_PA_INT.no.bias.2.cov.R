ibrary(tidyverse)

packages <- c("sf", "terra", "ggpubr", "RISDM", "DescTools", "spatstat", "Metrics", "scoringutils")

walk(packages, require, character.only = T)


######## PO model random covariate ----------------------------------------

spp_process <- cbind(x = lg.s$x, y = lg.s$y)
colnames(spp_process) <- c("x", "y")

# Load occurrence data ----------------------------------------------------
PO <- spp_process

# Load enviro. covs -------------------------------------------------------

# Covariates add together
# cov <- c(rast(rand.cov1, type = "xyz"),
#          rast(rand.cov2,  type = "xyz"))

cov <- c(rast(GRF.cov1, type = "xyz"),
         rast(GRF.cov2,  type = "xyz"))

crs(cov) <- "epsg:3031"

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