library(NLMR)
library(landscapetools)
library(RandomFields) # See below for download from archived files.
library(RISDM)

# 1.Simulate_Covariates ---------------------------------------------------


# Install RandomFields package (required for covariate simulation). No longer on CRAN. 
# Downloaded archived RandomFields utils and RandomFields with code below, accessed from: https://cran.r-project.org/src/contrib/Archive/RandomFieldsUtils/ and https://cran.r-project.org/src/contrib/Archive/RandomFields/ 

# install.packages("C:/Users/n11222026/OneDrive - Queensland University of Technology/Code/Downloaded_packages/RandomFields_3.3.14.tar.gz", repos = NULL, type="source")

sim_covariates_func <- function(plot, 
                                ncol, 
                                nrow,
                                res,
                                seed,
                                range_cov1,
                                range_cov2,
                                east_min,
                                east_max,
                                north_min,
                                north_max) {
  
 
    # cov1 <- nlm_gaussianfield(ncol = ncol,
  #                           nrow = nrow,
  #                           resolution = res,
  #                           autocorr_range = range_cov1, # Maximum range (raster units) of spatial autocorrelation
  #                           mag_var = 1, # Magnitude of variation over the landscape
  #                           nug = 0.01, # Magnitude of variation in the scale of the autocorr_range (smaller values = more homogenous)
  #                           mean = 0.5, # Mean value over the field
  #                           user_seed = NULL, # Set random seed for the simulation
  #                           rescale = T # If T, the values are rescaled between 0 and 1
  # ) %>% 
  #   rast()
  
  # cov2 <- nlm_gaussianfield(ncol = ncol,
  #                           nrow = nrow,
  #                           resolution = res,
  #                           autocorr_range = range_cov2, # Maximum range (raster units) of spatial autocorrelation
  #                           mag_var = 1, # Magnitude of variation over the landscape
  #                           nug = 0.01, # Magnitude of variation in the scale of the autocorr_range (smaller values = more homogenous)
  #                           mean = 0.5, # Mean value over the field
  #                           user_seed = NULL, # Set random seed for the simulation
  #                           rescale = T # If T, the values are rescaled between 0 and 1
  # ) %>% 
  #   rast()
  
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
  
  cov1 <- RISDM:::fftGPsim2( x=xSeq, y=ySeq, sig2 = 1 , rho = range_cov1, nu = 1/2) 
  
  cov1 <- rast(cov1)
  
  crs(cov1) <- "epsg:3857" # Setting to WGS 84 / Pseudo-Mercator projection for later functions requiring cell size
  
  names(cov1) <- "cov"
  
  cov2 <- RISDM:::fftGPsim2( x=xSeq, y=ySeq, sig2 = 1 , rho = range_cov2, nu = 1/2) 
  
  cov2 <- rast(cov2)
  
  crs(cov2) <- "epsg:3857" # Setting to WGS 84 / Pseudo-Mercator projection for later functions requiring cell size
  
  names(cov2) <- "cov"
  
  
  covs <- c(cov1, cov2)
  
  names(covs) <- c("cov1", "cov2")
  
  # Note to self FOR SIMULATING LATENT STATE NEXT - the im function requires a matrix of a certain input order
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
  
  cov.list <- list(cov1 = cov1, cov2 = cov2, covs = covs, coords = coords, cov1.df = cov1.df, cov2.df = cov2.df, cov1.mat = cov1.mat, cov2.mat = cov2.mat)
  
  if(plot == TRUE) {
    
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
    
    
    covs.plot <- ggarrange(c1, c2)
    
    cov.list <- append(cov.list, list(covs.plot = covs.plot))
    
  }
  
  
  
  return(cov.list)
  
  
}
