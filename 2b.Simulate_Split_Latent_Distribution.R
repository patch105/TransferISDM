

nu <- 1 # Smoothness parameter - ONLY FOR MATERN

# Assumed intensity at mean of enviro. variables
# log(lambda) = intercept + b1*(mean of cov1) + b2*(mean of cov2)
# exp(answer) is intensity
exp(beta0 + beta1*(0.5) + beta2*(0.5))

# Can do with one or two covariates

  
  fe <- beta0 + beta2*cov.list$cov2.mat[, "cov"]


mu <- cov.list$cov1.df %>% mutate(cov = fe)
mu <- spatstat.geom::as.im(mu)
  
  
  
  
  
  # IDEAS ON HOW TO SPLIT LATENT & COVARIATE EFFECTS ------------------------

  GRF <- nlm_gaussianfield(ncol = ncol,
                           nrow = nrow,
                           resolution = res,
                           autocorr_range = scal, # Maximum range (raster units) of spatial autocorrelation
                           mag_var = variance, # Magnitude of variation over the landscape
                           nug = 0.01, # Magnitude of variation in the scale of the autocorr_range (smaller values = more homogenous)
                           mean = 0, # Mean value over the field
                           user_seed = NULL, # Set random seed for the simulation
                           rescale = F # If T, the values are rescaled between 0 and 1
  ) %>%
    rast()


  # Note to self FOR SIMULATING LATENT STATE NEXT - the im function requires a matrix of a certain input order
  # Terra::as.matrix just converts all cells to a matrix in the same order as they appear in the raster

  # The code below therefore: converts the raster to a matrix, retaining the terra ordering
  # We then reshape into long (x, y) format
  # We extract the coords from the original raster and bind to the covariate matrix

  # # Get coords of original raster
  # coords <- xyFromCell(GRF, 1:ncell(GRF))

  # Convert raster to matrix object
  GRF.df <- as.data.frame(GRF, xy = T)
  GRF.mat <- as.matrix(GRF.df)
  colnames(GRF.mat) <- c("x", "y", "GRF")

  mu <- mu + exp(GRF.mat[, "GRF"])

  lg.s <- rpoispp(mu)
  
  plot(lg.s)

  lg.s <- rLGCP('matern', mu = mu,
                var=variance, scale=scal, nu=nu)



