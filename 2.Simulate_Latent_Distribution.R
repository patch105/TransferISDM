

# Simulate Latent Distribution --------------------------------------------

sim_latent_dist_func <- function(beta0,
                                 beta1,
                                 beta2,
                                 scal,
                                 variance,
                                 cov1,
                                 cov1.mat,
                                 cov2.mat,
                                 cov1.df,
                                 response.type = "linear",
                                 plot.mu = FALSE,
                                 plot.lg.s = FALSE,
                                 latent.type) {
  
  nu <- 1 # Smoothness parameter - ONLY FOR MATERN
  
  # Assumed intensity at mean of enviro. variables
  # log(lambda) = intercept + b1*(mean of cov1) + b2*(mean of cov2)
  # exp(answer) is intensity
  exp(beta0 + beta1*(0.5) + beta2*(0.5))
  
  # Can do with one or two covariates
  
  if(response.type == "linear") {
    
    fe <- beta0 + beta1*cov1.mat[, "cov"] + beta2*cov2.mat[, "cov"]
    
  }
  
  mu <- cov1.df %>% mutate(cov = fe)
  mu <- spatstat.geom::as.im(mu)
  
  if(latent.type == "lgcp") {
    
    # Create LGCP with environmental covariate
    # lg.s <- rLGCP('exp', mu = mu,
    #               var=var, scale=scal)
    
    lg.s <- rLGCP('matern', mu = mu,
                  var=variance, scale=scal, nu=nu)
    
  } 
  
  if(latent.type == "ipp") {
    
    # Create IPP with environmental covariate
    lg.s <- rpoispp(mu)
   
  }
  
  
  
  latent.list <- list(mu = mu, lg.s = lg.s) 
  
  if(plot.mu == TRUE) {
    
    mu.plot <- plot(mu)
    
    latent.list <- append(latent.list, list(mu.plot = mu.plot))
  }
  
  if(plot.lg.s == TRUE) {
    
    lg.s.plot <- plot(lg.s)
    
    latent.list <- append(latent.list, list(lg.s.plot = lg.s.plot))
  }
 
  return(latent.list)
    
}



# # IDEAS ON HOW TO SPLIT LATENT & COVARIATE EFFECTS ------------------------
# 
# GRF <- nlm_gaussianfield(ncol = ncol,
#                          nrow = nrow,
#                          resolution = res,
#                          autocorr_range = scal, # Maximum range (raster units) of spatial autocorrelation
#                          mag_var = variance, # Magnitude of variation over the landscape
#                          nug = 0.01, # Magnitude of variation in the scale of the autocorr_range (smaller values = more homogenous)
#                          mean = 0, # Mean value over the field
#                          user_seed = NULL, # Set random seed for the simulation
#                          rescale = F # If T, the values are rescaled between 0 and 1
# ) %>% 
#   rast()
# 
# 
# # Note to self FOR SIMULATING LATENT STATE NEXT - the im function requires a matrix of a certain input order
# # Terra::as.matrix just converts all cells to a matrix in the same order as they appear in the raster
# 
# # The code below therefore: converts the raster to a matrix, retaining the terra ordering
# # We then reshape into long (x, y) format 
# # We extract the coords from the original raster and bind to the covariate matrix
# 
# # # Get coords of original raster
# # coords <- xyFromCell(GRF, 1:ncell(GRF))
# 
# # Convert raster to matrix object
# GRF.df <- as.data.frame(GRF, xy = T)
# GRF.mat <- as.matrix(GRF.df)
# colnames(GRF.mat) <- c("x", "y", "GRF")
# 
# mu <- mu + exp(GRF.mat[, "GRF"])
# 
# lg.s <- rpoispp(mu)



# TO DOs ------------------------------------------------------------------

# How does virtual species code in a function for the mean??
# How do we include a habitat covariate and a latent process?
# Sample presence-only records from the latent process
# Figuring out sampling strategy for presence-only records
# To add: A separate realisation was generated from the log-Gaussian Cox process for each sampling process to represent the fact that the individuals sampled by each method were unlikely to be the same i.e. data collection were at different points in time.

