

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
  
  nu <- 1/2 # Smoothness parameter - ONLY FOR MATERN
  
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
    # Separate out fixed and random effects
    
    xSeq <- terra::xFromCol(cov1)
    ySeq <- terra::yFromRow(cov1)
    
    # Simulate the random effect
    REff <- RISDM:::fftGPsim2( x=xSeq, y=ySeq, sig2 = variance , rho = scal, nu = nu)
    
    REff <- as.numeric(REff)
    
    GRF.mat <- cov1.mat
    
    GRF.mat[, "cov"] <- as.numeric(REff)
    
    colnames(GRF.mat)[colnames(GRF.mat) == "cov"] <- "GRF"
    
    # Save a version that removes mu and just keeps GRF for plotting
    GRF.rast <- rast((mu*0 + GRF.mat[, "GRF"]))
    
    crs(GRF.rast) <- crs(cov1)
    names(GRF.rast) <- "GRF"
    
    # Save a version that just keeps fixed effects for plotting
    fixed.rast <- rast(mu)
    crs(fixed.rast) <- crs(cov1)
    names(fixed.rast) <- "Fixed"
    
    # Add fixed and random effects
    mu <- mu + GRF.mat[, "GRF"]
    
    # Simulate the latent Gaussian field
    lg.s <- rpoispp(exp(mu))
    
    latent.list <- list(mu = mu, lg.s = lg.s, fixed.rast = fixed.rast, GRF.rast = GRF.rast) 
    
  } 
  
  if(latent.type == "ipp") {
    
    # Create IPP with environmental covariates
    lg.s <- rpoispp(exp(mu))
   
    latent.list <- list(mu = mu, lg.s = lg.s) 
    
  }
  
  
  
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


# TO DOs ------------------------------------------------------------------

# How does virtual species code in a function for the mean??
# How do we include a habitat covariate and a latent process?
# Sample presence-only records from the latent process
# Figuring out sampling strategy for presence-only records
# To add: A separate realisation was generated from the log-Gaussian Cox process for each sampling process to represent the fact that the individuals sampled by each method were unlikely to be the same i.e. data collection were at different points in time.

