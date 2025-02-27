
########################################################################
################### 2. Simulate latent distribution ###################
########################################################################

# Simulate the latent species distribution, either with or without spatial autocorrelation (determined by parameter 'latent.type' in the 0a.Run_ALL_Replicates.R script).

# In the spatial autocorrelation scenario, the random and fixed effect relative contributions are controlled with the GRF.var.multiplier parameter. This parameter is multiplied by the variance of the fixed effect to determine the target variance of the random effect.


# Inputs: 

# Intercept and coefficients (beta0, beta1, beta2)

# Range of spatial autocorrelation for the random effect (scal)

# Multiplier for the variance of the random effect (GRF.var.multiplier)

# Type of response (only linear supported)

# Whether the species distribution has spatial autocorrelation (latent.type)


# Outputs:

# A list containing the latent species distribution, fixed and random effects saved separately as rasters, and the correlation between the random effect and the covariates.


########################################################################


sim_latent_dist_func <- function(beta0,
                                 beta1,
                                 beta2,
                                 scal,
                                 GRF.var.multiplier,
                                 cov1,
                                 cov1.mat,
                                 cov2,
                                 cov2.mat,
                                 cov1.df,
                                 var_cov1,
                                 var_cov2,
                                 response.type = "linear",
                                 plot.mu = FALSE,
                                 plot.lg.s = FALSE,
                                 latent.type
                                 ) {
  print(scal)
  
  nu <- 1/2 # Smoothness parameter - for matern function
  
  # Assumed intensity at mean of enviro. variables
  # log(lambda) = intercept + b1*(mean of cov1) + b2*(mean of cov2)
  # exp(answer) is intensity
  exp(beta0 + beta1*(0) + beta2*(0))
  
  # Can do with one or two covariates
  
  if(response.type == "linear") {
    
    fe <- beta0 + beta1*cov1.mat[, "cov"] + beta2*cov2.mat[, "cov"]
    
  }
  
  mu <- cov1.df %>% mutate(cov = fe)
  mu <- spatstat.geom::as.im(mu)
  
  # plot(mu)
  
  if(latent.type == "lgcp") {
    
    # Create log-Gaussian Cox Process with environmental covariates
    # Separate out fixed and random effects
    
    xSeq <- terra::xFromCol(cov1)
    ySeq <- terra::yFromRow(cov1)
    
    ############
    #### Calculate variance of GRF to be proportional to variance of fixed effect
    ############
    
    # Calculate 'variance' of fixed effect
    fe.var <- beta1^2*var_cov1 + beta2^2*var_cov2 
    
    # Multiply variance by multiplier to get target variance for GRF
    target.var <- GRF.var.multiplier * fe.var
    
    # Simulate the random effect with target variance
    REff <- RISDM:::fftGPsim2( x=xSeq, y=ySeq, sig2 = target.var , rho = scal, nu = nu, min_exponent2 = 12)
    
    REff <- as.numeric(REff)
    
    GRF.mat <- cov1.mat
    
    GRF.mat[, "cov"] <- as.numeric(REff)
    
    colnames(GRF.mat)[colnames(GRF.mat) == "cov"] <- "GRF"
    
    # Save a version that removes mu and just keeps random effect for later comparison
    GRF.rast <- rast((mu*0 + GRF.mat[, "GRF"]))
    
    crs(GRF.rast) <- crs(cov1)
    names(GRF.rast) <- "GRF"
    
    # Extract cell size for comparison with prediction because RISDM predictions are with reference to cell area
    log.cell_size <- log(cellSize(cov1))
    
    GRF.rast <- GRF.rast + log.cell_size
    
    # Calculate the correlation between the GRF and the covariates
    cor.GRF.cov1 <- cor(as.vector(GRF.rast), as.vector(cov1),
                      method = "spearman")
    
    cor.GRF.cov2 <- cor(as.vector(GRF.rast), as.vector(cov2),
                        method = "spearman")
    
    # Save a version that just keeps fixed effects for later comparison (removes the intercept)
    fixed <- beta1*cov1.mat[, "cov"] + beta2*cov2.mat[, "cov"]
    mu.fixed <- cov1.df %>% mutate(cov = fixed)
    mu.fixed <- spatstat.geom::as.im(mu.fixed)
    
    fixed.rast <- rast(mu.fixed)
    crs(fixed.rast) <- crs(cov1)
    names(fixed.rast) <- "Fixed"
    
    fixed.rast <- fixed.rast + log.cell_size
    
    # Combine intercept +  fixed and random effects
    mu <- mu + GRF.mat[, "GRF"]
  
    # Simulate the latent Gaussian field
    lg.s <- rpoispp(exp(mu))
    
    latent.list <- list(mu = mu, 
                        lg.s = lg.s, 
                        fixed.rast = fixed.rast, 
                        GRF.rast = GRF.rast, 
                        cor.GRF.cov1 = cor.GRF.cov1, 
                        cor.GRF.cov2 = cor.GRF.cov2,
                        fixed.variance = fe.var,
                        GRF.variance = target.var) 
    
  } 
  
  # If no spatial autocorrelation, species process is an inhomogenous Poisson point process
  
  if(latent.type == "ipp") { 
    
    print(paste0("Max. Mu is ", max(exp(mu))))
    print(paste0("Min. Mu is ", min(exp(mu))))
    
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

