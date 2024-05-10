
# Set Parameters Data Generation ----------------------------------------------------------

beta0 <- -2 # Intercept
beta1 <- 2 # Coefficient for cov 1
beta2 <- 2 # Coefficient for cov 2
# beta3 <- 5 # Coefficient for cov 1*2 interaction
sigma2x <- 0.25 # Variance of the Gaussian field (changed  from 0.5)  
range <- 20 # Scale parameter (equivalent to kappa 0.05)
nu <- 1 # Smoothness parameter
seed <- 3L

# Mean of process dependent on continuous covariates ----------------------------------

# Note to self - the im inputs the matrix and outputs a pixel image that is the correct xy values (transposes the matrix)

mu_gcov1 <- im(beta0 + beta1*gridcov1, xcol=x0, yrow=y0)
mu_gcov2 <- im(beta0 + beta2*gridcov2, xcol=x0, yrow=y0)
mu_gcovs <- im(beta0 + beta1*gridcov1 + beta2*gridcov2, xcol=x0, yrow=y0)
# mu_int <- im(beta0 + beta1*gridcov1 + beta2*gridcov2 + gridcov1*gridcov2*beta3, xcol=x0, yrow=y0)

# Testing a quadratic function
mu_covs.quad <- im(beta0 + beta1*gridcov1.mat + beta2*gridcov2.mat + beta3*grid2^2, xcol=x0, yrow=y0)

plot(mu_gcov1)
plot(mu_gcov2)
plot(mu_gcovs)
# plot(mu_int)

# Set seed 
set.seed(seed)

# Create LGCP with environmental covariate
lg.s <- rLGCP('matern', mu = mu_gcovs,
              var=sigma2x, scale=range, nu=nu)

plot(lg.s)

# Mean of process dependent on spatially-varying covariates ----------------------------------

# Note to self - the im function requires a matrix of a certain input order
# Terra::as.matrix just converts all cells to a matrix in the same order as they appear in the raster
# So we must reverse the order of the rows to get the correct order for the im function

# Convert raster to matrix object
cov1.mat <- terra::as.matrix(cov1, wide = T) %>% 
  .[nrow(.):1, ] # Reverse the order of the rows

cov2.mat <- terra::as.matrix(cov2, wide = T) %>% 
  .[nrow(.):1, ] 

mu_cov1 <- im(beta0 + beta1*cov1.mat, xcol=x0, yrow=y0)
mu_cov2 <- im(beta0 + beta2*cov2.mat, xcol=x0, yrow=y0)
mu_covs <- im(beta0 + beta1*cov1.mat + beta2*cov2.mat, xcol=x0, yrow=y0)
# mu_int <- im(beta0 + beta1*gridcov1 + beta2*gridcov2 + gridcov1*gridcov2*beta3, xcol=x0, yrow=y0)

plot(mu_cov1)
plot(mu_cov2)
plot(mu_covs)
# plot(mu_int)

# Set seed 
set.seed(seed)

# Create LGCP with environmental covariate
lg.s <- rLGCP('matern', mu = mu_covs,
              var=sigma2x, scale=range, nu=nu)
plot(lg.s)


# TO DOs ------------------------------------------------------------------

# How does virtual species code in a function for the mean??
# How do we include a habitat covariate and a latent process?
# Sample presence-only records from the latent process
# Figuring out sampling strategy for presence-only records
# To add: A separate realisation was generated from the log-Gaussian Cox process for each sampling process to represent the fact that the individuals sampled by each method were unlikely to be the same i.e. data collection were at different points in time.