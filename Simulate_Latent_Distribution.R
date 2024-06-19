
# Set Parameters Data Generation ----------------------------------------------------------

beta0 <- 5 # Intercept
beta1 <- 0.5 # Coefficient for cov 1
beta2 <- 0.1 # Coefficient for cov 2
# beta3 <- 5 # Coefficient for cov 1*2 interaction
var <- 1 # Variance of the Gaussian field (changed  from 0.5)  
scal <- 0.2 # Scale parameter 
nu <- 1 # Smoothness parameter - ONLY FOR MATERN
seed <- 3L

# Version 1. Mean of process dependent on continuous covariates ----------------------------------

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
              var=var, scale=scal, nu=nu)

plot(lg.s)


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

plot(mu)

# Set seed 
set.seed(seed)

# Create LGCP with environmental covariate
lg.s <- rLGCP('exp', mu = mu,
              var=var, scale=scal)
plot(lg.s)


# Version 2. Mean of process dependent on ONE spatially-varying covariates  ----------------------------------

# Note to self - the im function requires a matrix of a certain input order
# Terra::as.matrix just converts all cells to a matrix in the same order as they appear in the raster
# So we must reverse the order of the rows to get the correct order for the im function

# # Get coords of original raster
coords <- xyFromCell(cov1, 1:ncell(cov1))

# Convert raster to matrix object
# Convert raster to matrix object
cov1.df <- as.data.frame(cov1, xy = T)
cov1.mat <- as.matrix(cov1.df)
colnames(cov1.df) <- c("x", "y", "cov")
colnames(cov1.mat) <- c("x", "y", "cov")

fe <- beta0 + beta1*cov1.mat[, "cov"]

mu <- cov1.df %>% mutate(cov = fe)
mu <- spatstat.geom::as.im(mu)

plot(mu)

# Set seed 
set.seed(seed)

# Create LGCP with environmental covariate
lg.s <- rLGCP('exp', mu = mu,
              var=var, scale=scal)
plot(lg.s)


# Version 3. Mean of process dependent on TWO random covariates - XZ code ----------------

# Fixed effect (intercept + covariate effect)

fe <- beta0 + beta1*rand.cov1[,"cov"] + beta2*rand.cov2[,"cov"]

# fe <- cbind(1, cov[,"cov"]) %*% true_betas
# fe <- rep(6, n_bau_east * n_bau_north) # here we consider only an intercept, so no covariates

mu <- data.frame(x = coords[,1],  y = coords[, 2], z = fe)
mu <- spatstat.geom::as.im(mu, W = win)

plot(mu)

# Create LGCP with environmental covariate
lg.s <- rLGCP('exp', mu = mu,
              var=var, scale=scal)

plot(lg.s$x, lg.s$y)


# Mean of process dependent on ONE random covariates - XZ code ----------------

# Fixed effect (intercept + covariate effect)

fe <- beta0 + beta1*rand.cov1[,"cov"]

# fe <- cbind(1, cov[,"cov"]) %*% true_betas
# fe <- rep(6, n_bau_east * n_bau_north) # here we consider only an intercept, so no covariates

mu <- data.frame(x = coords[,1],  y = coords[, 2], z = fe)
mu <- spatstat.geom::as.im(mu, W = win)

plot(mu)

# Create LGCP with environmental covariate
lg.s <- rLGCP('exp', mu = mu,
              var=var, scale=scal)

plot(lg.s$x, lg.s$y)



# TO DOs ------------------------------------------------------------------

# How does virtual species code in a function for the mean??
# How do we include a habitat covariate and a latent process?
# Sample presence-only records from the latent process
# Figuring out sampling strategy for presence-only records
# To add: A separate realisation was generated from the log-Gaussian Cox process for each sampling process to represent the fact that the individuals sampled by each method were unlikely to be the same i.e. data collection were at different points in time.




### ARCHIVE 19/06/2024 #######

# 
# # Get coords of original raster
coords <- xyFromCell(cov1, 1:ncell(cov1))

# Convert raster to matrix object
cov1.df <- as.data.frame(cov1, xy = T)
cov1.mat <- as.matrix(cov1.df)
colnames(cov1.df) <- c("x", "y", "cov")
colnames(cov1.mat) <- c("x", "y", "cov")

# cov1.mat <- terra::as.matrix(cov1, wide = T) 
# 
# cov2.mat <- terra::as.matrix(cov2, wide = T) 
# 
# cov1.mat2 <- cov1.mat %>% 
#   reshape2::melt(c("x", "y"), value.name = "cov") 

# # GRF.cov1 <- cbind(x = coords[,1], y = coords[,2], cov = cov1.mat2["cov"]) 
# GRF.cov1 <- cbind(x = coords[,1], y = coords[,2], cov = as.vector(cov1.mat))
# 
# # cov2.mat2 <- cov2.mat %>% 
# #   reshape2::melt(c("x", "y"), value.name = "cov")
# 
# # GRF.cov2 <- cbind(x = coords[,1], y = coords[,2], cov = cov2.mat2["cov"])
# 
# # Can do with one or two covariates
# fe <- beta0 + beta1*GRF.cov1[, "cov"] + beta2*GRF.cov2[, "cov"]
fe <- beta0 + beta1*cov1.mat[, "cov"]
# fe <- beta0 + beta2*GRF.cov2[, "cov"]

# mu <- data.frame(x = coords[,1], y = coords[, 2], z = fe)
mu <- cov1.df %>% mutate(cov = fe)
mu <- spatstat.geom::as.im(mu)

plot(mu)


# Set seed 
set.seed(seed)

# Create LGCP with environmental covariate
lg.s <- rLGCP('exp', mu = mu,
              var=var, scale=scal)
plot(lg.s)