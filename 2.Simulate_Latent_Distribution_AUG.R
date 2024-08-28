
# Set Parameters Data Generation ----------------------------------------------------------

beta0 <- 6 # Intercept
beta1 <- 0.5 # Coefficient for cov 1
beta2 <- 0.1 # Coefficient for cov 2
# beta3 <- 5 # Coefficient for cov 1*2 interaction
var <- 1 # Variance of the Gaussian field (changed  from 0.5)  
scal <- 0.2 # Scale parameter (range of spatial effect)
nu <- 1 # Smoothness parameter - ONLY FOR MATERN
seed <- 3L

# Assumed intensity at mean of enviro. variables
# log(lambda) = intercept + b1*(mean of cov1) + b2*(mean of cov2)
# exp(answer) is intensity
exp(beta0 + beta1*(0.5) + beta2*(0.5))

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

lg.s2 <- rLGCP('matern', mu = mu,
              var=var, scale=scal, nu=nu)

plot(lg.s)


# TO DOs ------------------------------------------------------------------

# How does virtual species code in a function for the mean??
# How do we include a habitat covariate and a latent process?
# Sample presence-only records from the latent process
# Figuring out sampling strategy for presence-only records
# To add: A separate realisation was generated from the log-Gaussian Cox process for each sampling process to represent the fact that the individuals sampled by each method were unlikely to be the same i.e. data collection were at different points in time.

