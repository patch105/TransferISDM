m <- matrix(1:25, nrow=5, ncol=5)

m %>% reshape2::melt(c("y", "x"), value.name = "int") %>% rast(.) %>% plot(.)

m <- m[nrow(m):1,] %>% rast(.)

rm <- rast(m)
plot(rm)

c1 <- gridcov1 %>% 
  reshape2::melt(c("y", "x"), value.name = "cov") 


##############################################################################
# TEST CODE SPATSTAT TO TERRA
test <- matrix(c("A", "B", 
                 "C", "D"), 
               nrow = 2, 
               ncol = 2, 
               byrow = TRUE)

plot(rast(test))

test <- as.data.frame(test, xy  = T)

# Reverse the row order
test2 <- apply(test, 2, rev)

# Transpose the matrix to match the raster layout
test3 <- t(test2)

test4 <- test3 %>% 
  reshape2::melt(c("x", "y"), value.name = "int") 

true_log_int.rast <- cbind(x = coords[,1], y = coords[,2], true.int = true_log_int.melt["int"]) %>% rast(.)

true_log_int.rast <- cbind(x = coords[,1], y = coords[,2]) %>% rast(.)


## TEST CODE TERRA TO SPATSTAT
x_vals <- rep(1:2, each = 2)
y_vals <- rep(1:2, 2)

# Create a vector for z values
z_vals <- c(5,6,7,8)

# Create a data frame with x, y, and z values
fake_raster_df <- data.frame(x = x_vals, y = y_vals, z = z_vals)

# Create a SpatRaster object from the data frame
fake_raster <- rast(ncol = 2, nrow = 2, xmin = 1, xmax = 2, ymin = 1, ymax = 2)

# Assign z values to the raster
values(fake_raster) <- z_vals

plot(fake_raster)

test <- as.data.frame(fake_raster, xy  = T)


# Get coords of original raster
coords <- xyFromCell(fake_raster, 1:ncell(fake_raster))

# Convert raster to matrix object
cov1.mat <- terra::as.matrix(fake_raster, wide = T) 

df <- as.data.frame(fake_raster, xy = T)
test <- as.matrix(df)
colnames(test) <- c("x", "y", "cov")
colnames(df) <- c("x", "y", "cov")

cov1.mat2 <- cov1.mat %>% 
  reshape2::melt(c("x","y"), value.name = "cov") 

GRF.cov1 <- cbind(x = coords[,1], y = coords[,2], cov = cov1.mat2["cov"]) 
GRF.cov2 <- cbind(x = coords[,1], y = coords[,2], cov = as.vector(cov1.mat))

# Can do with one or two covariates
fe <- beta0 + beta1*GRF.cov1[, "cov"] 
fe <- beta0 + beta1*test[, "cov"] 
# fe <- beta0 + beta2*GRF.cov2[, "cov"]

mu <- data.frame(x = coords[,1], y = coords[, 2], z = fe)

mu <- df %>% mutate(cov = fe)

east_min <- 1
east_max <- 2
north_min <- 1
north_max <- 2
dom <- spatstat.geom::owin(c(east_min, east_max), c(north_min, north_max))

mu <- spatstat.geom::as.im(mu)

plot(mu)
