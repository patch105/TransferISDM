
# Extract the 'true' intensity values
# Code modified from Simmonds et al. (2020).
# NOTE - Need to fix so I extract the median intensity, right now I think it's the mean

# True intensity
Lam <- attr(lg.s, "Lambda")

# Get the (v) log intensity values (expected number of points per unit area)
# NOTE - the format is from the lgcp package, so I need to reverse the order if want to plot
true_log_int<- log(Lam$v) 

# This code basically reverses the row order from 100 to 1 due to layout in lgcp output
# For plotting
true_log_int.rast <- true_log_int %>% 
  reshape2::melt(c("y", "x"), value.name = "int") %>% rast(.)

true_log_int %>% 
  reshape2::melt(c("y", "x"), value.name = "int") %>% 
  ggplot() + 
  geom_tile(aes(x = x, y = y, fill = int)) + 
  scale_fill_viridis() +
  coord_fixed() + 
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank()) +
  ggtitle('True log intensity')

plot(true_log_int.rast, main = "True log intensity", col = rev(heat.colors(100)))
plot(true_log_int, main = "True log intensity", col = rev(heat.colors(100)))

# QUESTION - HOW DOES RAST MAKE RASTER FROM MATRIX??
m <- matrix(1:25, nrow=5, ncol=5)

m %>% reshape2::melt(c("y", "x"), value.name = "int") %>% rast(.) %>% plot(.)

m <- m[nrow(m):1,] %>% rast(.)

rm <- rast(m)
plot(rm)

c1 <- gridcov1 %>% 
  reshape2::melt(c("y", "x"), value.name = "cov") 