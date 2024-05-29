m <- matrix(1:25, nrow=5, ncol=5)

m %>% reshape2::melt(c("y", "x"), value.name = "int") %>% rast(.) %>% plot(.)

m <- m[nrow(m):1,] %>% rast(.)

rm <- rast(m)
plot(rm)

c1 <- gridcov1 %>% 
  reshape2::melt(c("y", "x"), value.name = "cov") 