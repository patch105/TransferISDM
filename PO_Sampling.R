


# Approach from Simmonds et al. -------------------------------------------

# Takes the coordinates of the randomly generated points
xy <- cbind(lg.s$x, lg.s$y)
# Access attribute (Lambda) of lg.s object and create Lam 
Lam <- attr(lg.s, 'Lambda') 
rf.s.vect <- as.vector(log(Lam$v))
rf.s <- log(Lam$v)

# dat1$Lam is the lambda attribute as above
maxprob <- 0.2

minprob = maxprob/10 # keep the relative difference between maximum and minimum probabilities the same across different scenarios of maxprob (i.e. strength of bias is the same)

probseq <-  exp(seq(log(maxprob), log(minprob), length.out = dim[1])) # Dim if from original code

# Makes a matrix with left to right decreasing probabilities 
# Remove t() to make it go top to bottom
bias.mat <- t(outer(y0,x0, function (x,y) probseq + 0*y + 0*x))

#### NOTE TO SELF - NEED TO CHECK IF CHANGING MELT XY ORDER HERE CHANGES ANYTHING

# Turn it into a df (x, y, bias amount)
bias.df <- bias.mat %>% 
  reshape2::melt(c("x", "y"), value.name = "bias")


# Plot bias grid
bias.df %>% 
  ggplot() + 
  geom_tile(aes(x = x, y = y, fill = bias)) + 
  scale_fill_viridis() +
  coord_fixed() + 
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank()) +
  ggtitle('Bias')

# Turn bias into a dataframe

# Make dataframe with xy location of points from LGCP
pp1 <- as.data.frame(xy)
names(pp1) <- c("x", "y")

# Add spatial bias info to PP data
pp2 <- merge(round(pp1), bias.df, by.x = c("x","y"), by.y = c("x","y"))

# Thin points using the detection probability
# Reducing also to presence absence here not abundance
pp2$presence <- rbinom(nrow(pp2),1,pp2$bias)

# Make it presence only data
thinpp <- pp2[pp2$presence == 1,]

# PLOT
ggplot() +
  geom_tile(data = bias.df, aes(x = x, y = y, fill = bias)) +
  scale_fill_viridis() +
  geom_point(data = thinpp, aes(x = x, y = y), color = "white", alpha = 0.5)+
  theme_bw() +
  theme(legend.ticks = element_blank())

# # Archive


# source('Generate strata levels Lam.R')

# #generate a stratification to use for sampling bias
# strata1 <- genStrataLam(dat1$Lam, strata = strata, rows = rows, cols = cols)
# 
# biasfield <- addSpatialBias(strata1, maxprob = probs[1], correlated = correlated, rho = rho)
# lookup <- data.frame(grid = 1:dim[1], probs = probseq)
# lookup$covariate <- lookup$probs
# 
# # Make spatial bias covariate
# biascov <- xtabs(covariate ~ y + x, data = biasfield)

