


# Approach from Simmonds et al. -------------------------------------------

# Takes the coordinates of the randomly generated points
xy <- cbind(lg.s$x, lg.s$y)
# Access attribute (Lambda) of lg.s object and create Lam 
Lam <- attr(lg.s, 'Lambda') 
rf.s.vect <- as.vector(log(Lam$v))


source('Generate strata levels Lam.R')

#generate a stratification to use for sampling bias
strata1 <- genStrataLam(dat1$Lam, strata = strata, rows = rows, cols = cols)

biasfield <- addSpatialBias(strata1, maxprob = probs[1], correlated = correlated, rho = rho)

# dat1$Lam is the lambda attribute as above
maxprob <- 0.2

minprob = prob/10 # keep the relative difference between maximum and minimum probabilities the same across different scenarios of maxprob (i.e. strength of bias is the same)

probseq <-  exp(seq(log(maxprob), log(minprob), length.out = dim[1])) # Dim if from original code

# Makes a matrix with left to right decreasing probabilities 
# Remove t() to make it go top to bottom
biasgrid <- t(outer(y0,x0, function (x,y) probseq + 0*y + 0*x))

# Plot bias grid
biasgrid %>% 
  reshape2::melt(c("y", "x"), value.name = "bias") %>% 
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
pp2 <- merge(round(pp1), bias, by.x = c("x","y"), by.y = c("x","y"))

test <- round(pp1)

# # Archive
# lookup <- data.frame(grid = 1:dim[1], probs = probseq)
# lookup$covariate <- lookup$probs
# 
# # Make spatial bias covariate
# biascov <- xtabs(covariate ~ y + x, data = biasfield)

