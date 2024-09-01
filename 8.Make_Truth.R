

# 8. Make Truth -----------------------------------------------------------

# Extract the 'true' intensity (lambda) values from the LGCP model so that they can be compared with the estimated intensity

# The rLGCP function has produced a realisation of the LGCP but also saved the intensity values 

# Code modified from Simmonds et al. (2020).
# NOTE - Need to fix so I extract the median intensity, right now I think it's the mean

make_truth_func <- function() {
  
  
  
  # True intensity
  Lam <- attr(rep$latent.list$lg.s, "Lambda")
  
}
