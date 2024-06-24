

# Validate with independent Presence/Absence data -------------------------

# First, extract intensity predictions from locations of validation data 
# Then, calculate prediction accuracy with the Brier Score

# Using the median posterior prediction of probability of presence per cell. Using the Brier Score via the package 'DescTools'

library(DescTools)

imap(mod.list, function(x, y) {
  
  
  # Extract the median prediction for each cell that has validation data
  val.med <- extract(x$preds.probs$field$Median, PA_val[,1:2], xy = T)
  
  # Add the validation data P/A into the dataframe
  val.med <- val.med %>% 
    mutate(presence = PA_val$presence)  
  
  print(paste0("Brier Score for ", y, ": ", DescTools::BrierScore(resp = val.med$presence,
                                                                  pred = val.med$Median)))
  
})



