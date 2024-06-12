extrap.reps.out.mods
true_log_int

# Make a dataframe of the format I want to save the results in
true.validation.df <- data.frame(extrap.type = character(),
                                 rep = numeric(),
                                 mod.type = character(),
                                 correlation = numeric(),
                                 MAE = numeric(),
                                 RMSE = numeric(),
                                 Sum.Int.Score = numeric(),
                                 Mean.Int.Score = numeric())








imap(mod.list, function(x, y) {
  
  # Pull out the mean intensity prediction for each cell
  mean.int.pred <- x$preds$field$Mean
  
  differences <- mean.int.pred - truth_grid
  
  plot(differences)
  
  # Metrics from Simmonds et al. 
  # Compare the predicted intensity to the true intensity 
  print(paste0("Correlation:",  cor(as.vector(mean.int.pred), as.vector(truth_grid))))
  
  print(paste0("MAE of difference for ", y, ": ", mean(abs(as.vector(differences)))))
  
  print(paste0("Root Mean Square Error for ", y, ": ", Metrics::rmse(actual = as.vector(truth_grid), 
                                                                     predicted = as.vector(mean.int.pred)))) 
  
  ### Calculating the Interval Score ###
  
  # Pull out the lower and upper bounds of the prediction
  lower.int.pred <- x$preds$field$Lower
  
  upper.int.pred <- x$preds$field$Upper
  
  ## Function to calculate a different quantile
  
  # #Extract posterior samples
  # samples <- m.int$preds$cell.samples %>% 
  #   
  # # Function to calculate x% quantiles of each row
  # calculate_quantiles <- function(row) {
  #   quantiles <- quantile(row, c(0.025, 0.975))
  #   return(quantiles)
  # }
  # 
  # # Apply the function to each row of the matrix
  # quantiles_per_row <- t(apply(samples, 1, calculate_quantiles))  
  
  interval_score <- interval_score(true_values = as.vector(truth_grid), 
                                   lower = as.vector(lower.int.pred), 
                                   upper = as.vector(upper.int.pred),
                                   interval_range = 95,
                                   weigh = TRUE)
  
  print(paste0("Sum of Interval Score for ",y, ": ", sum(interval_score)))
  print(paste0("Mean of Interval Score for ", y, ": ", mean(interval_score)))
  
  # plot(interval_score)
  
})