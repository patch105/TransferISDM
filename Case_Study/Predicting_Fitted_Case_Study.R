
# Run for intensity prediction first
for(i in 1:length(mod.list)) {
  
  mod.list[[i]]$preds <- predict(mod.list[[i]],
                                 covars = cov,
                                 S = 50, 
                                 intercept.terms = "PO_Intercept",
                                 type = "link")
  
  
  plot(mod.list[[i]]$preds$field[[1:3]], nc = 3)                           
  
}


# Now plot random field ---------------------------------------------------
# 
# for(i in 1:length(mod.list)) {
#   
#   mod.list[[i]]$preds.GRF <- predict(mod.list[[i]],
#                                      covars = cov,
#                                      S = 30, 
#                                      intercept.terms = "PO_Intercept",
#                                      type = "link",
#                                      includeRandom = TRUE,
#                                      includeFixed = FALSE)
#   
#   
#   plot(mod.list[[i]]$preds.GRF$field[[1:3]], nc = 3)                           
#   
# }


# Inference ---------------------------------------------------------------

# Adding a temporary cell area layer
cov_inter <- c(East.Ant.covs.stk, East.Ant.covs.stk[[1]]) 
names(cov_inter) <- c(names(East.Ant.covs.stk), "tmp.habiArea") # Rename the new covariate
values(cov_inter$tmp.habiArea) <- 1

# ELEVATION

posterior_plots <- map(mod.list, function(x) {
  
  interpPreds <- predict(x, 
                         covars=cov_inter,
                         habitatArea= "tmp.habiArea", S=50,
                         includeFixed="elev",# Include fixed effect
                         includeRandom=FALSE, 
                         type="link") # Difference is you use type = "link"
  
  # compile covariate and prediction
  pred.df <- as.data.frame(cbind(elev = values(East.Ant.covs.stk$elev),
                                 values(interpPreds$field[[c("Median", "Lower", "Upper")]]))) 
  
  # Plot
  pred.df <- pred.df[!is.na(pred.df$elev),]
  pred.df <- pred.df[order(pred.df$elev),]
  
  matplot(pred.df[,1], pred.df[,2:4], pch = "", xlab = "cov", ylab = "Effect",
          main = "Effect plot for elevation")
  
  polygon( x=c( pred.df$elev, rev( pred.df$elev)),
           c(pred.df$Upper, rev(pred.df$Lower)),
           col=grey(0.95), bor=NA)
  
  lines( pred.df[,c("Elevation","Median")], type='l', lwd=2)
  
  
})

# SLOPE

posterior_plots <- map(mod.list, function(x) {
  
  interpPreds <- predict(x, 
                         covars=cov_inter,
                         habitatArea= "tmp.habiArea", S=50,
                         includeFixed="slope",# Include fixed effect
                         includeRandom=FALSE, 
                         type="link") # Difference is you use type = "link"
  
  # compile covariate and prediction
  pred.df <- as.data.frame(cbind(slope = values(East.Ant.covs.stk$slope),
                                 values(interpPreds$field[[c("Median", "Lower", "Upper")]]))) 
  
  # Plot
  pred.df <- pred.df[!is.na(pred.df$slope),]
  pred.df <- pred.df[order(pred.df$slope),]
  
  matplot(pred.df[,1], pred.df[,2:4], pch = "", xlab = "cov", ylab = "Effect",
          main = "Effect plot for slope")
  
  polygon( x=c( pred.df$slope, rev( pred.df$slope)),
           c(pred.df$Upper, rev(pred.df$Lower)),
           col=grey(0.95), bor=NA)
  
  lines( pred.df[,c("Slope","Median")], type='l', lwd=2)
  
  
})

# ASPECT

posterior_plots <- map(mod.list, function(x) {
  
  interpPreds <- predict(x, 
                         covars=cov_inter,
                         habitatArea= "tmp.habiArea", S=50,
                         includeFixed="aspect",# Include fixed effect
                         includeRandom=FALSE, 
                         type="link") # Difference is you use type = "link"
  
  # compile covariate and prediction
  pred.df <- as.data.frame(cbind(aspect = values(East.Ant.covs.stk$aspect),
                                 values(interpPreds$field[[c("Median", "Lower", "Upper")]]))) 
  
  # Plot
  pred.df <- pred.df[!is.na(pred.df$aspect),]
  pred.df <- pred.df[order(pred.df$aspect),]
  
  matplot(pred.df[,1], pred.df[,2:4], pch = "", xlab = "cov", ylab = "Effect",
          main = "Effect plot for aspect")
  
  polygon( x=c( pred.df$aspect, rev( pred.df$aspect)),
           c(pred.df$Upper, rev(pred.df$Lower)),
           col=grey(0.95), bor=NA)
  
  lines( pred.df[,c("Aspect","Median")], type='l', lwd=2)
  
  
})

