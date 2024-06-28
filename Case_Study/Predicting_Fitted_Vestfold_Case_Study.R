
# Run for intensity predictions first

for(i in seq_along(mod.list)) {
  
  if(names(mod.list)[i] == "PA.no.GRF" || names(mod.list)[i] == "PA.GRF") {
    
    mod.list[[i]]$preds.INT.VESTFOLD <- predict(mod.list[[i]],
                                                covars = East.Ant.covs.stk,
                                                S = 50, 
                                                intercept.terms = "PA_Intercept",
                                                type = "intensity")
    
    png(paste0(output.path, "/plot.pred.int.vestfold_", names(mod.list)[i], ".png"), width = 10, height = 10, units = "in", res = 300)
    
    plot(mod.list[[i]]$preds.INT.VESTFOLD$field[[2]])                           
    
    dev.off()
    
    # Run for probability next 
    
    
    mod.list[[i]]$preds.PROB.VESTFOLD <- predict(mod.list[[i]],
                                                 covars = East.Ant.covs.stk,
                                                 S = 50, 
                                                 intercept.terms = "PA_Intercept",
                                                 type = "probability")
    
    png(paste0(output.path, "/plot.pred.prob.vestfold_", names(mod.list)[i], ".png"), width = 10, height = 10, units = "in", res = 300)
    
    plot(mod.list[[i]]$preds.PROB.VESTFOLD$field[[2]])                           
    
    dev.off()
    
    
  } else {
    
    mod.list[[i]]$preds.INT.VESTFOLD <- predict(mod.list[[i]],
                                                covars = East.Ant.covs.stk,
                                                S = 50, 
                                                intercept.terms = "PO_Intercept",
                                                type = "intensity")
    
    png(paste0(output.path, "/plot.pred.int.vestfold_", names(mod.list)[i], ".png"), width = 10, height = 10, units = "in", res = 300)
    
    plot(mod.list[[i]]$preds.INT.VESTFOLD$field[[2]], nc = 3)                           
    
    dev.off()
    
    
    # Run for probability next 
    mod.list[[i]]$preds.PROB.VESTFOLD <- predict(mod.list[[i]],
                                                 covars = East.Ant.covs.stk,
                                                 S = 50, 
                                                 intercept.terms = "PO_Intercept",
                                                 type = "probability")
    
    png(paste0(output.path, "/plot.pred.prob.vestfold_", names(mod.list)[i], ".png"), width = 10, height = 10, units = "in", res = 300)
    
    plot(mod.list[[i]]$preds.PROB.VESTFOLD$field[[2]], nc = 3)                           
    
    dev.off()
  }}




# Now plot random field ---------------------------------------------------

m.int.GRF$preds.GRF <- predict(m.int.GRF, 
                               covars = East.Ant.covs.stk,
                               S = 30,
                               intercept.terms = "PO_Intercept",
                               type = "link",
                               includeRandom = T,
                               includeFixed = F)

png(paste0(output.path, "/GRF.plot_m.int.GRF.png"), width = 10, height = 10, units = "in", res = 300)
plot(m.int.GRF$preds.GRF$field[[2]])
dev.off()

m.PO.GRF$preds.GRF <- predict(m.PO.GRF, 
                              covars = East.Ant.covs.stk,
                              S = 30,
                              intercept.terms = "PO_Intercept",
                              type = "link",
                              includeRandom = T,
                              includeFixed = F)

png(paste0(output.path, "/GRF.plot_m.PO.GRF.png"))
plot(m.PO.GRF$preds.GRF$field[[2]])
dev.off()

m.PA.GRF$preds.GRF <- predict(m.PA.GRF, 
                              covars = East.Ant.covs.stk,
                              S = 30,
                              intercept.terms = "PA_Intercept",
                              type = "link",
                              includeRandom = T,
                              includeFixed = F)

png(paste0(output.path, "/GRF.plot_m.PA.GRF.png"),width = 10, height = 10, units = "in", res = 300)
plot(m.PO.GRF$preds.GRF$field[[2]])
dev.off()


## Another set of plots for GRF 
png(paste0(output.path, "/GRF.plot_ZOOMED.m.int.GRF.png"),width = 10, height = 10, units = "in", res = 300)
pred.GRF.df <- as.data.frame(m.int.GRF$preds.GRF$field$Median, xy = T)

ggplot() +
  geom_tile(data = pred.GRF.df, aes(x = x, y = y, fill = Median)) +
  scale_fill_viridis() +
  coord_sf(
    xlim = c(st_bbox(Vestfold.landsat.sf)$xmin, st_bbox(Vestfold.landsat.sf)$xmax), 
    ylim = c(st_bbox(Vestfold.landsat.sf)$ymin, st_bbox(Vestfold.landsat.sf)$ymax)) +
  theme_bw()

dev.off()

png(here("output", "GRF.plot.ZOOMED.m.PO.GRF.png"), width = 10, height = 10, units = "in", res = 300)

pred.GRF.df <- as.data.frame(m.PO.GRF$preds.GRF$field$Median, xy = T)

ggplot() +
  geom_tile(data = pred.GRF.df, aes(x = x, y = y, fill = Median)) +
  scale_fill_viridis() +
  coord_sf(
    xlim = c(st_bbox(Vestfold.landsat.sf)$xmin, st_bbox(Vestfold.landsat.sf)$xmax), 
    ylim = c(st_bbox(Vestfold.landsat.sf)$ymin, st_bbox(Vestfold.landsat.sf)$ymax)) +
  theme_bw()

dev.off()

png(here("output", "GRF.plot.ZOOMED.m.PA.GRF.png"), width = 10, height = 10, units = "in", res = 300)

pred.GRF.df <- as.data.frame(m.PA.GRF$preds.GRF$field$Median, xy = T)

ggplot() +
  geom_tile(data = pred.GRF.df, aes(x = x, y = y, fill = Median)) +
  scale_fill_viridis() +
  coord_sf(
    xlim = c(st_bbox(Vestfold.landsat.sf)$xmin, st_bbox(Vestfold.landsat.sf)$xmax), 
    ylim = c(st_bbox(Vestfold.landsat.sf)$ymin, st_bbox(Vestfold.landsat.sf)$ymax)) +
  theme_bw()

dev.off()

# Inference ---------------------------------------------------------------
# 
# # Adding a temporary cell area layer
# cov_inter <- c(East.Ant.covs.stk, East.Ant.covs.stk[[1]]) 
# names(cov_inter) <- c(names(East.Ant.covs.stk), "tmp.habiArea") # Rename the new covariate
# values(cov_inter$tmp.habiArea) <- 1
# 
# # ELEVATION
# 
# posterior_plots <- map(mod.list, function(x) {
#   
#   interpPreds <- predict(x, 
#                          covars=cov_inter,
#                          habitatArea= "tmp.habiArea", S=50,
#                          includeFixed="elev",# Include fixed effect
#                          includeRandom=FALSE, 
#                          type="link") # Difference is you use type = "link"
#   
#   # compile covariate and prediction
#   pred.df <- as.data.frame(cbind(elev = values(East.Ant.covs.stk$elev),
#                                  values(interpPreds$field[[c("Median", "Lower", "Upper")]]))) 
#   
#   # Plot
#   pred.df <- pred.df[!is.na(pred.df$elev),]
#   pred.df <- pred.df[order(pred.df$elev),]
#   
#   matplot(pred.df[,1], pred.df[,2:4], pch = "", xlab = "cov", ylab = "Effect",
#           main = "Effect plot for elevation")
#   
#   polygon( x=c( pred.df$elev, rev( pred.df$elev)),
#            c(pred.df$Upper, rev(pred.df$Lower)),
#            col=grey(0.95), bor=NA)
#   
#   lines( pred.df[,c("Elevation","Median")], type='l', lwd=2)
#   
#   
# })
# 
# # SLOPE
# 
# posterior_plots <- map(mod.list, function(x) {
#   
#   interpPreds <- predict(x, 
#                          covars=cov_inter,
#                          habitatArea= "tmp.habiArea", S=50,
#                          includeFixed="slope",# Include fixed effect
#                          includeRandom=FALSE, 
#                          type="link") # Difference is you use type = "link"
#   
#   # compile covariate and prediction
#   pred.df <- as.data.frame(cbind(slope = values(East.Ant.covs.stk$slope),
#                                  values(interpPreds$field[[c("Median", "Lower", "Upper")]]))) 
#   
#   # Plot
#   pred.df <- pred.df[!is.na(pred.df$slope),]
#   pred.df <- pred.df[order(pred.df$slope),]
#   
#   matplot(pred.df[,1], pred.df[,2:4], pch = "", xlab = "cov", ylab = "Effect",
#           main = "Effect plot for slope")
#   
#   polygon( x=c( pred.df$slope, rev( pred.df$slope)),
#            c(pred.df$Upper, rev(pred.df$Lower)),
#            col=grey(0.95), bor=NA)
#   
#   lines( pred.df[,c("Slope","Median")], type='l', lwd=2)
#   
#   
# })
# 
# # ASPECT
# 
# posterior_plots <- map(mod.list, function(x) {
#   
#   interpPreds <- predict(x, 
#                          covars=cov_inter,
#                          habitatArea= "tmp.habiArea", S=50,
#                          includeFixed="aspect",# Include fixed effect
#                          includeRandom=FALSE, 
#                          type="link") # Difference is you use type = "link"
#   
#   # compile covariate and prediction
#   pred.df <- as.data.frame(cbind(aspect = values(East.Ant.covs.stk$aspect),
#                                  values(interpPreds$field[[c("Median", "Lower", "Upper")]]))) 
#   
#   # Plot
#   pred.df <- pred.df[!is.na(pred.df$aspect),]
#   pred.df <- pred.df[order(pred.df$aspect),]
#   
#   matplot(pred.df[,1], pred.df[,2:4], pch = "", xlab = "cov", ylab = "Effect",
#           main = "Effect plot for aspect")
#   
#   polygon( x=c( pred.df$aspect, rev( pred.df$aspect)),
#            c(pred.df$Upper, rev(pred.df$Lower)),
#            col=grey(0.95), bor=NA)
#   
#   lines( pred.df[,c("Aspect","Median")], type='l', lwd=2)
#   
#   
# })

