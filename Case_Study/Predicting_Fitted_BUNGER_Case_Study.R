
gc()
rm(rema)
rm(slope)
rm(aspect)

for(i in seq_along(mod.list)) {
  
  if(names(mod.list)[i] == "PA.no.GRF" || names(mod.list)[i] == "PA.GRF") { 
    
    mod.list[[i]]$preds.INT.BUNGER <- predict(mod.list[[i]],
                                              covars = predictors.icefree.Bunger.crop,
                                              S = 50, 
                                              intercept.terms = "PA_Intercept",
                                              type = "intensity")
    
    png(paste0(output.path, "/plot.pred.int.bunger_", names(mod.list)[i], ".png"), width = 10, height = 10, units = "in", res = 300)
    
    plot(mod.list[[i]]$preds.INT.BUNGER$field[[2]])                           
    
    dev.off()
    
    
    # Run for probability next 
    mod.list[[i]]$preds.PROB.BUNGER <- predict(mod.list[[i]],
                                               covars = predictors.icefree.Bunger.crop,
                                               S = 50, 
                                               intercept.terms = "PA_Intercept",
                                               type = "probability")
    
    png(paste0(output.path, "/plot.pred.prob.bunger_", names(mod.list)[i], ".png"), width = 10, height = 10, units = "in", res = 300)
    
    plot(mod.list[[i]]$preds.PROB.BUNGER$field[[2]])                           
    
    dev.off() 
    
    
    } else {
  
  mod.list[[i]]$preds.INT.BUNGER <- predict(mod.list[[i]],
                                            covars = predictors.icefree.Bunger.crop,
                                            S = 50, 
                                            intercept.terms = "PO_Intercept",
                                            type = "intensity")
  
  png(paste0(output.path, "/plot.pred.int.bunger_", names(mod.list)[i], ".png"), width = 10, height = 10, units = "in", res = 300)
  
  plot(mod.list[[i]]$preds.INT.BUNGER$field[[2]])                           
  
  dev.off()


# Run for probability next 
  mod.list[[i]]$preds.PROB.BUNGER <- predict(mod.list[[i]],
                                             covars = predictors.icefree.Bunger.crop,
                                             S = 50, 
                                             intercept.terms = "PO_Intercept",
                                             type = "probability")
  
  png(paste0(output.path, "/plot.pred.prob.bunger_", names(mod.list)[i], ".png"), width = 10, height = 10, units = "in", res = 300)
  
  plot(mod.list[[i]]$preds.PROB.BUNGER$field[[2]])                           
  
  dev.off()
    }}


