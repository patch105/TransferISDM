
## Summary
map(mod.list, function(x) {summary(x)})

## Residual plots
output.path <- here("Case_Study/Figures")

## Residual plots
for(i in seq_along(mod.list)) {
  
  png(paste0(output.path, "/residuals.isdm_", names(mod.list)[i], ".png"), width = 10, height = 10, units = "in", res = 300)
  
  plot(mod.list[[i]], nFigRow = 2, ask = FALSE)
  
  dev.off()
}


# Posteriors of marginal effects ------------------------------------------

mod.GRF.list <- list(m.int.GRF, m.PO.GRF, m.PA.GRF)

map(mod.GRF.list, function(x) {
  
  ggplot(data = as.data.frame(x$mod$marginals.hyperpar$`Range for isdm.spat.XXX`)) + 
    geom_line(aes(x = x, y = y)) +
    ylab ("Posterior density")+
    xlab("GRF Range Parameter")
  
  
})


map(mod.GRF.list, function(x) {
ggplot(data = as.data.frame(x$mod$marginals.hyperpar$`Stdev for isdm.spat.XXX`)) + 
  geom_line(aes(x = x, y = y)) +
  ylab ("Posterior density")+
  xlab("GRF Std. Dev Parameter")  

})



