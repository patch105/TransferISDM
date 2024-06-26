
## Summary
map(mod.list, function(x) {summary(x)})

## Residual plots
output.path <- here("Case_Study/Figures")

## Residual plots
imap(mod.list, function(x, i) {
  
  png(paste0(output.path, "/plot.isdm_", names(mod.list)[i], ".png"), width = 10, height = 10, units = "in", res = 300)
  
  plot(x, nFigRow = 2, ask = FALSE)
  
  dev.off()
})


# Posteriors of marginal effects ------------------------------------------


map(mod.list, function(x) {
  
  ggplot(data = as.data.frame(x$mod$marginals.hyperpar$`Range for isdm.spat.XXX`)) + 
    geom_line(aes(x = x, y = y)) +
    ylab ("Posterior density")+
    xlab("GRF Range Parameter")
  
  
})


map(mod.list, function(x) {
ggplot(data = as.data.frame(x$mod$marginals.hyperpar$`Stdev for isdm.spat.XXX`)) + 
  geom_line(aes(x = x, y = y)) +
  ylab ("Posterior density")+
  xlab("GRF Std. Dev Parameter")  

})



