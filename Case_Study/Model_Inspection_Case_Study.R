
## Summary
map(mod.list, function(x) {summary(x)})

## Residual plots
map(mod.list, function(x) {
  
  plot(x, nFigRow = 2, ask = FALSE)
  
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




