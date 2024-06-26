

# Validate with independent Presence/Absence data -------------------------

# First, extract intensity predictions from locations of validation data 
# Then, calculate prediction accuracy with the Brier Score

# Using the median posterior prediction of probability of presence per cell. Using the Brier Score via the package 'DescTools'

library(DescTools)

for(i in seq_along(mod.list)) {
  
  # Extract the median prediction for each cell that has validation data
  val.med <- extract(mod.list[[i]]$preds.PROB.BUNGER$field$Median, PA_val[,1:2], xy = T)
  
  # Add the validation data P/A into the dataframe
  val.med <- val.med %>% 
    mutate(presence = PA_val$presence) %>% 
    filter(!is.na(Median))  
  
  print(paste0("Brier Score for ", names(mod.list)[i], ": ", DescTools::BrierScore(resp = val.med$presence,
                                                                  pred = val.med$Median)))
  
  
}


# Final Plots -------------------------------------------------------------

for(i in seq_along(mod.list)) {
  
  pred.prob.df <- as.data.frame(mod.list[[i]]$preds.PROB.VESTFOLD$field$Median, xy = T)
  
  p <- ggplot() +
    geom_tile(data = pred.prob.df, aes(x = x, y = y, fill = Median)) +
    scale_fill_viridis() +
    geom_point(data = PA_fit, aes(x = x, y = y, color = as.factor(presence)), size = 2) +
    coord_sf(
      xlim = c(st_bbox(Vestfold.landsat.sf)$xmin, st_bbox(Vestfold.landsat.sf)$xmax), 
      ylim = c(st_bbox(Vestfold.landsat.sf)$ymin, st_bbox(Vestfold.landsat.sf)$ymax)) +
    theme_bw()
  
  ggsave(plot = p, paste0(output.path, "/plot.prob.vs.true.vestfold_", names(mod.list)[i], ".png"), width = 10, height = 10, units = "in", dpi = 300)
  

}


for(i in seq_along(mod.list)) {
  
   pred.prob.df <- as.data.frame(mod.list[[i]]$preds.PROB.BUNGER$field$Median, xy = T)
  
  p <- ggplot() +
    geom_tile(data = pred.prob.df, aes(x = x, y = y, fill = Median)) +
    scale_fill_viridis() +
    geom_point(data = PA_val, aes(x = x, y = y, color = as.factor(presence)), size = 2) +
    theme_bw()
  
  ggsave(plot = p, paste0(output.path, "/plot.prob.vs.true.bunger_", names(mod.list)[i], ".png"), width = 10, height = 10, units = "in", dpi = 300)
  
}










#### RUNNING FOR BUNGER HILLS TEST 1.

# Extract the median prediction for each cell that has validation data
val.med <- extract(m.int.no.GRF$preds.BUNGER$field$Median, PA_val[,1:2], xy = T)

# Add the validation data P/A into the dataframe
val.med <- val.med %>% 
  mutate(presence = PA_val$presence) %>% 
  filter(!is.na(Median))

print(paste0("Brier Score for m.in.no.GRF: ", DescTools::BrierScore(resp = val.med$presence,
                                                                pred = val.med$Median)))
DescTools::BrierScore(resp = val.med$presence,
                      pred = val.med$Median)


png(here("output", "Prob.vs.True.BUNGER.m.int.no.GRF.png"), width = 10, height = 10, units = "in", res = 300)

pred.prob.df <- as.data.frame(m.int.no.GRF$preds.BUNGER$field$Median, xy = T)

ggplot() +
  geom_tile(data = pred.prob.df, aes(x = x, y = y, fill = Median)) +
  scale_fill_viridis() +
  geom_point(data = PA_val, aes(x = x, y = y, color = as.factor(presence)), size = 2) +
  theme_bw()

dev.off()

png(here("output", "Prob.vs.True.VESTFOLD.m.int.no.GRF.png"), width = 10, height = 10, units = "in", res = 300)

pred.prob.df <- as.data.frame(m.int.no.GRF$preds$field$Median, xy = T)

ggplot() +
  geom_tile(data = pred.prob.df, aes(x = x, y = y, fill = Median)) +
  scale_fill_viridis() +
  geom_point(data = PA_fit, aes(x = x, y = y, color = as.factor(presence)), size = 2) +
  coord_sf(
    xlim = c(st_bbox(Vestfold.landsat.sf)$xmin, st_bbox(Vestfold.landsat.sf)$xmax), 
    ylim = c(st_bbox(Vestfold.landsat.sf)$ymin, st_bbox(Vestfold.landsat.sf)$ymax)) +
  theme_bw()

dev.off()

##### RUNNING FOR BUNGER HILLS EXAMPLE TWO WITH PRESENCE-ONLY MODEL

# Extract the median prediction for each cell that has validation data
val.med <- extract(m.PO$preds.BUNGER$field$Median, PA_val[,1:2], xy = T)

# Add the validation data P/A into the dataframe
val.med <- val.med %>% 
  mutate(presence = PA_val$presence) %>% 
  filter(!is.na(Median))

print(paste0("Brier Score for m.in.no.GRF:", DescTools::BrierScore(resp = val.med$presence,
                                                                   pred = val.med$Median)))
DescTools::BrierScore(resp = val.med$presence,
                      pred = val.med$Median)


png(here("output", "Prob.vs.True.BUNGER.m.PO.png"), width = 10, height = 10, units = "in", res = 300)

pred.prob.df <- as.data.frame(m.PO$preds.BUNGER$field$Median, xy = T)

ggplot() +
  geom_tile(data = pred.prob.df, aes(x = x, y = y, fill = Median)) +
  scale_fill_viridis() +
  geom_point(data = PA_val, aes(x = x, y = y, color = as.factor(presence)), size = 2) +
  theme_bw()

dev.off()

png(here("output", "Prob.vs.True.VESTFOLD.m.PO.png"), width = 10, height = 10, units = "in", res = 300)

pred.prob.df <- as.data.frame(m.PO$preds.VESTFOLD$field$Median, xy = T)

ggplot() +
  geom_tile(data = pred.prob.df, aes(x = x, y = y, fill = Median)) +
  scale_fill_viridis() +
  geom_point(data = PA_fit, aes(x = x, y = y, color = as.factor(presence)), size = 2) +
  coord_sf(
    xlim = c(st_bbox(Vestfold.landsat.sf)$xmin, st_bbox(Vestfold.landsat.sf)$xmax), 
    ylim = c(st_bbox(Vestfold.landsat.sf)$ymin, st_bbox(Vestfold.landsat.sf)$ymax)) +
  theme_bw()

dev.off()


##### RUNNING FOR BUNGER HILLS EXAMPLE THREE WITH PRESENCE-ABSENCE MODEL

# Extract the median prediction for each cell that has validation data
val.med <- extract(m.PA$preds.BUNGER$field$Median, PA_val[,1:2], xy = T)

# Add the validation data P/A into the dataframe
val.med <- val.med %>% 
  mutate(presence = PA_val$presence) %>% 
  filter(!is.na(Median))

print(paste0("Brier Score for m.in.no.GRF:", DescTools::BrierScore(resp = val.med$presence,
                                                                   pred = val.med$Median)))
DescTools::BrierScore(resp = val.med$presence,
                      pred = val.med$Median)


png(here("output", "Prob.vs.True.BUNGER.m.PA.png"), width = 10, height = 10, units = "in", res = 300)

pred.prob.df <- as.data.frame(m.PA$preds.BUNGER$field$Median, xy = T)

ggplot() +
  geom_tile(data = pred.prob.df, aes(x = x, y = y, fill = Median)) +
  scale_fill_viridis() +
  geom_point(data = PA_val, aes(x = x, y = y, color = as.factor(presence)), size = 2) +
  theme_bw()

dev.off()

png(here("output", "Prob.vs.True.VESTFOLD.m.PA.png"), width = 10, height = 10, units = "in", res = 300)

pred.prob.df <- as.data.frame(m.PA$preds.VESTFOLD$field$Median, xy = T)

ggplot() +
  geom_tile(data = pred.prob.df, aes(x = x, y = y, fill = Median)) +
  scale_fill_viridis() +
  geom_point(data = PA_fit, aes(x = x, y = y, color = as.factor(presence)), size = 2) +
  coord_sf(
    xlim = c(st_bbox(Vestfold.landsat.sf)$xmin, st_bbox(Vestfold.landsat.sf)$xmax), 
    ylim = c(st_bbox(Vestfold.landsat.sf)$ymin, st_bbox(Vestfold.landsat.sf)$ymax)) +
  theme_bw()

dev.off()
