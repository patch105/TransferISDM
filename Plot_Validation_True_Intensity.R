library(ggpubr)

# Plot the validation

cor <- true.validation.df %>% 
  ggplot(aes(x = extrap.type, y = correlation, fill = mod.type)) +
  geom_boxplot() +
  labs(x = "Extrapolation", y = "Correlation", fill = "Model Type") +
  scale_x_discrete(labels = c("Low", "Mod", "High")) +
  scale_fill_manual(values = c("Integrated" = "purple", "PO" = "skyblue")) +
  theme_bw()

MAE <- true.validation.df %>% 
  ggplot(aes(x = extrap.type, y = MAE, fill = mod.type)) +
  geom_boxplot() +
  labs(x = "Extrapolation", y = "MAE", fill = "Model Type") +
  scale_x_discrete(labels = c("Low", "Mod", "High")) +
  scale_fill_manual(values = c("Integrated" = "purple", "PO" = "skyblue")) +
  theme_bw()

RMSE <- true.validation.df %>% 
  ggplot(aes(x = extrap.type, y = RMSE, fill = mod.type)) +
  geom_boxplot() +
  labs(x = "Extrapolation", y = "RMSE", fill = "Model Type") +
  scale_x_discrete(labels = c("Low", "Mod", "High")) +
  scale_fill_manual(values = c("Integrated" = "purple", "PO" = "skyblue")) +
  theme_bw()

Int.score.mean <- true.validation.df %>% 
  ggplot(aes(x = extrap.type, y = Mean.Int.Score, fill = mod.type)) +
  geom_boxplot() +
  labs(x = "Extrapolation", y = "Mean Interval Score", fill = "Model Type") +
  scale_x_discrete(labels = c("Low", "Mod", "High")) +
  scale_fill_manual(values = c("Integrated" = "purple", "PO" = "skyblue")) +
  theme_bw()

Int.score.sum <- true.validation.df %>% 
  ggplot(aes(x = extrap.type, y = Sum.Int.Score, fill = mod.type)) +
  geom_boxplot() +
  labs(x = "Extrapolation", y = "Sum Interval Score", fill = "Model Type") +
  scale_x_discrete(labels = c("Low", "Mod", "High")) +
  scale_fill_manual(values = c("Integrated" = "purple", "PO" = "skyblue")) +
  theme_bw()

p1 <- ggarrange(RMSE, MAE, common.legend = T,  ncol = 2, nrow = 1)

p2 <- ggarrange(Int.score.mean, Int.score.sum, common.legend = T,  ncol = 2, nrow = 1)

p1 

p2

cor

true_val__plot

ggsave(plot = p1, filename = paste0("output/Extrap_10_rep_PO_ISDM_RMSE_MAE_plot.png"), w = 21.5, h = 15, units = "cm", dpi = 400, device = "png")

ggsave(plot = p2, filename = paste0("output/Extrap_10_rep_PO_ISDM_Int_Score_plot.png"), w = 21.5, h = 15, units = "cm", dpi = 400, device = "png")
