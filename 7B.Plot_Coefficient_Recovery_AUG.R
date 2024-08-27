library(ggpubr)


# Pretend code
extrap.scenario.df <- data.frame(
  extrap.type = c("Low", "Low", "Low", "Low", "Mod", "Mod", "Mod", "Mod", "High", "High", "High", "High"),
  rep = c(1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2),
  mod.type = c("Integrated", "PO", "Integrated", "PO", "Integrated", "PO", "Integrated", "PO", "Integrated", "PO", "Integrated", "PO"),
  beta1 = c(0.48, 0.3, 0.52, 0.2, 0.49, 0.4, 0.51, 0.3, 0.5, 0.25, 0.53, 0.2),
  beta2 = c(0.09, 0.04, 0.095, 0.03, 0.1, 0.05, 0.105, 0.035, 0.11, 0.045, 0.115, 0.04),
  beta1_25 = c(0.48, 0.3, 0.52, 0.2, 0.49, 0.4, 0.51, 0.3, 0.5, 0.25, 0.53, 0.2) * 0.025,
  beta1_975 = c(0.48, 0.3, 0.52, 0.2, 0.49, 0.4, 0.51, 0.3, 0.5, 0.25, 0.53, 0.2) * 0.975,
  beta2_25 = c(0.09, 0.04, 0.095, 0.03, 0.1, 0.05, 0.105, 0.035, 0.11, 0.045, 0.115, 0.04) * 0.025,
  beta2_975 = c(0.09, 0.04, 0.095, 0.03, 0.1, 0.05, 0.105, 0.035, 0.11, 0.045, 0.115, 0.04) * 0.975
)

# 
# beta0 <- 5 # Intercept
# beta1 <- 0.5 # Coefficient for cov 1
# beta2 <- 0.1 # Coefficient for cov 2

# Plot the coefficients

b1 <- extrap.scenario.df %>% 
  ggplot(aes(x = extrap.type, y = beta1.mean, fill = mod.type)) +
  geom_boxplot() +
  # geom_hline(yintercept = beta1, linetype = "dashed", color = "red") +
  labs(x = "Extrapolation", y = expression(beta[1]), fill = "Model Type") +
  scale_x_discrete(labels = c("Low", "Mod", "High")) +
  scale_fill_manual(values = c("Integrated" = "purple", "PO" = "skyblue")) +
  theme_bw()

b2 <- extrap.scenario.df %>% 
  ggplot(aes(x = extrap.type, y = beta2.mean, fill = mod.type)) +
  geom_boxplot() +
  # geom_hline(yintercept = beta2, linetype = "dashed", color = "red") +
  labs(x = "Extrapolation", y = expression(beta[2]), fill = "Model Type") +
  scale_x_discrete(labels = c("Low", "Mod", "High")) +
  scale_fill_manual(values = c("Integrated" = "purple", "PO" = "skyblue")) +
  theme_bw()

beta_plot <- ggarrange(b1 , b2, common.legend = T,  ncol = 2, nrow = 1)

beta_plot

ggsave(plot = beta_plot, filename = paste0("output/Extrap_10_rep_PO_ISDM_beta_plot.png"), w = 21.5, h = 15, units = "cm", dpi = 400, device = "png")


#### Plot mean width of the credible interval #################

extrap.scenario.df.CI <- extrap.scenario.df %>% 
  mutate(beta1.cred.int = beta1_975 - beta1_25,
         beta2.cred.int = beta2_975 - beta2_25,
         beta1.cred.int.true = ifelse(beta1 >= beta1_25 &  beta1 <= beta1_975, 1, 0),
         beta2.cred.int.true = ifelse(beta2 >= beta2_25 & beta2 <= beta2_975, 1, 0))

b1.CI.width <- extrap.scenario.df.CI %>% 
  ggplot(aes(x = extrap.type, y = beta1.cred.int, fill = mod.type)) +
  geom_boxplot() +
  labs(x = "Extrapolation", y = bquote(beta[1] ~ " Credible Interval Width"), fill = "Model Type") +
  scale_x_discrete(labels = c("Low", "Mod", "High")) +
  scale_fill_manual(values = c("Integrated" = "purple", "PO" = "skyblue")) +
  theme_bw()

b2.CI.width <- extrap.scenario.df.CI %>% 
  ggplot(aes(x = extrap.type, y = beta2.cred.int, fill = mod.type)) +
  geom_boxplot() +
  labs(x = "Extrapolation", y = bquote(beta[2] ~ " Credible Interval Width"), fill = "Model Type") +
  scale_x_discrete(labels = c("Low", "Mod", "High")) +
  scale_fill_manual(values = c("Integrated" = "purple", "PO" = "skyblue")) +
  theme_bw()

beta_CI_width_plot <- ggarrange(b1.CI.width , b2.CI.width, common.legend = T,  ncol = 2, nrow = 1)

beta_CI_width_plot

ggsave(plot = beta_plot, filename = paste0("output/Extrap_10_rep_PO_ISDM_beta_CI_width_plot.png"), w = 21.5, h = 15, units = "cm", dpi = 400, device = "png")


# Plot whether credible interval contains true betas ----------------------

b1.CI.true <- extrap.scenario.df.CI %>% 
  ggplot(aes(x = extrap.type, y = beta1.cred.int.true, fill = mod.type)) +
  geom_boxplot() +
  labs(x = "Extrapolation", y = "Beta 1 Credible Interval Contains True Beta", fill = "Model Type") +
  scale_x_discrete(labels = c("Low", "Mod", "High")) +
  scale_fill_manual(values = c("Integrated" = "purple", "PO" = "skyblue")) +
  theme_bw()

b2.CI.true <- extrap.scenario.df.CI %>%
  ggplot(aes(x = extrap.type, y = beta2.cred.int.true, fill = mod.type)) +
  geom_boxplot() +
  labs(x = "Extrapolation", y = "Beta 2 Credible Interval Contains True Beta", fill = "Model Type") +
  scale_x_discrete(labels = c("Low", "Mod", "High")) +
  scale_fill_manual(values = c("Integrated" = "purple", "PO" = "skyblue")) +
  theme_bw()

beta_CI_true_plot <- ggarrange(b1.CI.true , b2.CI.true, common.legend = T,  ncol = 2, nrow = 1)

beta_CI_true_plot
