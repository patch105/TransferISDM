
m.PO.glm <- glm(formula = ,
                 family = poisson(link = "log"))


travers_vect <- vect(travers_sf)

covs.PA <- terra::extract(predictors.NorthEastAnt, travers_vect)

PA_fit.glm <- cbind(PA_fit, covs.PA[, 2:4])

m.PA.glm <- glm(formula = presence ~ 0 + elev + slope + aspect + offset(log(area)),
                data = PA_fit.glm,
                family = binomial(link = "logit"))
                

summary(m.PA.glm)
anova(m.PA.glm)
