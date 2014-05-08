range(mine$n.min)
bxplts(value= "n.min", ofst= 2.7, data = mine)

# remove the lowest value

bxplts(value= "n.min", ofst= 2.7, data = subset(mine, n.min > min(n.min)))
# homogeneity in variance is still highly violated but carry on anyway this time with log
N_min_rmOl <- subset(mine, n.min > min(n.min))


# different random factor structure
m1 <- lme(log(n.min + 2.7) ~ temp * time, random = ~1|chamber/side, data = N_min_rmOl)
m2 <- lme(log(n.min + 2.7) ~ temp * time, random = ~1|chamber, data = N_min_rmOl)
m3 <- lme(log(n.min + 2.7) ~ temp * time, random = ~1|id, data = N_min_rmOl)
anova(m1, m2, m3)
#m3 is slight ly better

# autocorrelation
atcr.cmpr(m3, rndmFac = "id")
# no need for autocorrelation

# model simplification
MdlSmpl(m3)
# interaction by temp x time and temp is removed

Fml <- MdlSmpl(m3)$model.reml

# The final model is
lme(log(n.min + 2.7) ~ time, random = ~1|id, data = N_min_rmOl)

Anova(Fml)
summary(Fml)
plot(allEffects(Fml))

# model diagnosis
plot(Fml)
qqnorm(Fml, ~ resid(.)|chamber)
qqnorm(residuals.lm(Fml))
qqline(residuals.lm(Fml))
# not very good....
