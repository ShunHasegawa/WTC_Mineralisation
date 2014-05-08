range(mine$p.min)
bxplts(value = "p.min", ofst= 0.06, data = mine)
# sqrt seems better

# different random factor structure
m1 <- lme(sqrt(p.min + .06) ~ temp * time, random = ~1|chamber/side, data = mine)
m2 <- lme(sqrt(p.min + .06) ~ temp * time, random = ~1|chamber, data = mine)
m3 <- lme(sqrt(p.min + .06) ~ temp * time, random = ~1|id, data = mine)
anova(m1, m2, m3)
# m1 is slightly better

# autocorrelation
atcr.cmpr(m1, rndmFac= "chamber/side")
# no need for auto-correlation

# model simplification
MdlSmpl(m1)
# interaction of temp x time is removed

Fml <- MdlSmpl(m1)$model.reml

# The final model is
lme(sqrt(p.min + .06) ~ temp + time, random = ~1|chamber/side, data = mine)

Anova(Fml)
summary(Fml)
plot(allEffects(Fml))

# model diagnosis
plot(Fml)
qqnorm(Fml, ~ resid(.)|chamber)
qqnorm(residuals.lm(Fml))
qqline(residuals.lm(Fml))
