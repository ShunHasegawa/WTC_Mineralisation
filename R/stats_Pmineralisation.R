range(mine$p.min)
bxplts(value = "p.min", ofst= 0.06, data = mine)
bxcxplts(value = "p.min", data = mine, sval = 0.06, fval = .1)

# log seems better

# different random factor structure
m1 <- lme(log(p.min + .06) ~ temp * time, random = ~1|chamber/side, data = mine)
m2 <- lme(log(p.min + .06) ~ temp * time, random = ~1|chamber, data = mine)
m3 <- lme(log(p.min + .06) ~ temp * time, random = ~1|id, data = mine)
anova(m1, m2, m3)
# m3 or (m2) is slightly better

# autocorrelation
atcr.cmpr(m3, rndmFac= "id")$models
# no need for auto-correlation

Iml <- atcr.cmpr(m3, rndmFac= "id")[[1]]

# The initial model is:
Iml$call
Anova(Iml)

# model simplification
MdlSmpl(Iml)
# interaction of temp x time is removed

Fml <- MdlSmpl(Iml)$model.reml

# The final model is:
Fml$call

Anova(Fml)

summary(Fml)

# model diagnosis
plot(Fml)
qqnorm(Fml, ~ resid(.)|id)
qqnorm(residuals.lm(Fml))
qqline(residuals.lm(Fml))
