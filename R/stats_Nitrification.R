range(mine$nitrification)
bxplts(value= "nitrification", ofst= 2.6, data = mine)
# remove the lowest value

bxplts(value= "nitrification", ofst= 2.6, data = subset(mine, nitrification > min(nitrification)))
# homogeneity in variance is still highly violated but carry on anyway this time with log
NhrmOl <- subset(mine, nitrification > min(nitrification))


# different random factor structure
m1 <- lme(log(nitrification + 2.6) ~ temp * time, random = ~1|chamber/side, data = NhrmOl)
m2 <- lme(log(nitrification + 2.6) ~ temp * time, random = ~1|chamber, data = NhrmOl)
m3 <- lme(log(nitrification + 2.6) ~ temp * time, random = ~1|id, data = NhrmOl)
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
lme(log(nitrification + 2.6) ~ time, random = ~1|id, data = NhrmOl)

Anova(Fml)
summary(Fml)
plot(allEffects(Fml))

# model diagnosis
plot(Fml)
qqnorm(Fml, ~ resid(.)|chamber)
qqnorm(residuals.lm(Fml))
qqline(residuals.lm(Fml))
# not very good....
