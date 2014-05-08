## ----Stat_WTC_Mine_Nitrification
range(mine$nitrification)
bxplts(value= "nitrification", ofst= 2.6, data = mine)
bxcxplts(value= "nitrification", data = mine, sval = 2.531, fval = 3)

# remove the lowest value
NhrmOl <- subset(mine, nitrification > min(nitrification))

range(NhrmOl$nitrification)
bxplts(value= "nitrification", ofst= 1.74, data = NhrmOl)
bxcxplts(value= "nitrification", data = NhrmOl, sval = 1.74, fval = 3)
# homogeneity in variance is still highly violated
# but carry on anyway with box-cox lambda this time



# different random factor structure
m1 <- lme((nitrification + 1.74)^(0.7475) ~ temp * time, random = ~1|chamber/side, data = NhrmOl)
m2 <- lme((nitrification + 1.74)^(0.7475) ~ temp * time, random = ~1|chamber, data = NhrmOl)
m3 <- lme((nitrification + 1.74)^(0.7475) ~ temp * time, random = ~1|id, data = NhrmOl)
anova(m1, m2, m3)
#m3 is slight ly better

# autocorrelation
atcr.cmpr(m3, rndmFac = "id")$models
# no need for autocorrelation
Iml <- atcr.cmpr(m3, rndmFac = "id")[[1]]

# The initial model is:
Iml$call
Anova(Iml)

# model simplification
MdlSmpl(Iml)
# interaction by temp x time and temp is removed

Fml <- MdlSmpl(Iml)$model.reml

# The final model is
Fml$call

Anova(Fml)

summary(Fml)

# model diagnosis
plot(Fml)
qqnorm(Fml, ~ resid(.)|id)
qqnorm(residuals.lm(Fml))
qqline(residuals.lm(Fml))
# not very good....

## ----Stat_WTC_Mine_Nitrification_Smmry
# The initial model is:
Iml$call

Anova(Iml)

# The final model is:
Fml$call

Anova(Fml)
