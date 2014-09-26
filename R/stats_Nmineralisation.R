## ----Stat_WTC_Mine_Nmineralisation

range(mine$n.min)

bxplts(value= "n.min", ofst= 2.7, data = mine)
bxcxplts(value= "n.min", data = mine, sval = 2.6858, fval = 2.9)
# remove the lowest value

N_min_rmOl <- subset(mine, n.min > min(n.min))
range(N_min_rmOl$n.min)

bxplts(value= "n.min", ofst= 1.83, data = N_min_rmOl)
bxcxplts(value= "n.min", data = N_min_rmOl, sval = 1.822, fval = 3)
# any of transofmrtion seem to work well, just use row data this time

# different random factor structure
m1 <- lme(n.min ~ temp * time, random = ~1|chamber/side, data = N_min_rmOl)
m2 <- lme(n.min ~ temp * time, random = ~1|chamber, data = N_min_rmOl)
m3 <- lme(n.min ~ temp * time, random = ~1|id, data = N_min_rmOl)
anova(m1, m2, m3)
#m3 is slight ly better

# autocorrelation
atcr.cmpr(m3, rndmFac = "id")$models
# model5 is slightly better
Iml <- atcr.cmpr(m3, rndmFac = "id")[[5]]

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
qqnorm(Fml, ~ resid(.)|chamber)
qqnorm(residuals.lm(Fml))
qqline(residuals.lm(Fml))
# not very good....

## ----Stat_WTC_Mine_Nmineralisation_Smmry
# The initial model is:
Iml$call

Anova(Iml)

# The final model is:
Fml$call

Anova(Fml)
