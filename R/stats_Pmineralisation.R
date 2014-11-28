## ----Stat_WTC_Mine_Pmineralisation

range(mine$p.min)
bxplts(value = "p.min", ofst= 0.06, data = mine)
bxcxplts(value = "p.min", data = mine, sval = 0.06, fval = .1)
# log seems better

# The initial model is
Iml <- lmer(log(p.min + .06) ~ temp * time + (1|chamber) + (1|id), data = mine)
Anova(Iml)

# The final model is
Fml <- stepLmer(Iml)
Anova(Fml)
AnvF_P <- Anova(Fml, test.statistic = "F")
AnvF_P

summary(Fml)

plot(allEffects(Fml))

# model diagnosis
plot(Fml)
qqnorm(resid(Fml))
qqline(resid(Fml))

## ----Stat_WTC_Mine_Pmineralisation_Smmry
# The initial model is:
Iml@call

Anova(Iml)

# The final model is:
Fml@call

Anova(Fml)
AnvF_P
