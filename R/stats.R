mine$id <- mine$chamber:mine$side

###################
# P minerlisation #
###################
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


#################
# Nitrification #
#################
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


####################
# N mineralisation #
####################
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
