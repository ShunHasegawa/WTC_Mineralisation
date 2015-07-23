## ----Stat_WTC_Mine_Pmineralisation

range(Mine_DF$p.min)
bxplts(value = "p.min", ofst= 0.04, data = Mine_DF)

# The initial model is
Iml_pmin <- lmer(sqrt(p.min + .04) ~ temp * time + (1|chamber), data = Mine_DF)
Anova(Iml_pmin)

# The final model is
Fml_pmin <- stepLmer(Iml_pmin)
Anova(Fml_pmin)
AnvF_pmin <- Anova(Fml_pmin, test.statistic = "F")
AnvF_pmin

# model diagnosis
plot(Fml_pmin)
qqnorm(resid(Fml_pmin))
qqline(resid(Fml_pmin))

############################
# ANCOVA fit soil variable #
############################

#######################
# plot soil variables #
#######################
scatterplotMatrix(~ p.min + moist + Temp5_Mean, data = Mine_DF, diag = "boxplot", 
                  groups = Mine_DF$temp, by.group = TRUE)
scatterplotMatrix(~ sqrt(p.min + .04) + moist + Temp5_Mean, data = Mine_DF, diag = "boxplot", 
                  groups = Mine_DF$temp, by.group = TRUE)
# each chamber
xyplot(sqrt(p.min + .04) ~ moist|temp, groups = chamber, type = c("r", "p"), data = Mine_DF)
xyplot(sqrt(p.min + .04) ~ moist|chamber, type = c("r", "p"), data = Mine_DF)

# each time
xyplot(sqrt(p.min + .04) ~ moist|temp, groups = time, type = c("r", "p"), data = Mine_DF)
xyplot(sqrt(p.min + .04) ~ moist|time, type = c("r", "p"), data = Mine_DF)

m1 <- lmer(sqrt(p.min + .04) ~ temp * (Temp5_Mean + moist) + (1|chamber), data = Mine_DF)
m2 <- lmer(p.min ~ temp * (Temp5_Mean + moist) + (1|chamber), data = Mine_DF)
ldply(list(m1, m2), r.squared)
# no difference

Iml_ancv_pmin <- lmer(sqrt(p.min + .04) ~ temp * (moist + Temp5_Mean) + (1|chamber), data = Mine_DF)
Fml_ancv_pmin <- stepLmer(Iml_ancv_pmin, alpha.fixed = .1)
AnvF_ancv_pmin <- Anova(Fml_ancv_pmin, test.statistic = "F")
AnvF_ancv_pmin

par(mfrow = c(1, 2))
TransVirsreg(visreg(Fml_ancv_pmin, xvar = "Temp5_Mean", by = "temp", plot = FALSE),
             trans = function(x) x^2 - 0.04,
             overlay = TRUE, line = list(col = c(1, 2)))

TransVirsreg(visreg(Fml_ancv_pmin, xvar = "moist", by = "temp", plot = FALSE),
             trans = function(x) x^2 - 0.04,
             overlay = TRUE, line = list(col = c(1, 2)))


# model diagnosis
plot(Fml_ancv_pmin)
qqnorm(resid(Fml_ancv_pmin))
qqline(resid(Fml_ancv_pmin))

## ----Stat_WTC_Mine_Pmineralisation_Smmry
# The initial model is:
Iml_pmin@call

Anova(Iml_pmin)

# The final model is:
Fml_pmin@call

# Chi
Anova(Fml_pmin)
AnvF_pmin

# ANCOVA
Iml_ancv_pmin@call
Fml_ancv_pmin@call

Anova(Fml_ancv_pmin)
AnvF_ancv_pmin

par(mfrow = c(1, 2))
TransVirsreg(visreg(Fml_ancv_pmin, xvar = "Temp5_Mean", by = "temp", plot = FALSE),
             trans = function(x) x^2 - 0.04,
             overlay = TRUE, line = list(col = c(1, 2)))

TransVirsreg(visreg(Fml_ancv_pmin, xvar = "moist", by = "temp", plot = FALSE),
             trans = function(x) x^2 - 0.04,
             overlay = TRUE, line = list(col = c(1, 2)))
