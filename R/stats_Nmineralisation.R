## ----Stat_WTC_Mine_Nmineralisation

range(Mine_DF$n.min)

bxplts(value= "n.min", ofst= 2, data = Mine_DF)

# use square root
Iml_nmin <- lmer(sqrt(n.min + 2) ~ temp * time  + (1|chamber), data = Mine_DF)
Anova(Iml_nmin)

Fml_nmin <- stepLmer(Iml_nmin)
AnvF_nmin <- Anova(Fml_nmin, test.statistic = "F")
AnvF_nmin

# model diagnosis
plot(Fml_nmin)
qqnorm(resid(Fml_nmin))
qqline(resid(Fml_nmin))

############################
# ANCOVA fit soil variable #
############################

#######################
# plot soil variables #
#######################
scatterplotMatrix(~ n.min + moist + Temp5_Mean, data = Mine_DF, diag = "boxplot", 
                  groups = Mine_DF$temp, by.group = TRUE)
scatterplotMatrix(~ sqrt(n.min + 2) + moist + Temp5_Mean, data = Mine_DF, diag = "boxplot", 
                  groups = Mine_DF$temp, by.group = TRUE)
# each chamber
xyplot(sqrt(n.min + 2) ~ moist|temp, groups = chamber, type = c("r", "p"), data = Mine_DF)
xyplot(sqrt(n.min + 2) ~ moist|chamber, type = c("r", "p"), data = Mine_DF)

# each time
xyplot(sqrt(n.min + 2) ~ moist|temp, groups = time, type = c("r", "p"), data = Mine_DF)
xyplot(sqrt(n.min + 2) ~ moist|time, type = c("r", "p"), data = Mine_DF)

Iml_ancv_nmin <- lmer(sqrt(n.min + 2) ~ temp * moist + (1|time) + (1|chamber), data = Mine_DF)
m2 <- update(Iml_ancv_nmin, ~. - (1|time))
m3 <- update(Iml_ancv_nmin, ~. - (1|chamber))
anova(Iml_ancv_nmin, m2, m3)

Anova(Iml_ancv_nmin)
Fml_ancv_nmin <- update(Iml_ancv_nmin, ~ . -temp:moist)
anova(Iml_ancv_nmin, Fml_ancv_nmin)
AnvF_ancv_nmin <- Anova(Fml_ancv_nmin, test.statistic = "F")
AnvF_ancv_nmin
# none is significant

# model diagnosis
plot(Fml_ancv_nmin)
qqnorm(resid(Fml_ancv_nmin))
qqline(resid(Fml_ancv_nmin))

# what if I rmeove outlier
ol <- which(qqnorm(resid(Fml_ancv_nmin))$y == min(qqnorm(resid(Fml_ancv_nmin))$y))
mm <- update(Fml_ancv_nmin, subset = -ol)
Anova(mm, test.statistic = "F")
plot(mm)
qqnorm(resid(mm))
qqline(resid(mm))
# interaction is indicated. but moisture range differs between teratments
ddply(Mine_DF, .(temp), summarise, range(moist))
m2 <- update(mm, subset = moist < 0.14)
Anova(m2) # no interaction in the same moisture range. so just use the 1st one

## ----Stat_WTC_Mine_Nmineralisation_Smmry
# The initial model is:
Iml_nmin@call

# The final model is:
Fml_nmin@call

# Chi
Anova(Fml_nmin)

# F test
AnvF_nmin

# ANCOVA
Iml_ancv_nmin@call
Fml_ancv_nmin@call
Anova(Fml_ancv_nmin)
AnvF_ancv_nmin
