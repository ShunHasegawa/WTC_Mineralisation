## ----Stat_WTC_Mine_Nitrification
range(Mine_DF$nitrification)

bxplts(value= "nitrification", ofst= .8, data = Mine_DF)

# use square root
Iml_nit <- lmer(sqrt(nitrification + .8) ~ temp * time  + (1|chamber), data = Mine_DF)
Anova(Iml_nit)

Fml_nit <- stepLmer(Iml_nit)
Anova(Fml_nit)
AnvF_nit <- Anova(Fml_nit, test.statistic = "F")
AnvF_nit

# model diagnosis
plot(Fml_nit)
qqnorm(resid(Fml_nit))
qqline(resid(Fml_nit))

############################
# ANCOVA fit soil variable #
############################

#######################
# plot soil variables #
#######################
scatterplotMatrix(~ nitrification + moist + Temp5_Mean, data = Mine_DF, diag = "boxplot", 
                  groups = Mine_DF$temp, by.group = TRUE)
scatterplotMatrix(~ sqrt(nitrification + .8) + moist + Temp5_Mean, data = Mine_DF, diag = "boxplot", 
                  groups = Mine_DF$temp, by.group = TRUE)
# each chamber
xyplot(sqrt(nitrification + .8) ~ moist|temp, groups = chamber, type = c("r", "p"), data = Mine_DF)
xyplot(sqrt(nitrification + .8) ~ moist|chamber, type = c("r", "p"), data = Mine_DF)

# each time
xyplot(sqrt(nitrification + .8) ~ moist|temp, groups = time, type = c("r", "p"), data = Mine_DF)
xyplot(sqrt(nitrification + .8) ~ moist|time, type = c("r", "p"), data = Mine_DF)

Iml_ancv_nit <- lmer(sqrt(nitrification + .8) ~ temp * (moist + Temp5_Mean) + (1|chamber), data = Mine_DF)
Fml_ancv_nit <- Iml_ancv_nit
AnvF_ancv_nit <- Anova(Fml_ancv_nit, test.statistic = "F")
AnvF_ancv_nit
# none is significant

# model diagnosis
plot(Fml_ancv_nit)
qqnorm(resid(Fml_ancv_nit))
qqline(resid(Fml_ancv_nit))

# what if I rmeove outlier
ol <- which(qqnorm(resid(Fml_ancv_nit))$y == min(qqnorm(resid(Fml_ancv_nit))$y))
mm <- update(Fml_ancv_nit, subset = -ol)
Anova(mm, test.statistic = "F")
plot(mm)
qqnorm(resid(mm))
qqline(resid(mm))
  # interaction is indicated. but moisture range differs between teratments
ddply(Mine_DF, .(temp), summarise, range(moist))
m2 <- update(mm, subset = moist < 0.14)
Anova(m2) # no interaction in the same moisture range. so just use the 1st one

## ----Stat_WTC_Mine_Nitrification_Smmry
# The initial model is:
Iml_nit@call

Anova(Iml_nit)

# The final model is:
Fml_nit@call

# Chi
Anova(Fml_nit)

# F test
AnvF_nit

# ANCOVA
Iml_ancv_nit@call
Fml_ancv_nit@call
Anova(Fml_ancv_nit)
AnvF_ancv_nit
