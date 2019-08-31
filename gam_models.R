require(ggplot2)
require(rgdal)
require(rgeos)
require(raster)
require(ncdf4)
require(data.table)
require(forecast)
require(nlme)

require(mgcv)

data_dir = "C:/github/MPB_Overwintering_Projects/data/"

aggregate_means = fread(paste0(data_dir, "aggregate_forests/aggregate_means.csv"))
#aggregate_means$log_mortality = aggregate_means[, log(mortality)]

intvl = 10

intervals =  1:16

fit1 = gam(mortality ~ s(surv), data = dat)
fit1_ar = gamm(mortality ~ s(surv), data = dat, cor = corAR1(form = ~ year))
summary(fit1_ar)
summary(fit1)
plot(fit1_ar$lme)

plot(fit1)


table(aggregate_means$interval)
dat = aggregate_means[interval == intvl]

plt1 = ggplot(dat, aes(x = surv, y = mortality)) + geom_point()
sm = stat_smooth(method = "gam", formula = y ~ s(x), size = 1)

plt1 + sm






plt1 + stat_smooth(method = "gam", formula = y ~ s(x), size = 1)

plt1


p + 
  
  plt1 + sm

dat
fit1 = gam(mortality ~ s(surv), data = dat)
fit1 = gam(mortality ~ surv, data = dat)
summary(fit1)

pred1 = predict(fit1, se = TRUE, type = "response")
I1 = order(dat$surv)


> M3pred <- predict(M3, se = TRUE, type = "response")
> plot(Depth16, Sources16, type = "p")
> I1 <- order(Depth16)
> lines(Depth16[I1], M3pred$fit[I1], lty=1)
> lines(Depth16[I1], M3pred$fit[I1]+2*M3pred$se[I1],lty=2)
> lines(Depth16[I1], M3pred$fit[I1]-2*M3pred$se[I1],lty=2)


gam


plot(fit1, se = T)

plot(M3, se = TRUE)
> M3pred <- predict(M3, se = TRUE, type = "response")
> plot(Depth16, Sources16, type = "p")
> I1 <- order(Depth16)
> lines



  
  
  
#### gamm after Zuur 2009
intvl = 6
table(aggregate_means$interval)
dat = aggregate_means[interval == intvl]
dat
gamm_1 =    gamm(mortality ~ s(surv, bs = "cr"), data = dat, weights = varIdent(form = ~1))
gamm_1_ar = gamm(mortality ~ s(surv, bs = "cr"), data = dat, weights = varIdent(form = ~1), correlation = corAR1(form = ~ year))

gam_1 =    gam(mortality ~ s(surv, bs = "cr"), data = dat)
gam_1_ar = gam(mortality ~ s(surv, bs = "cr"), data = dat, correlation = corAR1(form = ~ year))


plot(gamm_1$gam)
plot(gamm_1_ar$gam)

summary(gamm_1$gam)
summary(gamm_1_ar$gam)

plot(gam_1)
plot(gam_1_ar)

anova(gam_1, gam_1_ar)

anova(gamm_1$gam, gamm_1_ar$gam)
AIC(gamm_1$lme, gamm_1_ar$lme)

AIC(gam_1_ar)
plot(gamm_1$lme)
plot(gamm_1_ar$lme)


summary(gamm_1$gam)

plot(fit1_ar$gam)


