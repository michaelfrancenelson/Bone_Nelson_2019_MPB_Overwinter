require(ggplot2)
require(rgdal)
require(rgeos)
require(raster)
require(ncdf4)
require(data.table)
require(forecast)
require(nlme)


data_dir = "C:/github/MPB_Overwintering_Projects/data/"

aggregate_means = fread(paste0(data_dir, "aggregate_forests/aggregate_means.csv"))
aggregate_means$log_mortality = aggregate_means[, log(mortality)]

aggregate_means


intvl = 9
fit1_ar_9 = gls(log(mortality) ~ surv, data = aggregate_means[interval == intvl], correlation = corAR1(form = ~year))

intvl = 2
fit1_ar_2 = gls(log(mortality) ~ surv, data = aggregate_means[interval == intvl], correlation = corAR1(form = ~year))


fit1_ = gls(log(mortality) ~ surv, data = aggregate_means[interval == intvl])

AIC(fit1_ar_9)
AIC(fit1_ar_2)




gamm


fit1_1 = lm(log(mortality) ~ surv, data = aggregate_means[interval == intvl])
summary(fit1_ar)
summary(fit1_)
str(fit1)
fit1$residuals

qqnorm(fit1_)
qqnorm(fit1_ar)
