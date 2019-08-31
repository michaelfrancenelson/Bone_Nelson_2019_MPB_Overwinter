require(ggplot2)
require(rgdal)
require(rgeos)
require(raster)
require(ncdf4)
require(data.table)
require(forecast)


data_dir = "C:/github/MPB_Overwintering_Projects/data/"

aggregate_means = fread(paste0(data_dir, "aggregate_forests/aggregate_means.csv"))
aggregate_means$log_mortality = aggregate_means[, log(mortality)]

i = 1

arimas = vector(mode = "list", length = 16)
log_arimas = vector(mode = "list", length = 16)
for (i in 1:16)
# for (i in c(1, 4, 8))
{
dat_i = aggregate_means[interval == i]
  arimas[[i]] = auto.arima(dat_i$mortality, xreg = c(dat_i$surv))
  log_arimas[[i]] = auto.arima(dat_i$log_mortality, xreg = c(dat_i$surv))
}






#### Choose arima order
i = 8
arimas_log_aic_test = vector(mode = "list", length = 16)

aic_tests = data.frame()


for(i in 1:16)
{
  print(i)
dat = aggregate_means[interval == i]
ar_0 = arima(dat$log_mortality, xreg = dat$surv, order = c(0, 0, 0))
ar_1 = arima(dat$log_mortality, xreg = dat$surv, order = c(1, 0, 0))
ar_2 = arima(dat$log_mortality, xreg = dat$surv, order = c(2, 0, 0))
aic_tests = rbind(aic_tests, c(i, AIC(ar_0, ar_1, ar_2)$AIC))
arimas_log_aic_test[[i]] = list(ar_0, ar_1, ar_2)
}


i = 10
plot(log_arimas[[i]]$residuals)
log_arimas[[i]]
shapiro.test(log_arimas[[i]]$residuals)

arimas_log_aic_test[[i]]

c(log_arimas[[i]]$residuals)

res_dat = data.frame(resid = c(log_arimas[[i]]$residuals[]), obs = 1:length(c(log_arimas[[i]]$residuals[])))

dwtest(resid ~ obs, data = res_dat)

plot(arimas_log_aic_test[[i]][[1]])
plot(residuals(arimas_log_aic_test[[i]][[3]])); abline(h = 0)
arima(dat$log_mortality, xreg = dat$surv, order = c(2, 0, 0))
auto.arima(dat$log_mortality, xreg = dat$surv)

apply(aic_tests[10:16, ], 2, mean)
plot(aic_tests[, 1], aic_tests[, 2])


arimas_log_aic_test

AIC(ar_0, ar_1, ar_2)

arimas_log_aic_tes

AIC(ar_0, ar_1, ar_2)
anova(ar_0, ar_1, ar_2)

plot(arimas[[i]])
summary(arimas[[i]])
summary(log_arimas[[i]])
plot(residuals(log_arimas[[i]]) ~ dat_i$surv)
var.test(arimas[[i]])

c(arimas[[i]]$coef)["xreg"]
log_coefs = sapply(1:16, function(i) c(log_arimas[[i]]$coef)["xreg"])
coefs = sapply(1:16, function(i) c(arimas[[i]]$coef)["xreg"])

plot(1:16, coefs)
plot(1:16, log_coefs)




i = 10


