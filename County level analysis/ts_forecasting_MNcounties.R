# Repeat for MN Counties
# Anoka
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(tsAnokaMN)
Anoka_cases <- tsAnokaMN[, "Cases"]
Anoka_cases_train <- Anoka_cases %>% window(end = c(2020, 120))
Anoka_cases_test <- Anoka_cases %>% window(end = c(2020, 130))
Anoka_deaths <- tsAnokaMN[, "Deaths"]
Anoka_deaths_train <- Anoka_deaths %>% window(end = c(2020, 120))
Anoka_deaths_test <- Anoka_deaths %>% window(end = c(2020, 130))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Anoka_cases <- auto.arima(Anoka_cases, stepwise = FALSE, approximation = FALSE)
fit_Anoka_cases # ARIMA(2, 1, 3), AICc - 614.11
autoplot(fit_Anoka_cases)
sarima.for(Anoka_cases_train, n.ahead = 20, 2, 1, 3) # error?!?
checkresiduals(fit_Anoka_cases) # passes Ljung Box test
fit_Anoka_deaths <- auto.arima(Anoka_deaths, stepwise = FALSE, approximation = FALSE)
fit_Anoka_deaths # ARIMA(4, 1, 1), AICc - 162.94
autoplot(fit_Anoka_deaths)
sarima.for(Anoka_deaths_train, n.ahead = 20, 4, 1, 1) # error?!?
checkresiduals(fit_Anoka_deaths) # passes
# Use best models to forecast further ahead
fc_10_Anoka <- sarima.for(Anoka_cases, n.ahead = 10, 2, 1, 3)
fc_10_Anoka$pred
fcd_10_Anoka <- sarima.for(Anoka_deaths, n.ahead = 10, 4, 1, 1) 
fcd_10_Anoka$pred

# Clay
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(tsClayMN)
Clay_cases <- tsClayMN[, "Cases"]
Clay_cases_train <- Clay_cases %>% window(end = c(2020, 120))
Clay_cases_test <- Clay_cases %>% window(end = c(2020, 130))
Clay_deaths <- tsClayMN[, "Deaths"]
Clay_deaths_train <- Clay_deaths %>% window(end = c(2020, 120))
Clay_deaths_test <- Clay_deaths %>% window(end = c(2020, 130))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Clay_cases <- auto.arima(Clay_cases, stepwise = FALSE, approximation = FALSE)
fit_Clay_cases # ARIMA(3, 1, 0), AICc - 570.6
autoplot(fit_Clay_cases)
sarima.for(Clay_cases_train, n.ahead = 20, 3, 1, 0)
lines(Clay_cases_test)
checkresiduals(fit_Clay_cases) # passes
fit_Clay_deaths <- auto.arima(Clay_deaths, stepwise = FALSE, approximation = FALSE)
fit_Clay_deaths # ARIMA(0, 1, 5), AICc - 117.18
autoplot(fit_Clay_deaths)
sarima.for(Clay_deaths_train, n.ahead = 20, 0, 1, 5)
lines(Clay_deaths_test)
checkresiduals(fit_Clay_deaths) # passes
# Use best models to forecast further ahead
fc_10_Clay <- sarima.for(Clay_cases, n.ahead = 10, 3, 1, 0)
fc_10_Clay$pred
fcd_10_Clay <- sarima.for(Clay_deaths, n.ahead = 10, 0, 1, 5) 
fcd_10_Clay$pred

# Dakota
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(tsDakotaMN)
Dakota_cases <- tsDakotaMN[, "Cases"]
Dakota_cases_train <- Dakota_cases %>% window(end = c(2020, 120))
Dakota_cases_test <- Dakota_cases %>% window(end = c(2020, 130))
Dakota_deaths <- tsDakotaMN[, "Deaths"]
Dakota_deaths_train <- Dakota_deaths %>% window(end = c(2020, 120))
Dakota_deaths_test <- Dakota_deaths %>% window(end = c(2020, 130))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Dakota_cases <- auto.arima(Dakota_cases, stepwise = FALSE, approximation = FALSE)
fit_Dakota_cases # ARIMA(4, 1, 1), AICc - 602.04
autoplot(fit_Dakota_cases)
sarima.for(Dakota_cases_train, n.ahead = 20, 4, 1, 1)
lines(Dakota_cases_test)
checkresiduals(fit_Dakota_cases) # passes
fit_Dakota_deaths <- auto.arima(Dakota_deaths, stepwise = FALSE, approximation = FALSE)
fit_Dakota_deaths # ARIMA(0, 1, 5), AICc - 117.18
autoplot(fit_Dakota_deaths)
sarima.for(Dakota_deaths_train, n.ahead = 20, 0, 1, 5)
lines(Dakota_deaths_test)
checkresiduals(fit_Dakota_deaths) # passes
# Use best models to forecast further ahead
fc_10_Dakota <- sarima.for(Dakota_cases, n.ahead = 10, 4, 1, 1)
fc_10_Dakota$pred
fcd_10_Dakota <- sarima.for(Dakota_deaths, n.ahead = 10, 0, 1, 5) 
fcd_10_Dakota$pred

# Hennepin
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(tsHennepinMN)
Hennepin_train <- tsHennepinMN %>% window(end = c(2020, 120))
Hennepin_test <- tsHennepinMN %>% window(start = c(2020, 121), end = c(2020, 130))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Hennepin_cases <- auto.arima(Hennepin_train[, "Cases"], stepwise = FALSE, approximation = FALSE)
fit_Hennepin_cases # ARIMA(0, 1, 5), AICc - 841.86
autoplot(fit_Hennepin_cases)
checkresiduals(fit_Hennepin_cases) # passes
fit_Hennepin_cases <- sarima.for(Hennepin_train[, "Cases"], n.ahead = 20, 0, 1, 5); lines(Hennepin_test[, "Cases"])
RMSE(fit_Hennepin_cases$pred, MN_test[, "Cases"])/mean(MN_test[, "Cases"]) # 0.87
fit_Hennepin_deaths <- auto.arima(Hennepin_train[, "Deaths"], stepwise = FALSE, approximation = FALSE)
fit_Hennepin_deaths # ARIMA(5, 1, 0), AICc - 438.16
autoplot(fit_Hennepin_deaths)
checkresiduals(fit_Hennepin_deaths) # passes
fit_Hennepin_deaths <- sarima.for(Hennepin_train[, "Deaths"], n.ahead = 10, 5, 1, 0); lines(Hennepin_test[, "Deaths"])
RMSE(fit_Hennepin_deaths$pred, MN_test[, "Deaths"])/mean(MN_test[, "Deaths"]) # 0.59
# Use best models to forecast further ahead
fc_10_Hennepin <- sarima.for(Hennepin_cases, n.ahead = 10, 0, 1, 5)
fc_10_Hennepin$pred
fcd_10_Hennepin <- sarima.for(Hennepin_deaths, n.ahead = 10, 4, 1, 1) 
fcd_10_Hennepin$pred

# Kandiyohi
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(tsKandiyohiMN)
Kandi_train <- tsKandiyohiMN %>% window(end = c(2020, 120))
Kandi_test <- tsKandiyohiMN %>% window(start = c(2020, 121), end = c(2020, 130))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Kandiyohi_cases <- auto.arima(Kandi_train[, "Cases"], stepwise = FALSE, approximation = FALSE)
fit_Kandiyohi_cases # ARIMA(3, 1, 2), AICc - 371.47
autoplot(fit_Kandiyohi_cases)
checkresiduals(fit_Kandiyohi_cases) # passes
fit_Kandiyohi_cases <- sarima.for(Kandi_train[, "Cases"], n.ahead = 10, 3, 1, 2); lines(Kandi_test[, "Cases"])
RMSE(fit_Kandiyohi_cases$pred, MN_test[, "Cases"])/mean(MN_test[, "Cases"]) # 0.98
fit_Kandiyohi_deaths <- auto.arima(Kandi_train[, "Deaths"], stepwise = FALSE, approximation = FALSE)
fit_Kandiyohi_deaths # ARIMA(0, 0, 0), AICc - 169.17 White Noise Model
checkresiduals(fit_Kandiyohi_deaths) # passes
fit_Kandiyohi_deaths <- sarima.for(Kandi_train[, "Deaths"], n.ahead = 10, 0, 0, 0); lines(Kandi_test[, "Deaths"])
RMSE(fit_Kandiyohi_deaths$pred, MN_test[, "Cases"])/mean(MN_test[, "Cases"]) # 1.02

# Use best models to forecast further ahead
fc_10_Kandiyohi <- sarima.for(Kandiyohi_cases, n.ahead = 10, 4, 1, 0)
fc_10_Kandiyohi$pred
fcd_10_Kandiyohi <- sarima.for(Kandiyohi_deaths, n.ahead = 10, 0, 0, 0) 
fcd_10_Kandiyohi$pred

# Nobles
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(tsNoblesMN)
Nobles_cases <- tsNoblesMN[, "Cases"]
Nobles_cases_train <- Nobles_cases %>% window(end = c(2020, 120))
Nobles_cases_test <- Nobles_cases %>% window(end = c(2020, 130))
Nobles_deaths <- tsNoblesMN[, "Deaths"]
Nobles_deaths_train <- Nobles_deaths %>% window(end = c(2020, 120))
Nobles_deaths_test <- Nobles_deaths %>% window(end = c(2020, 130))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Nobles_cases <- auto.arima(Nobles_cases, stepwise = FALSE, approximation = FALSE)
fit_Nobles_cases # ARIMA(1, 1, 3), AICc - 864.04
autoplot(fit_Nobles_cases)
sarima.for(Nobles_cases_train, n.ahead = 20, 1, 1, 3)
lines(Nobles_cases_test)
checkresiduals(fit_Nobles_cases) # passes
fit_Nobles_deaths <- auto.arima(Nobles_deaths, stepwise = FALSE, approximation = FALSE)
fit_Nobles_deaths # ARIMA(0, 0, 0), AICc - 122.28 White Noise Model
autoplot(fit_Nobles_deaths)
sarima.for(Nobles_deaths_train, n.ahead = 20, 0, 0, 0)
lines(Nobles_deaths_test)
checkresiduals(fit_Nobles_deaths) # passes
# Use best models to forecast further ahead
fc_10_Nobles <- sarima.for(Nobles_cases, n.ahead = 10, 1, 1, 3)
fc_10_Nobles$pred
fcd_10_Nobles <- sarima.for(Nobles_deaths, n.ahead = 10, 0, 0, 0) 
fcd_10_Nobles$pred

# Ramsey
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(tsRamseyMN)
Ramsey_cases <- tsRamseyMN[, "Cases"]
Ramsey_cases_train <- Ramsey_cases %>% window(end = c(2020, 120))
Ramsey_cases_test <- Ramsey_cases %>% window(end = c(2020, 130))
Ramsey_deaths <- tsRamseyMN[, "Deaths"]
Ramsey_deaths_train <- Ramsey_deaths %>% window(end = c(2020, 120))
Ramsey_deaths_test <- Ramsey_deaths %>% window(end = c(2020, 130))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Ramsey_cases <- auto.arima(Ramsey_cases, stepwise = FALSE, approximation = FALSE)
fit_Ramsey_cases # ARIMA(5, 1, 0), AICc - 647.71
autoplot(fit_Ramsey_cases)
sarima.for(Ramsey_cases_train, n.ahead = 20, 5, 1, 0)
lines(Ramsey_cases_test)
checkresiduals(fit_Ramsey_cases) # does not pass
fit_Ramsey_cases2 <- arima(Ramsey_cases, order = c(6, 3, 5))
checkresiduals(fit_Ramsey_cases2) # passes
fit_Ramsey_deaths <- auto.arima(Ramsey_deaths, stepwise = FALSE, approximation = FALSE)
fit_Ramsey_deaths # ARIMA(0, 1, 1), AICc - 224.01 White Noise Model
autoplot(fit_Ramsey_deaths)
sarima.for(Ramsey_deaths_train, n.ahead = 20, 0, 1, 1)
lines(Ramsey_deaths_test)
checkresiduals(fit_Ramsey_deaths) # does not pass
fit_Ramsey_deaths2 <- arima(Ramsey_deaths, order = c(1, 1, 2))
checkresiduals(fit_Ramsey_deaths2) # passes
# Use best models to forecast further ahead
fc_10_Ramsey <- sarima.for(Ramsey_cases, n.ahead = 10, 6, 3, 5)
fc_10_Ramsey$pred
fcd_10_Ramsey <- sarima.for(Ramsey_deaths, n.ahead = 10, 1, 1, 2) 
fcd_10_Ramsey$pred

# Stearns
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(tsStearnsMN)
Stearns_train <- tsStearnsMN %>% window(end = c(2020, 120))
Stearns_test <- tsStearnsMN %>% window(start = c(2020, 121), end = c(2020, 130))
fit_Stearns_cases <- auto.arima(Stearns_train[, "Cases"], stepwise = FALSE, approximation = FALSE)
fit_Stearns_cases # ARIMA(1, 1, 4), AICc - 543.66
checkresiduals(fit_Stearns_cases) # passes
fit_Stearns_cases <- sarima.for(Stearns_train[, "Cases"], n.ahead = 10, 1, 1, 4); lines(Stearns_test[, "Cases"])
RMSE(fit_Stearns_cases$pred, MN_test[, "Cases"])/mean(MN_test[, "Cases"]) # 0.59
fit_Stearns_deaths <- auto.arima(Stearns_train[, "Deaths"], stepwise = FALSE, approximation = FALSE)
fit_Stearns_deaths # ARIMA(0, 0, 0), AICc - Inf White Noise Model
checkresiduals(fit_Stearns_deaths) # p-value NA
fit_Stearns_deaths <- sarima.for(Stearns_train[, "Deaths"], n.ahead = 10, 0, 0, 0); lines(Stearns_test[, "Deaths"])
RMSE(fit_Kandiyohi_deaths$pred, MN_test[, "Cases"])/mean(MN_test[, "Cases"]) # error - White Noise Model
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Stearns_cases <- auto.arima(Stearns_cases, stepwise = FALSE, approximation = FALSE)
fit_Stearns_cases # ARIMA(0, 1, 4), AICc - 887.11
autoplot(fit_Stearns_cases)
sarima.for(Stearns_cases_train, n.ahead = 20, 0, 1, 4)
lines(Stearns_cases_test)
checkresiduals(fit_Stearns_cases) # passes
fit_Stearns_deaths <- auto.arima(Stearns_deaths, stepwise = FALSE, approximation = FALSE)
fit_Stearns_deaths # ARIMA(2, 1, 0), AICc - 35.04
autoplot(fit_Stearns_deaths)
sarima.for(Stearns_deaths_train, n.ahead = 20, 2, 1, 0)
lines(Stearns_deaths_test)
checkresiduals(fit_Stearns_deaths) # passes
# Use best models to forecast further ahead
fc_10_Stearns <- sarima.for(Stearns_cases, n.ahead = 10, 0, 1, 4)
fc_10_Stearns$pred
fcd_10_Stearns <- sarima.for(Stearns_deaths, n.ahead = 10, 2, 1, 0) 
fcd_10_Stearns$pred

# Washington
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(tsWashingtonMN)
Washington_cases <- tsWashingtonMN[, "Cases"]
Washington_cases_train <- Washington_cases %>% window(end = c(2020, 120))
Washington_cases_test <- Washington_cases %>% window(end = c(2020, 130))
Washington_deaths <- tsWashingtonMN[, "Deaths"]
Washington_deaths_train <- Washington_deaths %>% window(end = c(2020, 120))
Washington_deaths_test <- Washington_deaths %>% window(end = c(2020, 130))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Washington_cases <- auto.arima(Washington_cases, stepwise = FALSE, approximation = FALSE)
fit_Washington_cases # ARIMA(1, 1, 2), AICc - 465.94
autoplot(fit_Washington_cases)
sarima.for(Washington_cases_train, n.ahead = 20, 0, 1, 4)
lines(Washington_cases_test)
checkresiduals(fit_Washington_cases) # passes
fit_Washington_deaths <- auto.arima(Washington_deaths, stepwise = FALSE, approximation = FALSE)
fit_Washington_deaths # ARIMA(2, 1, 3), AICc - 84.25
autoplot(fit_Washington_deaths)
sarima.for(Washington_deaths_train, n.ahead = 20, 2, 1, 3)
lines(Washington_deaths_test)
checkresiduals(fit_Washington_deaths) # does not pass
fit_Washington_deaths2 <- arima(Washington_deaths, order = c(3, 1, 8))
checkresiduals(fit_Washington_deaths2) # p-value of 0.02847, closest I could get to 0.05
# Use best models to forecast further ahead
fc_10_Washington <- sarima.for(Washington_cases, n.ahead = 10, 0, 1, 4)
fc_10_Washington$pred
fcd_10_Washington <- sarima.for(Washington_deaths, n.ahead = 10, 3, 1, 8) 
fcd_10_Washington$pred
