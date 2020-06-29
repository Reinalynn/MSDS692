
# filter to US totals only (remove state)
train_US <- kaggle %>% filter(Country_Region == "US") %>% filter(Province_State == "")
head(train_US)
tail(train_US)
# convert dataset to time series and plot
tsUS <- ts(train_US[, 6:7], start = c(2020, 23), frequency = 365)
str(tsUS)
autoplot(tsUS)

# filter tsUS data
casesUS <- tsUS[, "Cases"]
cases_train <- casesUS %>% window(end = c(2020, 120))
cases_test <- casesUS %>% window(end = c(2020, 130))
deathsUS <- tsUS[, "Deaths"]
deaths_train <- deathsUS %>% window(end = c(2020, 120))
deaths_test <- deathsUS %>% window(end = c(2020, 130))

# investigate autocorrelation of US data (check for trend, seaonality, cyclicity)
gglagplot(casesUS)
ggAcf(casesUS) # random walk with trend, need to difference data
acf(casesUS, plot = FALSE)
ggPacf(casesUS)
Pacf(casesUS, plot = FALSE)
Box.test(casesUS, lag = 7, fitdf = 0, type = "Lj") # p value is small, significant (trend in data)
adf.test(casesUS) # non-stationary
gglagplot(deathsUS)
ggAcf(deathsUS) # same as above - need to difference data
acf(deathsUS, plot = FALSE)
ggPacf(deathsUS)
Pacf(deathsUS, plot = FALSE)
Box.test(deathsUS, lag = 7, fitdf = 0, type = "Lj") # also significant (trend in data)
adf.test(deathsUS) # non-stationary

# transformations - diff will help with trend
diff_casesUS <- diff(casesUS) # remove trend
adf.test(diff_casesUS) # stationary
par(mfrow = c(2, 1))
acf(diff_casesUS)
pacf(diff_casesUS)
acf(diff(diff_casesUS)) #better - do we need a double diff?
pacf(diff(diff_casesUS)) # too much - stick with single diff
ndiffs(casesUS) # 1 difference
nsdiffs(casesUS) # 0 seasonal differences
Box.test(diff_casesUS, lag = 7, fitdf = 0, type = "Lj") # significant
diff_deathsUS <- diff(deathsUS) # remove trend
adf.test(diff_deathsUS) # stationary
ndiffs(deathsUS) # 1 difference
nsdiffs(deathsUS) # 0 diff
par(mfrow = c(1, 1))

# Possible models for CASES 
# simple exponential smoothing (SES)
fc_ses <- ses(casesUS)
fc_ses$model # AIC - 2170.186, BIC - 2178.232 
fcd_ses <- ses(diff_casesUS)
fcd_ses$model # AIC - 2148.847, BIC - 2156.865
fcd_ses2 <- ses(diff(cases_train), h = 20)
checkresiduals((fcd_ses2)) # does not pass Ljung Box test (residuals)
autoplot(fcd_ses2, series = "SES on differenced data") +
  autolayer(diff(cases_test), series = "Actual differenced data")
# trend models
fc_holt <- holt(casesUS) 
fc_holt$model # AIC - 2174.142, BIC - 2187.552 
fcd_holt <- holt(diff_casesUS)
fcd_holt$model # differenced, AIC - 2153.450, BIC - 2166.815
fcd_holt_damp <- holt(diff_casesUS, damped = TRUE)
fcd_holt_damp$model # diff and damped, AIC - 2155.826, BIC - 2171.863
fcd_holt2 <- holt(diff(cases_train), h = 20)
checkresiduals(fcd_holt2)
autoplot(fcd_holt2, series = "Holt's method on differenced data") +
  autolayer(diff(cases_test), series = "Actual differenced data")
# multiple regression models
# ARIMA
fit_cases <- auto.arima(casesUS)
fit_cases # ARIMA(0, 1, 0), RANDOM WALK, AIC - 1949.81, BIC - 1952.48
fit_cases2 <- auto.arima(casesUS, stepwise = FALSE, approximation = FALSE)
fit_cases2 # ARIMA(3, 1, 0), AIC - 1941.16, BIC - 1951.85, log likelihood = -966.58
autoplot(fit_cases2)
sarima.for(cases_train, n.ahead = 20, 3, 1, 0)
lines(cases_test)
checkresiduals(fit_cases2) # too low

# Compare accuracy of CASES models
accuracy(fcd_ses2)
accuracy(fcd_holt2)
accuracy(fit_cases2) # most accurate plus best AIC/BIC

# Possible models for DEATHS
# simple exponential smoothing (SES)
fc2_ses <- ses(deathsUS)
fc2_ses$model # AIC - 1811.439, BIC - 1819.486
fcd2_ses <- ses(diff_deathsUS)
fcd2_ses$model # AIC - 1795.522, BIC - 1803.540
fcd2_ses2 <- ses(diff(deaths_train), h = 20)
checkresiduals(fcd2_ses2)
autoplot(fcd2_ses2, series = "SES on differenced data") +
  autolayer(diff(deaths_test), series = "Actual differenced data")
# trend models
fc2_holt <- holt(deathsUS) 
fc2_holt$model # AIC - 1815.441, BIC - 1828.852 
fcd2_holt <- holt(diff_deathsUS)
fcd2_holt$model # differenced, AIC - 1799.681, BIC - 1813.045
fcd2_holt_damp <- holt(diff_deathsUS, damped = TRUE)
fcd2_holt_damp$model # damped, AIC - 1801.531, BIC - 1817.568
fcd2_holt_damp2 <- holt(diff(deaths_train), damped = TRUE, h = 20)
checkresiduals(fcd2_holt)
autoplot(fcd2_holt, series = "Holt's method on differenced data") +
  autolayer(diff(cases_test), series = "Actual differenced data")
# multiple regression models
# ARIMA
fit_deaths <- auto.arima(deathsUS)
fit_deaths # ARIMA(3, 1, 2), AIC - 1560.5, BIC - 1576.54
fit_deaths2 <- auto.arima(deathsUS, stepwise = FALSE, approximation = FALSE)
fit_deaths2 # ARIMA(3, 1, 2), same as above
autoplot(fit_deaths2)
sarima.for(deaths_train, n.ahead = 20, 3, 1, 0)
lines(deaths_test)
checkresiduals(fit_deaths2) # p-value above 0.05, so residuals are not sig

# Compare accuracy of DEATHS models
accuracy(fcd2_ses2)
accuracy(fcd2_holt)
accuracy(fit_deaths2) # most accurate plus best AIC/BIC

# Tune the model
# Regression with ARIMA errors in R
cases_errors <- auto.arima(casesUS, xreg = deathsUS, stepwise = FALSE, approximation = FALSE)
cases_errors # ARIMA(0, 1, 5) AIC - 1936.95, BIC - 1955.66
checkresiduals(cases_errors) # p-value too low, still some trend in the residuals
fc_errors <- forecast(cases_errors, xreg = rep(mean(deathsUS)), 20)
autoplot(fc_errors)
# compare to most accurate CASES model
accuracy(fc_errors) # better model
accuracy(fit_cases2)
deaths_errors <- auto.arima(deathsUS, xreg = casesUS, stepwise = FALSE, approximation = FALSE)
deaths_errors # ARIMA(0, 0, 5) AIC - 1585.72, BIC - 1604.49
checkresiduals(deaths_errors) # too low
fc2_errors <- forecast(deaths_errors, xreg = rep(mean(casesUS), 20))
autoplot(fc2_errors)
# compare to most accurate DEATHS model
accuracy(fc2_errors) 
accuracy(fit_deaths2) # better model

