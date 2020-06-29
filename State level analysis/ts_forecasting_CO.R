# Repeat for CO
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS, DIFFERENCED DATA SETS
CO_train <- tsCO %>% window(end = c(2020, 120))
CO_test <- tsCO %>% window(start = c(2020, 121), end = c(2020, 130))
autoplot(tsCO, main = "COVID-19 Cases and Deaths - Colorado")

# BEST MODELS - use auto.arima models for simplicity and consistency
fit_casesCO <- auto.arima(CO_train[, "Cases"], stepwise = FALSE, approximation = FALSE)
fit_casesCO # ARIMA(4, 1, 1), AICc - 1251.68
autoplot(fit_casesCO)
checkresiduals(fit_casesCO) # passes the Ljung Box test
fit_casesCO <- sarima.for(CO_train[, "Cases"], n.ahead = 10, 4, 1, 1); lines(CO_test[, "Cases"]); lines(CO_test[, "Cases"])
RMSE(fit_casesCO$pred, CO_test[, "Cases"])/mean(CO_test[, "Cases"])
fit_deathsCO <- auto.arima(CO_train[, "Deaths"], stepwise = FALSE, approximation = FALSE)
fit_deathsCO # ARIMA(2, 1, 3), AICc - 773.04
autoplot(fit_deathsCO)
checkresiduals(fit_deathsCO) # passes
fit_deathsCO <- sarima.for(CO_train[, "Deaths"], n.ahead = 10, 2, 1, 3); lines(CO_test[, "Deaths"])
RMSE(fit_deathsCO$pred, CO_test[, "Deaths"])/mean(CO_test[, "Deaths"]) # 0.93 (quite high, not very accurate)

# Use best models to forecast further ahead
fc_10_CO <- sarima.for(casesCO, n.ahead = 10, 4, 1, 1)
fc_10_CO$pred
fcd_10_CO <- sarima.for(deathsCO, n.ahead = 10, 10, 1, 2) 
fcd_10_CO$pred

# cases are trending up but deaths appear to be steady
