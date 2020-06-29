# US Data
kaggle <- read.csv(url("https://raw.githubusercontent.com/Reinalynn/MSDS692/master/Data/kaggle.csv"), header = TRUE, stringsAsFactors = FALSE)
# Convert data to time series
train_US <- kaggle %>% filter(Country_Region == "US") %>% filter(Province_State == "")
tsUS <- ts(train_US[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsUS)
# Create train and test data
US_train <- tsUS %>% window(end = c(2020, 120))
US_test <- tsUS %>% window(start = c(2020, 121), end = c(2020, 130))
# Build models for US cases and deaths
fit_casesUS <- auto.arima(tsUS[, "Cases"], stepwise = FALSE, approximation = FALSE)
fit_casesUS # ARIMA(3, 1, 0), AICc - 1941.55
checkresiduals(fit_casesUS) # p-value too low
accuracy(fit_casesUS)
fit_casesUS2 <- arima(US_train[, "Cases"], order = c(6, 1, 1))
checkresiduals(fit_casesUS2) # still too low, but closest to 0.05
accuracy(fit_casesUS2) # more accurate than (3, 1, 0) model
fit_deathsUS <- auto.arima(US_train[, "Deaths"], stepwise = FALSE, approximation = FALSE)
fit_deathsUS # ARIMA(3, 1, 2), AICc - 1419.15
checkresiduals(fit_deathsUS) # passes
# Use best models to forecast further ahead
fc_10_US <- sarima.for(tsUS[, "Cases"], n.ahead = 10, 6, 1, 1)
fc_10_US$pred
actual_US <- c(19710, 18618, 21693, 20832, 27368, 25050, 24994, 18937, 21551, 20260) # actual US cases for 5/10 = 5/19
RMSE(fc_10_US$pred, actual_US)/mean(actual_US) # 0.10 VERY GOOD
fcd_10_US <- sarima.for(tsUS[, "Deaths"], n.ahead = 10, 3, 1, 2) 
fcd_10_US$pred
actual_USd <- c(731, 1156, 1694, 1743, 1779, 1632, 1224, 808, 785, 1574)
RMSE(fcd_10_US$pred, actual_USd)/mean(actual_USd) # 0.53 NOT AS GOOD AS CASES
# models show cases declining while deaths are steady