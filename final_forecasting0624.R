US_7cases2 <- read.csv(url("https://raw.githubusercontent.com/Reinalynn/MSDS692/master/Data/US_7cases2.csv"), header = TRUE, stringsAsFactors = FALSE)
US_7deaths2 <- read.csv(url("https://raw.githubusercontent.com/Reinalynn/MSDS692/master/Data/US_7deaths2.csv"), header = TRUE, stringsAsFactors = FALSE)
US_7cases2t <- as.data.frame(t(US_7cases2))
colnames(US_7cases2t) <- c("CO", "MI", "MN", "NE", "PA", "SD", "TX")
dim(US_7cases2t)
# truncate to remove days prior to reporting (first case occurs on row 45)
US_7cum2 <- US_7cases2t[43:156, ]
str(US_7cum2)
US_7cum2 <- US_7cum2 %>% mutate_if(is.factor, as.character)
US_7cum2 <- US_7cum2 %>% mutate_if(is.character, as.integer)
# difference data
US_72 <- diffM(US_7cum2)
# create ts
US_7ts2 <- ts(US_72, start = c(2020, 65), frequency = 365)
# use recent data from usafacts and limit to deaths but include multiple counties
str(US_7deaths2)
US_7dcum2 <- as.data.frame(t(US_7deaths2))
tail(US_7dcum2)
# truncate
US_7dcum2 <- US_7dcum2[43:156, ]
colnames(US_7dcum2) <- c("CO", "MI", "MN", "NE", "PA", "SD", "TX")
dim(US_7dcum2)
US_7dcum2 <- US_7dcum2 %>% mutate_if(is.factor, as.character)
US_7dcum2 <- US_7dcum2 %>% mutate_if(is.character, as.integer)
# difference
US_7d2 <- diffM(US_7dcum2)
# create ts
US_7dts2 <- ts(US_7d2, start = c(2020, 65), frequency = 365)

autoplot(US_7ts2, main = "COVID-19 Cases for 7 US states")
autoplot(US_7dts2, main = "COVID-19 Deaths for 7 US states")
# BEST MODELS - use auto.arima models for simplicity and consistency
# CO
fit_CO <- auto.arima(US_7ts2[, "CO"], stepwise = FALSE, approximation = FALSE)
checkresiduals(fit_CO) # too low
fit_CO2 <- arima(US_7ts2[, "CO"], order = c(6, 1, 2))
checkresiduals(fit_CO2) # passes, use ARIMA(6, 1, 2)
fit_CO2 <- sarima.for(US_7ts2[, "CO"], n.ahead = 10, 6, 1, 2)
fit_CO2$pred # cases for CO 06.25 through 07.04
# to check, create actual vector and use RMSE(fit_CO2$pred, *actual)/mean(*actual)
fit_COd <- auto.arima(US_7dts2[, "CO"], stepwise = FALSE, approximation = FALSE)
checkresiduals(fit_COd) # too low
fit_CO2d <- arima(US_7dts2[, "CO"], order = c(10, 1, 2))
checkresiduals(fit_CO2d) # passes, use ARIMA(10, 1, 2)
fit_CO2d <- sarima.for(US_7dts2[, "CO"], n.ahead = 10, 10, 1, 2)
fit_CO2d$pred # deaths for CO 06.25 through 07.04
# MI
fit_MI <- auto.arima(US_7ts2[, "MI"], stepwise = FALSE, approximation = FALSE)
checkresiduals(fit_MI) # passes, use ARIMA(1,0,1)
fit_MI <- sarima.for(US_7ts2[, "MI"], n.ahead = 10, 1, 0, 1)
fit_MI$pred # cases for MI 06.25 through 07.04
fit_MId <- auto.arima(US_7dts2[, "MI"], stepwise = FALSE, approximation = FALSE)
checkresiduals(fit_MId) # passes, use ARIMA(0, 1, 4)
fit_MId <- sarima.for(US_7dts2[, "MI"], n.ahead = 10, 0, 1, 4)
fit_MId$pred # deaths for MI 06.25 through 07.04
# MN
fit_MN <- auto.arima(US_7ts2[, "MN"], stepwise = FALSE, approximation = FALSE)
checkresiduals(fit_MN) # good, use ARIMA(3, 1, 2)
fit_MN <- sarima.for(US_7ts2[, "MN"], n.ahead = 10, 3, 1, 2)
fit_MN$pred # cases for MN 06.25 through 07.04
fit_MNd <- auto.arima(US_7dts2[, "MN"], stepwise = FALSE, approximation = FALSE)
checkresiduals(fit_MNd) # too low
fit_MN2d <- arima(US_7dts2[, "MN"], order = c(5, 1, 3))
checkresiduals(fit_MN2d) # passes, use ARIMA(5, 1, 3)
fit_MN2d <- sarima.for(US_7dts2[, "MN"], n.ahead = 10, 3, 1, 3)
fit_MN2d$pred # deaths for MN 06.25 through 07.04
# NE
fit_NE <- auto.arima(US_7ts2[, "NE"], stepwise = FALSE, approximation = FALSE)
checkresiduals(fit_NE) # too low
fit_NE2 <- arima(US_7ts2[, "NE"], order = c(9, 1, 4))
checkresiduals(fit_NE2) # good, use ARIMA(9, 1, 4)
fit_NE2 <- sarima.for(US_7ts2[, "NE"], n.ahead = 10, 9, 1, 4)
fit_NE2$pred # cases for NE 06.25 through 07.04
fit_NEd <- auto.arima(US_7dts2[, "NE"], stepwise = FALSE, approximation = FALSE)
checkresiduals(fit_NEd) # good, use ARIMA(0, 1, 1)
fit_NEd <- sarima.for(US_7dts2[, "NE"], n.ahead = 10, 0, 1, 1)
fit_NEd$pred # deaths for NE 06.25 through 07.04
# PA
fit_PA <- auto.arima(US_7ts2[, "PA"], stepwise = FALSE, approximation = FALSE)
checkresiduals(fit_PA) # good, use ARIMA(3, 1, 2)
fit_PA <- sarima.for(US_7ts2[, "PA"], n.ahead = 10, 3, 1, 2)
fit_PA$pred # cases for PA 06.25 through 07.04
fit_PAd <- auto.arima(US_7dts2[, "PA"], stepwise = FALSE, approximation = FALSE)
checkresiduals(fit_PAd) # good, use ARIMA(1, 1, 4)
fit_PAd <- sarima.for(US_7dts2[, "PA"], n.ahead = 10, 1, 1, 4)
fit_PAd$pred # deaths for PA 06.25 through 07.04
# SD
fit_SD <- auto.arima(US_7ts2[, "SD"], stepwise = FALSE, approximation = FALSE)
checkresiduals(fit_SD) # good, use ARIMA(5, 1, 0)
fit_SD <- sarima.for(US_7ts2[, "SD"], n.ahead = 10, 5, 1, 0)
fit_SD$pred # cases for SD 06.25 through 07.04
fit_SDd <- auto.arima(US_7dts2[, "SD"], stepwise = FALSE, approximation = FALSE)
checkresiduals(fit_SDd) # too low
fit_SD2d <- arima(US_7dts2[, "SD"], order = c(6, 1, 3))
checkresiduals(fit_SD2d) # good, use ARIMA(6, 1, 3)
fit_SD2d <- sarima.for(US_7dts2[, "SD"], n.ahead = 10, 6, 1, 3)
fit_SD2d$pred # deaths for SD 06.25 through 07.04
# TX
fit_TX <- auto.arima(US_7ts2[, "TX"], stepwise = FALSE, approximation = FALSE)
checkresiduals(fit_TX) # too low
fit_TX2 <- arima(US_7ts2[, "TX"], order = c(7, 1, 3))
checkresiduals(fit_TX2) # use ARIMA(7, 1, 3)
fit_TX2 <- sarima.for(US_7ts2[, "TX"], n.ahead = 10, 7, 1, 3)
fit_TX2$pred # cases for TX 06.25 through 07.04
fit_TXd <- auto.arima(US_7dts2[, "TX"], stepwise = FALSE, approximation = FALSE)
checkresiduals(fit_TXd) # too low
fit_TX2d <- arima(US_7dts2[, "TX"], order = c(10, 2, 3))
checkresiduals(fit_TX2d) # close, use ARIMA(10, 2, 3)
fit_TX2d <- sarima.for(US_7dts2[, "TX"], n.ahead = 10, 10, 2, 3)
fit_TX2d$pred # deaths for TX 06.25 through 07.04
