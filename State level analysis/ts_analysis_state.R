# filter to US states only (remove county)
train_states <- kaggle %>% filter(Country_Region == "US") %>% filter(County == "")
head(train_states)
tail(train_states)
# filter to MN
train_MN <- train_states %>% filter(Province_State == "Minnesota")
head(train_MN)
tail(train_MN)
# convert dataset to time series and plot
tsMN <- ts(train_MN[, 6:7], start = c(2020, 23), frequency = 365)
str(tsMN)
autoplot(tsMN, main = "COVID-19 Cases and Deaths - Minnesota")
# repeat for CO
train_CO <- train_states %>% filter(Province_State == "Colorado") %>% filter(County == "")
tail(train_CO)
tsCO <- ts(train_CO[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsCO, main = "COVID-19 Cases and Deaths - Colorado")
# repeat for MI
train_MI <- train_states %>% filter(Province_State == "Michigan") %>% filter(County == "")
tail(train_MI)
tsMI <- ts(train_MI[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsMI, main = "COVID-19 Cases and Deaths - Michigan")
# repeat for NE
train_NE <- train_states %>% filter(Province_State == "Nebraska") %>% filter(County == "")
tail(train_NE)
tsNE <- ts(train_NE[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsNE, main = "COVID-19 Cases and Deaths - Nebraska")
# repeat for PA
train_PA <- train_states %>% filter(Province_State == "Pennsylvania") %>% filter(County == "")
tail(train_PA)
tsPA <- ts(train_PA[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsPA, main = "COVID-19 Cases and Deaths - Pennsylvania")
# repeat for SD
train_SD <- train_states %>% filter(Province_State == "South Dakota") %>% filter(County == "")
tail(train_SD)
tsSD <- ts(train_SD[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsSD, main = "COVID-19 Cases and Deaths - South Dakota")
# repeat for TX
train_TX <- train_states %>% filter(Province_State == "Texas") %>% filter(County == "")
tail(train_TX)
tsTX <- ts(train_TX[, 6:7], start = c(2020, 23), frequency = 365)
autoplot(tsTX, main = "COVID-19 Cases and Deaths - Texas")

# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS, DIFFERENCED DATA SETS
MN_train <- tsMN %>% window(end = c(2020, 120))
MN_test <- tsMN %>% window(start = c(2020, 121), end = c(2020, 130))

# BEST MODELS - use auto.arima models for simplicity and consistency
fit_casesMN <- auto.arima(tsMN[, "Cases"], stepwise = FALSE, approximation = FALSE)
fit_casesMN # ARIMA(4, 2, 1), AICc - 1105.85
autoplot(fit_casesMN)
sarima.for(MN_train[, "Cases"], n.ahead = 10, 4, 2, 1); lines(tsMN[, "Cases"])
checkresiduals(fit_casesMN) # p-value too low
fit_casesMN2 <- sarima.for(MN_train[, "Cases"], n.ahead = 10, 4, 2, 2); lines(tsMN[, "Cases"])
checkresiduals(fit_casesMN2) # passes Ljung-Box test, residuals are not sig
coeftest(fit_casesMN2)
RMSE(fit_casesMN2$pred, MN_test[, "Cases"])/mean(MN_test[, "Cases"]) # 0.19 low = accurate model
fit_deathsMN <- auto.arima(MN_train[, "Deaths"], stepwise = FALSE, approximation = FALSE)
fit_deathsMN # ARIMA(0, 1, 2), AICc - 498.11
autoplot(fit_deathsMN)
checkresiduals(fit_deathsMN) # passes
fit_deathsMN <- sarima.for(MN_train[, "Deaths"], n.ahead = 10, 0, 1, 2); lines(tsMN[, "Deaths"])
RMSE(fit_deathsMN2$pred, MN_test[, "Deaths"])/mean(MN_test[, "Deaths"]) # 0.31

# Use best models to forecast further ahead
fc_10_MN <- sarima.for(MN_train[, "Cases"], n.ahead = 10, 4, 2, 2)
fc_10_MN$pred
fcd_10_MN <- sarima.for(MN_train[, "Deaths"], n.ahead = 10, 0, 1, 2) 
fcd_10_MN$pred
# both models are trending up but deaths are climbing steadily while cases show variation
