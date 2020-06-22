
getwd()
setwd('/Users/reginaduval/Grad_Work/MSDS692_Practicum1/Project/Data')

# load time series data
library(astsa)
library(forecast)
library(fpp2)
library(MARSS)
library(MTS)
library(quantmod)
library(readxl)
library(tidyverse)
library(tseries)
library(urca)
library(vars)

train <- read.csv("train2.csv", header = TRUE, stringsAsFactors = FALSE)
head(train)
# filter to US states only (remove county)
train_states <- train %>% filter(Country_Region == "US") %>% filter(County == "")
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
casesMN <- tsMN[, "Cases"]
casesMN_train <- casesMN %>% window(end = c(2020, 120))
casesMN_test <- casesMN %>% window(end = c(2020, 130))
diff_casesMN <- diff(casesMN)
deathsMN <- tsMN[, "Deaths"]
deathsMN_train <- deathsMN %>% window(end = c(2020, 120))
deathsMN_test <- deathsMN %>% window(end = c(2020, 130))
diff_deathsMN <- diff(deathsMN)

auto.arima(tsMN, stepwise, approximation = FALSE)
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_casesMN <- auto.arima(casesMN, stepwise = FALSE, approximation = FALSE)
fit_casesMN # ARIMA(4, 2, 1), AICc - 1105.85
autoplot(fit_casesMN)
sarima.for(casesMN_train, n.ahead = 20, 4, 2, 1)
lines(casesMN_test)
checkresiduals(fit_casesMN) # p-value too low
fit_casesMN2 <- arima(casesMN, order = c(6, 2, 1))
checkresiduals(fit_casesMN2) # passes Ljung-Box test
coeftest(fit_casesMN2)
fit_deathsMN <- auto.arima(deathsMN, stepwise = FALSE, approximation = FALSE)
fit_deathsMN # ARIMA(1, 1, 2), AICc - 577.18
autoplot(fit_deathsMN)
sarima.for(deathsMN_train, n.ahead = 20, 1, 1, 2)
lines(deathsMN_test)
checkresiduals(fit_deathsMN)
fit_deathsMN2 <- arima(deathsMN, order = c(4, 1, 2))
checkresiduals(fit_deathsMN2) # passes Ljung-Box test
coeftest(fit_deathsMN2)

# Use best models to forecast further ahead
fc_10_MN <- sarima.for(casesMN, n.ahead = 10, 6, 2, 1)
fc_10_MN$pred
fcd_10_MN <- sarima.for(deathsMN, n.ahead = 10, 4, 1, 2) 
fcd_10_MN$pred

# both models are trending up but deaths are climbing steadily while cases show variation


# https://datascienceplus.com/time-series-analysis-using-arima-model-in-r/
# https://www.medrxiv.org/content/10.1101/2020.04.17.20069237v1.full.pdf
# https://subscription.packtpub.com/book/big_data_and_business_intelligence/9781783552078/1/ch01lvl1sec08/multivariate-time-series-analysis
