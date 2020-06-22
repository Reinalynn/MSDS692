# repeat MNcounties_VAR for chosen USstates

getwd()
setwd('/Users/reginaduval/Grad_Work/MSDS692_Practicum1/Project/Data/State Level')

library(astsa)
library(broom)
library(dynlm)
library(forecast)
library(knitr)
library(MTS)
library(tidyverse)
library(tseries)
library(urca)
library(varhandle)
library(vars)

# use recent data from usafacts and limit to cases but include multiple counties
US_7cum <- read.csv("covid_cases_0613.csv", header = TRUE, stringsAsFactors = FALSE)
str(US_7cum)
US_7cum <- as.data.frame(t(US_7cum))
US_7cum <- US_7cum[2:145, ]
colnames(US_7cum) <- c("CO", "MI", "MN", "NE", "PA", "SD", "TX")
dim(US_7cum)
str(US_7cum)
tail(US_7cum)
US_7cum <- US_7cum %>% mutate_if(is.factor, as.character)
US_7cum <- US_7cum %>% mutate_if(is.character, as.integer)
# truncate to remove days prior to reporting (first case occurs on row 45)
US_7cum <- US_7cum[44:144, ]
US_7cum
# SKIP THIS STEP TO SEE IF MODEL WORKS WITH CUM DATA
# add col that shows difference between values (USAfacts data is cumulative)
US_7 <- US_7cum
start = 0
US_7$CO <- c(start, diff(US_7$CO))
US_7$MI <- c(start, diff(US_7$MI))
US_7$MN <- c(start, diff(US_7$MN))
US_7$NE <- c(start, diff(US_7$NE))
US_7$PA <- c(start, diff(US_7$PA))
US_7$SD <- c(start, diff(US_7$SD))
US_7$TX <- c(start, diff(US_7$TX))
head(US_7, 10)
# convert to ts
US_7ts <- ts(US_7, start = c(2020, 44), frequency = 365)
str(US_7ts)
autoplot(US_7ts, main = "Time series plot of the US_7 data")
# determine k for adf.test
trunc((length(US_7)-1)^(1/3))
# perform adf.test to check for stationarity (H0 = non-stationary)
apply(US_7ts, 1, adf.test) # ts is not stationary
# difference the entire multivariate ts
US_7diff <- diffM(US_7)
US_7[45:55, ]
US_7diff[45:55, ]
apply(US_7diff, 2, adf.test) # now stationary (p value < 0.05)
US_7tsdiff <- ts(US_7diff)
autoplot(US_7tsdiff, main = "Time series plot of the stationary (differenced) US_7ts data")
# lag order identification
VARselect(US_7tsdiff, type = "none", lag.max = 15) # 13 is best lag for most selection criteria
# create the VAR model
var.US_7 <- VAR(US_7tsdiff, type = "none", lag.max = 10, ic = "AIC") # errors above 10 due to small size of ts
summary(var.US_7) # relatively good model with low p-values and high adj r-squared
# diagnostic tests = Portmanteau Test
serial.test(var.US_7) # p-value below 0.05 indicates we have autocorrelation in errors
# Granger test for causality (all show causality)
causality(var.US_7, cause = c("CO")) # there is causality (low p-values for both Granger and Instant)
causality(var.US_7, cause = c("MI")) # there is causality (low p-values for both Granger and Instant)
causality(var.US_7, cause = c("MN")) # there is causality (low p-values for both Granger and Instant)
causality(var.US_7, cause = c("NE")) # causality
causality(var.US_7, cause = c("PA")) # causality
causality(var.US_7, cause = c("SD")) # causality
causality(var.US_7, cause = c("TX")) # causality
# other diagnostic tests
arch.test(var.US_7) # test for autoregressive conditional heteroscedasticity (ARCH), p-value = 1
normality.test(var.US_7) # p-value below 0.05
# forecast
fcastUS_7 <- predict(var.US_7, n.ahead = 12) # predict through 06.25.20
fcastUS_7 # values seem to be volatile considering some counties have small #'s of cases at tail of data
plot(fcastUS_7)
fanchart(fcastUS_7)
# pull out just the CO fcasts
CO <- fcastUS_7$fcst[1]; CO
CO <- CO$CO[, 1]; CO
# Invert the differencing (add the last value of the ts data to the forecasts)
tail(US_7) # cases added each day - use this # b/c we differenced this data
tail(US_7cum) # total cases
CO <- CO + 196
CO # many of these are impossible #'s because lowest possible is 0
par(mfrow = c(1, 1))
plot.ts(CO)
# add data and forecast to one time series
CO_full <- ts(c(US_7[, 1], CO), start = c(2020, 44), frequency = 365)
plot(CO_full)
# not a good model at all! 

#Try fewer states? 3 counties were successful, so narrow down to 3 states
# use CO, MN, and TX (my hypothesis is that CO is lagging indicator for MN and TX)
US_3 <- US_7[, c(1, 3, 7)]
US_3diff <- diffM(US_3)
US_3tsdiff <- ts(US_3diff, start = c(2020, 45), frequency = 365)
autoplot(US_3tsdiff, main = "Time series plot of the stationary (differenced) US_3 data")
# build model
VARselect(US_3tsdiff, type = "none") # try 5, 2, 1, 3
var.US_3 <- VAR(US_3tsdiff, type = "none", p = 5)
serial.test(var.US_3) # p-value high enough! Not autocorrelated
# generate impulse response functions to describe response of MN to TX changes
# https://kevinkotze.github.io/ts-7-tut/
# https://www.r-econometrics.com/timeseries/varintro/ 
irf.MN <- irf(var.US_3, n.ahead = 12, impulse = "TX", response = "MN", runs = 500)
plot(irf.MN, ylab = "MN Cases", main = "Shock from TX Cases")
irf.TX <- irf(var.US_3, impulse = "MN", response = "TX", n.ahead = 12, runs = 500)
plot(irf.TX, ylab = "TX Cases", main = "Shock from MN Cases")
US_3.vardec <- fevd(var.US_3, n.ahead = 12) # forecast error variance decompositions
plot(US_3.vardec)
# view forecasts using US_3 model VAR(9)
fcastUS_3 <- predict(var.US_3, n.ahead = 12) # predict through 06.25.20
fcastUS_3 # better than full model
plot(fcastUS_3, names = "CO")
plot(fcastUS_3, names = "MN")
plot(fcastUS_3, names = "TX")
fanchart(fcastUS_3)
# pull out just the CO fcasts
CO3 <- fcastUS_3$fcst[1]; CO3
CO3 <- CO3$CO[, 1]; CO3
# Invert the differencing (add the last value of the ts data to the forecasts)
tail(US_3) # cases added each day - use this # b/c we differenced this data
CO3 <- CO3 + 196
CO3 # these values seem more realistic (except for the first negative)
par(mfrow = c(1, 1))
plot.ts(CO3)
# add data and forecast to one time series
CO3_full <- ts(c(US_3[, 1], CO3), start = c(2020, 44), frequency = 365)
plot(CO3_full)
# model looks somewhat better
# view MN and TX data
MN3 <- fcastUS_3$fcst[2]
MN3 <- MN3$MN[, 1]
MN3 <- MN3 + 377
MN3
MN3_full <- ts(c(US_3[, 2], MN3), start = c(2020, 44), frequency = 365)
plot(MN3_full)
TX3 <- fcastUS_3$fcst[3]
TX3 <- TX3$TX[, 1]
TX3 <- TX3 + 2340
TX3 # looks good
TX3_full <- ts(c(US_3[, 3], TX3), start = c(2020, 44), frequency = 365)
plot(TX3_full)

# use recent data from usafacts and limit to deaths but include multiple counties
US_7dcum <- read.csv("covid_deaths_0613.csv", header = TRUE, stringsAsFactors = FALSE)
US_7dcum <- as.data.frame(t(US_7dcum))
str(US_7dcum)
US_7dcum <- US_7dcum[2:145, ]
colnames(US_7dcum) <- c("CO", "MI", "MN", "NE", "PA", "SD", "TX")
dim(US_7dcum)
str(US_7dcum)
tail(US_7dcum)
US_7dcum <- US_7dcum %>% mutate_if(is.factor, as.character)
US_7dcum <- US_7dcum %>% mutate_if(is.character, as.integer)
head(US_7dcum, 60)
# truncate to remove days prior to reporting (first death occurs on row 49)
US_7dcum <- US_7dcum[48:144, ]
head(US_7dcum, 15)
# add col that shows difference between values (USAfacts data is cumulative)
US_7d <- US_7dcum
start = 0
US_7d$CO <- c(start, diff(US_7d$CO))
US_7d$MI <- c(start, diff(US_7d$MI))
US_7d$MN <- c(start, diff(US_7d$MN))
US_7d$NE <- c(start, diff(US_7d$NE))
US_7d$PA <- c(start, diff(US_7d$PA))
US_7d$SD <- c(start, diff(US_7d$SD))
US_7d$TX <- c(start, diff(US_7d$TX))
head(US_7d, 10)
# convert to ts
US_7dts <- ts(US_7d, start = c(2020, 48), frequency = 365)
str(US_7dts)
autoplot(US_7dts, main = "Time series plot of the US_7d data")
# determine k for adf.test
trunc((length(US_7d)-1)^(1/3))
# perform adf.test to check for stationarity (H0 = non-stationary)
apply(US_7dts, 1, adf.test) # entire ts is not stationary yet
# difference the entire multivariate ts
US_7dtsdiff <- diffM(US_7d)
US_7d[45:55, ]
US_7dtsdiff[45:55, ]
apply(US_7dtsdiff, 2, adf.test) # now stationary (p value < 0.05)
US_7dtsdiff <- ts(US_7dtsdiff)
autoplot(US_7dtsdiff, main = "Time series plot of the stationary (differenced) MN_9ts data")
# lag order identification
VARselect(US_7dtsdiff, type = "none", lag.max = 15) # 12 is best lag for all selection criteria
# create the VAR model
var.US_7d <- VAR(US_7dtsdiff, type = "none", lag.max = 10, ic = "AIC") # errors above 10 due to small size of ts
summary(var.US_7d) # relatively good model with low p-values and high adj r-squared
# diagnostic tests = Portmanteau Test
serial.test(var.US_7d) # p-value below 0.05 indicates we have autocorrelation in errors
# Granger test for causality (all show causality)
causality(var.US_7d, cause = c("CO")) # there is causality (low p-values for both Granger and Instant)
causality(var.US_7d, cause = c("MI")) # there is causality (low p-values for both Granger and Instant)
causality(var.US_7d, cause = c("MN")) # there is causality (low p-values for both Granger and Instant)
causality(var.US_7d, cause = c("NE")) # causality, significant
causality(var.US_7d, cause = c("PA")) # causality
causality(var.US_7d, cause = c("SD")) # causality
causality(var.US_7d, cause = c("TX")) # causality
# other diagnostic tests
arch.test(var.US_7d) # test for autoregressive conditional heteroscedasticity (ARCH), p-value = 1
normality.test(var.US_7d) # p-value below 0.05
# use original model to forecast deaths
fcastUS_7d <- predict(var.US_7d, n.ahead = 12) # predict through 06.25.20
fcastUS_7d
par(mar = c(0.5, 0.5, 0.5, 0.5))
plot(fcastUS_7d) # forecasts are much too varied compared to differenced data (same problem as cases VAR model)
# pull out just the CO fcasts
COd <- fcastUS_7d$fcst[1]; COd
COd <- COd$CO[, 1]; COd
# Invert the differencing (add the last value of the ts data to the forecasts)
tail(US_7d) # deaths added each day - use this # b/c we differenced this data
tail(US_7dcum) # total deaths
COd <- COd + 3
COd # many of these are impossible #'s because lowest possible is 0
par(mfrow = c(1, 1))
plot.ts(COd)
# add data and forecast to one time series
CO_fulld <- ts(c(US_7d[, 1], COd), start = c(2020, 48), frequency = 365)
plot(CO_fulld) # far too volatile
# not a good model at all! 
#Try 3 states
# use CO, MN, and TX (my hypothesis is that CO is lagging indicator for MN and TX)
US_3d <- US_7d[, c(1, 3, 7)]
US_3ddiff <- diffM(US_3d)
US_3dtsdiff <- ts(US_3ddiff, start = c(2020, 48), frequency = 365)
autoplot(US_3dtsdiff, main = "Time series plot of the stationary (differenced) US_3d data")
# build model
VARselect(US_3dtsdiff, type = "none") # 10, 2
var.US_3d <- VAR(US_3dtsdiff, type = "none", ic = "AIC")
serial.test(var.US_3d) # p-value too low (but better than cases model)
# generate impulse response functions to describe response of Stearns to Hennepin changes
# https://kevinkotze.github.io/ts-7-tut/
# https://www.r-econometrics.com/timeseries/varintro/ 
irf.COd <- irf(var.US_3d, n.ahead = 12, impulse = "CO", response = "MN", runs = 500)
plot(irf.COd, ylab = "MN Cases", main = "Shock from CO Cases")
irf.MNd <- irf(var.US_3d, impulse = "MN", response = "CO", n.ahead = 12, runs = 500)
plot(irf.MNd, ylab = "CO Cases", main = "Shock from MN Cases")
US_3d.vardec <- fevd(var.US_3d, n.ahead = 12) # forecast error variance decompositions
plot(US_3d.vardec) # TX is influenced by MN and CO, but others are not influenced by any other state
# view forecasts using US_3d model
fcastUS_3d <- predict(var.US_3d, n.ahead = 12) # predict through 06.25.20
fcastUS_3d # better than full model
plot(fcastUS_3d, names = "CO") 
plot(fcastUS_3d, names = "MN")
plot(fcastUS_3d, names = "TX")
fanchart(fcastUS_3d)
# pull out just the Hennepin county fcasts
CO3d <- fcastUS_3d$fcst[1]; CO3d
CO3d <- CO3d$CO[, 1]; CO3d
# Invert the differencing (add the last value of the ts data to the forecasts)
tail(US_7d) # cases added each day - use this # b/c we differenced this data
tail(US_7dcum) # total cases
CO3d <- CO3d + 3
CO3d # these values seem more realistic
par(mfrow = c(1, 1))
plot.ts(CO3d)
# add data and forecast to one time series
CO3d_full <- ts(c(US_3d[, 1], CO3d), start = c(2020, 48), frequency = 365)
plot(CO3d_full)
# model looks much better
# view MN and TX data
MN3d <- fcastUS_3d$fcst[2]
MN3d <- MN3d$MN[, 1]
MN3d <- MN3d + 9
MN3d # slooks good
MN3d_full <- ts(c(US_3d[, 2], MN3d), start = c(2020, 48), frequency = 365)
plot(MN3d_full)
TX3d <- fcastUS_3d$fcst[3]
TX3d <- TX3d$TX[, 1]
TX3d <- TX3d + 1
TX3d # negative values? 
TX3d_full <- ts(c(US_3d[, 3], TX3d), start = c(2020, 48), frequency = 365)
plot(TX3d_full) # values too small to predict well

