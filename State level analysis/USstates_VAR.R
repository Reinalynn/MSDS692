
# use recent data from usafacts and limit to cases but include multiple counties
str(US_7cases)
US_7casest <- as.data.frame(t(US_7cases))
colnames(US_7casest) <- c("CO", "MI", "MN", "NE", "PA", "SD", "TX")
dim(US_7casest)
# truncate to remove days prior to reporting (first case occurs on row 45)
US_7cum <- US_7casest[44:145, ]
str(US_7cum)
US_7cum <- US_7cum %>% mutate_if(is.factor, as.character)
US_7cum <- US_7cum %>% mutate_if(is.character, as.integer)
US_7 <- diffM(US_7cum)
# convert to ts
US_7ts <- ts(US_7, start = c(2020, 64), frequency = 365)
str(US_7ts)
autoplot(US_7ts, main = "Time series plot of the US_7 data")
# perform adf.test to check for stationarity (H0 = non-stationary)
apply(US_7cum, 2, adf.test) # ts is not stationary
# difference the entire multivariate ts
US_7diff <- diffM(US_7)
US_7cum[50:60, ]
US_7diff[50:60, ]
apply(US_7diff, 2, adf.test) # now stationary (p value > 0.05)
US_7tsdiff <- ts(US_7diff)
autoplot(US_7tsdiff, main = "Time series plot of the stationary (differenced) US_7ts data")
# lag order identification
VARselect(US_7tsdiff, type = "none", lag.max = 15) # 13 is best lag for most selection criteria
# create the VAR model
var.US_7 <- VAR(US_7tsdiff, type = "none", lag.max = 11, ic = "AIC") # errors above 11 due to small size of ts
summary(var.US_7) # relatively good model with low p-values and high adj r-squared
# diagnostic tests = Portmanteau Test
serial.test(var.US_7) # p-value below 0.05 indicates we have autocorrelation in errors
# Granger test for causality (all show causality)
causality(var.US_7, cause = c("CO")) # there is causality (low p-values for both Granger and Instant)
causality(var.US_7, cause = c("MI")) # there is causality (low p-values for both Granger and Instant)
causality(var.US_7, cause = c("MN")) # there is causality (low p-values for both Granger and Instant)
# other diagnostic tests
arch.test(var.US_7) # test for autoregressive conditional heteroscedasticity (ARCH), p-value = 1
normality.test(var.US_7) # p-value below 0.05
# forecast
fcastUS_7 <- predict(var.US_7, n.ahead = 10) # predict through 06.23
fcastUS_7 # values seem to be volatile considering some counties have small #'s of cases at tail of data
plot(fcastUS_7)
plot.ts(US_7diff)
fanchart(fcastUS_7)
# pull out just the CO fcasts
CO <- fcastUS_7$fcst[1]; CO
CO <- CO$CO[, 1]; CO
# Invert the differencing (add the last value of the ts data to the forecasts)
tail(US_7) # total cases
CO <- CO + 196
CO # many of these are impossible #'s because lowest possible is 0
par(mfrow = c(1, 1))
plot.ts(CO)
# add data and forecast to one time series
CO_full <- ts(c(US_7diff[, 1], CO), start = c(2020, 64), frequency = 365)
plot(CO_full)
# not a good model at all! 

#Try fewer states? 3 counties were successful, so narrow down to 3 states
# use CO, MN, and TX (my hypothesis is that CO is lagging indicator for MN and TX)
US_3 <- US_7[, c(1, 3, 7)]
US_3ts <- ts(US_3, start = c(2020, 64), frequency = 365)
autoplot(US_3ts, main = "Time series plot of the US_3 data")
US_3diff <- diffM(US_3)
US_3diffts <- ts(US_3diff, start = c(2020, 64, frequency = 365))
autoplot(US_3diffts, main = "Time series plot of the stationary (differenced) US_3 data")
# build model
VARselect(US_3diff, type = "none") # 5 or 2?
var.US_3 <- VAR(US_3ts, type = "none", lag.max = 10, ic = "AIC")
serial.test(var.US_3) # p-value nearly high enough - not as autocorrelated
# generate impulse response functions to describe response of MN to TX changes
irf.MN <- irf(var.US_3, n.ahead = 10, impulse = "TX", response = "MN", runs = 500)
plot(irf.MN, ylab = "MN Cases", main = "Shock from TX Cases")
irf.TX <- irf(var.US_3, impulse = "MN", response = "TX", n.ahead = 10, runs = 500)
plot(irf.TX, ylab = "TX Cases", main = "Shock from MN Cases")
US_3.vardec <- fevd(var.US_3, n.ahead = 10) # forecast error variance decomposition
plot(US_3.vardec)
# view forecasts using US_3 model
fcastUS_3 <- predict(var.US_3, n.ahead = 10) # predict through 06.24.20
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
CO3 # these values seem more realistic
par(mfrow = c(1, 1))
# add data and forecast to one time series
CO3_full <- ts(c(US_3[, 1], CO3), start = c(2020, 64), frequency = 365)
plot(CO3_full)
CO_actual = c(112, 169, 133, 241, 229, 286, 162, 188, 168, 188) # actual cases for 6/14 through 6/23
RMSE(CO3, CO_actual)/mean(CO_actual) # 0.41 
# model looks somewhat better
# view MN and TX data
MN3 <- fcastUS_3$fcst[2]
MN3 <- MN3$MN[, 1]
MN3 <- MN3 + 377
MN3
MN3_full <- ts(c(US_3[, 2], MN3), start = c(2020, 64), frequency = 365)
plot(MN3_full)
MN_actual <- c(303, 222, 188, 415, 373, 365, 434, 455, 306, 244)
RMSE(MN3, MN_actual)/mean(MN_actual) # 0.55
TX3 <- fcastUS_3$fcst[3]
TX3 <- TX3$TX[, 1]
TX3 <- TX3 + 2340
TX3 # looks good
TX3_full <- ts(c(US_3[, 3], TX3), start = c(2020, 64), frequency = 365)
plot(TX3_full)
TX_actual <- c(1918, 1265, 3815, 3141, 3509, 3471, 4391, 3865, 3279, 5532)
RMSE(TX3, TX_actual)/mean(TX_actual) # 0.39

# use recent data from usafacts and limit to deaths but include multiple counties
str(US_7deaths)
US_7dcum <- as.data.frame(t(US_7deaths))
tail(US_7dcum)
US_7dcum <- US_7dcum[45:145, ]
colnames(US_7dcum) <- c("CO", "MI", "MN", "NE", "PA", "SD", "TX")
dim(US_7dcum)
US_7dcum <- US_7dcum %>% mutate_if(is.factor, as.character)
US_7dcum <- US_7dcum %>% mutate_if(is.character, as.integer)
US_7d <- diffM(US_7dcum)
head(US_7d)
# convert to ts
US_7dts <- ts(US_7d, start = c(2020, 64), frequency = 365)
str(US_7dts)
autoplot(US_7dts, main = "Time series plot of the US_7d data")
# difference the entire multivariate ts
US_7ddiff <- diffM(US_7d)
US_7d[45:55, ]
US_7ddiff[45:55, ]
apply(US_7ddiff, 2, adf.test) # now stationary (p value < 0.05)
US_7dtsdiff <- ts(US_7ddiff)
autoplot(US_7dtsdiff, main = "Time series plot of the stationary (differenced) MN_9ts data")
# lag order identification
VARselect(US_7dtsdiff, type = "none", lag.max = 15) # 11 is best lag for all selection criteria
# create the VAR model
var.US_7d <- VAR(US_7dtsdiff, type = "none", lag.max = 11, ic = "AIC") # errors above 10 due to small size of ts
summary(var.US_7d) # relatively good model with low p-values and high adj r-squared
# diagnostic tests = Portmanteau Test
serial.test(var.US_7d) # p-value below 0.05 indicates we have autocorrelation in errors
# Granger test for causality (all show causality)
causality(var.US_7d, cause = c("CO")) # there is causality (low p-values for both Granger and Instant)
causality(var.US_7d, cause = c("MI")) # there is causality (low p-values for both Granger and Instant)
causality(var.US_7d, cause = c("MN")) # there is causality (low p-values for both Granger and Instant)
# other diagnostic tests
arch.test(var.US_7d) # test for autoregressive conditional heteroscedasticity (ARCH), p-value = 1
normality.test(var.US_7d) # p-value below 0.05
# use original model to forecast deaths
fcastUS_7d <- predict(var.US_7d, n.ahead = 10) # predict through 06.25.20
fcastUS_7d
par(mar = c(0.5, 0.5, 0.5, 0.5))
plot(fcastUS_7d) # forecasts are much too varied compared to differenced data (same problem as cases VAR model)
# pull out just the CO fcasts
COd <- fcastUS_7d$fcst[1]; COd
COd <- COd$CO[, 1]; COd
# Invert the differencing (add the last value of the ts data to the forecasts)
tail(US_7d) # deaths added each day - use this # b/c we differenced this data
COd <- COd + 3
COd # many of these are impossible #'s because lowest possible is 0
par(mfrow = c(1, 1))
plot.ts(COd)
# add data and forecast to one time series
CO_fulld <- ts(c(US_7d[, 1], COd), start = c(2020, 64), frequency = 365)
plot(CO_fulld) # far too volatile
# not a good model at all! 
#Try 3 states
# use CO, MN, and TX (my hypothesis is that CO is lagging indicator for MN and TX)
US_3d <- US_7d[, c(1, 3, 7)]
US_3ddiff <- diffM(US_3d)
US_3dts <- ts(US_3ddiff, start = c(2020, 64), frequency = 365)
autoplot(US_3dts, main = "Time series plot of the stationary (differenced) US_3d data")
# build model
VARselect(US_3dts, type = "none") # 10
var.US_3d <- VAR(US_3dts, type = "none", lag.max = 10)
serial.test(var.US_3d) # p-value too low (but better than cases model)
# generate impulse response functions to describe response of Stearns to Hennepin changes
irf.COd <- irf(var.US_3d, n.ahead = 10, impulse = "CO", response = "MN", runs = 500)
plot(irf.COd, ylab = "MN Cases", main = "Shock from CO Cases")
irf.MNd <- irf(var.US_3d, impulse = "MN", response = "CO", n.ahead = 10, runs = 500)
plot(irf.MNd, ylab = "CO Cases", main = "Shock from MN Cases")
US_3d.vardec <- fevd(var.US_3d, n.ahead = 10) # forecast error variance decompositions
plot(US_3d.vardec) # TX is influenced by MN and CO, but others are not influenced by any other state
# view forecasts using US_3d model
fcastUS_3d <- predict(var.US_3d, n.ahead = 10) # predict through 06.25.20
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
CO3d <- CO3d + 3
CO3d # these values seem more realistic but still volatile
par(mfrow = c(1, 1))
plot.ts(CO3d)
# add data and forecast to one time series
CO3d_full <- ts(c(US_3d[, 1], CO3d), start = c(2020, 64), frequency = 365)
plot(CO3d_full)
CO_actual = c(1, 5, 13, 14, 6, 6, 4, 0, 4, 14) # actual cases for 6/14 through 6/23
RMSE(CO3d, CO_actual)/mean(CO_actual) # 2.46 NOT GREAT
# model looks much better
# view MN and TX data
MN3d <- fcastUS_3d$fcst[2]
MN3d <- MN3d$MN[, 1]
MN3d <- MN3d + 9
MN3d # looks good
MN3d_full <- ts(c(US_3d[, 2], MN3d), start = c(2020, 48), frequency = 365)
plot(MN3d_full)
MN_actual <- c(15, 6, 9, 12, 18, 20, 11, 8, 4, 9)
RMSE(MN3d, MN_actual)/mean(MN_actual) # 0.64

TX3d <- fcastUS_3d$fcst[3]
TX3d <- TX3d$TX[, 1]
TX3d <- TX3d + 1
TX3d # negative values? 
TX3d_full <- ts(c(US_3d[, 3], TX3d), start = c(2020, 48), frequency = 365)
plot(TX3d_full) # values too small to predict well
TX_actual <- c(19, 7, 47, 33, 43, 36, 25, 16, 10, 29)
RMSE(TX3d, TX_actual)/mean(TX_actual) # 1.17

