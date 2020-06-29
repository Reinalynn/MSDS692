# use recent data from usafacts and limit to cases but include multiple counties
MN_9cum <- read.csv("MN_9_0613.csv", header = TRUE, stringsAsFactors = FALSE)
MN_9cum <- as.data.frame(t(MN_9cum))
MN_9cum <- MN_9cum[5:148, ]
colnames(MN_9cum) <- c("A", "C", "D", "H", "K", "N", "R", "S", "W")
dim(MN_9cum)
str(MN_9cum)
tail(MN_9cum)
MN_9cum <- MN_9cum %>% mutate_if(is.factor, as.character)
MN_9cum <- MN_9cum %>% mutate_if(is.character, as.integer)
# truncate to remove days prior to reporting (first case occurs on row 45) - pause b/c of VAR restrictions
tail(MN_9cum, 15)
# add col that shows difference between values (USAfacts data is cumulative)
MN_9 <- MN_9cum
start = 0
MN_9$Anoka <- c(start, diff(MN_9$A))
MN_9$Clay <- c(start, diff(MN_9$C))
MN_9$Dakota <- c(start, diff(MN_9$D))
MN_9$Hennepin <- c(start, diff(MN_9$H))
MN_9$Kandiyohi <- c(start, diff(MN_9$K))
MN_9$Nobles <- c(start, diff(MN_9$N))
MN_9$Ramsey <- c(start, diff(MN_9$R))
MN_9$Stearns <- c(start, diff(MN_9$S))
MN_9$Washington <- c(start, diff(MN_9$W))
head(MN_9, 10)
MN_9 <- MN_9[, 10:18]
# convert to ts
MN_9ts <- ts(MN_9, start = c(2020, 51), frequency = 365)
str(MN_9ts)
autoplot(MN_9ts, main = "Time series plot of the MN_9 data")
# determine k for adf.test
trunc((length(MN_9)-1)^(1/3))
# perform adf.test to check for stationarity (H0 = non-stationary)
apply(MN_9ts, 2, adf.test) # none of the ts is stationary yet
# difference the entire multivariate ts
MN_9tsdiff <- diffM(MN_9)
MN_9[45:55, ]
MN_9tsdiff[45:55, ]
apply(MN_9tsdiff, 2, adf.test) # now stationary (p value < 0.05)
MN_9tsdiff <- ts(MN_9tsdiff)
autoplot(MN_9tsdiff, main = "Time series plot of the stationary (differenced) MN_9ts data")
# lag order identification
VARselect(MN_9tsdiff, type = "none", lag.max = 15) # 11 is best lag for most selection criteria
# https://www.rdocumentation.org/packages/vars/versions/1.5-3/topics/VARselect
# https://cran.r-project.org/web/packages/vars/vars.pdf
# if we include dist, pop, and meat_plant here, are they exogenous regressors?
# create the VAR model
var.MN_9 <- VAR(MN_9tsdiff, type = "none", lag.max = 10, ic = "AIC") # errors above 10 due to small size of ts
summary(var.MN_9) # relatively good model with low p-values and high adj r-squared
# diagnostic tests = Portmanteau Test
serial.test(var.MN_9) # p-value below 0.05 indicates we have autocorrelation in errors
# Granger test for causality (all show causality)
causality(var.MN_9, cause = c("Anoka")) # there is causality (low p-values for both Granger and Instant)
causality(var.MN_9, cause = c("Clay")) # there is causality (low p-values for both Granger and Instant)
causality(var.MN_9, cause = c("Dakota")) # there is causality (low p-values for both Granger and Instant)
causality(var.MN_9, cause = c("Hennepin")) # causality, significant
causality(var.MN_9, cause = c("Kandiyohi")) # causality
causality(var.MN_9, cause = c("Nobles")) # causality
causality(var.MN_9, cause = c("Ramsey")) # causality
causality(var.MN_9, cause = c("Stearns")) # causality
causality(var.MN_9, cause = c("Washington")) # causality
# other diagnostic tests
arch.test(var.MN_9) # test for autoregressive conditional heteroscedasticity (ARCH), p-value = 1
normality.test(var.MN_9) # p-value below 0.05
# try p = 5 b/c that's what the Augmented Dickey-Fuller test indicated
var.MN_9a <- VAR(MN_9tsdiff, type = "none", p = 5)
serial.test(var.MN_9a) # p-value is same as original model, much too low
# even though both models have flaws, use original model to forecast 
fcastMN_9 <- predict(var.MN_9, n.ahead = 12) # predict through 06.25.20
fcastMN_9 # values seem to be volatile considering some counties have small #'s of cases at tail of data
par(mar = c(0.5, 0.5, 0.5, 0.5))
plot(fcastMN_9) # forecasts are much too varied compared to differenced data
# pull out just the Hennepin county fcasts
Hennepin <- fcastMN_9$fcst[4]; Hennepin
Hennepin <- Hennepin$Hennepin[, 1]; Hennepin
# Invert the differencing (add the last value of the ts data to the forecasts)
tail(MN_9) # cases added each day - use this # b/c we differenced this data
tail(MN_9cum) # total cases
Hennepin <- Hennepin + 135
Hennepin # many of these are impossible #'s because lowest possible is 0
par(mfrow = c(1, 1))
plot.ts(Hennepin)
# add data and forecast to one time series
Henn_full <- ts(c(MN_9[, 4], Hennepin), start = c(2020, 22), frequency = 365)
plot(Henn_full)
# not a good model at all! 
# First, truncate data and use lower p in VAR model (because data is smaller) - first case appears on day 45
MN_9b<- MN_9[45:144, ]
MN_9bdiff <- diffM(MN_9b)
MN_9btsdiff <- ts(MN_9bdiff)
autoplot(MN_9btsdiff, main = "Time series plot of the stationary (differenced) MN_9ts data, truncated")
# lag order identification
VARselect(MN_9btsdiff, type = "none", lag.max = 9) # 9 is best lag for  selection criteria
# create the VAR model
var.MN_9b <- VAR(MN_9btsdiff, type = "none", lag.max = 9, ic = "AIC") # errors above 10 due to small size of ts
summary(var.MN_9b)
# diagnostic tests = Portmanteau Test
serial.test(var.MN_9b) # p-value below 0.05 still
#Try fewer counties?
# use Hennepin, Kandiyohi, and Stearns (my hypothesis is that Henn is lagging indicator for Kandi and Stearns)
MN_3 <- MN_9b[, c(4, 5, 8)]
MN_3diff <- diffM(MN_3)
MN_3tsdiff <- ts(MN_3diff, start = c(2020, 45), frequency = 365)
autoplot(MN_3tsdiff, main = "Time series plot of the stationary (differenced) MN_3 data")
# build model
VARselect(MN_3tsdiff, type = "none") # try 9 (can't do 10 b/c of error)
var.MN_3 <- VAR(MN_3tsdiff, type = "none", p = 9)
serial.test(var.MN_3) # p-value too low
# generate impulse response functions to describe response of Stearns to Hennepin changes
# https://kevinkotze.github.io/ts-7-tut/
# https://www.r-econometrics.com/timeseries/varintro/ 
irf.stearns <- irf(var.MN_3, n.ahead = 12, impulse = "Hennepin", response = "Stearns", runs = 500)
plot(irf.stearns, ylab = "Stearns Cases", main = "Shock from Hennepin Cases")
irf.hennepin <- irf(var.MN_3, impulse = "Stearns", response = "Hennepin", n.ahead = 12, runs = 500)
plot(irf.hennepin, ylab = "Hennepin Cases", main = "Shock from Stearns Cases")
irf.henn2 <- irf(var.MN_3, impulse = "Hennepin", response = "Hennepin", n.ahead = 12, runs = 500)
plot(irf.henn2, ylab = "Hennepin Cases", main = "Shock from Hennepin Cases")
MN_3.vardec <- fevd(var.MN_3, n.ahead = 12) # forecast error variance decompositions
plot(MN_3.vardec)
# view forecasts using MN_3 model VAR(9)
fcastMN_3 <- predict(var.MN_3, n.ahead = 12) # predict through 06.25.20
fcastMN_3 # better than full model
plot(fcastMN_3, names = "Stearns") 
plot(fcastMN_3, names = "Hennepin")
plot(fcastMN_3, names = "Kandiyohi")
fanchart(fcastMN_3)
# pull out just the Hennepin county fcasts
Hennepin3 <- fcastMN_3$fcst[1]; Hennepin
Hennepin3 <- Hennepin3$Hennepin[, 1]; Hennepin
# Invert the differencing (add the last value of the ts data to the forecasts)
tail(MN_9) # cases added each day - use this # b/c we differenced this data
tail(MN_9cum) # total cases
Hennepin3 <- Hennepin3 + 135
Hennepin3 # these values seem more realistic
par(mfrow = c(1, 1))
plot.ts(Hennepin3)
# add data and forecast to one time series
Henn3_full <- ts(c(MN_3[, 1], Hennepin3), start = c(2020, 45), frequency = 365)
plot(Henn3_full)
# model looks somewhat better
# view Kandiyohi and Stearns data
Kandi3 <- fcastMN_3$fcst[2]
Kandi3 <- Kandi3$Kandiyohi[, 1]
Kandi3 <- Kandi3 + 3
Kandi3 # some negative values (cases cannot be below 0)
Kandi3_full <- ts(c(MN_3[, 2], Kandi3), start = c(2020, 45), frequency = 365)
plot(Kandi3_full)
Stearns3 <- fcastMN_3$fcst[3]
Stearns3 <- Stearns3$Stearns[, 1]
Stearns3 <- Stearns3 + 4
Stearns3 # negative value
Stearns3_full <- ts(c(MN_3[, 3], Stearns3), start = c(2020, 45), frequency = 365)
plot(Stearns3_full)

# add pop, distance, and meat_plant to 3 counties to see if they are exogenous regressors
MN_9
MN_3b <- MN_3
# query pop, dist, and meat_plant from MNdata_full (from MNcounties.regression.R code)
tail(MNdata_full)
HennVariables <- MNdata_full %>% filter(County == "Hennepin") %>% filter(Date == "5/9/20")
HennVariables # pop = 1265843, dist = 0, meat = N
KandiVariables <- MNdata_full %>% filter(County == "Kandiyohi") %>% filter(Date == "5/9/20")
KandiVariables # pop = 43199, dist = 75.32, meat = Y
StearnsVariables <- MNdata_full %>% filter(County == "Stearns") %>% filter(Date == "5/9/20")
StearnsVariables # pop = 161075, dist = 66.98, meat = Y
MN_3b$HennPop <- 1265843
MN_3b$HennDist <- 0
MN_3b$HennMeat <- 1
MN_3b$KandiPop <- 43199
MN_3b$KandiDist <- 75.32
MN_3b$KandiMeat <- 2
MN_3b$StearnsPop <- 161075
MN_3b$StearnsDist <- 66.98
MN_3b$StearnsMeat <- 2
MN_3b
# build new VAR model with MN_3b
MN_3btsdiff <- ts(diffM(MN_3b), start = c(2020, 45), frequency = 365)
autoplot(MN_3btsdiff, main = "Time series plot of the stationary (differenced) MN_3b data")
VARselect(MN_3btsdiff, type = "trend") # cannot build model (all scores = -Inf)
var.MN_3b <- VAR(MN_3btsdiff, type = "none", p = 1)
serial.test(var.MN_3b) # error

# CONSTANTS cannot be added to VAR Models - look into regression model or xreg in ARIMA (ARIMAX)

# use recent data from usafacts and limit to deaths but include multiple counties
MN_9dcum <- read.csv("MN_9d_0613.csv", header = TRUE, stringsAsFactors = FALSE)
MN_9dcum <- as.data.frame(t(MN_9dcum))
MN_9dcum <- MN_9dcum[5:148, ]
colnames(MN_9dcum) <- c("A", "C", "D", "H", "K", "N", "R", "S", "W")
dim(MN_9dcum)
str(MN_9dcum)
tail(MN_9dcum)
MN_9dcum <- MN_9dcum %>% mutate_if(is.factor, as.character)
MN_9dcum <- MN_9dcum %>% mutate_if(is.character, as.integer)
head(MN_9dcum, 60)
# truncate to remove days prior to reporting (first death occurs on row 59)
MN_9dcum <- MN_9dcum[58:144, ]
tail(MN_9dcum, 15)
# add col that shows difference between values (USAfacts data is cumulative)
MN_9d <- MN_9dcum
start = 0
MN_9d$Anoka <- c(start, diff(MN_9d$A))
MN_9d$Clay <- c(start, diff(MN_9d$C))
MN_9d$Dakota <- c(start, diff(MN_9d$D))
MN_9d$Hennepin <- c(start, diff(MN_9d$H))
MN_9d$Kandiyohi <- c(start, diff(MN_9d$K))
MN_9d$Nobles <- c(start, diff(MN_9d$N))
MN_9d$Ramsey <- c(start, diff(MN_9d$R))
MN_9d$Stearns <- c(start, diff(MN_9d$S))
MN_9d$Washington <- c(start, diff(MN_9d$W))
head(MN_9d, 10)
MN_9d <- MN_9d[, 10:18]
# convert to ts
MN_9dts <- ts(MN_9d, start = c(2020, 58), frequency = 365)
str(MN_9dts)
autoplot(MN_9dts, main = "Time series plot of the MN_9 data")
# determine k for adf.test
trunc((length(MN_9d)-1)^(1/3))
# perform adf.test to check for stationarity (H0 = non-stationary)
apply(MN_9dts, 2, adf.test) # none of the ts is stationary yet
# difference the entire multivariate ts
MN_9dtsdiff <- diffM(MN_9d)
MN_9d[45:55, ]
MN_9dtsdiff[45:55, ]
apply(MN_9dtsdiff, 2, adf.test) # now stationary (p value < 0.05)
MN_9dtsdiff <- ts(MN_9dtsdiff)
autoplot(MN_9dtsdiff, main = "Time series plot of the stationary (differenced) MN_9ts data")
# lag order identification
VARselect(MN_9dtsdiff, type = "none", lag.max = 15) # 8 is best lag for all selection criteria
# create the VAR model
var.MN_9d <- VAR(MN_9dtsdiff, type = "none", lag.max = 8, ic = "AIC") # errors above 10 due to small size of ts
summary(var.MN_9d) # relatively good model with low p-values and high adj r-squared
# diagnostic tests = Portmanteau Test
serial.test(var.MN_9d) # p-value below 0.05 indicates we have autocorrelation in errors
# Granger test for causality (all show causality)
causality(var.MN_9d, cause = c("Anoka")) # there is causality (low p-values for both Granger and Instant)
causality(var.MN_9d, cause = c("Clay")) # there is causality (low p-values for both Granger and Instant)
causality(var.MN_9d, cause = c("Dakota")) # there is causality (low p-values for both Granger and Instant)
causality(var.MN_9d, cause = c("Hennepin")) # causality, significant
causality(var.MN_9d, cause = c("Kandiyohi")) # causality
causality(var.MN_9d, cause = c("Nobles")) # causality
causality(var.MN_9d, cause = c("Ramsey")) # causality
causality(var.MN_9d, cause = c("Stearns")) # causality
causality(var.MN_9d, cause = c("Washington")) # causality
# other diagnostic tests
arch.test(var.MN_9d) # test for autoregressive conditional heteroscedasticity (ARCH), p-value = 1
normality.test(var.MN_9d) # p-value below 0.05
# use original model to forecast deaths
fcastMN_9d <- predict(var.MN_9d, n.ahead = 12) # predict through 06.25.20
fcastMN_9d
par(mar = c(0.5, 0.5, 0.5, 0.5))
plot(fcastMN_9d) # forecasts are much too varied compared to differenced data (same problem as cases VAR model)
# pull out just the Hennepin county fcasts
HennepinD <- fcastMN_9d$fcst[4]; Hennepin
HennepinD <- HennepinD$Hennepin[, 1]; Hennepin
# Invert the differencing (add the last value of the ts data to the forecasts)
tail(MN_9d) # cases added each day - use this # b/c we differenced this data
tail(MN_9dcum) # total cases
HennepinD <- HennepinD + 4
HennepinD # many of these are impossible #'s because lowest possible is 0
par(mfrow = c(1, 1))
plot.ts(HennepinD)
# add data and forecast to one time series
Henn_fulld <- ts(c(MN_9d[, 4], HennepinD), start = c(2020, 58), frequency = 365)
plot(Henn_fulld) # far too volatile
# not a good model at all! 
#Try 3 counties
# use Hennepin, Kandiyohi, and Stearns (my hypothesis is that Henn is lagging indicator for Kandi and Stearns)
MN_3d <- MN_9d[, c(4, 5, 8)]
MN_3ddiff <- diffM(MN_3d)
MN_3dtsdiff <- ts(MN_3ddiff, start = c(2020, 58), frequency = 365)
autoplot(MN_3dtsdiff, main = "Time series plot of the stationary (differenced) MN_3d data")
# build model
VARselect(MN_3dtsdiff, type = "none") # 8, 2, 1
var.MN_3d <- VAR(MN_3dtsdiff, type = "none", ic = "AIC")
serial.test(var.MN_3d) # p-value too low (but better than cases model)
# generate impulse response functions to describe response of Stearns to Hennepin changes
# https://kevinkotze.github.io/ts-7-tut/
# https://www.r-econometrics.com/timeseries/varintro/ 
irf.stearnsd <- irf(var.MN_3d, n.ahead = 12, impulse = "Hennepin", response = "Stearns", runs = 500)
plot(irf.stearnsd, ylab = "Stearns Cases", main = "Shock from Hennepin Cases")
irf.hennepind <- irf(var.MN_3d, impulse = "Stearns", response = "Hennepin", n.ahead = 12, runs = 500)
plot(irf.hennepind, ylab = "Hennepin Cases", main = "Shock from Stearns Cases")
irf.henn2d <- irf(var.MN_3d, impulse = "Hennepin", response = "Hennepin", n.ahead = 12, runs = 500)
plot(irf.henn2d, ylab = "Hennepin Cases", main = "Shock from Hennepin Cases")
MN_3d.vardec <- fevd(var.MN_3d, n.ahead = 12) # forecast error variance decompositions
plot(MN_3d.vardec) # other counties have less influence on forecasting deaths than cases
# view forecasts using MN_3d model
fcastMN_3d <- predict(var.MN_3d, n.ahead = 12) # predict through 06.25.20
fcastMN_3d # better than full model
plot(fcastMN_3d, names = "Stearns") 
plot(fcastMN_3d, names = "Hennepin")
plot(fcastMN_3d, names = "Kandiyohi")
fanchart(fcastMN_3d)
# pull out just the Hennepin county fcasts
Hennepin3d <- fcastMN_3d$fcst[1]; Hennepin
Hennepin3d <- Hennepin3d$Hennepin[, 1]; Hennepin
# Invert the differencing (add the last value of the ts data to the forecasts)
tail(MN_9d) # cases added each day - use this # b/c we differenced this data
tail(MN_9dcum) # total cases
Hennepin3d <- Hennepin3d + 4
Hennepin3d # these values seem more realistic
par(mfrow = c(1, 1))
plot.ts(Hennepin3d)
# add data and forecast to one time series
Henn3d_full <- ts(c(MN_3d[, 1], Hennepin3d), start = c(2020, 58), frequency = 365)
plot(Henn3d_full)
# model looks somewhat better
# view Kandiyohi and Stearns data
Kandi3d <- fcastMN_3d$fcst[2]
Kandi3d <- Kandi3d$Kandiyohi[, 1]
Kandi3d <- Kandi3d + 0
Kandi3d # some negative values (cases cannot be below 0), values too small
Kandi3d_full <- ts(c(MN_3d[, 2], Kandi3d), start = c(2020, 58), frequency = 365)
plot(Kandi3d_full)
Stearns3d <- fcastMN_3d$fcst[3]
Stearns3d <- Stearns3d$Stearns[, 1]
Stearns3d <- Stearns3d + 1
Stearns3d
Stearns3d_full <- ts(c(MN_3d[, 3], Stearns3d), start = c(2020, 58), frequency = 365)
plot(Stearns3d_full) # values too small to predict well

