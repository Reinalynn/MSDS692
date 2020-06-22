# ADD MORE RECENT DATA TO US, MN, SELECT COUNTIES (FROM USAFACTS)
# RERUN TS FORECASTS USING UPDATED DATA AND MEASURE ACCURACY, PREDICT NEXT 10 DAYS

setwd('/Users/reginaduval/Grad_Work/MSDS692_Practicum1/Project/Data/County Level')

library(data.table)
library(dplyr)
library(matrixStats)
library(varhandle)


# data through 06/12/2020
# https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/
cases0612 <- read.csv("covid_confirmed_usafacts.csv", header = TRUE, stringsAsFactors = FALSE)
deaths0612 <- read.csv("covid_deaths_usafacts.csv", header = TRUE, stringsAsFactors = FALSE)

# steps to combine data
# filter cases to single county
MN0612c <- cases0612 %>% filter(State == "MN")
Hennepin0612c <- MN0612c %>% filter(County.Name == "Hennepin County")
Hennepin0612c <- Hennepin0612c[, 5:148]
dim(Hennepin0612c)
class(Hennepin0612c)
# invert columns and rows
x <- as.data.frame(t(Hennepin0612c))
dim(x)
class(x)
colnames(x) <- "V1"
str(x)
start <- x[1, 1]
start
tail(x)
# add col that shows difference between values (USAfacts data is cumulative)
x$diff <- c(start, diff(x$V1))
x
colnames(x) <- c("Cum", "Cases")
# repeat for deaths
MN0612d <- deaths0612 %>% filter(State == "MN")
Hennepin0612d <- MN0612d %>% filter(County.Name == "Hennepin County")
Hennepin0612d <- Hennepin0612d[, 5:148]
str(Hennepin0612d)
dim(Hennepin0612d)
# invert columns and rows
y <- as.data.frame(t(Hennepin0612d))
dim(y)
colnames(y) <- "V1"
str(y)
start <- y[1, 1]
start
tail(y)
# add col that shows difference between values (USAfacts data is cumulative)
y$diff <- c(start, diff(y$V1))
head(y)
tail(y)
colnames(y) <- c("Cum", "Deaths")
# combine Cases and Deaths
HennMN0612 <- cbind(x$Cases, y$Deaths)
colnames(HennMN0612) <- c("Cases", "Deaths")
HennMN0612
str(HennMN0612)
tail(HennMN0612)
HennMNts <- ts(HennMN0612, start = c(2020, 22), frequency = 365)
str(HennMNts)
head(HennMNts)
HennMNts
autoplot(HennMNts, main = "COVID-19 Cases and Deaths - Minnesota")


# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(HennMNts)
Henn_cases <- HennMNts[, "Cases"]
Henn_cases_train <- Henn_cases %>% window(end = c(2020, 124))
Henn_cases_test <- Henn_cases %>% window(end = c(2020, 144))
Henn_deaths <- HennMNts[, "Deaths"]
Henn_deaths_train <- Henn_deaths %>% window(end = c(2020, 124))
Henn_deaths_test <- Henn_deaths %>% window(end = c(2020, 144))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Henn_cases <- auto.arima(Henn_cases, stepwise = FALSE, approximation = FALSE)
fit_Henn_cases # ARIMA(0, 1, 1), AICc - 1603.27
autoplot(fit_Henn_cases)
sarima.for(Henn_cases_train, n.ahead = 20, 0, 1, 1)
lines(Henn_cases_test)
checkresiduals(fit_Henn_cases) # passes
fit_Henn_deaths <- auto.arima(Henn_deaths, stepwise = FALSE, approximation = FALSE)
fit_Henn_deaths # ARIMA(3, 1, 2), AICc - 725.33
autoplot(fit_Henn_deaths)
sarima.for(Henn_deaths_train, n.ahead = 20, 3, 1, 2)
lines(Henn_deaths_test)
checkresiduals(fit_Henn_deaths) # p-value too low, does not pass Ljung-Box test
fit_Henn_deaths2 <- arima(Henn_deaths, order = c(6, 1, 1))
sarima.for(Henn_deaths_train, n.ahead = 20, 6, 1, 1)
lines(Henn_deaths_test)
checkresiduals(fit_Henn_deaths2) # passes
# Use best models to forecast further ahead
fc_10_Henn <- sarima.for(Henn_cases, n.ahead = 10, 0, 1, 1)
fc_10_Henn$pred
fcd_10_Henn <- sarima.for(Henn_deaths, n.ahead = 10, 6, 1, 1) 
fcd_10_Henn$pred

# REPEAT PROCESS
# steps to combine data
# filter cases to single county
MN0612c <- cases0612 %>% filter(State == "MN")
Stearns0612c <- MN0612c %>% filter(County.Name == "Stearns County")
Stearns0612c <- Stearns0612c[, 5:148]
dim(Stearns0612c)
# invert columns and rows
x <- as.data.frame(t(Stearns0612c))
colnames(x) <- "V1"
start <- x[1, 1]
start
# add col that shows difference between values (USAfacts data is cumulative)
x$diff <- c(start, diff(x$V1))
colnames(x) <- c("Cum", "Cases")
# repeat for deaths
MN0612d <- deaths0612 %>% filter(State == "MN")
Stearns0612d <- MN0612d %>% filter(County.Name == "Stearns County")
Stearns0612d <- Stearns0612d[, 5:148]
dim(Stearns0612d)
# invert columns and rows
y <- as.data.frame(t(Stearns0612d))
colnames(y) <- "V1"
start <- y[1, 1]
start
# add col that shows difference between values (USAfacts data is cumulative)
y$diff <- c(start, diff(y$V1))
colnames(y) <- c("Cum", "Deaths")
# combine Cases and Deaths
StearnsMN0612 <- cbind(x$Cases, y$Deaths)
colnames(StearnsMN0612) <- c("Cases", "Deaths")
StearnsMNts <- ts(StearnsMN0612, start = c(2020, 22), frequency = 365)
autoplot(StearnsMNts, main = "COVID-19 Cases and Deaths - MN, Stearns County")
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(StearnsMNts)
Stearns_cases <- StearnsMNts[, "Cases"]
Stearns_cases_train <- Stearns_cases %>% window(end = c(2020, 124))
Stearns_cases_test <- Stearns_cases %>% window(end = c(2020, 144))
Stearns_deaths <- StearnsMNts[, "Deaths"]
Stearns_deaths_train <- Stearns_deaths %>% window(end = c(2020, 124))
Stearns_deaths_test <- Stearns_deaths %>% window(end = c(2020, 144))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Stearns_cases <- auto.arima(Stearns_cases, stepwise = FALSE, approximation = FALSE)
fit_Stearns_cases # ARIMA(4, 1, 0), AICc - 1197.98
autoplot(fit_Stearns_cases)
sarima.for(Stearns_cases_train, n.ahead = 20, 4, 1, 0)
lines(Stearns_cases_test)
checkresiduals(fit_Stearns_cases) # p-value too low
fit_Stearns_cases2 <- arima(Stearns_cases, order = c(4, 1, 2))
checkresiduals(fit_Stearns_cases2) # passes
fit_Stearns_deaths <- auto.arima(Stearns_deaths, stepwise = FALSE, approximation = FALSE)
fit_Stearns_deaths # ARIMA(0, 1, 5), AICc - 94.28
autoplot(fit_Stearns_deaths)
sarima.for(Stearns_deaths_train, n.ahead = 20, 0, 1, 5)
checkresiduals(fit_Stearns_deaths) # p-value too low, does not pass Ljung-Box test
fit_Stearns_deaths2 <- arima(Stearns_deaths, order = c(0, 1, 6))
checkresiduals(fit_Stearns_deaths2) # passes
# Use best models to forecast further ahead
fc_10_Stearns <- sarima.for(Stearns_cases, n.ahead = 10, 4, 1, 2)
fc_10_Stearns$pred
fcd_10_Stearns <- sarima.for(Stearns_deaths, n.ahead = 10, 0, 1, 6) 
fcd_10_Stearns$pred

# REPEAT PROCESS
# steps to combine data
# filter cases to single county
MN0612c <- cases0612 %>% filter(State == "MN")
Kandiyohi0612c <- MN0612c %>% filter(County.Name == "Kandiyohi County")
Kandiyohi0612c <- Kandiyohi0612c[, 5:148]
dim(Kandiyohi0612c)
# invert columns and rows
x <- as.data.frame(t(Kandiyohi0612c))
colnames(x) <- "V1"
start <- x[1, 1]
start
# add col that shows difference between values (USAfacts data is cumulative)
x$diff <- c(start, diff(x$V1))
colnames(x) <- c("Cum", "Cases")
# repeat for deaths
MN0612d <- deaths0612 %>% filter(State == "MN")
Kandiyohi0612d <- MN0612d %>% filter(County.Name == "Kandiyohi County")
Kandiyohi0612d <- Kandiyohi0612d[, 5:148]
dim(Kandiyohi0612d)
# invert columns and rows
y <- as.data.frame(t(Kandiyohi0612d))
colnames(y) <- "V1"
start <- y[1, 1]
start
# add col that shows difference between values (USAfacts data is cumulative)
y$diff <- c(start, diff(y$V1))
colnames(y) <- c("Cum", "Deaths")
# combine Cases and Deaths
KandiyohiMN0612 <- cbind(x$Cases, y$Deaths)
colnames(KandiyohiMN0612) <- c("Cases", "Deaths")
KandiyohiMNts <- ts(KandiyohiMN0612, start = c(2020, 22), frequency = 365)
autoplot(KandiyohiMNts, main = "COVID-19 Cases and Deaths - MN, Kandiyohi County")
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(KandiyohiMNts)
Kandiyohi_cases <- KandiyohiMNts[, "Cases"]
Kandiyohi_cases_train <- Kandiyohi_cases %>% window(end = c(2020, 124))
Kandiyohi_cases_test <- Kandiyohi_cases %>% window(end = c(2020, 144))
Kandiyohi_deaths <- KandiyohiMNts[, "Deaths"]
Kandiyohi_deaths_train <- Kandiyohi_deaths %>% window(end = c(2020, 124))
Kandiyohi_deaths_test <- Kandiyohi_deaths %>% window(end = c(2020, 144))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_Kandiyohi_cases <- auto.arima(Kandiyohi_cases, stepwise = FALSE, approximation = FALSE)
fit_Kandiyohi_cases # ARIMA(2, 1, 3), AICc - 914.05
autoplot(fit_Kandiyohi_cases)
sarima.for(Kandiyohi_cases_train, n.ahead = 20, 2, 1, 3)
lines(Kandiyohi_cases_test)
checkresiduals(fit_Kandiyohi_cases) # passes
fit_Kandiyohi_deaths <- auto.arima(Kandiyohi_deaths, stepwise = FALSE, approximation = FALSE)
fit_Kandiyohi_deaths # ARIMA(0, 0, 0), AICc - -304.97 White noise model
autoplot(fit_Kandiyohi_deaths)
sarima.for(Kandiyohi_deaths_train, n.ahead = 20, 0, 0, 0)
checkresiduals(fit_Kandiyohi_deaths) # passes Ljung-Box test
# Use best models to forecast further ahead
fc_10_Kandiyohi <- sarima.for(Kandiyohi_cases, n.ahead = 10, 2, 1, 3)
fc_10_Kandiyohi$pred
fcd_10_Kandiyohi <- sarima.for(Kandiyohi_deaths, n.ahead = 10, 0, 0, 0) 
fcd_10_Kandiyohi$pred

# REPEAT for MN Data (full state)
MN0612c <- cases0612 %>% filter(State == "MN")
MN0612c <- MN0612c[, 5:148]
dim(MN0612c)
total <- colSums(MN0612c)
length(total)
total <- as.integer(total)
MN0612c <- rbind(MN0612c, total)
class(MN0612c)
MN0612c <- MN0612c[89, ]
MN0612c
# invert columns and rows
x <- as.data.frame(t(MN0612c))
class(x)
str(x)
colnames(x) <- "V1"
str(x)
start <- x[1, 1]
start
# add col that shows difference between values (USAfacts data is cumulative)
x$diff <- c(start, diff(x$V1))
colnames(x) <- c("Cum", "Cases")
tail(x)
# repeat for deaths (full state)
MN0612d <- deaths0612 %>% filter(State == "MN")
MN0612d <- MN0612d[, 5:148]
dim(MN0612d)
total <- colSums(MN0612d)
length(total)
total <- as.integer(total)
MN0612d <- rbind(MN0612d, total)
class(MN0612d)
MN0612d <- MN0612d[89, ]
MN0612d
# invert columns and rows
y <- as.data.frame(t(MN0612d))
colnames(y) <- "V1"
str(y)
start <- y[1, 1]
start
# add col that shows difference between values (USAfacts data is cumulative)
y$diff <- c(start, diff(y$V1))
colnames(y) <- c("Cum", "Deaths")
tail(y)
# combine Cases and Deaths
MN0612 <- cbind(x$Cases, y$Deaths)
colnames(MN0612) <- c("Cases", "Deaths")
MNts <- ts(MN0612, start = c(2020, 22), frequency = 365)
autoplot(MNts, main = "COVID-19 Cases and Deaths - MN")
# CREATE TEST/TRAIN SETS FOR CASES AND DEATHS
autoplot(MNts)
MN_cases <- MNts[, "Cases"]
MN_cases_train <- MN_cases %>% window(end = c(2020, 124))
MN_cases_test <- MN_cases %>% window(end = c(2020, 144))
MN_deaths <- MNts[, "Deaths"]
MN_deaths_train <- MN_deaths %>% window(end = c(2020, 124))
MN_deaths_test <- MN_deaths %>% window(end = c(2020, 144))
# BEST MODELS - use auto.arima models for simplicity and consistency
fit_MN_cases <- auto.arima(MN_cases, stepwise = FALSE, approximation = FALSE)
fit_MN_cases # ARIMA(3, 1, 2), AICc - 1716.48
autoplot(fit_MN_cases)
sarima.for(MN_cases_train, n.ahead = 20, 3, 1, 2)
lines(MN_cases_test)
checkresiduals(fit_MN_cases) # passes
fit_MN_deaths <- auto.arima(MN_deaths, stepwise = FALSE, approximation = FALSE)
fit_MN_deaths # ARIMA(3, 1, 2), AICc - 836.77
autoplot(fit_MN_deaths)
sarima.for(MN_deaths_train, n.ahead = 20, 3, 1, 2)
lines(MN_deaths_test)
checkresiduals(fit_MN_deaths) # does not pass
fit_MN_deaths2 <- arima(MN_deaths, order = c(4, 1, 3))
checkresiduals(fit_MN_deaths2) # passes
sarima.for(MN_deaths_train, n.ahead = 20, 4, 1, 3)
lines(MN_deaths_test)
# Use best models to forecast further ahead
fc_10_MN <- sarima.for(MN_cases, n.ahead = 10, 3, 1, 2)
fc_10_MN$pred
fcd_10_MN <- sarima.for(MN_deaths, n.ahead = 10, 4, 1, 3) 
fcd_10_MN$pred