# https://stats.stackexchange.com/questions/99907/how-to-test-the-influence-of-external-factors-on-time-series
# https://bookdown.org/ccolonescu/RPoE4/
# USE USAFACTS CUMULATIVE CASES AND DEATHS FILES
# EXPLODE OUT COLUMNS SO THAT EACH REPRESENTS A RECORD
# ADD POPULATION, TOTAL CASES, TOTAL DEATHS
# RUN ML ALGORITHM TO PREDICT (LINEAR REG, DECISION TREE, RANDOM FOREST)

getwd()
setwd('/Users/reginaduval/Grad_Work/MSDS692_Practicum1/Project/Data')

library(astsa)
library(caret)
library(DAAG)
library(forecast)
library(fpp2)
library(funModeling)
library(forecast)
library(MARSS)
library(MTS)
library(quantmod)
library(readxl)
library(reshape)
library(tidyverse)
library(tseries)
library(urca)
library(vars)

# load in MN county level data from usafacts (wide form)
# http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
setwd('/Users/reginaduval/Grad_Work/MSDS692_Practicum1/Project/Data/County Level')
MN_0613 <- read.csv("covid_cases_0613.csv", header = TRUE, stringsAsFactors = FALSE)
MN_0613 <- MN_0613 %>% filter(State == "MN") %>% filter(countyFIPS != "0") # remove unallocated cases
MN_0613 <- MN_0613[, -(3)] # remove State
str(MN_0613)
tail(MN_0613)
# load in extra variables
county_data <- read.csv("County_data_distance2.csv", header = TRUE, stringsAsFactors = FALSE)
str(county_data)
county_data <- county_data[, c(1, 7, 9, 10)] # remove unnecessary columns
MN_0613_full <- MN_0613 %>% left_join(county_data, by = "countyFIPS")
str(MN_0613_full)
summary(MN_0613_full) # notice which columns contain nothing but 0 values (cols 3:46)
tail(MN_0613_full)
MN_0613_full <- MN_0613_full[,-(3:46)] # remove columns with no cases
data_wide <- as.data.frame(t(MN_0613_full))
class(data_wide)
head(data_wide, 1)
colnames(data_wide) <- MN_0613_full$countyname
data_wide <- data_wide[-1, ] # remove countyFIPS row
str(data_wide)
head(data_wide)
data_long <- gather(MN_0613_full, date, cases, X3.5.20:X6.13.20, factor_key = TRUE)
str(data_long)
summary(data_long)
data_long$meat_plant <- as.factor(data_long$meat_plant)
data_long$meat_plant <- as.numeric(data_long$meat_plant)
str(data_long)
data_long[500:550, ] # view a piece of the data frame
# basic EDA
plot_num(data_long)
cor(data_long[, c(2, 3, 4, 6)]) # no obvious correlation between variables, highest cor is cases and pop
pairs(data_long[, c(2, 3, 4, 6)])
write.csv(data_long, "MN_0613_full.csv", row.names = FALSE)
write.csv(data_wide, "MN_0613_full2.csv", row.names = FALSE)
# predict cases using linear regression
# https://www.statmethods.net/stats/regression.html
# http://r-statistics.co/Model-Selection-in-R.html
# https://bookdown.org/ccolonescu/RPoE4/simplelm.html
mod1 <- lm(cases ~ ., data = data_long)
summary(mod1)# variables are all significant but adj R-squared is quite low (0.3985)
selectedMod <- step(mod1) # no improvement to AIC to exclude any variable
summary(selectedMod) # best model includes all variables
# can't use data.wide b/c there are too many variables
# http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/
# split data into training and test set
set.seed(307)
training.samples <- data_long$cases %>% createDataPartition(p = 0.8, list = FALSE)
reg_train <- data_long[training.samples, ]
reg_test <- data_long[-training.samples, ]
mod2 <- lm(cases ~ ., data = reg_train)
pred <- mod2 %>% predict(reg_test)
data.frame(R2 = R2(pred, reg_test$cases),
           RMSE = RMSE(pred, reg_test$cases),
           MAE = MAE(pred, reg_test$cases)) # look for lowest test sample RMSE, Root Mean Sq Error (365.1917)
RMSE(pred, reg_test$cases)/mean(reg_test$cases) # we want this as small as possible (4.12)
# K-fold cv
set.seed(307)
train.control <- trainControl(method = "cv", number = 10)
mod4 <- train(cases ~ ., data = data_long, method = "lm", trControl = train.control)
print(mod4) # RMSE 422.8452 higher than base model
pred4 <- mod4 %>% predict(reg_train)
RMSE(pred4, reg_test$cases)/mean(reg_test$cases) # 6.13 higher than base model
# repeated K-fold cv
set.seed(307)
train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
mod5 <- train(cases ~ ., data = data_long, method = "lm", trControl = train.control)
print(mod5) # RMSE 422.9541 higher than other two models
pred5 <- mod5 %>% predict(reg_train)
RMSE(pred5, reg_test$cases)/mean(reg_test$cases) # 6.13

# Repeat for deaths
MN_0613d <- read.csv("covid_deaths_0613.csv", header = TRUE, stringsAsFactors = FALSE)
MN_0613d <- MN_0613d %>% filter(State == "MN") %>% filter(countyFIPS != "0") # remove unallocated deaths
MN_0613d <- MN_0613d[, -(3)] # remove State
str(MN_0613d)
tail(MN_0613d)
# load in extra variables
MN_0613d_full <- MN_0613d %>% left_join(county_data, by = "countyFIPS")
str(MN_0613d_full)
summary(MN_0613d_full) # notice which columns contain nothing but 0 values (cols 4:62)
tail(MN_0613d_full)
MN_0613d_full <- MN_0613d_full[,-(3:62)] # remove columns with no cases
data_wideD <- as.data.frame(t(MN_0613d_full))
class(data_wideD)
head(data_wideD, 1)
colnames(data_wideD) <- MN_0613d_full$countyname
data_wideD <- data_wideD[-1, ] # remove countyFIPS row
str(data_wideD)
head(data_wideD)
data_longD <- gather(MN_0613d_full, date, deaths, X3.21.20:X6.13.20, factor_key = TRUE)
str(data_longD)
summary(data_longD)
data_longD <- data_longD[, 2:7] # drop countyFIPS column
data_longD$meat_plant <- as.factor(data_longD$meat_plant)
data_longD$meat_plant <- as.numeric(data_longD$meat_plant)
str(data_longD)
data_longD[500:550, ] # view a piece of the data frame
# basic EDA
plot_num(data_longD)
cor(data_longD[, c(2, 3, 4, 6)]) # no obvious correlation between variables, highest cor is deaths and pop
pairs(data_longD[, c(2, 3, 4, 6)])
write.csv(data_longD, "MN_0613d_full.csv", row.names = FALSE)
# predict cases using linear regression
mod1d <- lm(deaths ~ ., data = data_longD)
summary(mod1d)# variables are all significant and adj R-squared is okay (0.5631)
selectedModD <- step(mod1d) # no improvement to AIC to exclude any variable
summary(selectedModD) # best model includes all variables
# split data into training and test set
set.seed(307)
training.samplesD <- data_longD$deaths %>% createDataPartition(p = 0.8, list = FALSE)
reg_trainD <- data_longD[training.samplesD, ]
reg_testD <- data_longD[-training.samplesD, ]
mod2D <- lm(deaths ~ ., data = reg_trainD)
predD <- mod2D %>% predict(reg_testD)
data.frame(R2 = R2(predD, reg_testD$deaths),
           RMSE = RMSE(predD, reg_testD$deaths),
           MAE = MAE(predD, reg_testD$deaths)) # look for lowest test sample RMSE, Root Mean Sq Error (30.59)
RMSE(predD, reg_testD$deaths)/mean(reg_testD$deaths) # we want this as small as possible (5.87)
# K-fold cv
set.seed(307)
train.control <- trainControl(method = "cv", number = 10)
mod4D <- train(deaths ~ ., data = data_longD, method = "lm", trControl = train.control)
print(mod4D) # RMSE 0.55 much lower than base model
pred4D <- mod4D %>% predict(reg_trainD)
RMSE(pred4D, reg_testD$deaths)/mean(reg_testD$deaths) # 9.99 higher than base model
# repeated K-fold cv
set.seed(307)
train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
mod5D <- train(deaths ~ ., data = data_longD, method = "lm", trControl = train.control)
print(mod5D) # RMSE 0.55 same as cv model
pred5D <- mod5D %>% predict(reg_trainD)
RMSE(pred5D, reg_testD$deaths)/mean(reg_testD$deaths) # 9.99 very poor predictor