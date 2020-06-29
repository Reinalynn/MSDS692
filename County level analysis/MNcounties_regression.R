
# filter to MN county level data (wide form)
MN_0624 <- cases0624 %>% filter(State == "MN") %>% filter(FIPS != "0") # remove unallocated cases
# load in extra variables
MN_0624_full <- MN_0624 %>% left_join(county_data, by = "FIPS")
MN_0624_full <- MN_0624_full[, -c(3, 4, 160, 161, 162, 163, 164)] # remove unnecessary columns
summary(MN_0624_full) # notice which columns contain nothing but 0 values (cols 3:46)
MN_0624_full <- MN_0624_full[, -(3:46)]
# explode data so that each date has a row for each column
cases_data <- gather(MN_0624_full, date, cases, X3.6.20:X6.24.20, factor_key = FALSE)
# repeat for deaths and add to data
MN_0624d <- deaths0624 %>% filter(State == "MN") %>% filter(FIPS != "0")
MN_0624d <- MN_0624d[, -c(2, 3, 4, 160, 161, 162, 163, 164)]
MN_0624d <- MN_0624d[, -(2:45)]
death_data <- gather(MN_0624d, date, deaths, X3.6.20:X6.24.20, factor_key = FALSE)
long_data <- left_join(cases_data, death_data, by = c("FIPS", "date"))
str(long_data)
long_data[500:550, ] # view a piece of the data frame
long_data <- long_data[, -c(1, 3)] # remove FIPS and mi_to_county value
filter(long_data, County.Name == "Kandiyohi County")
# basic EDA
plot_num(long_data)
cor(long_data[, c(2, 3, 4, 6, 7)]) # no obvious correlation between variables, highest cor is cases and deaths
pairs(long_data[, c(2, 3, 4, 6, 7)])
# predict cases using linear regression
mod1 <- lm(cases ~ dist_cat + meat_plant + Population + date, data = long_data)
summary(mod1)$adj.r.squared # mediocre adjusted R-squared (0.4367)
selectedMod <- step(mod1) # no improvement to AIC to exclude any variable
summary(selectedMod) # best model includes all variables
# try to predict
newdata <- data.frame(dist_cat = 4, meat_plant = 1, Population = 43000, date = "X6.24.20")
pred_lm <- predict(mod1, newdata)
pred_lm # a county that is 75.01-150 miles from Hennepin County, w/a meat plant, pop of 30k, date = 6/25/20
# predict deaths using linear regression
mod1d <- lm(deaths ~ dist_cat + meat_plant + Population + date, data = long_data)
summary(mod1d)$adj.r.squared # slightly higher adj R-squared (0.4511), all variables are significant

# split data into training and test set for cases
set.seed(307)
training.samples <- long_data$cases %>% createDataPartition(p = 0.8, list = FALSE)
reg_train <- long_data[training.samples, ]
reg_test <- long_data[-training.samples, ]
mod2 <- lm(cases ~ dist_cat + meat_plant + Population + deaths, data = reg_train)
summary(mod2) # adj R-Sq = 0.90
pred <- mod2 %>% predict(reg_test)
data.frame(R2 = R2(pred, reg_test$cases),
           RMSE = RMSE(pred, reg_test$cases),
           MAE = MAE(pred, reg_test$cases)) # look for lowest test sample RMSE, Root Mean Sq Error (210.0411)
RMSE(pred, reg_test$cases)/mean(reg_test$cases) # we want this as small as possible (1.82)
# K-fold cv
set.seed(307)
train.control <- trainControl(method = "cv", number = 10)
mod3 <- train(cases ~ dist_cat + meat_plant + Population + deaths, data = long_data, method = "lm", trControl = train.control)
summary(mod3) # Adj R-sq = 0.8977
pred3 <- mod3 %>% predict(reg_train)
RMSE(pred3, reg_test$cases)/mean(reg_test$cases) # 7.44 much higher than base model
# repeated K-fold cv
set.seed(307)
train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
mod4 <- train(cases ~ dist_cat + meat_plant + Population + deaths, data = long_data, method = "lm", trControl = train.control)
summary(mod4) # same as mod3
pred4 <- mod4 %>% predict(reg_train)
RMSE(pred4, reg_test$cases)/mean(reg_test$cases) # 7.44 still high - first model has lowest error

# split data into training and test set for deaths
set.seed(307)
training.samplesD <- long_data$deaths %>% createDataPartition(p = 0.8, list = FALSE)
reg_trainD <- long_data[training.samplesD, ]
reg_testD <- long_data[-training.samplesD, ]
mod2D <- lm(deaths ~ dist_cat + meat_plant + Population + cases, data = reg_train)
summary(mod2D) # adj R-Sq = 0.8976
predD <- mod2D %>% predict(reg_test)
data.frame(R2 = R2(predD, reg_test$deaths),
           RMSE = RMSE(predD, reg_test$deaths),
           MAE = MAE(predD, reg_test$deaths)) # look for lowest test sample RMSE, Root Mean Sq Error (13.68704)
RMSE(predD, reg_test$deaths)/mean(reg_test$deaths) # we want this as small as possible (2.90)
