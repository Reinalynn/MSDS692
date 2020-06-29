# Create new file with extra variables (pop, dist, meat) by FIPS

library(tidyverse)
getwd()
setwd('/Users/reginaduval/Grad_Work/MSDS692_Practicum1/Project/Data')

data <- read.csv("Distance Data/County_dist_maxpop.csv", header = TRUE)
class(data)
head(data)
data2 <- read.csv("Distance Data/uscities.csv", header = TRUE)
head(data2)

data3 <- read.csv("Distance Data/sf12010countydistancemiles.csv", header = TRUE)
tail(data3)
data3$concatenate <- paste0(data3$county1, data3$county2)
tail(data3)

data4 <- read.csv("County_dist_maxpop.csv", header = TRUE)
head(data4)
data4 <- data4[, 1:6]
head(data4)
data4$concatenate <- data4$concatenate_BD
head(data4)
data4 <- data4[, -6]
head(data4)
dim(data4)
data_full <- merge(data3[, c("concatenate", "mi_to_county")], data4, by.y, all.y = all)
tail(data_full)
summary(data_full)
str(data_full)
data_full$state <- as.character(data_full$state)
data_full <- data_full %>% arrange(state)
head(data_full)
final_data <- data_full[, c(3, 4, 5, 6, 7, 2)]
head(final_data)
is.na(final_data)
write.csv(final_data, "County_data_distance.csv", row.names = FALSE)

# add pop to "County_data_distance.csv" from train2 dataset
# combine county data with train2 dataset
dist <- read.csv("County Level/County_data_distance.csv", header = TRUE, stringsAsFactors = FALSE)
head(dist)
dim(dist)
train <- read.csv("train2.csv", header = TRUE, stringsAsFactors = FALSE)
train_US <- train %>% filter(Country_Region == "US") %>% filter(County != "")
train_US$concatenate <- paste(train_US$County, train_US$Province_State, sep = "")
head(train_US)
dim(train_US)
train_US <- train_US[, c(4, 8)]
dist2 <- left_join(dist, train_US, by = "concatenate")
dim(dist2)
head(dist2)
dist2 <- unique(dist2)
dim(dist2)
head(dist2)
write.csv(dist2, "Distance Data/County_data.csv", row.names = FALSE)
