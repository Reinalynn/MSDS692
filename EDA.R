getwd()
setwd('/Users/reginaduval/Grad_Work/MSDS692_Practicum1/Project/Data/County Level')

library(dplyr)
library(funModeling)
library(sf)
library(tidyverse)
library(tidycensus)
library(tmap)
library(tmaptools)
library(urbnmapr)

# load file containing county distances
dist <- read.csv("County_data_distance.csv", header = TRUE)
head(dist, 10)
dist$countyFIPS <- dist$county
# add file with population
pop <- read.csv("covid_county_population_usafacts.csv")
head(pop, 10)
# left join dist and pop
dist1 <- left_join(dist, pop, by = "countyFIPS")
# perform basic EDA
summary(dist1)
str(dist1)
# convert meat_plant category to integer
dist1$meat_plant <- as.integer(dist1$meat_plant)
# add total cases/deaths by county and map
total_cases <- read.csv("total_cases.csv", header = TRUE)
str(total_cases)
total_cases$countyFIPS <- total_cases$county_fips
data <- left_join(dist1, total_cases, by = "countyFIPS")
str(data)
data <- data[, c(4, 5, 6, 7, 8, 9, 10, 11, 12, 16)]
head(data)
total_deaths <- read.csv("total_deaths.csv", header = TRUE)
str(total_deaths)
total_deaths$countyFIPS <- total_deaths$county_fips
total_deaths <- total_deaths[, c(4, 5)]
data <- left_join(data, total_deaths, by = "countyFIPS")
str(data)

# EDA on data
summary(data)
freq(data)
plot_num(data)
hist(data$mi_to_county)
hist(data$dist_cat) # categories  = 1 (0mi), 2 (< 25), 3 (< 75), 4 (< 150), 5 (< 300), 6 (300 +)
plot(data$total_cases, data$total_deaths)
plot(data$total_cases, data$population)
ggplot(data, aes(x = total_cases, y = total_deaths, color = dist_cat)) +
  geom_point(aes(size = meat_plant))

#load file with selected counties and repeat process
dist2 <- read.csv("Select_county_data_distance.csv", header = TRUE)
head(dist2, 10)
dist2$countyFIPS <- dist2$FIPS
# perform basic EDA
summary(dist2)
str(dist2)
# convert meat_plant category to integer
dist2$meat_plant <- as.integer(dist2$meat_plant)
# add total cases/deaths by county and map
data2<- left_join(dist2, total_cases, by = "countyFIPS")
str(data2)
data2 <- data2[, c(5, 7, 8, 9, 10, 11, 12, 13)]
head(data2)

# EDA on data2
summary(data2)
str(data2)
plot_num(data2)
hist(data2$dist_from_maxpop)
hist(data2$dist_cat) # categories  = 1 (0mi), 2 (< 25), 3 (< 75), 4 (< 150), 5 (< 300), 6 (300 +)
plot(data2$cases, data2$population)
plot(data2$deaths, data2$cases)
ggplot(data2, aes(x = cases, y = deaths, color = dist_from_maxpop)) +
  geom_point(aes(size = meat_plant))
# map using urbnmapr
# https://medium.com/@urban_institute/how-to-create-state-and-county-maps-easily-in-r-577d29300bb2
head(states)
counties <- get_urbn_map("counties", sf = TRUE)
counties$countyFIPS <- as.integer(counties$county_fips)
head(counties)
str(counties)
spatial_data <- merge(counties, data, by = "countyFIPS")
head(spatial_data)
spatial_data2 <- merge(counties, dist2, by = "countyFIPS")
# https://mgimond.github.io/ES218/Week12a.html
# https://mgimond.github.io/Spatial/mapping-data-in-r.html
ggplot(spatial_data) + geom_sf(aes(fill = mi_to_county))
ggplot(spatial_data) + geom_sf(aes(fill = dist_cat))
ggplot(spatial_data) + geom_sf(aes(fill = -total_cases))
ggplot(spatial_data) + geom_sf(aes(fill = -total_deaths))
spatial_data_MN <- spatial_data %>% filter(state_abbv == "MN")
ggplot(spatial_data_MN) + geom_sf(aes(fill = mi_to_county))
ggplot(spatial_data_MN) + geom_sf(aes(fill = dist_cat))
ggplot(spatial_data_MN) + geom_sf(aes(fill = -total_cases))
ggplot(spatial_data_MN) + geom_sf(aes(fill = -total_deaths))

# use tmap package
# http://zevross.com/blog/2018/10/02/creating-beautiful-demographic-maps-in-r-with-the-tidycensus-and-tmap-packages/
# https://cran.r-project.org/web/packages/tmap/tmap.pdf
cuts <- c(0, 25, 75, 150, 300, 1400)
palette_explorer()
# Distance maps of US counties
tm_shape(spatial_data, projections = 2163) +
  tm_polygons(col = "mi_to_county", breaks = cuts, palette = "-BuPu") +
  tm_layout(title = "Distance to State Population Center",
            title.size = 1.1,
            title.position = c("center", "top"))
plants <- spatial_data %>% filter(meat_plant == "2") # add counties w/meat plants
tm_shape(spatial_data, projections = 2163) +
  tm_polygons(col = "mi_to_county", breaks = cuts, palette = "-BuPu") +
  tm_shape(plants, projections = 2163) +
  tm_polygons(col = "meat_plant", palette = "Oranges") +
  tm_layout(title = "Dist to State Pop Center, Counties with Meat Plants",
            title.size = 1.1,
            title.position = c("center", "top"))
# COVID maps of US counties
tm_shape(spatial_data, projections = 2163) +
  tm_polygons(col = "total_cases", style = "quantile", palette = "OrRd") +
  tm_layout(title = "Total COVID cases by County",
            title.size = 1.1,
            title.position = c("center", "top"))
tm_shape(spatial_data, projections = 2163) +
  tm_polygons(col = "total_deaths", style = "log10_pretty", palette = "OrRd") +
  tm_layout(title = "Total COVID deaths by County",
            title.size = 1.1,
            title.position = c("center", "top"))
# Distance maps of MN counties
MNmap <- tm_shape(spatial_data_MN, projection = 2163) + 
  tm_polygons(col = "mi_to_county", style = "quantile", palette = "-YlGnBu") + 
  tm_legend(position = c("right", "center")) +
  tm_layout(title = "Minnesota Counties,\nDist to Twin Cities",
            title.size = 1.1,
            title.position = c("center", "top"))
MNmap
MNmap + tm_shape(plants, projection = 2163) +
  tm_polygons(col = "meat_plant", palette = "Oranges")
# COVID maps of MN counties
MNmapCOVID <- tm_shape(spatial_data_MN, projection = 2163) + 
  tm_polygons(c("total_cases", "total_deaths"), style = c("quantile", "pretty"), 
              palette = list("YlGnBu", "YlOrRd")) + 
  tm_legend(position = c("right", "center")) +
  tm_layout(title = " MN Counties, COVID Data",
            title.size = 1.1,
            title.position = c("center", "top"))
MNmapCOVID
