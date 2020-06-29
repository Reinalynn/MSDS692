# EDA

# add total cases/deaths by county to the file with county distances, pop, and meat plant
total_cases <- cases0624[, c(1, 159)] # first column is FIPS, last column is cumulative cases as of 0624
total_deaths <- deaths0624[, c(1, 159)] # repeat for deaths
total <- merge(total_cases, total_deaths, by = "FIPS")
colnames(total) <- c("FIPS", "total_cases", "total_deaths")
data <- left_join(county_data, total, by = "FIPS")
str(data)

# EDA on data
summary(data) # meat plant 0 = no, 1 = yes
freq(data)
plot_num(data)
hist(data$mi_to_county)
hist(data$dist_cat) # categories  = 1 (0mi), 2 (< 25), 3 (< 75), 4 (< 150), 5 (< 300), 6 (300 +)
ggplot(data, aes(x = total_cases, y = total_deaths, color = dist_cat)) + 
  geom_point()
#load file with selected counties and repeat process
dist2 <- data %>% filter(state == "Minnesota")
head(dist2, 10)
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
head(states)
counties <- get_urbn_map("counties", sf = TRUE)
counties$FIPS <- as.integer(counties$county_fips)
head(counties)
str(counties)
spatial_data <- merge(counties, data, by = "FIPS")
head(spatial_data)
spatial_data2 <- merge(counties, dist2, by = "FIPS")
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
cuts <- c(0, 25, 75, 150, 300, 1400)
# palette_explorer()
# Distance maps of US counties
tm_shape(spatial_data, projections = 2163) +
  tm_polygons(col = "mi_to_county", breaks = cuts, palette = "-BuPu") +
  tm_layout(title = "Distance to State Population Center",
            title.size = 1.1,
            title.position = c("center", "top"))
plants <- spatial_data %>% filter(meat_plant == "1") # add counties w/meat plants
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
spatial_data_MN <- spatial_data %>% filter(state == "Minnesota")
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
