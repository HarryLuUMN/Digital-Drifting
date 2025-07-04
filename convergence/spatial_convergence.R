install.packages("spdep")
install.packages("sf")
install.packages("spatialreg")

library(spatialreg)
library(sf)
library(spdep)

counties_sf <- st_read("./data/geo_data/cb_2013_us_county_500k.shp")

library(readr)
data_with_county <- read_csv("./data/cleaned_data/data_with_county.csv")


panel_data <- data_with_county %>%
  filter(YEAR %in% c(2013, 2023), !is.na(digital_index), !is.na(COUNTYFIP)) %>%
  group_by(YEAR, COUNTYFIP) %>%
  summarise(avg_index = mean(digital_index, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = YEAR, values_from = avg_index, names_prefix = "y") %>%
  filter(!is.na(y2013), !is.na(y2023)) %>%
  mutate(change = y2023 - y2013)

panel_data$COUNTYFIP <- as.integer(panel_data$COUNTYFIP)
counties_sf$COUNTYFP <- as.integer(counties_sf$COUNTYFP)  
names(counties_sf)

county_merged <- counties_sf %>%
  left_join(panel_data, by = c("COUNTYFP" = "COUNTYFIP")) %>%
  filter(!is.na(change) & !is.na(y2013))  #

nb <- poly2nb(county_merged)
listw <- nb2listw(nb, style = "W")


spatial_lag_model <- lagsarlm(change ~ y2013, data = county_merged, listw = listw)
summary(spatial_lag_model)




