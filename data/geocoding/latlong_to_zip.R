library(tidyverse)
library(tidygeocoder)
#install.packages("tidygeocoder")

lat_long_data = tibble(
lat = 40.7162469,
long = -74.0334588)

address_data <- lat_long_data %>%
  reverse_geocode(lat = lat, long = long, method = 'osm', full_results = TRUE) |>
  select(lat,long,postcode)
