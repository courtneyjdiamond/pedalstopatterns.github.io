P8105 Fall 2023 Final Project
================

- [Load and tidy the Citibike ridership
  data](#load-and-tidy-the-citibike-ridership-data)
- [Other Data Sets](#other-data-sets)
  - [Load and tidy the AQI data](#load-and-tidy-the-aqi-data)
  - [Load and tidy the SDI data](#load-and-tidy-the-sdi-data)
  - [Load the Overweight data](#load-the-overweight-data)
- [Incorporate Geocoding](#incorporate-geocoding)
  - [Import UHF42/ZipCode crosswalk](#import-uhf42zipcode-crosswalk)
  - [Import UHF34 / UHF 42 crosswalk](#import-uhf34--uhf-42-crosswalk)
- [Merging Datasets](#merging-datasets)
  - [Join UHF data](#join-uhf-data)
  - [Join SDI data to UHF/Zip/Neighborhood
    data](#join-sdi-data-to-uhfzipneighborhood-data)
  - [Join Overweight data to UHF/Zip/Neighborhood
    data](#join-overweight-data-to-uhfzipneighborhood-data)
  - [Join Citibike data to UHF/Zip/Neighborhood
    data](#join-citibike-data-to-uhfzipneighborhood-data)
  - [Merge Citibike, SDI, Overweight
    Data](#merge-citibike-sdi-overweight-data)
    - [Merge SDI and Overweight data](#merge-sdi-and-overweight-data)
    - [Merge SDI and Overweight data onto
      citibike](#merge-sdi-and-overweight-data-onto-citibike)
  - [Citibike - Missing zips](#citibike---missing-zips)

# Load and tidy the Citibike ridership data

Citibike Jan/2019 ~ Dec/2019

**Note to group: should it actually be Dec/2018 - Jan/2020? – Laura
(Response from HZ, see below)**

``` r
citibike = 
  tibble(
    files = list.files("citibike"),
    path = str_c("citibike/", files)
  ) |>
  mutate(data = map(path, ~read_csv(.x, col_types = cols(
    'end station id' = col_double(),
    'start station id' = col_double()
  )))) |>
  unnest(cols = c(data))
```

Tidy dataset:

- recode gender
- filter trip duration to more than 5 minutes but less than 1 day
- include trips that started in 2018 but ended in 2019 and trips that
  started in 2019 but ended in 2020
- create an age variable
- create a trip duration in minutes variable

``` r
citibike_df <- citibike |>
  janitor::clean_names() |>
  select(-files, -path) |>
  rename(trip_duration_sec = tripduration,
         start_time = starttime,
         stop_time = stoptime,
         user_type = usertype)|>
  mutate(gender = recode(gender,
                         "0" = "Unknown",
                         "1" = "Male",
                         "2" = "Female"),
        trip_duration_min = trip_duration_sec / 60,
        age = 2019 - birth_year
        ) |>
  filter(
    trip_duration_sec >= 300,
    trip_duration_sec <= 86400,
    as.Date(stop_time) >= as.Date("2019-01-01"),
    as.Date(start_time) <= as.Date("2019-12-31")
  )  |>
  select(trip_duration_sec, trip_duration_min, everything())
```

# Other Data Sets

## Load and tidy the AQI data

``` r
air <- read_csv("data/air_quality/Air_Quality_20231126.csv") 
```

``` r
 air_quality_df =air |>
  janitor::clean_names() |>
  mutate(
    start_date = mdy(start_date),
    year = year(start_date)
  ) |>
  filter(year == "2019")
```

## Load and tidy the SDI data

``` r
SDI_df <- read_csv("data/SDI_data/rgcsdi-2015-2019-zcta.csv") |>
  janitor::clean_names() |>
  select(zcta5_fips, sdi_score)|>
  rename(zip=zcta5_fips)
```

## Load the Overweight data

Data Source: [NYC Data
Portal](https://a816-dohbesp.nyc.gov/IndicatorPublic/beta/data-explorer/overweight/?id=2061#display=summary)

- Data accessed: `11/27/2023`
- Full table loaded for “Overweight and Obese Adults”

``` r
overweight = read_csv('./data/SDI_data/nyc_overweight_or_obesity_adults.csv') |>
    janitor::clean_names()

overweight_df =
 overweight |>
  mutate(number = gsub("\\*", "", number)) |>
  mutate(number = gsub(",", "", number)) |>
  mutate(number = as.numeric(number)) |>
  mutate(
    percent_low = as.numeric(gsub("^.*\\(\\s*", "", gsub("\\s*,.*$", "", percent))),
    percent_high = as.numeric(gsub("^.*\\,\\s*", "", gsub("\\)$", "", percent))),
percent = as.numeric(ifelse(grepl("\\*", percent),
                                gsub("\\*.*$", "", percent),
                                gsub("\\s*\\(.*", "", percent)))) |>
  rename(year = time) |>
  filter (year == 2019)

#write_csv(overweight_df, "data/SDI_data/overweight_data_clean.csv")
```

# Incorporate Geocoding

## Import UHF42/ZipCode crosswalk

Data source:
`https://www.nyc.gov/assets/doh/downloads/pdf/ah/zipcodetable.pdf`

``` r
# install.packages("pdftables")
# install.packages("pdftools")

uhf_zip = 
  pdf_text("data/geocoding/zipcodetable.pdf") |> 
  extract(1)

uhf_zip_df =
  uhf_zip |> 
  str_split("\n") |>
  tibble() |> 
  rename("data" = "str_split(uhf_zip, \"\\n\")") |> 
  unnest("data") |> 
  filter(data != "") |> 
  filter(!row_number() %in% c(1, 2, 51)) |> 
  filter(!row_number() %in% c(1, 2, 10, 22, 33, 44)) |> 
  mutate(data = str_squish(data)) |> 
  mutate(data = str_remove_all(data, "-")) |> 
  mutate(data = str_squish(data)) |> 
  separate(data, into = c("uhf", "rest"), sep = "(?<=\\d\\s)") |> 
  separate(col = "rest", into = c("neighborhood", "zip"), sep = "(?=\\s\\d)", extra = "merge") |> 
  separate(col = "zip", into = c("zip1", "zip2", "zip3", "zip4", "zip5", "zip6", "zip7", "zip8", "zip9"), sep = ",") |> 
  mutate(across(everything(), ~ str_trim(.x))) |> 
  mutate(across(c("zip1":"zip9"), ~ as.numeric(.x))) |> 
  mutate(uhf = as.numeric(uhf)) |> 
  pivot_longer(c(zip1:zip9), names_to = "zip_name", values_to = "zip") |> 
  filter(!is.na(zip)) |> 
  select(!zip_name)
```

## Import UHF34 / UHF 42 crosswalk

``` r
uhf_34 = 
  pdf_text("data/geocoding/uhf34.pdf")

uhf_34_df =
  uhf_34 |> 
  str_split("\n") |>
  tibble() |> 
  rename("data" = "str_split(uhf_34, \"\\n\")") |> 
  unnest("data") |> 
  filter(data != "") |> 
  mutate(data = str_squish(data)) |> 
  filter(!row_number() %in% c(1, 2, 7, 14, 18, 21, 24, 27, 28, 31, 32, 34, 35, 39, 40, 42, 43, 44, 45, 47, 48, 52, 53, 57, 61:75)) |> 
  mutate(data = str_replace(data, "Kingsbridge - Riverdale", "101 Kingsbridge - Riverdale")) |> 
  mutate(data = str_replace(data, "Northeast Bronx", "102 Northeast Bronx")) |> 
  filter(!row_number() %in% c(1,2)) |> 
  mutate(data = str_remove_all(data, "-"),
         data = str_squish(data), 
         data = str_trim(data)) |>
  mutate(data = str_remove(data, "(\\d)+$")) |>
  mutate(data = str_remove(data, "(\\d)+\\s$")) |>
  separate(data, into = c("uhf34", "neighborhood"), sep = "(?<=\\d\\s)") |> 
  mutate(uhf34 = as.numeric(str_trim(uhf34)),
         neighborhood = str_trim(neighborhood)) |> 
  mutate(uhf2 = uhf34) |> 
  separate_wider_position(uhf2, widths = c("1_uhf" = 3, "2_uhf" = 3, "3_uhf" = 3), too_few = "align_start") |> 
  pivot_longer(c("1_uhf":"3_uhf"), names_to = "uhf_name", values_to = "uhf42") |> 
  select(!uhf_name) |> 
  mutate(uhf34 = as.numeric(uhf34),
         uhf42 = as.numeric(uhf42))
```

# Merging Datasets

## Join UHF data

``` r
joined_uhf_34_42 = 
  uhf_zip_df |> 
  left_join(y = uhf_34_df, by = join_by("uhf" == "uhf42")) |> 
  rename("uhf34_neighborhood" = "neighborhood.y", 
         "uhf42_neighborhood" = "neighborhood.x", 
         "uhf42" = "uhf")
```

## Join SDI data to UHF/Zip/Neighborhood data

``` r
joined_SDI_zip_neighborhood = 
  SDI_df |> 
  filter(zip %in% pull(joined_uhf_34_42, zip)) |> 
  mutate(zip = as.numeric(zip)) |> 
  left_join(y = joined_uhf_34_42, by = "zip")
```

## Join Overweight data to UHF/Zip/Neighborhood data

``` r
joined_overweight_zip_neighborhood = 
  overweight_df |> 
  left_join(y = joined_uhf_34_42, by = join_by("geo_id" == "uhf34")) |>
  rename(percent_overweight = percent)
```

## Join Citibike data to UHF/Zip/Neighborhood data

And finally, join the zipcodes/neighborhoods to the Citibike data!
First, we need to convert the Citibike location data from
latitude/longitude to zip code. We have done that in a separate script
and are loading that file here first.

Load latitude/longitude to zip code crosswalk to merge start_zip and
end_zip onto citibike_df. Then crosswalk to the UHF neighborhoods!

``` r
latlong_zip = read_csv('./data/geocoding/citibike_latlong_zip.csv')

citibike_zip =
  citibike_df |>
  left_join(latlong_zip, 
            by = c("start_station_latitude" = "latitude",
                   "start_station_longitude" = "longitude")) |>
  rename("start_zipcode" = "postcode") |>
  left_join(latlong_zip, 
            by = c("end_station_latitude" = "latitude",
                   "end_station_longitude" = "longitude")) |>
  rename("end_zipcode" = "postcode") 

citibike_zip_neighborhoods =
  citibike_zip |>
  left_join(y = (joined_uhf_34_42 |> select(zip, uhf42_neighborhood, uhf34_neighborhood)),
            by = join_by("start_zipcode" == "zip")) |> 
  rename("start_uhf34_neighborhood" = "uhf34_neighborhood",
         "start_uhf42_neighborhood" = "uhf42_neighborhood") |> 
  left_join(y = (joined_uhf_34_42 |> select(zip, uhf42_neighborhood, uhf34_neighborhood)),
            by = join_by("end_zipcode" == "zip")) |> 
  rename("end_uhf34_neighborhood" = "uhf34_neighborhood",
         "end_uhf42_neighborhood" = "uhf42_neighborhood")

head(citibike_zip_neighborhoods) |> 
  knitr::kable()
```

## Merge Citibike, SDI, Overweight Data

### Merge SDI and Overweight data

There are rows in the overweight data without zipcode that refer instead
to borough and city-wide overweight percentages. These are excluded from
joining on SDI/citibike data, which function by zipcode more granular
than borough

``` r
sdi_overweight = 
  merge(joined_overweight_zip_neighborhood, joined_SDI_zip_neighborhood,
                   by.x = c("zip", "uhf42", "uhf42_neighborhood"),
                   by.y = c("zip", "uhf42", "uhf42_neighborhood"),
                   all.x = TRUE)  |>
  select("zip","uhf34","uhf34_neighborhood.x","uhf42","uhf42_neighborhood",
         "sdi_score","percent_overweight") |>
  rename(uhf34_neighborhood = uhf34_neighborhood.x) |>
  filter(!is.na(sdi_overweight$zip)) 
```

### Merge SDI and Overweight data onto citibike

``` r
sample = citibike_zip_neighborhoods |>  slice(1)
citibike_zip_neighborhood_test = head(citibike_zip_neighborhoods, n = 5000)

colnames(joined_overweight_zip_neighborhood)
colnames(joined_SDI_zip_neighborhood)

citibike_df =
  citibike_zip_neighborhoods |>
  left_join(y = sdi_overweight,
            by = join_by("start_zipcode" == "zip")) |>
  rename("start_sdi_score" = "sdi_score",
         "start_percent_overweight" = "percent_overweight") 

citibike_df = citibike_df |>
  left_join(y = sdi_overweight,
            by = join_by("end_zipcode" == "zip")) |>
  rename("end_sdi_score" = "sdi_score",
         "end_percent_overweight" = "percent_overweight") 

citibike_df = citibike_df |>
  select(-uhf34.x, -uhf34_neighborhood.x, -uhf42.x, 
         -uhf42_neighborhood.x, -uhf34.x, -uhf34_neighborhood.x, 
         -uhf42.x, - uhf42_neighborhood.x)

write_csv(citibike_df, file = './citibike/citibike_clean.csv')
```

## Citibike - Missing zips

``` r
missing_startzip = citibike_df |>
  filter(is.na(start_zipcode)) 

missing_startzip |>
  group_by(start_station_name, start_station_latitude, start_station_longitude) |>
  summarize(n = n())

missing_endzip = citibike_df |>
  filter(is.na(end_zipcode)) 

missing_endzip |>
  group_by(end_station_name, end_station_latitude, end_station_longitude) |>
  summarize(n = n())

There are `r nrow(missing_startzip)` entries missing start zip and `rnrow(missing_endzip)` missing end zip. 
```
