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
    - [Manually add missing zipcodes](#manually-add-missing-zipcodes)
  - [Merge Citibike, SDI, Overweight
    Data](#merge-citibike-sdi-overweight-data)
    - [Merge SDI and Overweight data](#merge-sdi-and-overweight-data)
    - [Merge SDI and Overweight data onto
      citibike](#merge-sdi-and-overweight-data-onto-citibike)
    - [Merge AQ data onto citibike](#merge-aq-data-onto-citibike)
- [Note: still need to filter out rows with no UHF34 neighborhood from
  citibike sheet.
  –Laura](#note-still-need-to-filter-out-rows-with-no-uhf34-neighborhood-from-citibike-sheet-laura)

# Load and tidy the Citibike ridership data

Citibike Jan/2019 ~ Dec/2019

**Note to group: should it actually be Dec/2018 - Jan/2020? – Laura
(Response from HZ, see below)**

``` r
citibike = 
  tibble(
    files = list.files("../citibike"),
    path = str_c("../citibike/", files)
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

remove(citibike)

head(citibike_df) |>
  knitr::kable()
```

| trip_duration_sec | trip_duration_min | start_time          | stop_time           | start_station_id | start_station_name         | start_station_latitude | start_station_longitude | end_station_id | end_station_name         | end_station_latitude | end_station_longitude | bikeid | user_type  | birth_year | gender  | age |
|------------------:|------------------:|:--------------------|:--------------------|-----------------:|:---------------------------|-----------------------:|------------------------:|---------------:|:-------------------------|---------------------:|----------------------:|-------:|:-----------|-----------:|:--------|----:|
|             62663 |         1044.3833 | 2018-12-31 12:42:23 | 2019-01-01 06:06:47 |             3427 | Lafayette St & Jersey St   |               40.72431 |               -73.99601 |            529 | W 42 St & 8 Ave          |             40.75757 |             -73.99099 |  19573 | Customer   |       2000 | Female  |  19 |
|             86324 |         1438.7333 | 2018-12-31 13:21:55 | 2019-01-01 13:20:39 |              458 | 11 Ave & W 27 St           |               40.75140 |               -74.00523 |            127 | Barrow St & Hudson St    |             40.73172 |             -74.00674 |  16996 | Subscriber |       1953 | Female  |  66 |
|             77833 |         1297.2167 | 2018-12-31 16:54:58 | 2019-01-01 14:32:11 |             3055 | Greene Ave & Nostrand Ave  |               40.68833 |               -73.95092 |            437 | Macon St & Nostrand Ave  |             40.68098 |             -73.95005 |  15420 | Customer   |       1969 | Unknown |  50 |
|             52587 |          876.4500 | 2018-12-31 17:37:05 | 2019-01-01 08:13:33 |             3457 | E 58 St & Madison Ave      |               40.76303 |               -73.97210 |           3457 | E 58 St & Madison Ave    |             40.76303 |             -73.97210 |  28349 | Subscriber |       1966 | Female  |  53 |
|             76290 |         1271.5000 | 2018-12-31 18:00:44 | 2019-01-01 15:12:14 |             3521 | Lenox Ave & W 111 St       |               40.79879 |               -73.95230 |           3164 | Columbus Ave & W 72 St   |             40.77706 |             -73.97898 |  14710 | Subscriber |       1995 | Female  |  24 |
|             51131 |          852.1833 | 2018-12-31 18:40:45 | 2019-01-01 08:52:56 |             3581 | Underhill Ave & Lincoln Pl |               40.67401 |               -73.96715 |           3576 | Park Pl & Vanderbilt Ave |             40.67670 |             -73.96902 |  16935 | Customer   |       1987 | Male    |  32 |

# Other Data Sets

## Load and tidy the AQI data

``` r
air_quality_df = read_csv("../data/air_quality/Air_Quality_20231126.csv") |>
  janitor::clean_names() |>
  mutate(
    start_date = mdy(start_date),
    year = year(start_date)
  ) |>
  filter(year == "2019")
```

## Load and tidy the SDI data

``` r
SDI_df <- read_csv("../data/SDI_data/rgcsdi-2015-2019-zcta.csv") |>
  janitor::clean_names() |>
  select(zcta5_fips, sdi_score)|>
  rename(zip=zcta5_fips)

head(SDI_df) |>
  knitr::kable()
```

| zip   | sdi_score |
|:------|----------:|
| 01001 |        36 |
| 01002 |        72 |
| 01003 |        76 |
| 01005 |        14 |
| 01007 |        14 |
| 01008 |         8 |

## Load the Overweight data

Data Source: [NYC Data
Portal](https://a816-dohbesp.nyc.gov/IndicatorPublic/beta/data-explorer/overweight/?id=2061#display=summary)

- Data accessed: `11/27/2023`
- Full table loaded for “Overweight and Obese Adults”

``` r
#overweight = read_csv('./data/SDI_data/nyc_overweight_or_obesity_adults.csv') |>
#    janitor::clean_names()
overweight_df =
  read_csv('../data/SDI_data/nyc_overweight_or_obesity_adults.csv') |>
  janitor::clean_names() |>
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

head(overweight_df) |>
  knitr::kable()
```

| year | geo_type | geo_id | geo_rank | geography                  | number | percent | percent_low | percent_high |
|-----:|:---------|-------:|---------:|:---------------------------|-------:|--------:|------------:|-------------:|
| 2019 | UHF34    |    101 |        3 | Kingsbridge - Riverdale    |  37000 |    52.4 |        40.8 |         63.8 |
| 2019 | UHF34    |    102 |        3 | Northeast Bronx            | 103000 |    69.6 |        59.7 |         78.0 |
| 2019 | UHF34    |    103 |        3 | Fordham - Bronx Pk         | 122000 |    71.1 |        61.1 |         79.3 |
| 2019 | UHF34    |    104 |        3 | Pelham - Throgs Neck       | 152000 |    66.4 |        58.7 |         73.4 |
| 2019 | UHF34    |    201 |        3 | Greenpoint                 |  40000 |    41.8 |        31.0 |         53.6 |
| 2019 | UHF34    |    202 |        3 | Downtown - Heights - Slope |  86000 |    50.8 |        41.8 |         59.7 |

``` r
#write_csv(overweight_df, "data/SDI_data/overweight_data_clean.csv")
```

# Incorporate Geocoding

## Import UHF42/ZipCode crosswalk

Data source:
`https://www.nyc.gov/assets/doh/downloads/pdf/ah/zipcodetable.pdf`

``` r
uhf_zip = 
  pdf_text("../data/geocoding/zipcodetable.pdf") |> 
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
  pdf_text("../data/geocoding/uhf34.pdf")

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
latlong_zip = read_csv('../data/geocoding/citibike_latlong_zip.csv')

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

remove(citibike_df)
```

### Manually add missing zipcodes

``` r
citibike_zip |>
  filter(is.na(start_zipcode)) |>
  group_by(start_station_name, start_station_latitude, start_station_longitude) |>
  summarize(n = n())
```

    ## # A tibble: 12 × 4
    ## # Groups:   start_station_name, start_station_latitude [12]
    ##    start_station_name       start_station_latitude start_station_longitude     n
    ##    <chr>                                     <dbl>                   <dbl> <int>
    ##  1 Broadway & W 32 St                         40.7                   -74.0 14007
    ##  2 Broadway & W 36 St                         40.8                   -74.0 26108
    ##  3 Broadway & W 37 St                         40.8                   -74.0 32273
    ##  4 Broadway & W 38 St                         40.8                   -74.0 39144
    ##  5 Broadway & W 41 St                         40.8                   -74.0 69461
    ##  6 Cooper Square & Astor Pl                   40.7                   -74.0 62815
    ##  7 E 47 St & 1 Ave                            40.8                   -74.0 17988
    ##  8 Roebling St & N 4 St                       40.7                   -74.0 13617
    ##  9 South St & Gouverneur Ln                   40.7                   -74.0 43769
    ## 10 W 43 St & 6 Ave                            40.8                   -74.0 17682
    ## 11 W 52 St & 6 Ave                            40.8                   -74.0 48828
    ## 12 William St & Pine St                       40.7                   -74.0 20940

``` r
citibike_zip |>
  filter(is.na(end_zipcode)) |>
  group_by(end_station_name, end_station_latitude, end_station_longitude) |>
  summarize(n = n())
```

    ## # A tibble: 12 × 4
    ## # Groups:   end_station_name, end_station_latitude [12]
    ##    end_station_name         end_station_latitude end_station_longitude     n
    ##    <chr>                                   <dbl>                 <dbl> <int>
    ##  1 Broadway & W 32 St                       40.7                 -74.0 13737
    ##  2 Broadway & W 36 St                       40.8                 -74.0 24982
    ##  3 Broadway & W 37 St                       40.8                 -74.0 31809
    ##  4 Broadway & W 38 St                       40.8                 -74.0 39448
    ##  5 Broadway & W 41 St                       40.8                 -74.0 72968
    ##  6 Cooper Square & Astor Pl                 40.7                 -74.0 59873
    ##  7 E 47 St & 1 Ave                          40.8                 -74.0 18373
    ##  8 Roebling St & N 4 St                     40.7                 -74.0 13924
    ##  9 South St & Gouverneur Ln                 40.7                 -74.0 43034
    ## 10 W 43 St & 6 Ave                          40.8                 -74.0 18730
    ## 11 W 52 St & 6 Ave                          40.8                 -74.0 54714
    ## 12 William St & Pine St                     40.7                 -74.0 20933

There are r citibike_zip \|\> filter(is.na(start_zipcode) \|
is.na(end_code)) \|\> nrow() entries missing either the start or end
zipcode.

``` r
citibike_zip = citibike_zip |>
  mutate(start_zipcode = case_when(start_station_name == "Broadway & W 32 St" ~ 10001,
                               start_station_name == "Cooper Square & Astor Pl" ~ 10003,
                               start_station_name == "William St & Pine St" ~ 10005,
                               start_station_name %in% c("Broadway & W 36 St","Broadway & W 37 St","Broadway & W 38 St","Broadway & W 41 St") ~ 10018,
                               start_station_name == "W 52 St & 6 Ave" ~ 100019,
                               start_station_name %in% c("Broadway & W 41 St","W 43 St & 6 Ave") ~ 10036,
                               start_station_name == "South St & Gouverneur Ln" ~ 10043,
                               start_station_name == "E 47 St & 1 Ave" ~ 10075,
                               start_station_name == "Roebling St & N 4 St" ~ 11211,
                               TRUE ~ start_zipcode)) |>
  mutate(end_zipcode = case_when(end_station_name == "Broadway & W 32 St" ~ 10001,
                               end_station_name == "Cooper Square & Astor Pl" ~ 10003,
                               end_station_name == "William St & Pine St" ~ 10005,
                               end_station_name %in% c("Broadway & W 36 St","Broadway & W 37 St","Broadway & W 38 St","Broadway & W 41 St") ~ 10018,
                               end_station_name == "W 52 St & 6 Ave" ~ 100019,
                               end_station_name %in% c("Broadway & W 41 St","W 43 St & 6 Ave") ~ 10036,
                               end_station_name == "South St & Gouverneur Ln" ~ 10043,
                               end_station_name == "E 47 St & 1 Ave" ~ 10075,
                               end_station_name == "Roebling St & N 4 St" ~ 11211,
                               TRUE ~ end_zipcode))

# Should be empty now
citibike_zip |>
  filter(is.na(end_zipcode) | is.na(start_zipcode)) 
```

    ## # A tibble: 0 × 19
    ## # ℹ 19 variables: trip_duration_sec <dbl>, trip_duration_min <dbl>,
    ## #   start_time <dttm>, stop_time <dttm>, start_station_id <dbl>,
    ## #   start_station_name <chr>, start_station_latitude <dbl>,
    ## #   start_station_longitude <dbl>, end_station_id <dbl>,
    ## #   end_station_name <chr>, end_station_latitude <dbl>,
    ## #   end_station_longitude <dbl>, bikeid <dbl>, user_type <chr>,
    ## #   birth_year <dbl>, gender <chr>, age <dbl>, start_zipcode <dbl>, …

``` r
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

remove(citibike_zip)

#head(citibike_zip_neighborhoods) |> 
#  knitr::kable()
```

``` r
missing_startuhf = citibike_zip_neighborhoods |>
  filter(is.na(start_uhf34_neighborhood)) |>
  group_by(start_zipcode) |>
  summarize(n = n()) |>
  rename(zipcode = start_zipcode)
  
missing_enduhf = citibike_zip_neighborhoods |>
  filter(is.na(end_uhf34_neighborhood)) |>
  group_by(end_zipcode) |>
  summarize(n = n()) |>
  rename(zipcode = end_zipcode)

#write_csv(rbind(missing_startuhf, missing_enduhf) ,'../data/geocoding/missing_uhf34.csv')
```

There are r missing_startuhf \|\> pull(n) \|\> sum() entries whose start
zipcode do not have an associated uhf34, and r missing_enduhf \|\>
pull(n) \|\> sum() entries whose end zipcode do not have an associated
uhf34.

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
  filter(!is.na(zip)) 

remove(joined_overweight_zip_neighborhood)
remove(joined_SDI_zip_neighborhood)
remove(SDI_df)
remove(overweight_df)
```

### Merge SDI and Overweight data onto citibike

``` r
citibike_df =
  citibike_zip_neighborhoods |>
  left_join(y = sdi_overweight,
            by = join_by("start_zipcode" == "zip")) |>
  rename("start_sdi_score" = "sdi_score",
         "start_percent_overweight" = "percent_overweight") 

remove(citibike_zip_neighborhoods)

citibike_df = citibike_df |>
  left_join(y = sdi_overweight,
            by = join_by("end_zipcode" == "zip")) |>
  rename("end_sdi_score" = "sdi_score",
         "end_percent_overweight" = "percent_overweight") 

citibike_df = citibike_df |>
  select(bikeid, user_type, gender, age,
         start_time, stop_time, 
         start_station_id, start_station_name,
         start_zipcode, start_uhf34_neighborhood,
         end_station_id, end_station_name,
         end_zipcode, end_uhf34_neighborhood,
         start_sdi_score, start_percent_overweight,
         end_sdi_score, end_percent_overweight)

head(citibike_df) |>
  knitr::kable()
```

| bikeid | user_type  | gender  | age | start_time          | stop_time           | start_station_id | start_station_name         | start_zipcode | start_uhf34_neighborhood           | end_station_id | end_station_name         | end_zipcode | end_uhf34_neighborhood           | start_sdi_score | start_percent_overweight | end_sdi_score | end_percent_overweight |
|-------:|:-----------|:--------|----:|:--------------------|:--------------------|-----------------:|:---------------------------|--------------:|:-----------------------------------|---------------:|:-------------------------|------------:|:---------------------------------|----------------:|-------------------------:|--------------:|-----------------------:|
|  19573 | Customer   | Female  |  19 | 2018-12-31 12:42:23 | 2019-01-01 06:06:47 |             3427 | Lafayette St & Jersey St   |         10012 | Chelsea Village                    |            529 | W 42 St & 8 Ave          |       10036 | Chelsea Village                  |              60 |                     38.1 |            69 |                   38.1 |
|  16996 | Subscriber | Female  |  66 | 2018-12-31 13:21:55 | 2019-01-01 13:20:39 |              458 | 11 Ave & W 27 St           |         10001 | Chelsea Village                    |            127 | Barrow St & Hudson St    |       10014 | Chelsea Village                  |              70 |                     38.1 |            37 |                   38.1 |
|  15420 | Customer   | Unknown |  50 | 2018-12-31 16:54:58 | 2019-01-01 14:32:11 |             3055 | Greene Ave & Nostrand Ave  |         11216 | Bedford Stuyvesant Crown Heights   |            437 | Macon St & Nostrand Ave  |       11233 | Bedford Stuyvesant Crown Heights |              83 |                     62.9 |            97 |                   62.9 |
|  28349 | Subscriber | Female  |  53 | 2018-12-31 17:37:05 | 2019-01-01 08:13:33 |             3457 | E 58 St & Madison Ave      |         10022 | Upper East Side Gramercy           |           3457 | E 58 St & Madison Ave    |       10022 | Upper East Side Gramercy         |              26 |                     36.5 |            26 |                   36.5 |
|  14710 | Subscriber | Female  |  24 | 2018-12-31 18:00:44 | 2019-01-01 15:12:14 |             3521 | Lenox Ave & W 111 St       |         10037 | Central Harlem Morningside Heights |           3164 | Columbus Ave & W 72 St   |       10023 | Upper West Side                  |              97 |                     68.7 |            43 |                   43.4 |
|  16935 | Customer   | Male    |  32 | 2018-12-31 18:40:45 | 2019-01-01 08:52:56 |             3581 | Underhill Ave & Lincoln Pl |         11238 | Bedford Stuyvesant Crown Heights   |           3576 | Park Pl & Vanderbilt Ave |       11238 | Bedford Stuyvesant Crown Heights |              70 |                     62.9 |            70 |                   62.9 |

### Merge AQ data onto citibike

``` r
#Filter to only annual averages of fine particles (PM 2.5) mean measurement, mcg/m3
air_quality_df =   
  air_quality_df |>
  filter(geo_type_name == "UHF34") |>
  filter(time_period == "Annual Average 2019") |>
  filter(name == "Fine particles (PM 2.5)")

#Align UHF neighborhood names
air_quality_df = 
  left_join(air_quality_df, uhf_34_df, by = c("geo_join_id" = "uhf34"))

air_quality_df =
  air_quality_df |>
select(data_value, neighborhood ) |>
 distinct()

#Join to final dataframe
citibike_df =
  citibike_df |>
  left_join(y = air_quality_df,
            by = join_by("start_uhf34_neighborhood" == "neighborhood")) |>
  rename("start_uhf34_AQ" = "data_value")

citibike_df = citibike_df |>
  left_join(y = air_quality_df,
            by = join_by("end_uhf34_neighborhood" == "neighborhood")) |>
rename("end_uhf34_AQ" = "data_value")


#write_csv(citibike_df, file = '../citibike/citibike_clean.csv')
```

# Note: still need to filter out rows with no UHF34 neighborhood from citibike sheet. –Laura
