P8105 Fall 2023 Final Project
================

## Load and tidy the Citibike ridership data

Citibike Jan/2019 ~ Dec/2019

# Note to group: should it actually be Dec/2018 - Jan/2020? – Laura

``` r
citibike <- 
 tibble(
 files = list.files("data/citibike/"),
  path = str_c("data/citibike/", files)
  ) |>
mutate(data = map(path, ~read_csv(.x, col_types = cols(
  'end station id' = col_double(),
   'start station id' = col_double()
  )))) |>
unnest(cols = c(data))
```

    ## Warning: There were 6 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `data = map(...)`.
    ## Caused by warning:
    ## ! One or more parsing issues, call `problems()` on your data frame for details,
    ## e.g.:
    ##   dat <- vroom(...)
    ##   problems(dat)
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 5 remaining warnings.

Tidy dataset: \* recode gender \* filter trip duration to more than 5
minutes but less than 1 day \* include trips that started in 2018 but
ended in 2019 and trips that started in 2019 but ended in 2020 \* create
an age variable \* create a trip duration in minutes variable

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

## Load and tidy the AQI data

``` r
air <- read_csv("data/air_quality/Air_Quality_20231126.csv") 
```

    ## Rows: 16218 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (7): Name, Measure, Measure Info, Geo Type Name, Geo Place Name, Time Pe...
    ## dbl (4): Unique ID, Indicator ID, Geo Join ID, Data Value
    ## lgl (1): Message
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

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

    ## Rows: 32989 Columns: 18
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (1): ZCTA5_FIPS
    ## dbl (17): ZCTA5_population, SDI_score, PovertyLT100_FPL_score, Single_Parent...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

## Load the overweight data; initial import and tidying completed in \[here\] (import_overweight_data.html)

``` r
overweight <- read_csv("data/SDI_data/overweight_data_clean.csv")
```

    ## Rows: 40 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): geo_type, geography
    ## dbl (7): year, geo_id, geo_rank, number, percent, percent_low, percent_high
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

## Import UHF42/ZipCode crosswalk (available from `https://www.nyc.gov/assets/doh/downloads/pdf/ah/zipcodetable.pdf`)

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

    ## Warning: Expected 9 pieces. Missing pieces filled with `NA` in 41 rows [1, 2, 3, 4, 5,
    ## 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...].

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

## Join overweight data to UHF/Zip/Neighborhood data

``` r
joined_overweight_zip_neighborhood = 
  overweight |> 
  left_join(y = joined_uhf_34_42, by = join_by("geo_id" == "uhf34"))
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
```

    ## Rows: 1090 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (3): latitude, longitude, postcode
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
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

| trip_duration_sec | trip_duration_min | start_time          | stop_time           | start_station_id | start_station_name         | start_station_latitude | start_station_longitude | end_station_id | end_station_name         | end_station_latitude | end_station_longitude | bikeid | user_type  | birth_year | gender  | age | start_zipcode | end_zipcode | start_uhf42_neighborhood           | start_uhf34_neighborhood           | end_uhf42_neighborhood           | end_uhf34_neighborhood           |
|------------------:|------------------:|:--------------------|:--------------------|-----------------:|:---------------------------|-----------------------:|------------------------:|---------------:|:-------------------------|---------------------:|----------------------:|-------:|:-----------|-----------:|:--------|----:|--------------:|------------:|:-----------------------------------|:-----------------------------------|:---------------------------------|:---------------------------------|
|             62663 |         1044.3833 | 2018-12-31 12:42:23 | 2019-01-01 06:06:47 |             3427 | Lafayette St & Jersey St   |               40.72431 |               -73.99601 |            529 | W 42 St & 8 Ave          |             40.75757 |             -73.99099 |  19573 | Customer   |       2000 | Female  |  19 |         10012 |       10036 | Greenwich Village SoHo             | Chelsea Village                    | Chelsea Clinton                  | Chelsea Village                  |
|             86324 |         1438.7333 | 2018-12-31 13:21:55 | 2019-01-01 13:20:39 |              458 | 11 Ave & W 27 St           |               40.75140 |               -74.00523 |            127 | Barrow St & Hudson St    |             40.73172 |             -74.00674 |  16996 | Subscriber |       1953 | Female  |  66 |         10001 |       10014 | Chelsea Clinton                    | Chelsea Village                    | Greenwich Village SoHo           | Chelsea Village                  |
|             77833 |         1297.2167 | 2018-12-31 16:54:58 | 2019-01-01 14:32:11 |             3055 | Greene Ave & Nostrand Ave  |               40.68833 |               -73.95092 |            437 | Macon St & Nostrand Ave  |             40.68098 |             -73.95005 |  15420 | Customer   |       1969 | Unknown |  50 |         11216 |       11233 | Bedford Stuyvesant Crown Heights   | Bedford Stuyvesant Crown Heights   | Bedford Stuyvesant Crown Heights | Bedford Stuyvesant Crown Heights |
|             52587 |          876.4500 | 2018-12-31 17:37:05 | 2019-01-01 08:13:33 |             3457 | E 58 St & Madison Ave      |               40.76303 |               -73.97210 |           3457 | E 58 St & Madison Ave    |             40.76303 |             -73.97210 |  28349 | Subscriber |       1966 | Female  |  53 |         10022 |       10022 | Gramercy Park Murray Hill          | Upper East Side Gramercy           | Gramercy Park Murray Hill        | Upper East Side Gramercy         |
|             76290 |         1271.5000 | 2018-12-31 18:00:44 | 2019-01-01 15:12:14 |             3521 | Lenox Ave & W 111 St       |               40.79879 |               -73.95230 |           3164 | Columbus Ave & W 72 St   |             40.77706 |             -73.97898 |  14710 | Subscriber |       1995 | Female  |  24 |         10037 |       10023 | Central Harlem Morningside Heights | Central Harlem Morningside Heights | Upper West Side                  | Upper West Side                  |
|             51131 |          852.1833 | 2018-12-31 18:40:45 | 2019-01-01 08:52:56 |             3581 | Underhill Ave & Lincoln Pl |               40.67401 |               -73.96715 |           3576 | Park Pl & Vanderbilt Ave |             40.67670 |             -73.96902 |  16935 | Customer   |       1987 | Male    |  32 |         11238 |       11238 | Bedford Stuyvesant Crown Heights   | Bedford Stuyvesant Crown Heights   | Bedford Stuyvesant Crown Heights | Bedford Stuyvesant Crown Heights |
