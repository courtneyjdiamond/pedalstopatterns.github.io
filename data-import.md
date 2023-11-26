P8105 Fall 2023 Final Project
================

Citibike Jan/2019 ~ Dec/2019

``` r
citibike_df = 
  tibble(
    files = list.files("citibike/"),
    path = str_c("citibike/", files)
  ) |> 
  mutate(data = map(path, read_csv)) |> 
  unnest()
```

    ## Rows: 20205 Columns: 15
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (3): start station name, end station name, usertype
    ## dbl  (10): tripduration, start station id, start station latitude, start sta...
    ## dttm  (2): starttime, stoptime
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 19676 Columns: 15
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (3): start station name, end station name, usertype
    ## dbl  (10): tripduration, start station id, start station latitude, start sta...
    ## dttm  (2): starttime, stoptime
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 18565 Columns: 15
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (3): start station name, end station name, usertype
    ## dbl  (10): tripduration, start station id, start station latitude, start sta...
    ## dttm  (2): starttime, stoptime
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 23606 Columns: 15
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (3): start station name, end station name, usertype
    ## dbl  (10): tripduration, start station id, start station latitude, start sta...
    ## dttm  (2): starttime, stoptime
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 33056 Columns: 15
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (3): start station name, end station name, usertype
    ## dbl  (10): tripduration, start station id, start station latitude, start sta...
    ## dttm  (2): starttime, stoptime
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 36135 Columns: 15
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (3): start station name, end station name, usertype
    ## dbl  (10): tripduration, start station id, start station latitude, start sta...
    ## dttm  (2): starttime, stoptime
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 39430 Columns: 15
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (3): start station name, end station name, usertype
    ## dbl  (10): tripduration, start station id, start station latitude, start sta...
    ## dttm  (2): starttime, stoptime
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 43746 Columns: 15
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (3): start station name, end station name, usertype
    ## dbl  (10): tripduration, start station id, start station latitude, start sta...
    ## dttm  (2): starttime, stoptime
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 48711 Columns: 15
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (3): start station name, end station name, usertype
    ## dbl  (10): tripduration, start station id, start station latitude, start sta...
    ## dttm  (2): starttime, stoptime
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 49244 Columns: 15
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (3): start station name, end station name, usertype
    ## dbl  (10): tripduration, start station id, start station latitude, start sta...
    ## dttm  (2): starttime, stoptime
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 42253 Columns: 15
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (3): start station name, end station name, usertype
    ## dbl  (10): tripduration, start station id, start station latitude, start sta...
    ## dttm  (2): starttime, stoptime
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 30797 Columns: 15
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (3): start station name, end station name, usertype
    ## dbl  (10): tripduration, start station id, start station latitude, start sta...
    ## dttm  (2): starttime, stoptime
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Warning: `cols` is now required when using `unnest()`.
    ## ℹ Please use `cols = c(data)`.

Tidy dataset

``` r
citibike = citibike_df |>
  janitor::clean_names() |>
  select(-files, -path) |>
  mutate(gender = recode(gender,
                         "0" = "Unknown",
                         "1" = "Male",
                         "2" = "Female"))
```

Air Quality Data

``` r
air_df <- read_csv("air_quality/Air_Quality_20231126.csv")
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

Tidy Air Quality Data

``` r
air_quality = air_df |>
  janitor::clean_names() |>
  mutate(
    start_date = mdy(start_date),
    year = year(start_date)
  )

air_quality =
  air_quality |>
  filter(year == "2019")
```

SDI Data and tidy

``` r
SDI_df <- read_csv("SDI_data/rgcsdi-2015-2019-zcta.csv") |>
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
