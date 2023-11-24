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
