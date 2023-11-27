Import Overweight Data
================
2023-11-27

- [Import Data](#import-data)
- [Percentages over time](#percentages-over-time)
- [Save and Notes](#save-and-notes)

# Import Data

Data Source: [NYC Data
Portal](https://a816-dohbesp.nyc.gov/IndicatorPublic/beta/data-explorer/overweight/?id=2061#display=summary)

- Data accessed: `11/27/2023`
- Full table loaded for “Overweight and Obese Adults”

``` r
data = read_csv('./data/nyc_overweight_or_obesity_adults.csv') |>
  janitor::clean_names() |>
  mutate(number = gsub("\\*", "", number)) |>
  mutate(number = gsub(",", "", number)) |>
  mutate(number = as.numeric(number)) |>
  mutate(
    percent = as.numeric(ifelse(grepl("\\*", percent),
                                gsub("\\*.*$", "", percent),
                                gsub("\\s*\\(.*", "", percent))),
    percent_low = as.numeric(gsub("^.*\\(\\s*", "", gsub("\\s*,.*$", "", percent))),
    percent_high = as.numeric(gsub("^.*\\,\\s*", "", gsub("\\)$", "", percent)))
  ) |>
  rename(year = time)

head(data)
```

    ## # A tibble: 6 × 9
    ##    year geo_type geo_id geo_rank geography            number percent percent_low
    ##   <dbl> <chr>     <dbl>    <dbl> <chr>                 <dbl>   <dbl>       <dbl>
    ## 1  2020 UHF34       101        3 Kingsbridge - River…  42000    59.2        59.2
    ## 2  2020 UHF34       102        3 Northeast Bronx       95000    66.2        66.2
    ## 3  2020 UHF34       103        3 Fordham - Bronx Pk   134000    74.9        74.9
    ## 4  2020 UHF34       104        3 Pelham - Throgs Neck 156000    72.8        72.8
    ## 5  2020 UHF34       201        3 Greenpoint            42000    41.4        41.4
    ## 6  2020 UHF34       202        3 Downtown - Heights …  81000    46.8        46.8
    ## # ℹ 1 more variable: percent_high <dbl>

``` r
skimr::skim(data)
```

|                                                  |      |
|:-------------------------------------------------|:-----|
| Name                                             | data |
| Number of rows                                   | 720  |
| Number of columns                                | 9    |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |      |
| Column type frequency:                           |      |
| character                                        | 2    |
| numeric                                          | 7    |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |      |
| Group variables                                  | None |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| geo_type      |         0 |             1 |   5 |   8 |     0 |        3 |          0 |
| geography     |         0 |             1 |   5 |  36 |     0 |       40 |          0 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |          sd |      p0 |      p25 |      p50 |       p75 |         p100 | hist  |
|:--------------|----------:|--------------:|-----------:|------------:|--------:|---------:|---------:|----------:|-------------:|:------|
| year          |         0 |             1 |    2011.50 |        5.19 |  2003.0 |  2007.00 |   2011.5 |   2016.00 |       2020.0 | ▇▆▇▆▇ |
| geo_id        |         0 |             1 | 2686089.77 | 16412347.95 |     1.0 |   176.75 |    210.5 |    407.25 | 105106107\.0 | ▇▁▁▁▁ |
| geo_rank      |         0 |             1 |       2.67 |        0.79 |     0.0 |     3.00 |      3.0 |      3.00 |          3.0 | ▁▁▁▁▇ |
| number        |         0 |             1 |  260127.78 |   565004.11 | 32000.0 | 78000.00 | 103000.0 | 142250.00 |    3844000.0 | ▇▁▁▁▁ |
| percent       |         0 |             1 |      57.68 |        9.50 |    30.5 |    51.70 |     59.3 |     64.73 |         78.7 | ▁▃▆▇▂ |
| percent_low   |         0 |             1 |      57.68 |        9.50 |    30.5 |    51.70 |     59.3 |     64.73 |         78.7 | ▁▃▆▇▂ |
| percent_high  |         0 |             1 |      57.68 |        9.50 |    30.5 |    51.70 |     59.3 |     64.73 |         78.7 | ▁▃▆▇▂ |

# Percentages over time

``` r
percent_change = data |> 
  select(geography, year, percent, number) |>
  group_by(geography) |>
  pivot_wider(names_from = year, 
              values_from = c(percent,number)) |>
  mutate(percent_diff19_20 = (percent_2020 - percent_2019)/ percent_2019 * 100) |>
  mutate(percent_diff18_19 = (percent_2019 - percent_2018) /percent_2018 * 100) |>
  select(geography, percent_diff18_19, percent_diff19_20, percent_2018, percent_2019, percent_2020)
```

There are 31 areas with \>5% change yoy between 2018, 2019, 2020. There
are 14 areas with \>10% change yoy between 2018, 2019, 2020. There are 8
areas with \>15% change yoy between 2018, 2019, 2020.

Looking at cities with \>15%

``` r
percent_change |>
  filter(abs(percent_diff18_19) > 15 | abs(percent_diff19_20) > 15) |>
  select(geography, percent_2018, percent_2019, percent_2020) |>
  print()
```

    ## # A tibble: 8 × 4
    ## # Groups:   geography [8]
    ##   geography               percent_2018 percent_2019 percent_2020
    ##   <chr>                          <dbl>        <dbl>        <dbl>
    ## 1 Kingsbridge - Riverdale         65           52.4         59.2
    ## 2 Greenpoint                      58.4         41.8         41.4
    ## 3 Sunset Park                     49.1         41.1         49.5
    ## 4 East Harlem                     67.3         70.1         54.7
    ## 5 Flushing - Clearview            52.7         43           49  
    ## 6 Southwest Queens                65.2         59.3         70.5
    ## 7 Jamaica                         70.1         59.5         72.3
    ## 8 Chelsea-Village                 39.3         38.1         30.5

# Save and Notes

``` r
data_2019 = data |>
  filter(year == 2019)

data_2019 |>
  group_by(geo_type) |>
  summarize(n_obs = n())
```

    ## # A tibble: 3 × 2
    ##   geo_type n_obs
    ##   <chr>    <int>
    ## 1 Borough      5
    ## 2 Citywide     1
    ## 3 UHF34       34

``` r
write_csv(data_2019, "./data/overweight_data_clean.csv")
```

- Total of 34 specific geo_ids, 5 boroughs, and 1 citywide rate.  
- Years range 2003 to 2020
- Depending on what year we use for other data, can pull that year
  directly, or average over recent years. Rates for the most part are
  pretty consistent over time
