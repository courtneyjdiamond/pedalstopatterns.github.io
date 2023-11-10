P8105 Fall 2023 Final Project Proposal
================

## Proposed Title

From Pedals to Patterns: A P8105 Citibike Data Analysis Over 12 months

## Team Members

- Jesus Luevano, jl5934<br>
- Haley Zylberberg, hmz2105<br>
- Hyun Jin Jung, hj2660<br>
- Laura O’Carroll, lgo2107<br>
- Kayla Schiffer-Kane, khs2138<br>
- Courtney Diamond, cjd2195<br>

## Motivation

Increasing transportation costs, congested streets, and subway delays
across New York City are pressuring residents and tourists alike to find
cheaper and faster means of navigating the Big Apple. We, as young
professionals/students in the city, want to know if turning to CitiBike
is a viable alternative, or if the monetary, time, and weather-related
costs of biking negate any benefits gained by biking.

## Intended Final Products

Our aims are detailed below. In brief, we intend to make an interactive
website summarizing the findings based on our investigation into
Citibike data and weather/air quality in NYC.

## Anticipated Data Sources

We will use the publicly available CitiBike located at
`https://s3.amazonaws.com/tripdata/index.html`. These files contain
monthly logs of trips and record data elements such as the ride ID, the
user’s status (subscriber or casual user), the trip start location and
longitude/latitude coordinates, the trip end location and
latitude/longitude coordinates, and the trip start/stop times and
duration.

Additionally we plan to investigate weather trends in the area using
data from NYC Open Data, contributed by the Department of Hygiene and
Mental Health at
`https://data.cityofnewyork.us/Environment/Air-Quality/c3uy-2p5r`. These
data include the air quality indexes measured across boroughs in NYC.

Lastly, for temperature-related trends, we will use data from the Global
Historical Climatology Network - Daily (GHCN-Daily) dataset published by
the National Centers for Environmental Information (NCEI) located here
`https://www.ncei.noaa.gov/metadata/geoportal/rest/metadata/item/gov.noaa.ncdc:C00861/html`.
These data reflect those used in the rnoaa package, and include the
weather station identifier, the date, latitude/longitude, maximum
temperature, minimum temperature, and precipitation.

## Planned Analyses and Visualizations

We want to investigate:

**1) Who uses CitiBikes?**

- What is the ratio of subscriber : tourist (i.e. “customer”)
  utilization?
  - Does this vary by time of day?
  - Does this vary by time of year?

**2) When/how long do they use them?**

- How does this vary by subscriber/customer status?
- How does usage vary by time of day, time of year, and weather quality?

**3) Where are they leaving from and going toward?**

- What are the most common origin/destination stations?
- How do these vary by subscriber/customer status?
- How does this vary by time of day, time of year, and weather quality?

**4) What does it cost to use the CitiBikes, and how does this compare
to rideshares or subways?**

- Cost of subscription
- Cost of single-usage
- Cost of gear (for a commuter)

We want to create visualizations of:

**1) A map of CitiBike stations, indicating usage frequency**

**2) Plots breaking down crucial data: Linear/longitudinal for time
related changes. Bar plots of specific demographic breakdowns.**

## Anticipated Challenges/Limitations

Each of our datasets may be subject to missing data and thus require
determination of what to do with samples missing data (i.e. exclude or
impute). Further, it is possible that over the 12 month span, the data
format for each of these sources may have changed and thus availability
of certain variables may change (e.g. the initial CitiBike datasets
included gender and age of subscribers, but more recent months do not.)
Lastly, as is standard when using multiple datasets, we will need to be
careful when tidying such that we can integrate all datasets without
running into any formatting issues.

## Planned Timeline

We anticipate adhering to the following timeline:

**Week 1 (November 13 - 19)**

- Repository setup
- Data loading, tidying
- Exploratory data analysis
- Refinement of research questions

**Week 2 (November 20 - 26)**

- Data analysis
- Generation of visualizations

**Week 3 (November 27 - December 3)**

- Webpage and screencast setup

**Week 4 (December 4 - 9)**

- Website finalization
- Report write-up
