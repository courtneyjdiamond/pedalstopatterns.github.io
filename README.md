# p8105_final_project

This repository contains all code, data, analyses, visualizations, and other products for our team's final project. 

We explore interaction of Citibike and health datasets. Because the Citibike dataset is very large, it must be kept locally. After cloning the repository, please follow the steps below to setup the local Citibike file paths

## Citibike Data Load 

1. In `p8105_final_project`, create two folder:
  * `citibike` which will hold the raw Citibike `.csv` files 
  * `citibike_clean` , we will write the tidied Citibike dataframes here!
  
```bash
.
├── p8105_final_project
│   ├── citibike
│   └── citibike_clean
├── ...
```

2. Download the [Citibike data here](https://s3.amazonaws.com/tripdata/index.html). Each file has the format YYYYMM-citibike-tripdata.csv.zip. Download the files from 201812 - 202001. 
* Unzip
* Move all the `csv` files into your `citibike` folder
* Your directory should now look as follows: 


```bash
.
├── p8105_final_project
│   ├── citibike
│   │   ├── 201812-citibike-tripdata.csv
│   │   ├── 201901-citibike-tripdata.csv
│   │   ├── 201902-citibike-tripdata.csv
│   │   ├── 201903-citibike-tripdata.csv
│   │   ├── 201904-citibike-tripdata.csv
│   │   ├── 201905-citibike-tripdata.csv
│   │   ├── 201906-citibike-tripdata.csv
│   │   ├── 201907-citibike-tripdata.csv
│   │   ├── 201908-citibike-tripdata.csv
│   │   ├── 201909-citibike-tripdata.csv
│   │   ├── 201910-citibike-tripdata.csv
│   │   ├── 201911-citibike-tripdata.csv
│   │   ├── 201912-citibike-tripdata.csv
│   │   ├── 202001-citibike-tripdata.csv
│   └── citibike_clean
│   │   ├── 
├── ...
```

That's it! Our main data sources import and cleaning script, `data_import_clean.Rmd` should know where to find your files, where to write the cleaned dataframes, and it will knit. After you knit that file, your `citibike_clean` folder will be populated with tidied dataframes on citibike ridership and stations available as follows:

```bash
.
├── p8105_final_project
│   └── citibike_clean
│   │   ├── citibike_clean.csv
│   │   ├── stations.csv
├── ...
```

These files will be useful for knitting the exploratory and analysis pages. 
