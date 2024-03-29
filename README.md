# st_pm_hex

## About

This repository contains the code used to create a daily, 750 sq m, spatiotemporal PM2.5 exposure assessment model for the contiguous United States from 2000 to 2020.
	
![st_pm_hex](st_pm_hex.png)

If you are looking to use predictions from this model, please check out:

- manuscript describing model development: `Brokamp, C. A High Resolution Spatiotemporal Fine Particulate Matter Exposure Assessment Model for the Contiguous United States. Environmental Advances. In Press. 2021.` (accepted preprint available online: [https://doi.org/10.1016/j.envadv.2021.100155](https://doi.org/10.1016/j.envadv.2021.100155))
- the R package for attaching PM2.5 estimates given lat/lon and date range: https://geomarker.io/addPmData
- the [DeGAUSS](https://degauss.org) container attaching PM2.5 estimates given lat/lon and date range: https://degauss.org/pm
- an interactive [online map](http://app.geomarker.io/pm_cv_accuracy/) showing the crossvalidated accuracy metrics for specific subsets of the United States *and* the locations of all EPA monitoring stations used in the training data.


## Computing

### using `renv` for package management

- `renv` should be installed and activated automatically if starting R from the project root working directory
- run `renv::snapshot()` to add packages contained within the code to the library (make sure to commit the `renv` project infrastructure, like the `renv.lock` file to the github repository)
- after pulling this project from GitHub, run `renv::restore()` to synchronize the library with the lockfile
- see more info here: https://rstudio.github.io/renv/

### `h3` package

The `h3` package might not install properly using "normal" installation code for packages on GitHub because it requires installation of the h3 library ahead of time.  See full instructions here: https://github.com/crazycapivara/h3-r

- install h3 library on macOS with:
```
brew install h3
```

- install h3 library on debian/ubuntu with:
```
git clone https://github.com/uber/h3.git h3c
cd h3c
git pull origin master --tags
git checkout "v3.3.0"
cmake -DENABLE_FORMAT=OFF -DBUILD_SHARED_LIBS=ON .
sudo make install
cd ..
rm -rf h3c
```

### storing and retrieving data on AWS S3

- the aws cli must be installed and you must have proper permissions with `AWS_SECRET_ACCESS_KEY` and `AWS_ACCESS_KEY_ID` environment variables set
- storing data
    - sync the local working directory with a S3 directory with `aws s3 sync . s3://path/to/remote/folder`
    - for example sync clean AOD rasters with `aws s3 sync ./aod_clean_rasters s3://geomarker/aod`
- retrieving data
    - `aws s3 sync s3://geomarker/aod ./aod_clean_rasters`
	
## Code Descriptions

Each of the sections below corresponds to the `R` script with the same name and provides some notes, details, and a high-level description of what that file does.  They are designed to be used in sequential order.

### 1. make h3 grid

- use the [H3 hexagonal hierarchical spatial index](https://eng.uber.com/h3/) to create the grid at a precision of 8
    - average area = 0.74 sq km
    - average side length = 461.4 m
    - n unique in continental US = 11,932,970
    - for comparison, the actual resolution of the modis grid is 926 by 926 m = 0.86 sq km
    - see the [example cincinnati map](h3_cincy_example_map.html) for an example of the hierarchical h3 layout
- geohash
    - each hash is made up of 15 total characters, with the first character always being an `8`, the second character denoting the resolution and the remaining 13 being a combination of the geohash itself and "filler" f's
    - values can be `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `a`, `b`, `c`, `d`, `e`, `f`
    - for example, a geohash at a resolution of 6 would be `86______fffffff` where the `______` is the six digit geohash
    - overall, resolution ranges from 0 - 15
- see [h3 docs](https://h3geo.org/#/documentation/overview/use-cases) for more information on use cases and "why hexagons?"
- also good comparisons to other systems with slideable maps
- good [blog post](https://observablehq.com/@fil/h3-oddities) on some h3 oddities
- table describing H3 properties at different resolutions

   | res | n digits | code | avg area (sq km) | n unique in cont US |
   |-----|----------|------|------------------|---------------------|
   | 0   | 2        | `80` | 4,250,546        |                     |
   | 1   | 3        | `81` | 607,220          |                     |
   | 2   | 4        | `82` | 86,745           | 81                  |
   | 3   | 4        | `83` | 12,392           |                     |
   | 4   | 5        | `84` | 1,770            | 4,970               |
   | 5   | 6        | `85` | 252              |                     |
   | 6   | 6        | `86` | 36               | 139,160             |
   | 7   | 7        | `87` | 5                |                     |
   | 8   | 8        | `88` | 0.74             | 11,932,970          |
   | 9   | 9        | `89` | 0.11             |                     |
   | 10  | 10       | `8a` | 0.02             |                     |
    
- using the compact US h3 hex ids (saved locally as `us_h3_8_compact_hex_ids.rds`), takes up much less space and can be quickly down scaled to the desired resolution for different geospatial calculations; e.g.,

```r
d_hex <- readRDS('us_h3_8_compact_hex_ids.rds') %>%
  map(h3_to_children, res = 8) %>%
  unlist()
```

- these can then be translated into a simple features object with

```r
## points:
d <-
  bind_cols(tibble(h3 = d_hex),
            as_tibble(h3::h3_to_geo(d_hex))) %>%
  st_as_sf(coords = c('lng', 'lat'), crs = 4326)

## polygons:
d <-
  bind_cols(tibble(h3 = d_hex),
            as_tibble(h3::h3_to_geo_boundary_sf(d_hex)))
```

**be warned that `h3::geo_to_h3()` will give an incorrect geohash without warning or error if the `sf` object is not lat/lon (i.e. any epsg other than 4326)**

**don't use a resolution of 2 or 1 when going to children; this is unstable and will result in missing polygons**. an alternative is to go to children at resolution 8, unlist these results and then find the unique parents at a resolution of 2 or 1.

### 2. get AQS data

- get observed 24 hour average PM2.5 AQS data from 2000 through 2020
- average by date for co-located stations (2,598,268 total rows, but 2,416,892 total unique station/lat/lon/date combinations)
- subset to only the contiguous united states using intersection in EPSG 5072 (n = 2,374,589)
- data saved as `s3://geomarker/st_pm_hex/h3data_aqs.qs`
- 1,685 unique h3 grid cells with a median of 1,042 daily measurements each (min: 3, q25: 370, mean: 1409, q75: 2024, max: 7348)
- a summary of the observations used to train the model:

|       n| min_pm25| p25_pm25| mean_pm25| median_pm25| p75_pm25| max_pm25| var_pm25|
|-------:|--------:|--------:|---------:|-----------:|--------:|--------:|--------:|
| 2380891|     -0.1|      5.8|     10.76|           9|     13.6|    640.6|    55.47|

### 3. make nearby pm data

- most AQS data for 1 in 3 sampling fall on the same day
- summarizing number of measurements per day:
  - min: 0
  - 25p: 104
  - median: 150
  - mean: 309.6
  - 75p: 567.5
  - max: 1012
- aggregate data to h3 resolution 3 (see `h3_3_and_aqs_map.html` for measurement location with dot size by number of total days with measurements *and* all h5 resolution 3 polygons)
- steps to create nearby pm
  - average pm h3 res 8 measurements into pm h3 res 3 measurements
  - nearby daily pm calculated as mean of neighborhing cells (up to 5-ring away) weighted by the inverse distance (measured as number of cells) squared (assume center cell has weight of 100)
  - create moving three day average
- create "nearby pm2.5" column (`nearby_pm`) (this data saved as `s3://geomarker/st_pm_hex/nearby_pm.rds`)

![](./h3_3_and_aqs_map_screenshot.png)
  
### 4. get NLCD data

- National Land Cover Database information is taken from the [geomarker-io/addNlcdData](https://github.com/geomarker-io/addNlcdData) R package that uses `.fst` files to speed up extraction and summary for polygons
- training data only saved as `s3://geomarker/st_pm_hex/h3data_nlcd.qs`

### 5. get NARR data

- [NARR](https://www.esrl.noaa.gov/psd/data/gridded/data.narr.html) data details
- the NARR cell for each h3 polygon cell is determined using the NARR raster
- the [geomarker-io/addNarrData](https://github.com/geomarker-io/addNarrData) packagte is used to add daily NARR estimates for each NARR cell
- training data only saved as `s3://geomarker/st_pm_hex/h3data_narr.qs`

### 6. get MODIS data

- [MCD19A2: MODIS/Terra and Aqua MAIAC Land AOD Daily L2G 1km SIN Grid V006](https://lpdaac.usgs.gov/products/mcd19a2v006/)
- [MCD19A2 User Guide](https://lpdaac.usgs.gov/documents/110/MCD19_User_Guide_V6.pdf)
- cleaned AOD rasters are stored at [s3://geomarker/aod/](https://geomarker.s3.us-east-2.amazonaws.com/aod)

#### Running To Get All Clean Rasters

- repetitively run the script until all rasters are on disk
- getting a cleaned AOD raster often fails because of download failure
- sometimes, none of the desired tiles are available for download at all or all of the downloaded tiles don't have any quality, non-missing AOD measurements
- in these cases, the script will make a "dummy" file so that the date will be marked as "completed"

For example, create a wrapper bash script to run the script every minute if it isn't already running:

- create `run_it.sh`:

```sh
#!/bin/bash

cd /home/cole/st_pm_hex
rm qa_MCD19A2.A*
rm MCD19A2.A*
rm aod_MCD19A2.A*
/usr/bin/Rscript ./04_get_modis_data.R
```

- then setup a cron job (`crontab -e`) to run every minute if it isn't already running:

```sh
*/1 * * * * /usr/bin/flock -n /tmp/fcj.lockfile /home/cole/st_pm_hex/run_it.sh
```

- check how many dates are completed with `ll aod_clean_rasters/ | wc -l`
- sync to S3 drive with `aws s3 sync ./aod_clean_rasters s3://geomarker/aod`

### 7. geohash AOD data

- use `06_make_AOD_data.R` to extract all non-missing AOD data from folder of rasters as a data.table fst file keyed on h3 and date
- `h3data_aod.fst` (saved as `s3://geomarker/st_pm_hex/aod.fst`) takes up 6.9 GB in RAM and 1.1 GB on disk

### 8. get NEI data

- [NEI](https://www.epa.gov/air-emissions-inventories/national-emissions-inventory-nei) is the National Emissions Inventory Database
  - data available in 2008, 2011, 2014, 2017
- [scc](https://ofmpub.epa.gov/sccwebservices/sccsearch/) is a code which defines the type of emissions
- EIS is grouping of SCC codes into: point, nonpoint, onroad, nonroad, and event sources
- script will output `nei_county_pm25.rds`  and `nei_point_pm25.rds` files (saved as `s3://geomarker/nei/nei_county_pm25.rds` and `s3://geomarker/nei/nei_point_pm25.rds`)
- county file contains the estimated tons of pm2.5 emitted, as columns called `nonroad`, `onroad`, `nonpoint`, and `event`, for each `fips` and each `nei_year` (2008, 2011, 2014, or 2017)
    - event data is not available for 2008
    - at the time, nonpoint, and onroad data was not yet available for 2017
    - all other counties not listed for any given nei year were set to zero
- point file contains sf object with `h3` geohash, and `total_emissions` for each `nei_year` (all `eis` codes are equal to "point" for this file)

### 9. get FINN data

- [FINN](https://www2.acom.ucar.edu/modeling/finn-fire-inventory-ncar) is the Fire Emissions from NCAR database
  - https://doi.org/10.5194/gmd-4-625-2011
  - FINN v1.5 files avail daily at 1km resolution from 2002 to 2018
- script will output combined data in `s3://geomarker/st_pm_hex/h3data_finn.fst`, with columns for `date`, `area` (burned area), `h3`, and `fire_pm25` (estimated total pm25 emitted from fire)
- file is 98 MB on disk

### 10. get population density

- uses 2018 5-yr ACS census-tract level estimates of total population (`B01001_001`) to estimate total population for each *resolution 5* h3 cell using an area-weighted approach; population density is calculated as number of total population divided by area of each resolution 5 h3 cell in epsg:5072

### 11. make training data

- merge in all columns based on pm2.5 observations
- retain `h3` for grid cell identifier
- add in county fips for each geohash for merging NEI data
- 1 station was removed because the centroid of its containing h3 grid cell was not located in Mexico (n = 280, 0.01% of the total PM2.5 observations)
- include x and y coordinates (in epsg 5072) for geohashes
- create year, day of year, and day of week columns
- add indicator variable for major US holidays (new years, 4th july, TG, xmas, MLK, labor, memorial)
- merge in NARR data based on h3 and date
- merge in annual data to closest available calendar year (NEI and NLCD)
- add distance to closest NEI site for each grid centroid (merge year by nei_year)
- add in population density based on res-5 h3
- add distance to closest 2018 S1100 road for each grid centroid
- merge in aod data (set all AOD > 2 to `NA`, n = 0)
- merge in fire data
- total of 2,374,309 observations and 43 predictors
- file saved as `s3://geomarker/st_pm_hex/h3data_train.fst` (824 MB in RAM, 137 MB on disk)
- 7,184 (0.3%) of grid-days with pm25 had non-missing aod data
- observed PM2.5 used for training:
	- min: -0.10
	- q25: 5.80
	- median: 9.00
	- mean: 10.76
	- q75: 13.60
	- max: 640.60

#### missing data

- `nei_nonroad`, `nei_onroad`, `nei_nonpoint`, and `nei_event` values could be missing for earlier years
- there are a handful of missing `nlcd` and `narr` values, which are present in the sources
- `aod` is missing in most places
- will not be imputed because grf handles them by using them in splits (just like we did for cincy aod model, but automatically)
- https://grf-labs.github.io/grf/REFERENCE.html#missing-values
- this will also allow for missing variables for new predictions

### 12. train pred model

- used variable importance on initial random forest based on 2017 data only to filter out unimportant variables (see variable_importance.md)
- use GRF with cluster set to h3 identifier for valid prediction CIs
- there are 1,684 unique h3 and 17,813 total unique h3-years
- train grf objects by year and save as `s3://geomarker/st_pm_hex/st_pm_hex_grf_{year}.qs`
- create OOB predictions using predict without new obs and predict with new obs of same training set
- save preds as `s3://geomarker/st_pm_hex/st_pm_hex_grf_preds.rds`

### 13. cv error report

- creates figures and tables for quantifying CV error in `./cv_output/` folder

- Brokamp, 2021: LLO CV R2 = 0.84 (0.95 annual)
- Brokamp, 2018: LOOCV R2 = 0.91
- QD, 2019: 10-fold CV R2 = 0.86 (0.89 annual)
- Hu, 2017: 10-fold CV R2 = 0.84
- QD, 2016: 10-fold CV R2 = 0.80

See separately, an interactive [online map](http://app.geomarker.io/pm_cv_accuracy/) showing the crossvalidated accuracy metrics for specific subsets of the United States *and* the locations of all EPA monitoring stations used in the training data.

### 14. create Safe Harbor aggregated h3 cells

- same methods as for making Schwartz cells Safe Harbor Deidentified
- used h3 resolution of 3
  - average hexagon area of 12,392 sq km
  - average hexagon edge length of 59.8 km
  - 41,162 unique indexes
- a total of 710 cells needed to cover the US
- 169 (23.8%) have a population less than 20,000
- among these 169, the median population is 7,116 people (min: 0.346; max: 19,930)
- iterative merging results in 578 geohash chunks with median population of 192,095 people (min: 20,494; max: 17,148,917)
- list of merged geohash IDs saved as `s3://geomarker/st_pm_hex/us_h3_3_population_20k_minimum_hex_ids.rds`

### 15. make all prediction data

- function takes in `h3_5` (or hyphen separated combination of these) and outputs `h3_data/{h3_5}_h3data.qs` with all data needed for prediction (`s3://geomarker/st_pm_hex/h3_data{h3_5}_h3data.qs`)
	
### 16. make all predictions

- function takes in `h3_5` (or hyphen separated combination of these) and outputs one prediction file per year, `h3_pm/{h3_5}_{year}_h3pm.fst` with `date`, `h3`, `pm_pred`, and `pm_se` (`s3://geomarker/st_pm_hex/h3_pm{h3_5}_h3pm.qs`)
- these can be read in with `fst::fst_read(..., as.data.table = TRUE)` to utilize them as data.table objects, with the keys precalculated on `h3` and `date`
- one file size (with no aggregated chunks) is about 20 MB on disk and 200 MB in RAM
- the folder `st_pm_hex_big_predict` contains a script for predicting all where the Safe Harbor chunk is very large and requires more parallelization to compute on a feasible amount of available RAM (200 GB)
    
