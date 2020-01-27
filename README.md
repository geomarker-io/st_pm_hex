## using `renv` for package management

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

## storing and retrieving data on AWS S3

- the aws cli must be installed and you must have proper permissions with `AWS_SECRET_ACCESS_KEY` and `AWS_ACCESS_KEY_ID` environment variables set
- storing data
    - sync the local working directory with a S3 directory with `aws s3 sync . s3://path/to/remote/folder`
    - for example sync clean AOD rasters with `aws s3 sync ./aod_clean_rasters s3://geomarker/aod`
- retrieving data
    - `aws s3 sync s3://geomarker/aod ./aod_clean_rasters`

## 1. make h3 grid

- use the [H3 hexagonal hierarchical spatial index](https://eng.uber.com/h3/) to create the grid
    - average area of an h3 hexagon at a precision of 8 is 0.7373 sq km
    - average length of an h3 hexagon side at a precision of 8 is 0.4614 km
    - there are 11,932,970 unique valid H3 indexes at this resolution that we use to cover the contiguous US
    - each hash is made up of 15 total characters; for the precision of 8, the first characteristics 10 are used and the last 5 are "filler" f's (values can be 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, a, b, c, d, e, f)
- see the [example cincinnati map](h3_cincy_example_map.html) for an example of the hierarchical h3 layout
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

## 2. get AQS data

- average by date for co-located stations (5,032,639 total rows, but only 3,347,073 total unique station/lat/lon/date combinations)
- saved remotely as [s3://geomarker/st_pm_hex/data_aqs_pm25.rds](https://geomarker.s3.us-east-2.amazonaws.com/st_pm_hex/data_aqs_pm25.rds)

## 3. get NARR data

- [NARR](https://www.esrl.noaa.gov/psd/data/gridded/data.narr.html) data details
- each different NARR observation type (hpbl, precip, etc.) is in a RDS file with h3 id and date; stored at [s3://geomarker/st_pm_hex/](https://geomarker.s3.us-east-2.amazonaws.com/st_pm_hex) (i.e., `data_narr_hpbl_h3.rds`, `data_narr_precip_h3.rds`, etc.)

## 4. get MODIS data

- [MCD19A2: MODIS/Terra and Aqua MAIAC Land AOD Daily L2G 1km SIN Grid V006](https://lpdaac.usgs.gov/products/mcd19a2v006/)
- [MCD19A2 User Guide](https://lpdaac.usgs.gov/documents/110/MCD19_User_Guide_V6.pdf)
- cleaned AOD rasters are stored at [s3://geomarker/aod/](https://geomarker.s3.us-east-2.amazonaws.com/aod)

### Running It

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

## ?? get GFED data

- [GFED](https://www.geo.vu.nl/~gwerf/GFED/GFED4/) is the Global Fire Emissions Database
  - 0.25 x 0.25 degree grid
  - 1997 - present
  - monthly pm emissions with daily fraction
- [FINN](https://www2.acom.ucar.edu/modeling/finn-fire-inventory-ncar) is the Fire Emissions from NCAR database
  - https://doi.org/10.5194/gmd-4-625-2011
  - http://bai.acom.ucar.edu/Data/fire/data/FINNv1.5_2017.GEOSCHEM.tar.gz

## get NEI data

- [NEI](https://www.epa.gov/air-emissions-inventories/national-emissions-inventory-nei) is the National Emissions Inventory Database
  - point data available in 2008, 2011, 2014, 2017
- NEI has more than industrial emissions data
  - [scc](https://ofmpub.epa.gov/sccwebservices/sccsearch/) is a code which defines the type of emissions
  - includes aircraft, other non-point sources
  - county = "Multiple (portable facilities)" is for non-point sources averaged over a whole county; use `fips code` in this case?


## notes for later lookup using h3

```r
## geo_to_h3 takes an sf object
h3::geo_to_h3(sf_object, res = 8)

h3::h3_to_geo_boundary_sf()
```
