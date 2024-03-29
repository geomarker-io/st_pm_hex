library(data.table)
library(tidyr)
library(dplyr)
library(sf)

options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

if (!file.exists("county_fips_contig_us_5072.rds")) {
  states_to_keep <-
    tigris::states(year = 2019) %>%
    filter(!NAME %in% c(
    "United States Virgin Islands",
    "Guam", "Commonwealth of the Northern Mariana Islands",
    "American Samoa", "Puerto Rico",
    "Alaska", "Hawaii"
    )) %>%
  sf::st_drop_geometry() %>%
  select(NAME, STATEFP)

  county_fips <-
    tigris::counties() %>%
    filter(STATEFP %in% states_to_keep$STATEFP) %>%
    transmute(fips = GEOID) %>%
    sf::st_transform(5072)

  saveRDS(county_fips, "county_fips_contig_us_5072.rds")
}

county_fips <- readRDS("county_fips_contig_us_5072.rds")

year_nei_year <-
  tribble(
    ~year, ~nei_year,
    as.character(c(2000:2009)), "2008",
    as.character(c(2010:2012)), "2011",
    as.character(c(2013:2015)), "2014",
    as.character(c(2016:2020)), "2017",
  ) %>%
  unnest(cols = year)

d_nei_county <- readRDS("nei_county_pm25.rds") %>%
  left_join(year_nei_year, by = "nei_year") %>%
  rename(county_fips = fips) %>%
  as.data.table(key = c("county_fips", "year"))

nearby_pm <-
  readRDS("nearby_pm.rds") %>%
  rename(h3_3 = h3) %>%
  as.data.table()

r_narr_empty <-
  raster::raster(
    nrows = 277,
    ncols = 349,
    xmn = -16231.49,
    xmx = 11313351,
    ymn = -16231.5,
    ymx = 8976020,
    crs = "+proj=lcc +x_0=5632642.22547 +y_0=4612545.65137 +lat_0=50 +lon_0=-107 +lat_1=50",
    resolution = c(32462.99, 32463),
    vals = NULL
  )

safe_harbor_h3 <- readRDS("us_h3_3_population_20k_minimum_hex_ids.rds")

cincinnati_h3_6s <- c(
  "832a93fffffffff",
  "832a90fffffffff",
  "83266dfffffffff",
  "832a9efffffffff"
)
# example combined h3 geohashes
# the_geohash <- safe_harbor_h3[515]
# the_geohash <- safe_harbor_h3[573]

## TODO fix bug where outname is misread when using geohash combo with hyphens

# data we need to make predictions:
# nearby_pm, doy, x, y, nei_event, narr

create_training_data <-
  function(the_geohash = safe_harbor_h3[515], force = FALSE) {

    out_name <- glue::glue("h3_data/{the_geohash}_h3data.qs")

    if (fs::file_exists(out_name) & !force) {
      message(out_name, " already exists")
      return(invisible(NULL))
    }

    if (stringr::str_detect(the_geohash, stringr::fixed("-"))) {
      the_geohash <- stringr::str_split(the_geohash, stringr::fixed("-"), simplify = FALSE)[[1]]
    }

    children_geohashes <- unlist(purrr::map(the_geohash, h3::h3_to_children, res = 8))

    d_points <-
      children_geohashes %>%
      h3::h3_to_geo_sf() %>%
      st_transform(5072) %>%
      mutate(h3 = children_geohashes)

    d_points$county_fips <- st_join(d_points, county_fips)$fips

    d_points$x <- sf::st_coordinates(d_points)[, "X"]
    d_points$y <- sf::st_coordinates(d_points)[, "Y"]

    d_points$narr_cell <-
      d_points %>%
      sf::st_transform(crs = raster::crs(r_narr_empty)) %>%
      sf::st_coordinates() %>%
      as.matrix() %>%
      raster::cellFromXY(r_narr_empty, .)
    
    d_points$h3_3 <- purrr::map_chr(d_points$h3, h3::h3_to_parent, res = 3)

    d <-
      expand_grid(
        h3 = children_geohashes,
        date = seq.Date(as.Date("2000-01-01"), as.Date("2020-12-31"), by = 1)
      )

    d <- d %>%
      mutate(
        year = as.character(lubridate::year(date)),
        doy = as.numeric(as.character(lubridate::yday(date)))
      )

    d <- as.data.table(d, key = c("h3", "date", "year"))

    d <- merge(d, st_drop_geometry(d_points), by = "h3", allow.cartesian = TRUE)

    d <- merge(d, nearby_pm, by = c("h3_3", "date"))
    d$h3_3 <- NULL

    d[d_nei_county, nei_event := i.event, on = c("county_fips", "year")]

    d_for_narr <-
      select(d, narr_cell, date) %>%
      unique() %>%
      mutate(
        start_date = date,
        end_date = date
      )

    d_for_narr <-
      addNarrData::get_narr_data(d_for_narr, narr_variables = c("hpbl", "vwnd.10m", "air.2m")) %>%
      as.data.table(key = c("narr_cell", "date"))

    d[d_for_narr, hpbl := i.hpbl, on = c("narr_cell", "date")]
    d[d_for_narr, vwnd.10m := i.vwnd.10m, on = c("narr_cell", "date")]
    d[d_for_narr, air.2m := i.air.2m, on = c("narr_cell", "date")]

    fs::dir_create("h3_data")
    qs::qsave(d, out_name, nthreads = parallel::detectCores())
    system(glue::glue("aws s3 cp {out_name} s3://geomarker/st_pm_hex/{out_name}"))

    message(out_name, " completed")
    return(invisible(NULL))
  }


## tictoc::tic()
## create_training_data(cincinnati_h3_6s[1])
## tictoc::toc()
create_training_data(the_geohash = "83281afffffffff")

## purrr::walk(cincinnati_h3_6s, create_training_data)

## tictoc::tic()
## create_training_data(safe_harbor_h3[515])
## tictoc::toc()
## create_training_data(safe_harbor_h3[573])

purrr::walk(safe_harbor_h3, create_training_data)
