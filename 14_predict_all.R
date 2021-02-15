library(data.table)
library(tidyr)
library(dplyr)
library(sf)

options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

if (!file.exists("roads1100_sf_5072_unionized.rds")) {
  roads1100 <-
    tigris::primary_roads(year = 2018) %>%
    sf::st_transform(5072) %>%
    sf::st_union()
  saveRDS(roads1100, "roads1100_sf_5072_unionized.rds")
}

roads1100 <- readRDS("roads1100_sf_5072_unionized.rds")

if (!file.exists("county_fips_contig_us_5072.rds")) {
  states_to_keep <-
    tigris::states() %>%
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

d_pop <-
  readRDS("us_h3_5_population.rds") %>%
  as.data.table()

d_nei_point <-
  readRDS("nei_point_pm25.rds") %>%
  sf::st_as_sf() %>%
  sf::st_transform(5072)

d_nei_point_year_list <-
  d_nei_point %>%
  split(d_nei_point$nei_year) %>%
  purrr::map(sf::st_union)

d_nei_county <- readRDS("nei_county_pm25.rds")
d_nei_county$nei_year <- as.numeric(d_nei_county$nei_year)

d_aod <- fst::read_fst("h3data_aod.fst", as.data.table = TRUE)

d_finn <- fst::read_fst("h3data_finn.fst") %>%
  as.data.table(key = c("date", "h3"))

safe_harbor_h3 <- readRDS("us_h3_4_population_20k_minimum_hex_ids.rds")

# example combined h3 geohashes
# the_geohash <- safe_harbor_h3[515]
# the_geohash <- safe_harbor_h3[573]

create_training_data <-
  function(the_geohash = safe_harbor_h3[1], force = FALSE) {

    if (stringr::str_detect(the_geohash, stringr::fixed("-"))) {
      the_geohash <- stringr::str_split(the_geohash, stringr::fixed("-"), simplify = FALSE)[[1]]
    }

    out_name <- glue::glue("h3_data/{the_geohash}_h3data.rds")

    if (fs::file_exists(out_name) & !force) {
      message(out_name, " already exists")
      return(invisible(NULL))
    }

    children_geohashes <- unlist(purrr::map(the_geohash, h3::h3_to_children, res = 8))

    d_points <-
      children_geohashes %>%
      h3::h3_to_geo_sf() %>%
      st_transform(5072) %>%
      mutate(h3 = children_geohashes)

    d_points$s1100_dist <-
      mappp::mappp(
        seq_len(nrow(d_points)),
        ~ st_distance(d_points[., ], roads1100),
        parallel = TRUE
      ) %>%
      unlist()

    d_points$county_fips <- st_join(d_points, county_fips)$fips

    d_points$x <- sf::st_coordinates(d_points)[, "X"]
    d_points$y <- sf::st_coordinates(d_points)[, "Y"]

    d_points <- d_points %>%
      mutate(h3_5 = h3::h3_to_parent(h3, res = 5)) %>%
      left_join(select(d_pop, -geometry), by = "h3_5") %>%
      select(-h3_5, -population, -area)

    # get NARR cell number for each point
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

    d_points$narr_cell <-
      d_points %>%
      sf::st_transform(crs = raster::crs(r_narr_empty)) %>%
      as("Spatial") %>%
      raster::cellFromXY(r_narr_empty, .)

    d_polygons <-
      children_geohashes %>%
      h3::h3_to_geo_boundary_sf() %>%
      st_transform(5072) %>%
      mutate(h3 = children_geohashes)

    d_polygons_nlcd_data <-
      mappp::mappp(
        seq_len(nrow(d_polygons)),
        ~ addNlcdData::get_nlcd_data_polygons(d_polygons[., ]),
        parallel = TRUE
      )

    year_nlcd_year <-
      tribble(
        ~nlcd_year, ~year,
        "2001", as.character(2000:2003),
        "2006", as.character(2004:2008),
        "2011", as.character(2009:2013),
        "2016", as.character(2014:2020)
      ) %>%
      unnest(cols = c(year))

    d_nlcd <-
      d_polygons_nlcd_data[!is.na(d_polygons_nlcd_data)] %>%
      purrr::map_dfr(st_drop_geometry) %>%
      rename(nlcd_year = year) %>%
      left_join(year_nlcd_year, ., by = "nlcd_year")

    #### distance to NEI site
    get_nei_dists <- function(the_locs, nei_year) {
      mappp::mappp(
        seq_len(nrow(d_points)),
        ~ sf::st_distance(d_points[., ], d_nei_point_year_list[[nei_year]]),
        parallel = TRUE,
        cache = FALSE
      ) %>%
        unlist()
    }

    d_points$nei_dist.2008 <- get_nei_dists(d_points, "2008")
    d_points$nei_dist.2011 <- get_nei_dists(d_points, "2011")
    d_points$nei_dist.2014 <- get_nei_dists(d_points, "2014")
    d_points$nei_dist.2017 <- get_nei_dists(d_points, "2017")

    d_points <-
      d_points %>%
      sf::st_drop_geometry() %>%
      as_tibble() %>%
      pivot_longer(
        cols = c(starts_with("nei_dist")),
        names_to = c("foo", "nei_year"), values_to = "nei_dist",
        names_sep = "[.]"
      ) %>%
      select(-foo)

    year_nei_year <-
      tribble(
        ~year, ~nei_year,
        as.character(c(2000:2009)), "2008",
        as.character(c(2010:2012)), "2011",
        as.character(c(2013:2015)), "2014",
        as.character(c(2016:2020)), "2017",
      ) %>%
      unnest(cols = year)

    d_nei <-
      left_join(year_nei_year, d_points, by = "nei_year")

    ## create time series
    d <-
      expand_grid(
        date = seq.Date(as.Date("2000-01-01"), as.Date("2020-12-31"), by = 1),
        h3 = children_geohashes
      )

    # add in year, doy, dow
    d <- d %>%
      mutate(
        year = as.character(lubridate::year(date)),
        doy = lubridate::yday(date),
        dow = as.character(lubridate::wday(date))
      )

    d <- left_join(d, d_nlcd, by = c("h3", "year"))
    d <- left_join(d, d_nei, by = c("h3", "year"))

    # data.table it
    d <- as.data.table(d, key = c("h3", "year", "date"))
    
    # add in true/false for holidays
    major_holidays <- function(years) {
      out <- c(
        timeDate::USNewYearsDay(years),
        timeDate::USIndependenceDay(years),
        timeDate::USThanksgivingDay(years),
        timeDate::USChristmasDay(years),
        timeDate::USLaborDay(years),
        timeDate::USMLKingsBirthday(years),
        timeDate::USMemorialDay(years)
      )
      as.Date(out)
    }

    d$holiday <- d$date %in% major_holidays(2000:2020)

    # geohashed nei point data (make implicitly missing data equal to zero)
    d_nei_point_allyears <- left_join(d_nei_point, year_nei_year, by = "nei_year")
    d_nei_point_allyears <- as.data.table(d_nei_point_allyears, key = c("h3", "year"))
    d_nei_point_allyears$year <- as.character(d_nei_point_allyears$year)
    d[d_nei_point_allyears, nei_point := i.total_emissions, on = c("h3", "year")]
    d[is.na(nei_point), nei_point := 0]

    # county NEI data by year
    d_nei_county <- left_join(d_nei_county, year_nei_year, by = "nei_year")
    d_nei_county <- rename(d_nei_county, county_fips = fips)
    d_nei_county$year <- as.character(d_nei_county$year)
    d_nei_county <- as.data.table(d_nei_county, key = c("county_fips", "year"))

    d[d_nei_county, nei_nonroad := i.nonroad, on = c("county_fips", "year")]
    d[d_nei_county, nei_onroad := i.onroad, on = c("county_fips", "year")]
    d[d_nei_county, nei_nonpoint := i.nonpoint, on = c("county_fips", "year")]
    d[d_nei_county, nei_event := i.event, on = c("county_fips", "year")]

    # add NARR data
    d_for_narr <-
      select(d, narr_cell, date) %>%
      unique() %>%
      mutate(
        start_date = date,
        end_date = date
      )

    d_for_narr <-
      addNarrData::get_narr_data(d_for_narr) %>%
      as.data.table(key = c("narr_cell", "date"))

    d[d_for_narr, hpbl := i.hpbl, on = c("narr_cell", "date")]
    d[d_for_narr, vis := i.vis, on = c("narr_cell", "date")]
    d[d_for_narr, uwnd.10m := i.uwnd.10m, on = c("narr_cell", "date")]
    d[d_for_narr, vwnd.10m := i.vwnd.10m, on = c("narr_cell", "date")]
    d[d_for_narr, air.2m := i.air.2m, on = c("narr_cell", "date")]
    d[d_for_narr, rhum.2m := i.rhum.2m, on = c("narr_cell", "date")]
    d[d_for_narr, prate := i.prate, on = c("narr_cell", "date")]
    d[d_for_narr, pres.sfc := i.pres.sfc, on = c("narr_cell", "date")]

    # merge in aod
    d[d_aod, aod := i.aod, on = c("h3", "date")]

    d[d$aod > 2, "aod"] <- NA

    d[d_finn, fire_pm25 := i.fire_pm25, on = c("date", "h3")]
    d[d_finn, fire_area := i.area, on = c("date", "h3")]

    # replace missing finn estimates with zero
    d[is.na(fire_pm25), fire_pm25 := 0]
    d[is.na(fire_area), fire_area := 0]

    fs::dir_create("h3_data")
    saveRDS(d, out_name)
    system(glue::glue("aws s3 cp {out_name} s3://geomarker/st_pm_hex/{out_name}"))

    message(out_name, " completed")
    return(invisible(NULL))
  }

tictoc::tic()
create_training_data(safe_harbor_h3[1])
tictoc::toc()

#### predict

grf <- qs::qread("st_pm_hex_grf.qs", nthreads = parallel::detectCores())

create_predict_data <-
  function(the_geohash, force = FALSE) {

    out_name <- glue::glue("h3_pm/{the_geohash}_h3pm.rds")

    if (fs::file_exists(out_name) & !force) {
      message(out_name, " already exists")
      return(invisible(NULL))
    }

    d <- readRDS(glue::glue("h3_data/{the_geohash}_h3data.rds"))

    d$nei_year <- NULL
    d$nlcd_year <- NULL
    d$dow <- as.numeric(d$dow)
    d$holiday <- as.numeric(d$holiday)

    grf_preds <- predict(grf_cluster, d, estimate.variance = TRUE)
    #TODO round all predictions and SEs to 4 significant digits???
    # will this save any size?
    d <- d %>%
      mutate_at(vars(starts_with("pm_pred_")), signif, digits = 4)

    ## save it and upload
    saveRDS(d, out_name)
    system(glue::glue("aws s3 cp {out_name} s3://geomarker/st_pm_hex/{out_name}"))
  }
