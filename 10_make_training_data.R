library(data.table)
library(tidyr)
library(dplyr)
library(sf)

## get some tigris data up front if we don't already have it
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

# start with aqs data (or any geohashed points with a date and h3 column)
d <-
  qs::qread("h3data_aqs.qs") %>%
  as.data.table(key = "h3")

## do all the spatial, but non-temporal stuff first on unique locations to save compute time

d_locs_h3 <- d %>%
  pull(h3) %>%
  unique()
  
d_locs <-
  h3::h3_to_geo_sf(d_locs_h3) %>%
  st_transform(5072)

d_locs$h3 <- d_locs_h3

# add in distance to nearest S1100 road from 2018
d_locs$s1100_dist <-
  mappp::mappp(
    seq_len(nrow(d_locs)),
    ~ st_distance(d_locs[., ], roads1100),
    parallel = TRUE
  ) %>%
  unlist()

# add county fips
d_locs$county_fips <- st_join(d_locs, county_fips)$fips
# remove 1 point b/c h3 centroid is in Mexico
d_locs <- filter(d_locs, !is.na(county_fips))

# add in epsg 5072 coordinates
d_locs$x <- sf::st_coordinates(d_locs)[, "X"]
d_locs$y <- sf::st_coordinates(d_locs)[, "Y"]

# add in 2018 population density
d_pop <-
  readRDS("us_h3_5_population.rds") %>%
  as.data.table()

d$h3_5 <- purrr::map_chr(d$h3, h3::h3_to_parent, res = 5)
d[d_pop, population_density := i.population_density, on = c("h3_5")]
d$h3_5 <- NULL

## adding spatial data available at some years

# distance to NEI site
d_nei_point <-
  readRDS("nei_point_pm25.rds") %>%
  sf::st_as_sf() %>%
  sf::st_transform(5072)

d_nei_point_year_list <-
  d_nei_point %>%
  split(d_nei_point$nei_year) %>%
  purrr::map(sf::st_union)

get_nei_dists <- function(the_locs, nei_year) {
  mappp::mappp(
    seq_len(nrow(d_locs)),
    ~ sf::st_distance(d_locs[., ], d_nei_point_year_list[[nei_year]]),
    parallel = TRUE,
  ) %>%
  unlist()
}

d_locs$nei_dist.2008 <- get_nei_dists(d_locs, "2008")
d_locs$nei_dist.2011 <- get_nei_dists(d_locs, "2011")
d_locs$nei_dist.2014 <- get_nei_dists(d_locs, "2014")
d_locs$nei_dist.2017 <- get_nei_dists(d_locs, "2017")

d_locs <-
  d_locs %>%
  sf::st_drop_geometry() %>%
  as_tibble() %>%
  pivot_longer(
    cols = c(starts_with("nei_dist")),
    names_to = c("foo", "nei_year"), values_to = "nei_dist",
    names_sep = "[.]"
  ) %>%
  select(-foo) %>%
  mutate(nei_year = as.numeric(nei_year))

nei_year_lookup <-
   tribble(
  ~year, ~nei_year,
  c(2000:2009), 2008,
  c(2010:2012), 2011,
  c(2013:2015), 2014,
  c(2016:2020), 2017,
) %>%
  unnest(cols = year)

d_locs_all_years <- left_join(nei_year_lookup, d_locs, by = "nei_year")

## merge all into the spatiotemporal data.frame

# add in year, doy, dow
d <- d %>%
  mutate(
    year = lubridate::year(date),
    doy = lubridate::yday(date),
    dow = as.character(lubridate::wday(date))
  )

d <-
  left_join(d, d_locs_all_years, by = c("h3", "year")) %>%
  as.data.table(key = c("h3", "year", "date"))

d <- left_join(d, d_locs, by = c("h3"))
nrow(d)

# again remove prediction points if the h3 centroid for a location did not fall within the contiguous US
d <- d %>% filter(!is.na(county_fips))

# nrow here is 2,374,309 (with no missing data)

##################################

## merge in all spatiotemporal data

# add in true/false for holidays
major_holidays <- function(years){
    out <- c(timeDate::USNewYearsDay(years),
             timeDate::USIndependenceDay(years),
             timeDate::USThanksgivingDay(years),
             timeDate::USChristmasDay(years),
             timeDate::USLaborDay(years),
             timeDate::USMLKingsBirthday(years),
             timeDate::USMemorialDay(years))
    as.Date(out)
}

d$holiday <- d$date %in% major_holidays(2000:2020)

# now add in existing NEI data

d_nei_point_allyears <- left_join(d_nei_point, nei_year_lookup, by = "nei_year")
d_nei_point_allyears <- as.data.table(d_nei_point_allyears, key = c("h3", "year"))
d[d_nei_point_allyears, nei_point := i.total_emissions, on = c("h3", "year")]
# replace implicitly missing nei point emissions with zero
d[is.na(nei_point), nei_point := 0]

d_nei_county <- readRDS("nei_county_pm25.rds")
d_nei_county$nei_year <- as.numeric(d_nei_county$nei_year)
d_nei_county <- left_join(d_nei_county, nei_year_lookup, by = "nei_year")

d_nei_county <- rename(d_nei_county, county_fips = fips)
d_nei_county <- as.data.table(d_nei_county, key = c("county_fips", "year"))

d[d_nei_county, nei_nonroad := i.nonroad, on = c("county_fips", "year")]
d[d_nei_county, nei_onroad := i.onroad, on = c("county_fips", "year")]
d[d_nei_county, nei_nonpoint := i.nonpoint, on = c("county_fips", "year")]
d[d_nei_county, nei_event := i.event, on = c("county_fips", "year")]

# d$county_fips <- NULL

# data.table merge in narr data
# missing data in NARR is a known problem and seems to come from the source data
d_narr <-
  qs::qread("h3data_narr.qs") %>%
  as.data.table(key = c("h3", "date"))

# can't get multi join (`:=`(... = ...)) working, so join one-by-one
d[d_narr, hpbl := i.hpbl, on = c("h3", "date")]
d[d_narr, vis := i.vis, on = c("h3", "date")]
d[d_narr, uwnd.10m := i.uwnd.10m, on = c("h3", "date")]
d[d_narr, vwnd.10m := i.vwnd.10m, on = c("h3", "date")]
d[d_narr, air.2m := i.air.2m, on = c("h3", "date")]
d[d_narr, rhum.2m := i.rhum.2m, on = c("h3", "date")]
d[d_narr, prate := i.prate, on = c("h3", "date")]
d[d_narr, pres.sfc := i.pres.sfc, on = c("h3", "date")]

rm(d_narr)

# data.table merge in nlcd data
d_nlcd <-
  qs::qread("h3data_nlcd.qs") %>%
  mutate(year = as.numeric(year)) %>%
  as.data.table(key = c("h3", "year"))

d[d_nlcd, impervious := i.impervious, on = c("h3", "year")]
d[d_nlcd, green := i.green, on = c("h3", "year")]
d[d_nlcd, primary_urban := i.primary_urban, on = c("h3", "year")]
d[d_nlcd, primary_rural := i.primary_rural, on = c("h3", "year")]
d[d_nlcd, secondary_urban := i.secondary_urban, on = c("h3", "year")]
d[d_nlcd, secondary_rural := i.secondary_rural, on = c("h3", "year")]
d[d_nlcd, tertiary_urban := i.tertiary_urban, on = c("h3", "year")]
d[d_nlcd, tertiary_rural := i.tertiary_rural, on = c("h3", "year")]
d[d_nlcd, thinned_urban := i.thinned_urban, on = c("h3", "year")]
d[d_nlcd, thinned_rural := i.thinned_rural, on = c("h3", "year")]
d[d_nlcd, nonroad_urban := i.nonroad_urban, on = c("h3", "year")]
d[d_nlcd, nonroad_rural := i.nonroad_rural, on = c("h3", "year")]
d[d_nlcd, energyprod_urban := i.energyprod_urban, on = c("h3", "year")]
d[d_nlcd, energyprod_rural := i.energyprod_rural, on = c("h3", "year")]
d[d_nlcd, nonimpervious := i.nonimpervious, on = c("h3", "year")]

rm(d_nlcd)

# merge in aod
d_aod <- fst::read_fst("h3data_aod.fst", as.data.table = TRUE)
d[d_aod, aod := i.aod, on = c("h3", "date")]
rm(d_aod)

# there are aod values greater than 2, but not relating to any of the known pm2.5 measurements
d[d$aod > 2, "aod"] <- NA

# merge in finn data

d_finn <- fst::read_fst("h3data_finn.fst")
d_finn <- as.data.table(d_finn, key = c("date", "h3"))
d[d_finn, fire_pm25 := i.fire_pm25, on = c("date", "h3")]
d[d_finn, fire_area := i.area, on = c("date", "h3")]

# replace missing finn estimates with zero
d[is.na(fire_pm25), fire_pm25 := 0]
d[is.na(fire_area), fire_area := 0]

# save it
fst::write_fst(d, "h3data_train.fst", compress = 100)
system("aws s3 cp h3data_train.fst s3://geomarker/st_pm_hex/h3data_train.fst")
# when we do this for points, also save all h3 data as "h3data_{gh5}.fst" so we can have everything for further use later

# d <- fst::read_fst("h3data_train.fst", as.data.table = TRUE)

# some summary stuff
tables()

d[!is.na(aod), .N]
d[!is.na(aod), .N] / nrow(d) * 100
