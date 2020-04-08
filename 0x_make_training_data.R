library(data.table)
library(tidyr)
library(dplyr)
library(tibble)
library(magrittr)

# start with aqs data
d <-
  qs::qread("h3data_aqs.qs") %>%
  as_tibble()

# median of median pm25 from yesterday, today, and tomorrow for each 5-digit h3 area
d_nearby_pm <-
  d %>%
  mutate(h3_5 = substr(h3, 1, 5)) %>%
  group_by(date, h3_5) %>%
  summarize(median_pm25 = median(pm25, na.rm = TRUE)) %>%
  mutate(
    median_pm25_ytd = dplyr::lag(median_pm25, n = 1, order_by = date),
    median_pm25_tmw = dplyr::lead(median_pm25, n = 1, order_by = date)
  ) %>%
  group_by(date, h3_5) %>%
  mutate(nearby_pm25 = median(c(median_pm25, median_pm25_ytd, median_pm25_tmw), na.rm = TRUE)) %>%
  select(h3_5, date, nearby_pm25)

# TODO once this is finalized for the training data, then export this in the pm25 script for use in predictions

d$h3_5 <- substr(d$h3, 1, 5)

d <- left_join(d, d_nearby_pm, by = c("h3_5", "date"))
d$h3_5 <- NULL

# add in year, doy, dow
d <- d %>%
  mutate(
    year = lubridate::year(date),
    doy = lubridate::yday(date),
    dow = as.character(lubridate::wday(date))
  )

# add in epsg 5072 coordinates
d_geom <-
  h3::h3_to_geo_sf(d$h3) %>%
  sf::st_transform(5072)

# add in county while we're at it (for nei_county data)
county_fips <-
  tigris::counties() %>%
  sf::st_as_sf() %>%
  sf::st_transform(5072) %>%
  transmute(fips = GEOID)

d_geom <- sf::st_join(d_geom, county_fips)

d$county_fips <- d_geom$fips

d_geom <- sf::st_coordinates(d_geom)

d$x <- d_geom[, "X"]
d$y <- d_geom[, "Y"]

rm(d_geom, county_fips)

# data.table it
d <- data.table(d, key = c("h3", "year", "date"))

# data.table merge in narr and nlcd aod data

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

# remove rows without nlcd or narr b/c these were aqs observations outside contiguous US
d <- na.omit(d)

# merge in nei data

nei_year_lookup <- tribble(
  ~year, ~nei_year,
  2000, 2008,
  2001, 2008,
  2002, 2008,
  2003, 2008,
  2004, 2008,
  2005, 2008,
  2006, 2008,
  2007, 2008,
  2008, 2008,
  2009, 2008,
  2010, 2011,
  2011, 2011,
  2012, 2011,
  2013, 2014,
  2014, 2014,
  2015, 2014,
  2016, 2017,
  2017, 2017,
  2018, 2017,
  2019, 2017,
)

d_nei_point <- readRDS("nei_point_pm25.rds")
d_nei_point <- left_join(d_nei_point, nei_year_lookup, by = "nei_year")
d_nei_point <- as.data.table(d_nei_point, key = c("h3", "year"))

d[d_nei_point, nei_point := i.total_emissions, on = c("h3", "year")]

# replace missing nei point emissions with zero
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

d$county_fips <- NULL

# TODO merge in distance to nearest PM2.5 emission site??

# merge in aod (which could be missing)
d_aod <- fst::read_fst("h3data_aod.fst", as.data.table = TRUE)

d[d_aod, aod := i.aod, on = c("h3", "date")]

rm(d_aod)

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

# some summary stuff
tables()

d[!is.na(aod), .N]
d[!is.na(aod), .N] / nrow(d) * 100

