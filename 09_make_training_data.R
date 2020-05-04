library(data.table)
library(tidyr)
library(dplyr)
library(tibble)
library(magrittr)

# start with aqs data
d <-
  qs::qread("h3data_aqs.qs") %>%
  as_tibble()

d_geom <-
  h3::h3_to_geo_sf(d$h3) %>%
  sf::st_transform(5072)
d_geom$h3 <- d$h3

# add county fips, but subset aqs data to only contiguous US
county_fips <-
  tigris::counties() %>%
  sf::st_as_sf() %>%
  filter(STATEFP %in% c(
    "01", "04", "05", "06", "08",
    "09", "10", "11", "12", "13",
    "16", "17", "18", "19", "20",
    "21", "22", "23", "24", "25",
    "26", "27", "28", "29", "30",
    "31", "32", "33", "34", "35",
    "36", "37", "38", "39", "40",
    "41", "42", "44", "45", "46",
    "47", "48", "49", "50", "51",
    "53", "54", "55", "56"
  )) %>%
  transmute(fips = GEOID) %>%
  sf::st_transform(5072)

d$county_fips <- sf::st_join(d_geom, county_fips)$fips

d_geom <- filter(d_geom, !is.na(d$county_fips))
d <- d %>% filter(!is.na(county_fips))

# add in epsg 5072 coordinates
d$x <- sf::st_coordinates(d_geom)[, "X"]
d$y <- sf::st_coordinates(d_geom)[, "Y"]

# median of median pm25 from yesterday, today, and tomorrow for each res-5 h3 area
d_nearby_pm <-
  d %>%
  mutate(h3_5 = h3::h3_to_parent(h3, res = 5)) %>%
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

d$h3_5 <- h3::h3_to_parent(d$h3, res = 5)

d <- left_join(d, d_nearby_pm, by = c("h3_5", "date"))

d$h3_5 <- NULL
rm(d_nearby_pm)

# add in year, doy, dow
d <- d %>%
  mutate(
    year = lubridate::year(date),
    doy = lubridate::yday(date),
    dow = as.character(lubridate::wday(date))
  )

# data.table it
d <- data.table(d, key = c("h3", "year", "date"))

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

d_nei_point <-
  readRDS("nei_point_pm25.rds") %>%
  sf::st_as_sf() %>%
  sf::st_transform(sf::st_crs(d_geom))

# add in distance to nei site before we expand out to all years

library(sp)

d_nei_point_sp <-
  d_nei_point %>%
  split(d_nei_point$nei_year) %>%
  purrr::map(as, "Spatial")

nei_dists_2008 <- mappp::mappp(1:nrow(d_geom),
  ~ as.vector(rgeos::gDistance(
    as(d_geom[., ], "Spatial"),
    d_nei_point_sp[["2008"]],
    byid = c(TRUE, FALSE)
  )),
  parallel = TRUE
)

nei_dists_2011 <- mappp::mappp(1:nrow(d_geom),
  ~ as.vector(rgeos::gDistance(
    as(d_geom[., ], "Spatial"),
    d_nei_point_sp[["2011"]],
    byid = c(TRUE, FALSE)
  )),
  parallel = TRUE
)

nei_dists_2014 <- mappp::mappp(1:nrow(d_geom),
  ~ as.vector(rgeos::gDistance(
    as(d_geom[., ], "Spatial"),
    d_nei_point_sp[["2014"]],
    byid = c(TRUE, FALSE)
  )),
  parallel = TRUE
)

nei_dists_2017 <- mappp::mappp(1:nrow(d_geom),
  ~ as.vector(rgeos::gDistance(
    as(d_geom[., ], "Spatial"),
    d_nei_point_sp[["2017"]],
    byid = c(TRUE, FALSE)
  )),
  parallel = TRUE
)

nei_dists <-
  list(nei_dists_2008, nei_dists_2011, nei_dists_2014, nei_dists_2017) %>%
  purrr::map(unlist)

qs::qsave(nei_dists, "nei_jic.qs")
## nei_dists <- qs::qread("nei_jic.qs")

nei_dists <-
  nei_dists %>%
  set_names(c("2008", "2011", "2014", "2017")) %>%
  tibble::enframe(name = "nei_year", value = "nei_dist") %>%
  mutate(nei_year = as.numeric(nei_year),
         h3 = list(d$h3, d$h3, d$h3, d$h3))

nei_dists <- nei_dists %>% unnest(cols = c(nei_dist, h3))

nei_dists <- left_join(nei_dists, nei_year_lookup, by = "nei_year")
nei_dists <- as.data.table(nei_dists, key = c("h3", "year"))
d[nei_dists, nei_dist := i.nei_dist, on = c("h3", "year"), by = .EACHI]

# now add in existing NEI data

d_nei_point_allyears <- left_join(d_nei_point, nei_year_lookup, by = "nei_year")

d_nei_point_allyears <- as.data.table(d_nei_point_allyears, key = c("h3", "year"))

d[d_nei_point_allyears, nei_point := i.total_emissions, on = c("h3", "year")]

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

# add in distance to nearest S1100 road from 2018
roads1100 <-
  tigris::primary_roads(year = 2018) %>%
  sf::st_as_sf() %>%
  sf::st_transform(sf::st_crs(d_geom)) %>%
  as("Spatial")

s1100_dists <- mappp::mappp(1:nrow(d_geom),
  ~ as.vector(rgeos::gDistance(
    as(d_geom[., ], "Spatial"),
    roads1100,
    byid = c(TRUE, FALSE)
  )),
  parallel = TRUE,
  quiet = FALSE
)

qs::qsave(s1100_dists, "s1100_dists_jic.qs")
## s1100_dists <- qs::qread("s1100_dists_jic.qs")

d$s1100_dist <- unlist(s1100_dists)

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

# merge in aod
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

# impute missing aod, nei_nonroad, nei_onroad, nei_nonpoint, and nei_event values with missRanger package
# https://doi.org/10.1093/bioinformatics/btr597 (MissForest algorithm used to impute mixed-type datasets by chaining random forests)
# missRanger iterates multiple times over all variables until the average OOB prediction error of the models stops improving
# imputations done during the process are combined with a predictive mean matching (PMM) step, leading to more natural imputations and improved distributional properties of the resulting values

d_imp <-
  missRanger::missRanger(
    d,
    seed = 224,
    verbose = 2,
    . ~ . - date,
    pmm.k = 3,
    num.trees = 10
  )

fst::write_fst(d_imp, "h3data_train_imputed.fst", compress = 100)
system("aws s3 cp h3data_train_imputed.fst s3://geomarker/st_pm_hex/h3data_train_imputed.fst")
