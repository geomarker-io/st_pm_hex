library(data.table)
library(tidyr)
library(dplyr)
library(tibble)
library(magrittr)

# start with aqs data
d <-
  qs::qread("h3data_aqs.qs") %>%
  as_tibble()

## TODO add in "nearby" pm measurements?  how many days / much space would we average to have no missing data?? (check EST paper)

# add in year, doy, dow
d <- d %>%
  mutate(
    year = lubridate::year(date),
    doy = lubridate::yday(date),
    dow = lubridate::wday(date)
  )

# add in epsg 5072 coordinates
d_geom <-
  h3::h3_to_geo_sf(d$h3) %>%
  sf::st_transform(5072) %>%
  sf::st_coordinates()

d$x <- d_geom[ , "X"]
d$y <- d_geom[ , "Y"]
rm(d_geom)

d <- data.table(d, key = c("h3", "year", "date"))

# data.table merge in narr, nlcd, and aod data

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

# now read in aod (which could be missing)
d_aod <- fst::read_fst("h3data_aod.fst", as.data.table = TRUE)

d[d_aod, aod := i.aod, on = c("h3", "date")]

rm(d_aod)

# save it
fst::write_fst(d, "h3data_train.fst", compress = 100)
system("aws s3 cp h3data_train.fst s3://geomarker/st_pm_hex/h3data_train.fst")

# some summary stuff
tables()

d[!is.na(aod), .N]
d[!is.na(aod), .N] / nrow(d) * 100

