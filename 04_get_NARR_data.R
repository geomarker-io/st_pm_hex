library(sf)
library(dplyr)
library(tidyr)
library(purrr)
library(addNarrData)

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

## import training h3 as points
d_aqs <- qs::qread("./h3data_aqs.qs")
d <- h3::h3_to_geo_sf(d_aqs$h3)
d$date <- d_aqs$date
d$h3 <- d_aqs$h3
rm(d_aqs)

# get NARR cell number for each point
d <- d %>% sf::st_transform(crs = raster::crs(r_narr_empty))
d <- d %>% mutate(narr_cell = raster::cellFromXY(r_narr_empty, as(d, "Spatial")))
d <- st_drop_geometry(d)
d <- as_tibble(d)

# add NARR data
d$start_date <- d$date
d$end_date <- d$date
d_out <- get_narr_data(d)

d_out %>%
  select(-narr_cell, -start_date, -end_date) %>%
  qs::qsave("./h3data_narr.qs")

system("aws s3 cp h3data_narr.qs s3://geomarker/st_pm_hex/h3data_narr.qs")
