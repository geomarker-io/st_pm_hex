library(addNlcdData)
library(sf)
library(dplyr)
library(tidyr)
library(purrr)

# make unique listing of geometries
d_aqs <- qs::qread("./h3data_aqs.qs")
d_geom <- h3::h3_to_geo_boundary_sf(unique(d_aqs$h3))
d_geom$h3 <- unique(d_aqs$h3)

# calculate nlcd data for all unique geometries
d_geom_nlcd_data <- get_nlcd_data_polygons(d_geom)

# get desired nlcd year for each date
year_nlcd_year <-
  tribble(
    ~nlcd_year, ~year,
    "2001", as.character(2000:2003),
    "2006", as.character(2004:2008),
    "2011", as.character(2009:2013),
    "2016", as.character(2014:2020)
  ) %>%
  unnest(cols = c(year))

d_aqs <- d_aqs %>%
  mutate(year = as.character(lubridate::year(date))) %>%
  left_join(year_nlcd_year, by = "year")

# join nlcd-geometry-year to data
# but only for unique h3-year combinations
d <- d_aqs %>%
  select(h3, year, nlcd_year) %>%
  unique() %>%
  left_join(d_geom_nlcd_data, by = c("h3" = "h3", "nlcd_year" = "year"))

d <- d %>% select(-nlcd_year)

# save
qs::qsave(d, "h3data_nlcd.qs")
system("aws s3 cp h3data_nlcd.qs s3://geomarker/st_pm_hex/h3data_nlcd.qs")
