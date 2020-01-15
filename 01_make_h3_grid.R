## install h3 following instructions here: https://github.com/crazycapivara/h3-r

## macOS:
## brew install h3

## debian/ubuntu:
## git clone https://github.com/uber/h3.git h3c
## cd h3c
## git pull origin master --tags
## git checkout "v3.3.0"
## cmake -DENABLE_FORMAT=OFF -DBUILD_SHARED_LIBS=ON .
## sudo make install
## cd ..
## rm -rf h3c

library(tidyverse)
library(h3) # crazycapivara/h3-r
library(sf)
library(mapview)

us_boundary <-
  USAboundaries::us_boundaries() %>%
  filter(!state_name %in% c('Alaska', 'Hawaii', 'Puerto Rico')) %>%
  st_union() %>%
  st_as_sf() %>%
  st_transform(5072)

## generate a lot of h3 resolution 3 cells that will cover the US
us_h3_3 <-
  geo_to_h3(c(38.5, -98.5), 3) %>%
  k_ring(radius = 24)

## subset to only those needed to cover US
d <- us_h3_3 %>%
  h3_to_geo_boundary_sf() %>%
  mutate(h3_3 = us_h3_3) %>%
  st_transform(5072) %>%
  st_intersection(us_boundary)

## h3_to_geo_boundary_sf(d$h3) %>%
##   mapview()

## get h3 children for resolution 8
us_h3_8 <-
  map(d$h3_3, h3_to_children, res = 8) %>%
  unlist()

## save them using the compact ids since its cheaper to recreate the finer resolution than reading in the full list of h3_8 ids
saveRDS(h3::compact(us_h3_8), 'us_h3_8_compact_hex_ids.rds')

#### example map for cincy area ####

cincy_4 <- geo_to_h3(c(39.14, -84.51), 4)

map(8:5,
    ~ mapview(h3_to_geo_boundary_sf(h3_to_children(cincy_4, .)),
              layer.name = as.character(.))) %>%
  {Reduce(`+`, .)} %>%
  mapshot(url = 'h3_cincy_example_map.html')

