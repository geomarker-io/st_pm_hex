library(tidyverse)
library(sf)
library(ncdf4)
library(velox)

## import h3
d_hex <- readRDS('us_h3_8_compact_hex_ids.rds') %>%
  map(h3::h3_to_children, res = 8) %>%
  unlist()

## translate h3 to centroid points
d <-
  bind_cols(tibble(h3 = d_hex),
            as_tibble(h3::h3_to_geo(d_hex))) %>%
  st_as_sf(coords = c('lng', 'lat'), crs = 4326)

## transform points into CRS of raster (but only do this once since it takes awhile)
d_for_extract <- d %>%
  sf::st_transform("+proj=lcc +x_0=5632642.22547 +y_0=4612545.65137 +lat_0=50 +lon_0=-107 +lat_1=50 +ellps=GRS80")

## saveRDS(d_for_extract, 'd_hex_sf_points_in_NARR_projection.rds')

get_NARR <- function(data.name = 'hpbl', year = '2000') {

  ## download nc file
  ff <- paste0('ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/monolevel/',
               data.name,'.',
               year,'.nc')
  ff.short <- basename(ff)
  download.file(ff, destfile = ff.short)
  on.exit(unlink(ff.short))

  ## read in narr raster as velox object (this will read raster into RAM)
  d_narr <-
    raster::brick(ff.short) %>%
    velox::velox()

  ## overlay to hex points
  d_narr_matrix <- d_narr$extract_points(sp = d_for_extract)

  ## bind this to the original points and put in long format
  d_narr_tbl <-
    d_narr_matrix %>%
    as_tibble(.name_repair = 'minimal') %>%
    set_names(seq_len(ncol(d_narr_matrix)))

  d_narr_tbl_long <-
    bind_cols(d, d_narr_tbl) %>%
    st_drop_geometry() %>%
    pivot_longer(cols = -h3, names_to = 'day', values_to = data.name) %>%
    mutate(year = year)

  return(d_narr_tbl_long)

}

get_NARR()

for (dn in c('hpbl', 'vis', 'rhum.2m', 'prate', 'air.2m',
              'pres.sfc', 'uwnd.10m', 'vwnd.10m')) {
                message('#### \n\n\n now processing ', dn, '\n\n\n####')
                map_dfr(as.character(2000:2019), ~ get_NARR(data.name = dn, year = .)) %>%
                  saveRDS(paste0('data_narr_', dn, '_h3.rds'))
                }
