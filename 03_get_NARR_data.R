library(tidyverse)
library(sf)
library(ncdf4)

## import h3
d_hex <- readRDS('us_h3_8_compact_hex_ids.rds') %>%
  map(h3::h3_to_children, res = 8) %>%
  unlist()

## translate h3 to centroid points
d <-
  bind_cols(tibble(h3 = d_hex),
            as_tibble(h3::h3_to_geo(d_hex))) %>%
  st_as_sf(coords = c('lng', 'lat'), crs = 4326)

## transform points into CRS of raster
d_for_extract <- d %>%
  sf::st_transform("+proj=lcc +x_0=5632642.22547 +y_0=4612545.65137 +lat_0=50 +lon_0=-107 +lat_1=50 +ellps=GRS80")

#### get NARR raster cell numbers for each h3 centroid point

# download prototypical NARR file
download.file('ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/monolevel/hpbl.2000.nc', destfile = 'hpbl.2000.nc')

d_narr <- raster::brick('hpbl.2000.nc')

get_raster_cell <- function(.rows) {
  single_point <- d_for_extract[.rows, ] %>% as('Spatial')
  raster::cellFromXY(d_narr, single_point)
}

narr_cells <- CB::mappp(seq_len(nrow(d_for_extract)), get_raster_cell, parallel = TRUE)
d$narr_cell <- unlist(narr_cells)
d <- st_drop_geometry(d)

# nest so we have less values duplicated across hex values in the same NARR grid
d <- d %>% nest(h3 = c(h3))

## saveRDS(d, 'd_hex_with_NARR_cell_numbers.rds')
## d <- readRDS('d_hex_with_NARR_cell_numbers.rds')

#### extract NARR raster cell values

get_NARR_values <- function(data.name, year) {

  ## download nc file
  ff <- paste0('ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/monolevel/',
               data.name,'.',
               year,'.nc')
  ff.short <- basename(ff)
  download.file(ff, destfile = ff.short)
  on.exit(unlink(ff.short))

  d_narr <- raster::brick(ff.short)

  d_narr_tbl <-
    raster::extract(d_narr, d$narr_cell) %>%
    as_tibble(.name_repair = 'minimal')

  d_narr_tbl_long <-
    bind_cols(d, d_narr_tbl) %>%
    select(-narr_cell) %>%
    pivot_longer(cols = -h3, names_to = 'date', values_to = data.name) %>%
    mutate(date = as.Date(date, format = 'X%Y.%m.%d.'))

  return(d_narr_tbl_long)

}

get_NARR_values('hpbl', '2000')

for (dn in c('hpbl', 'vis', 'rhum.2m', 'prate', 'air.2m',
              'pres.sfc', 'uwnd.10m', 'vwnd.10m')) {
                message('#### \n\n\n now processing ', dn, '\n\n\n####')
                map_dfr(as.character(2000:2019), ~ get_NARR_values(data.name = dn, year = .)) %>%
                  saveRDS(paste0('data_narr_', dn, '_h3.rds'))
                }
