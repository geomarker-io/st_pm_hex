library(tidyverse)
library(sf)
library(stars)
library(ncmeta)

## import h3 coords
d_hex <- readRDS('us_h3_8_compact_hex_ids.rds') %>%
  map(h3::h3_to_children, res = 8) %>%
  unlist()

d <-
  bind_cols(tibble(h3 = d_hex),
            as_tibble(h3::h3_to_geo(d_hex))) %>%
  st_as_sf(coords = c('lng', 'lat'), crs = 4326)



## d_sp <- d %>%
##   sf::st_transform("+proj=lcc +x_0=5632642.22547 +y_0=4612545.65137 +lat_0=50 +lon_0=-107 +lat_1=50 +ellps=GRS80") %>%
##   sf::st_Spatial()

## download NARR raster

data.name <- 'hpbl'
year <- '2000'
ff <- paste0('ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/monolevel/',
             data.name,
             '.',
             year,'.nc')
ff.short <- basename(ff)

download.file(ff, destfile = ff.short)

## read in narr raster in 5072

d_narr <- raster::brick(ff.short)



## overlay to hex points


## could we use coarser hexagons?  reduce the resolution of the grid and then overlay since NARR grids are 12 x 12 km

d_test <- d %>%
  .[1:10, ] %>%
  st_transform(raster::crs(d_narr)@projargs)


str(d_narr)
d_narr@z$`Date/time`

d_over <- raster::extract(d_narr, d)


## library(spnc) # BigelowLab/spnc
## bug in spnc_flavor prevents using a newer version; use:
## remotes::install_github('BigelowLab/spnc@8f4a947490102586da13e60b4963230a3f53538b')

data.name <- 'hpbl'
year <- '2000'

ff <- paste0('ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/monolevel/',
             data.name,
             '.',
             year,'.nc')
ff.short <- basename(ff)

download.file(ff, destfile = ff.short)

r_out <- brick(ff.short)






str(r_out, 2)

r_out@z[['Date/time']]

str(r_out)

r_points <-
  r_out %>%
  rasterToPoints() %>%
  as_tibble() %>%
  sf::st_as_sf(coords = c('x', 'y'), crs = r_out@crs@projargs) %>%
  sf::st_transform(4326)

r_out@title

r_points %>%
  pivot_longer(cols = -geometry, names_to = 'date', values_to = r_out@title)

r_out[[1]] %>%
  rasterToPoints() %>%
  as_tibble() %>%
  sf::st_as_sf(coords = c('x', 'y'), crs = r_out@crs@projargs)

rasterToPoints(r_out) %>%
  head()



r_out@z

r_out@crs@projargs

d <- sf::st_as_sf(d, coords = c('lon', 'lat'), crs = 4326)


tmp.raster <- d.tmp$R %>%
  readAll %>%
  projectRaster(crs=CRS('+init=epsg:5072'))
tmp.points <- rasterToPoints(tmp.raster) %>%
  as_tibble() %>%
  gather(id, value, -x, -y) %>%
  mutate(year = substr(id, 2, 5),
         month = substr(id, 7, 8),
         day = substr(id, 10, 11)) %>%
  mutate_at(vars(year, month, day), as.numeric) %>%
  mutate(variable = data.name)



get_NARR <- function(data.name = 'hpbl', year = '2000') {
  ff <- paste0('ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/monolevel/',
               data.name,'.',
               year,'.nc')
  ff.short <- basename(ff)
  download.file(ff, destfile=ff.short)
  on.exit(unlink(ff.short))
  d.tmp <- SPNC(ff.short)
  tmp.raster <- d.tmp$R %>%
    readAll %>%
    projectRaster(crs=CRS('+init=epsg:5072'))
  tmp.points <- rasterToPoints(tmp.raster) %>%
    as_tibble() %>%
    gather(id, value, -x, -y) %>%
    mutate(year = substr(id, 2, 5),
           month = substr(id, 7, 8),
           day = substr(id, 10, 11)) %>%
    mutate_at(vars(year, month, day), as.numeric) %>%
    mutate(variable = data.name)
  return(tmp.points)
}


narr_all <- list()

for (dn in c('hpbl', 'vis', 'rhum.2m', 'prate', 'air.2m',
              'pres.sfc', 'uwnd.10m', 'vwnd.10m')) {
                message('#### \n\n\n now processing ', dn, '\n\n\n####')
                narr_all[dn] <-
                  map(as.character(2000:2018),
                      ~ get_NARR(data.name = dn, year = .x))
                saveRDS(narr_all, 'narr_all_JIC.rds')
                }

saveRDS(narr_all, 'narr_data.rds')
