library(tidyverse)

## interactively pick which MODIS tiles:

## download.file('https://modis.ornl.gov/files/modis_sin.kmz',
##               destfile = 'modis_sin.kmz')
## unzip('modis_sin.kmz')
## unlink('modis_sin.kmz')

## sf::read_sf('./modis_sin.kml', 'Features') %>%
##  select(Name) %>%
##   mapview::mapview()
## unlink('modis_sin.kml')

tiles_to_get <- tribble(~ h, ~ v,
                        8, 4,
                        9, 4,
                        10, 4,
                        11, 4,
                        12, 4,
                        13, 4,
                        8, 5,
                        9, 5,
                        10, 5,
                        11, 5,
                        12, 5,
                        8, 6,
                        9, 6,
                        10, 6) %>%
  mutate(h = stringr::str_pad(h, 2, side = 'left', pad = '0'),
         v = stringr::str_pad(v, 2, side = 'left', pad = '0')) %>%
  transmute(tiles = paste0('h', h, 'v', v)) %>%
  pull(tiles)

get_tile_urls <- function(Date) {
  MCD_url <- paste0('https://e4ftl01.cr.usgs.gov/MOTA/MCD19A2.006/',
                    gsub('-', '.', Date),
                    '/')
  lns <- xml2::read_html(MCD_url) %>%
    rvest::html_nodes('a') %>%
    rvest::html_attr('href')
  lns <- lns[str_detect(lns, fixed('.hdf'))]
  lns <- lns[! str_detect(lns, fixed('.xml'))]
  lns <- map(tiles_to_get, ~ lns[str_detect(lns, fixed(.))]) %>%
    unlist()
  return(paste0(MCD_url, lns))
}

## get_tile_urls(as.Date('2006-08-10'))
## get_tile_urls(Date = as.Date('2000-02-28'))
## tile_url <- get_tile_urls(as.Date('2006-08-10'))[1]


## use AOD data that meet the following criteria:
## QA.CloudMask: Clear or Possibly_Cloudy (bits 0-2: 001 or 010)
## QA.AdjacencyMask: Clear or AdjacentToASingle Cloudy Pixel (bits 5-7: 000 or 011)

## converting valid integers to decimals
dec2bin <- function(list_of_numbers) {
  map_chr(list_of_numbers,
          ~ substr(paste(as.integer(rev(intToBits(.))), collapse = ""), 17, 32))
}

modis_aod_qa_lookup_table <-
  tibble(dec = 1:65535) %>%
  mutate(bin = dec2bin(dec)) %>%
  mutate(cloud_mask = substr(bin, 1, 3) %in% c('001', '010'),
         adjacency_mask = substr(bin, 6, 8) %in% c('000', '011'),
         keep = cloud_mask & adjacency_mask)

int_keeps <- filter(modis_aod_qa_lookup_table, keep) %>% pull(dec)

## create data.frame in format expected by raster::subs()
int_keeps_df <- tibble(id = int_keeps, r = int_keeps)

tile_to_raster <- function(tile_url) {
  fl_name <- basename(tile_url)
  on.exit({unlink(fl_name)
    unlink(paste0('aod_', fl_name, '.tif'))
    unlink(paste0('aod_', fl_name, '.tif.aux.xml'))
    unlink(paste0('qa_', fl_name, '.tif'))
    unlink(paste0('qa_', fl_name, '.tif.aux.xml'))
  })

  download.file(url = tile_url,
                destfile = fl_name,
                method = 'wget',
                quiet = TRUE,
                extra = c('--continue',
                          '--user=brokamrc',
                          '--password=8u3wtE}RYGp3/Ux8'))

  fl_name_sds <- gdalUtils::get_subdatasets(fl_name)

  ## aod qa
  r_qa <- fl_name_sds %>%
    str_subset(fixed('AOD_QA')) %>%
    gdalUtils::gdal_translate(
                 dst_dataset = paste0('qa_', fl_name, '.tif'),
                 of = 'GTiff',
                 ot = 'UInt16',
                 output_Raster = TRUE)

  ## create mask based on qa level
  r_mask <- raster::subs(r_qa, int_keeps_df, subsWithNA = TRUE)

  ## aod at 47 nm
  r_aod <- fl_name_sds %>%
    str_subset(fixed('Optical_Depth_047')) %>%
    gdalUtils::gdal_translate(
                 of = 'GTiff',
                 dst_dataset = paste0('aod_', fl_name, '.tif'),
                 output_Raster = TRUE
               )

  ## set all non-keeper values to NA and average over all layers
  r_mean_aod <- raster::mask(r_aod, r_mask) %>%
    raster::overlay(., unstack = TRUE, fun = function(.x) mean(.x, na.rm = TRUE))

  return(r_mean_aod)

}

## get_tile_urls(date = as.Date('2006-08-10'))[[1]]
## tile_to_raster(get_tile_urls(date = as.Date('2006-08-10'))[[2]])

date_to_composite_raster <- function(the_date = as.Date('2006-08-10')) {
  message('now processing: ', the_date)
  dir.create('./aod_clean_rasters', showWarnings = FALSE)
  d_tmp <- CB::mappp(get_tile_urls(Date = the_date), tile_to_raster, parallel = TRUE)
  do.call(raster::merge, d_tmp[! is.na(d_tmp)]) %>%
    raster::writeRaster(paste0('aod_clean_rasters/aod_clean_', the_date, '.tif'))
}

## date_to_composite_raster(as.Date('2006-08-10'))
## date_to_composite_raster(as.Date('2003-04-13'))
## date_to_composite_raster(as.Date('2000-03-01'))

## run all
## seq.Date(as.Date('2000-01-01'), to = as.Date('2019-12-31'), by = 1) %>%
##   walk(purrr::safely(date_to_composite_raster, quiet = FALSE))

## run for dates we don't have yet
dates_we_have <-
  list.files(path = 'aod_clean_rasters') %>%
  substr(11, 20) %>%
  as.Date()

dates_to_get <- seq.Date(as.Date('2000-01-01'), to = as.Date('2019-12-31'), by = 1)

{! dates_to_get %in% dates_we_have} %>%
  dates_to_get[.] %>%
  rev() %>%
  walk(purrr::possibly(date_to_composite_raster, otherwise = NULL, quiet = FALSE))

## 2003-04-13 error: cannot coerce raster to...
## 2003-04-10 error: missing value where TRUE/FALSE needed

## write out all rasters as one raster brick per year

## year_files <- list.files(path = 'aod_clean_rasters', pattern = '2000')

## raster::brick(year_files) %>%
##   writeRaster('aod_2000.tif')
