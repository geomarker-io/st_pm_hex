library(sf)
library(tibble)
library(dplyr)
library(purrr)

aod_dates <-
  fs::dir_ls("aod", glob = "*.tif") %>%
  fs::path_rel(start = "aod") %>%
  fs::path_ext_remove() %>%
  stringr::str_remove(stringr::fixed("aod_clean_")) %>%
  as.Date()

convert_aod_to_geohash <- function(date) {
  fl <- glue::glue("./aod/aod_clean_{date}.tif")
  r_tmp <- raster::raster(fl)
  names(r_tmp) <- "aod"
  # auto removes missing value cells when converting to points
  r_sf <-
    raster::rasterToPoints(r_tmp, spatial = TRUE) %>%
    st_as_sf() %>%
    st_transform(4326)
  d_out <- tibble(
    date = date,
    h3 = h3::geo_to_h3(r_sf),
    aod = r_sf$aod
  )
  dir.create("./aod_fst", showWarnings = FALSE)
  fst::write_fst(d_out, glue::glue("./aod_fst/aod_{date}.fst"), compress = 0)
  return(invisible(NULL))
}

## convert_aod_to_geohash(date = as.Date("2017-05-03"))

# this takes about 1.5 hours on bisquick
mappp::mappp(aod_dates, convert_aod_to_geohash, parallel = TRUE)

## read all fst files in and create one big aod_geohash.fst file
# some of these date files will be non-existant if they had no non-missing values,
# so we use mappp for silent error handling

d <- mappp::mappp(aod_dates, ~ fst::read_fst(paste0("./aod_fst/aod_", ., ".fst")))

d_out <- bind_rows(d[!is.na(d)])

d_out <- data.table::data.table(d_out, key = c("h3", "date"))

fst::write_fst(d_out, "h3data_aod.fst", compress = 100)

system("aws s3 cp h3data_aod.fst s3://geomarker/st_pm_hex/h3data_aod.fst")
