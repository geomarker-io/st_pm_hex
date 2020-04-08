library(data.table)
library(tidyr)
library(dplyr)
library(tibble)
library(magrittr)
library(glue)

county_fips_bbox <-
  tigris::counties() %>%
  sf::st_as_sf() %>%
  filter(!STATEFP %in% c("60", "66", "69", "72", "78")) %>%
  sf::st_transform(4326) %>%
  sf::st_bbox()

get_finn_data <- function(yr = 2017) {
  finn_url <- glue("http://bai.acom.ucar.edu/Data/fire/data/FINNv1.5_{yr}.GEOSCHEM.tar.gz")
  tmp_file <- tempfile()
  download.file(finn_url, tmp_file)
  untar(tmp_file, exdir = basename(finn_url))
  on.exit(unlink(basename(finn_url), recursive = TRUE, force = TRUE))
  txt_file <- list.files(basename(finn_url), pattern = "*.txt", full.names = TRUE)

  out <- readr::read_csv(txt_file)
  out <- out %>%
    transmute(
      day = DAY,
      lat = LATI,
      lon = LONGI,
      area = AREA,
      fire_pm25 = PM25
    )
  out$date <- as.Date(paste0(yr, "-", out$day), "%Y-%j")
  out$day <- NULL

  out <- out %>% sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
  out$h3 <- h3::geo_to_h3(out, res = 8)

  out <- sf::st_crop(out, county_fips_bbox)
  out <- sf::st_drop_geometry(out)
  fst::write_fst(out, glue("finn_{yr}.fst"))
}

# run all
purrr::walk(2002:2018, get_finn_data)

# make into one big file and save
d <- purrr::map_dfr(glue("finn_{2002:2018}.fst"), fst::read_fst)
fst::write_fst(d, "h3data_finn.fst", compress = 100)
system("aws s3 cp h3data_finn.fst s3://geomarker/st_pm_hex/h3data_finn.fst")
