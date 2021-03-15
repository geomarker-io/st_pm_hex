library(dplyr)
library(tidyr)
library(tibble)
library(sf)

get_daily_PM <- function(year) {
  fl.name <- paste0("daily_88101_", year, ".zip")
  on.exit(unlink(c(fl.name, fl.name.csv)))
  download.file(paste0("https://aqs.epa.gov/aqsweb/airdata/", fl.name),
    destfile = fl.name
  )
  unzip(fl.name)
  fl.name.csv <- gsub(pattern = ".zip", ".csv", fl.name)
  d.tmp <- readr::read_csv(fl.name.csv)

  d.tmp %>%
    filter(`Sample Duration` == "24 HOUR") %>%
    transmute(
      id = paste(`State Code`, `County Code`, `Site Num`, sep = "-"),
      lat = Latitude,
      lon = Longitude,
      pm25 = `Arithmetic Mean`,
      date = `Date Local`
    )
}

d <- mappp::mappp(2000:2020, get_daily_PM)

d <- bind_rows(d)

d <- d %>%
  group_by(id, lat, lon, date) %>%
  summarize(pm25 = mean(pm25)) %>%
  ungroup()

d <- sf::st_as_sf(d, coords = c("lon", "lat"), crs = 4326)
d <- sf::st_as_sf(d)

# remove sites outside of contig US by intersecting in epsg 5072
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

us <-
  tigris::states() %>%
  filter(! NAME %in% c('United States Virgin Islands',
                       'Guam', 'Commonwealth of the Northern Mariana Islands',
                       'American Samoa', 'Puerto Rico',
                       'Alaska', 'Hawaii')) %>%
  st_union() %>%
  st_transform(5072)

d_intersection <- st_intersects(st_transform(d, 5072), us, sparse = FALSE)

d <- filter(d, d_intersection)

# geohash
d$h3 <- h3::geo_to_h3(d, res = 8)

# save
d %>%
  sf::st_drop_geometry() %>%
  select(date, pm25, h3) %>%
  qs::qsave("h3data_aqs.qs")

system("aws s3 cp h3data_aqs.qs s3://geomarker/st_pm_hex/h3data_aqs.qs")
