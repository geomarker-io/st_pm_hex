library(tidyverse)

get_daily_PM <- function(year) {
  fl.name <- paste0("daily_88101_", year, ".zip")
  on.exit(unlink(c(fl.name, fl.name.csv)))
  download.file(paste0("https://aqs.epa.gov/aqsweb/airdata/", fl.name),
    destfile = fl.name
  )
  unzip(fl.name)
  fl.name.csv <- gsub(pattern = ".zip", ".csv", fl.name)
  d.tmp <- read_csv(fl.name.csv)
  d.tmp %>%
    transmute(
      id = paste(`State Code`, `County Code`, `Site Num`, sep = "-"),
      lat = Latitude,
      lon = Longitude,
      pm25 = `Arithmetic Mean`,
      date = `Date Local`
    )
}

d <- CB::mappp(2000:2019, get_daily_PM)

d <- bind_rows(d)

d <- d %>%
  group_by(id, lat, lon, date) %>%
  summarize(pm25 = mean(pm25))

d <- sf::st_as_sf(d, coords = c("lon", "lat"), crs = 4326)

d$h3 <- h3::geo_to_h3(d, res = 8)

saveRDS(d, "data_aqs_pm25.rds")
