library(magrittr)
library(dplyr)
library(tidyr)

safe_harbor_h3 <- readRDS("us_h3_3_population_20k_minimum_hex_ids.rds")

# use 2020 to check if all years are avail
safe_s3_check <- function(h3 = cincinnati_h3_3s[85]) {
  ## s3_uris <- paste0("s3://pm25-brokamp/", h3, "_", as.character(2000:2020), "_h3pm.fst")
  s3_uris <- paste0("s3://pm25-brokamp/", h3, "_2020_h3pm.fst")
  safe_check <- purrr::possibly(s3:::s3_check_for_file_s3, otherwise = FALSE)
  ## purrr::map_lgl(s3_uris, safe_check)
  safe_check(s3_uris)
}

d_avail <-
  tibble(safe_harbor_h3 = safe_harbor_h3) %>%
  mutate(avail = purrr::map_lgl(safe_harbor_h3, safe_s3_check))

d_avail <-
  d_avail %>%
  mutate(safe_harbor_h3_split = purrr::map(safe_harbor_h3, strsplit, split = "-", fixed = TRUE)) %>%
  unnest(cols = c(safe_harbor_h3_split)) %>%
  unnest(cols = c(safe_harbor_h3_split))
  
d_avail %>%
  filter(avail) %>%
  pull(safe_harbor_h3_split) %>%
  h3::h3_to_geo_boundary_sf() %>%
  mapview::mapview()

safe_harbor_h3 %>%
  h3::h3_to_geo_boundary_sf() %>%
  mapview::mapview()
