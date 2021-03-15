library(dplyr)
library(tidyr)
library(tibble)
library(sf)
library(data.table)


################ viz

d_pm <- qs::qread("h3data_aqs.qs")

h3 <-
  readRDS("us_h3_8_compact_hex_ids.rds") %>%
  purrr::map(h3::h3_to_children, res = 3) %>%
  unlist()

## h3_cells <-
##   tibble(
##     h3 = h3,
##     geometry = h3::h3_to_geo_boundary_sf(h3)$geometry
##   ) %>%
##   st_as_sf()

## aqs_point_map <-
##   d_pm %>%
##   group_by(h3) %>%
##   summarize(n = n()) %>%
##   mutate(geometry = h3::h3_to_geo_sf(h3)$geometry) %>%
##   sf::st_as_sf() %>%
##   mapview::mapview(cex = "n")

## {
##   mapview::mapview(h3_cells, legend = FALSE) + aqs_point_map
## } %>%
##   mapview::mapshot("h3_3_and_aqs_map.html")

## d_date <-
##   d_pm %>%
##   group_by(date) %>%
##   summarize(n = n()) %>%
##   left_join(tibble(date = seq.Date(as.Date("2000-01-01"), as.Date("2020-12-31"), 1)),
##     .,
##     by = "date"
##     ) %>%
##   tidyr::replace_na(list(n = 0))

## summary(d_date$n)
  
## calendR::calendR(
##   year = 2001,
##   special.days = pull(d_date, 'n')[367:731],
##   gradient = TRUE, # Needed to create the heat map
##   special.col = rgb(1, 0, 0, alpha = 0.6), # Higher color
##   low.col = "white"
## )

## ggplot2::ggsave("AQS 2001 calendar plot.pdf")

############## making nearby pm

d_pm_h3 <-
  d_pm %>%
  mutate(h3 = purrr::map_chr(h3, h3::h3_to_parent, res = 3)) %>%
  group_by(date, h3) %>%
  summarize(pm25 = mean(pm25, na.rm = TRUE)) %>%
  as.data.table(key = c("h3", "date"))

h3 <-
  readRDS("us_h3_8_compact_hex_ids.rds") %>%
  purrr::map(h3::h3_to_children, res = 3) %>%
  unlist()

nearby_h3 <-
  purrr::map(h3, h3::k_ring_distances, radius = 5) %>%
  magrittr::set_names(h3) %>%
  enframe(name = "h3", value = "nearby_pm") %>%
  unnest(cols = c(nearby_pm)) %>%
  rename(h3_nearby = h3_index)

nearby_h3$weight <- 1 / (nearby_h3$distance^2) # inverse distance squared weighting
nearby_h3[nearby_h3$weight == Inf, "weight"] <- 100 # make weight for center polygon 100
nearby_h3 <- as.data.table(nearby_h3, key = "h3")

d <-
  expand_grid(
    date = seq.Date(as.Date("2000-01-01"), as.Date("2020-12-31"), by = 1),
    h3 = h3
  ) %>%
  as.data.table(key = c("h3", "date"))

d <-
  merge.data.table(d,
    nearby_h3,
    all.x = TRUE,
    all.y = FALSE,
    by.x = "h3",
    by.y = "h3",
    allow.cartesian = TRUE
  )

d[d_pm_h3, pm25 := i.pm25, on = c("h3_nearby" = "h3", "date")]

d <-
  d %>%
  group_by(h3, date) %>%
  nest()

d_nearby_pm <- mappp::mappp(d$data, ~ weighted.mean(.$pm25, .$weight, na.rm = TRUE), parallel = TRUE, num.cores = 12)
d$nearby_pm <- unlist(d_nearby_pm)
d[is.nan(d$nearby_pm), "nearby_pm"] <- NA


# how many are missing?
d %>%
  group_by(h3) %>%
  summarize(n_missing = sum(is.na(nearby_pm))) %>%
  knitr::kable()

## make three day moving average
d <-
  d %>%
  select(-data) %>%
  group_by(h3) %>%
  rename(nearby_pm_today = nearby_pm) %>%
  mutate(nearby_pm_tmw = lead(nearby_pm_today, 1, order_by = date),
         nearby_pm_ytd = lag(nearby_pm_today, 1, order_by = date)) %>%
  group_by(h3, date) %>%
  mutate(nearby_pm = mean(c(nearby_pm_today, nearby_pm_tmw, nearby_pm_ytd), na.rm = TRUE))
                              
  
# how many are missing?
d %>%
  group_by(h3) %>%
  summarize(n_missing = sum(is.na(nearby_pm))) %>%
  knitr::kable()

# less than 1% of days are missing for most stations

saveRDS(select(d, h3, date, nearby_pm), "nearby_pm.rds")
system("aws s3 cp nearby_pm.rds s3://geomarker/st_pm_hex/nearby_pm.rds")

