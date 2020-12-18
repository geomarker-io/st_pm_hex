library(data.table)
library(tidyr)
library(dplyr)
library(purrr)
library(sf)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

# TODO create a table of the n (%) of cells in each resolution
# "88": 11,932,970 (100%)
# "881": 151,263 (1%)
# "882": 8,924,517 (75%)
# "884": 2,857,190 (24%)
# etc, etc...

# create sf polygons for each res 5 h3 geohash polygon
d_hex <-
    readRDS("us_h3_8_compact_hex_ids.rds") %>%
    map(h3::h3_to_children, res = 5) %>%
    unlist()

d <-
    bind_cols(
        tibble(h3_5 = d_hex),
        as_tibble(h3::h3_to_geo_boundary_sf(d_hex))
    ) %>%
    st_as_sf() %>%
    st_transform(5072)

# get population for all tracts in contiguous US
states <- tigris::states(cb = TRUE) %>%
    select(NAME) %>%
    sf::st_drop_geometry() %>%
    filter(!NAME %in% c(
        "Guam",
        "Commonwealth of the Northern Mariana Islands", "Hawaii", "Alaska",
        "United States Virgin Islands", "American Samoa", "Puerto Rico"
    ))

tract_pop <-
    tidycensus::get_acs(
        geography = "tract",
        state = states$NAME,
        variables = "B01001_001",
        year = 2018,
        geometry = TRUE
    ) %>%
    select(
        tract_fips = GEOID,
        population = estimate
    ) %>%
    st_transform(5072)

# calculate area-weighted population for each cell

get_aw_pop <- function(grid_cell) {
    tracts_subset <- filter(
        tract_pop,
        st_intersects(tract_pop, grid_cell, sparse = FALSE)
    )
    population <- st_interpolate_aw(
        tracts_subset[, "population"],
        grid_cell,
        extensive = TRUE
    )
    return(population$population)
}

population <- mappp::mappp(
    1:nrow(d),
    ~ get_aw_pop(grid_cell = d[.x, ]),
    parallel = TRUE
)

d$population <- unlist(population)

# map populations

library(tmap)
tmap_mode('view')

tm <- tm_shape(us_geohash_3 %>%
               mutate(population = round(population)),
               projection = 4326) +
  tm_polygons(col = 'population',
              palette = 'viridis',
              alpha = 0.7,
              id = 'geohash',
              style = 'fixed',
              breaks = c(0, 20000, 100000,
                         500000, 1000000, 5000000,
                         12000000)) +
  tm_scale_bar(position = c("center", "bottom"))

tmap_save(tm, 'us_h3_5_population_map.png')

## combine cells until all have population of >= 20,000

d %>%
  st_drop_geometry() %>%
  group_by(population <= 20000) %>%
  tally() %>%
  mutate(`%` = scales::percent(n / sum(n), accuracy = 0.1)) %>%
  knitr::kable()

summary(filter(d, population <= 20000)$population)

## function finds the lowest population geohash; merges it with "touching" geohash with lowest population
## starts with dataset of geohash populations and returns new dataset with merged geohashes

merge_lowest_pop_geohash <- function(d_in = d) {
  lowest_pop_sf <- d_in[which.min(d_in$population), ]
  if (lowest_pop_sf$population > 20000) stop("no geohashes with population <= 20,000 found.", call. = FALSE)
  d_new <- filter(d_in, ! geohash == lowest_pop_sf$geohash)
  merge_candidates <- d_new[st_intersects(lowest_pop_sf, d_new)[[1]], ]
  merge_sf <- merge_candidates[which.min(merge_candidates$population), ]
  merged_sf <-
    st_union(lowest_pop_sf, merge_sf) %>%
    transmute(geohash = paste(geohash, geohash.1, sep = "-"),
              population = sum(population, population.1))
  d_new <- filter(d_new, ! geohash == merge_sf$geohash)
  d_out <- rbind(d_new, merged_sf)
  return(d_out)
}

# merge_lowest_pop_geohash()

while (min(d$population) < 20000) {
  message(min(d$population))
  d <- merge_lowest_pop_geohash(d)
}

saveRDS(d, "us_h3_5_population_20k_minimum.rds")

saveRDS(d$h3_5, "us_h3_5_population_20k_minimum_hex_ids.rds")

library(tmap)
tmap_mode('view')

tm <- tm_shape(d %>%
               mutate(population = round(population)),
               projection = 4326) +
  tm_polygons(col = 'population',
              palette = 'viridis',
              alpha = 0.7,
              id = 'geohash',
              style = 'fixed',
              breaks = c(0, 20000, 100000,
                         500000, 1000000, 5000000,
                         12000000)) +
  tm_scale_bar(position = c("center", "bottom"))

tmap_save(tm, 'us_h3_5_merged_20k_population_map.png')
