library(data.table)
library(tidyr)
library(dplyr)
library(purrr)
library(sf)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

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

# calculate area-weighted population density for each grid polygon

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

library(lwgeom)

# add in population density using area of geohash polygons
d <- d %>%
    mutate(
        area = st_area(.),
        population_density = population / area
    )

# save for future use
saveRDS(d, "us_h3_5_population.rds")
system("aws s3 cp us_h3_5_population.rds s3://geomarker/st_pm_hex/us_h3_5_population.rds")