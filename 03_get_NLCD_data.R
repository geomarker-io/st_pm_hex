# download nlcd chunk only if it doesn't already exist
download_nlcd_chunk <- function(nlcd_chunk_number) {
  dir.create("./nlcd_fst/", showWarnings = FALSE)
  nlcd_file <- glue::glue("./nlcd_fst/nlcd_chunk_{nlcd_chunk_number}.fst")
  if (file.exists(nlcd_file)) {
    message(glue::glue("{nlcd_file} already exists"))
    invisible(return(NULL))
  }
  message(glue::glue("downloading s3://geomarker/nlcd/nlcd_chunk_{nlcd_chunk_number}.fst to {nlcd_file}"))
  download.file(
    url = glue::glue(
      "https://geomarker.s3.us-east-2.amazonaws.com/",
      "nlcd/nlcd_fst/", "nlcd_chunk_{nlcd_chunk_number}.fst"
    ),
    destfile = nlcd_file
  )
}

# get raw nlcd values for specific cell number, year, and product
get_nlcd_data <- function(nlcd_cell_number,
                          year = c(2001, 2006, 2011, 2016),
                          product = c("nlcd", "impervious", "imperviousdescriptor")) {
  if (length(nlcd_cell_number) > 1) {
    warning("nlcd_cell is longer than one; processing only the first value")
    nlcd_cell <- nlcd_cell_number[1]
  }
  nlcd_chunk <- nlcd_cell_number %/% 1e+07
  nlcd_row <- nlcd_cell_number %% 1e+07
  nlcd_file <- glue::glue("./nlcd_fst/nlcd_chunk_{nlcd_chunk}.fst")
  nlcd_columns <- unlist(purrr::map(year, ~ glue::glue("{product}_{.}")))
  if (!file.exists(nlcd_file)) download_nlcd_chunk(nlcd_chunk)
  out <- fst::read_fst(
    path = nlcd_file,
    from = nlcd_row,
    to = nlcd_row,
    columns = nlcd_columns
  )
  out <- tibble::as_tibble(out)
  out
}

# download all chunks needed for nlcd multiple cell numbers ahead of time
download_nlcd_chunks <- function(nlcd_cell_numbers) {
  nlcd_chunks_needed <- unique(nlcd_cell_numbers %/% 1e+07)
  message("downloading ", length(nlcd_chunks_needed), " total chunk files to ./nlcd_fst/")
  purrr::walk(nlcd_chunks_needed, download_nlcd_chunk)
}

# define empty nlcd raster
r_nlcd_empty <-
  raster::raster(
    nrows = 104424,
    ncols = 161190,
    xmn = -2493045,
    xmx = 2342655,
    ymn = 177285,
    ymx = 3310005,
    crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
    resolution = c(30, 30),
    vals = NULL
  )

# define legends for raster values (and our green codes)
nlcd_legend <-
  tibble::tribble(
    ~value, ~landcover_class, ~landcover, ~green,
    11, "water", "water", FALSE,
    12, "water", "ice/snow", FALSE,
    21, "developed", "developed open", TRUE,
    22, "developed", "developed low intensity", TRUE,
    23, "developed", "developed medium intensity", FALSE,
    24, "developed", "developed high intensity", FALSE,
    31, "barren", "rock/sand/clay", FALSE,
    41, "forest", "deciduous forest", TRUE,
    42, "forest", "evergreen forest", TRUE,
    43, "forest", "mixed forest", TRUE,
    51, "shrubland", "dwarf scrub", TRUE,
    52, "shrubland", "shrub/scrub", TRUE,
    71, "herbaceous", "grassland", TRUE,
    72, "herbaceous", "sedge", TRUE,
    73, "herbaceous", "lichens", TRUE,
    74, "herbaceous", "moss", TRUE,
    81, "cultivated", "pasture/hay", TRUE,
    82, "cultivated", "cultivated crops", TRUE,
    90, "wetlands", "woody wetlands", TRUE,
    95, "wetlands", "emergent herbaceous wetlands", TRUE
  )

imperviousness_legend <-
  tibble::tribble(
    ~value, ~road_type,
    0, "non-impervious",
    1, "primary_urban",
    2, "primary_nonurban",
    3, "secondary_urban",
    4, "secondary_nonurban",
    5, "tertiary_urban",
    6, "tertiary_nonurban",
    7, "thinned_urban",
    8, "thinned_nonurban",
    9, "nonroad_urban",
    10, "nonroad_nonurban",
    11, "energy_prod_urban",
    12, "energy_prod_nonurban",
  )

#### do for all training data
library(sf)
library(dplyr)
library(tidyr)
library(purrr)

d_aqs <- qs::qread("./h3data_aqs.qs")

# make unique listing of geometries
d_geom <- h3::h3_to_geo_boundary_sf(unique(d_aqs$h3))
d_geom$h3 <- unique(d_aqs$h3)

d_geom <- d_geom %>%
  sf::st_transform(crs = raster::crs(r_nlcd_empty))

#### function to get all data for a single polygon

# this assumes that the query polygon is already in the CRS of the raster!
get_nlcd_percentages <- function(query_h3) {

  query_poly <- d_geom[d_geom$h3 == query_h3, ]
  nlcd_cells <- raster::cellFromPolygon(r_nlcd_empty, as(query_poly, "Spatial"))[[1]]

  nlcd_data <-
    purrr::map_dfr(nlcd_cells, get_nlcd_data) %>%
    mutate(.row = 1:nrow(.))

  nlcd_data <-
    nlcd_data %>%
    pivot_longer(cols = -.row, names_to = c("product", "year"), names_sep = "_") %>%
    pivot_wider(names_from = product, values_from = value) %>%
    left_join(nlcd_legend, by = c("nlcd" = "value")) %>%
    select(-nlcd) %>%
    left_join(imperviousness_legend, by = c("imperviousdescriptor" = "value")) %>%
    select(-imperviousdescriptor)

  road_type_percentage <- function(road_type_vector, road_type) {
    fraction_roads <- sum(road_type_vector == road_type) / length(road_type_vector)
    round(fraction_roads * 100, 0)
  }

  nlcd_data %>%
    select(-.row) %>%
    group_by(year) %>%
    summarize(
      impervious = round(mean(impervious), 0),
      green = round(100 * sum(green) / length(green), 0),
      primary_urban = road_type_percentage(road_type, "primary_urban"),
      primary_rural = road_type_percentage(road_type, "primary_rural"),
      secondary_urban = road_type_percentage(road_type, "secondary_urban"),
      secondary_rural = road_type_percentage(road_type, "secondary_rural"),
      tertiary_urban = road_type_percentage(road_type, "tertiary_urban"),
      tertiary_rural = road_type_percentage(road_type, "tertiary_rural"),
      thinned_urban = road_type_percentage(road_type, "thinned_urban"),
      thinned_rural = road_type_percentage(road_type, "thinned_rural"),
      nonroad_urban = road_type_percentage(road_type, "nonroad_urban"),
      nonroad_rural = road_type_percentage(road_type, "nonroad_rural"),
      energyprod_urban = road_type_percentage(road_type, "energyprod_urban"),
      energyprod_rural = road_type_percentage(road_type, "energyprod_rural"),
      nonimpervious = road_type_percentage(road_type, "non-impervious")
    )
}

## test for the hex polygon at my house
## get_nlcd_percentages("882a930ad3fffff")

# calculate nlcd data for all unique geometries
d_geom_nlcd_data <- mappp::mappp(d_geom$h3, get_nlcd_percentages, parallel = TRUE)
names(d_geom_nlcd_data) <- d_geom$h3
d_geom_nlcd_data <- tibble::enframe(d_geom_nlcd_data, name = "h3", value = "nlcd_data")

# some of the nlcd data is missing for certain hexagons
d_geom_nlcd_data <-
  d_geom_nlcd_data %>%
  filter(!is.na(nlcd_data)) %>%
  unnest(cols = c(nlcd_data))

# get desired nlcd year for each date
year_nlcd_year <-
  tribble(
    ~year, ~nlcd_year,
    "2000", "2001",
    "2001", "2001",
    "2002", "2001",
    "2003", "2001",
    "2004", "2006",
    "2005", "2006",
    "2006", "2006",
    "2007", "2006",
    "2008", "2006",
    "2009", "2011",
    "2010", "2011",
    "2011", "2011",
    "2012", "2011",
    "2013", "2011",
    "2014", "2016",
    "2015", "2016",
    "2016", "2016",
    "2017", "2016",
    "2018", "2016",
    "2019", "2016",
    "2020", "2016"
  )

d_aqs <- d_aqs %>%
  mutate(year = as.character(lubridate::year(date))) %>%
  left_join(year_nlcd_year, by = "year")

# join nlcd-geometry-year to data
# but only for unique h3-year combinations
d <- d_aqs %>%
  select(h3, year, nlcd_year) %>%
  unique() %>%
  left_join(d_geom_nlcd_data, by = c("h3" = "h3", "nlcd_year" = "year"))

d <- d %>% select(-nlcd_year)

# save
qs::qsave(d, "h3data_nlcd.qs")
system("aws s3 cp h3data_nlcd.qs s3://geomarker/st_pm_hex/h3data_nlcd.qs")




## # import h3
## d_hex <- readRDS("us_h3_8_compact_hex_ids.rds") %>%
##   map(h3::h3_to_children, res = 8) %>%
##   unlist()

## # translate h3 to polygons
## d <- h3::h3_to_geo_boundary_sf(d_hex)
## d$h3 <- d_hex

## # reproject points into NLCD projection for overlay
## d <- d %>%
##   sf::st_transform(crs = raster::crs(r_nlcd_empty))

## # save this JIC
## qs::qsave(d, "d_hex_nlcd_crs.qs", nthreads = parallel::detectCores())
## d <- qs::qread("d_hex_nlcd_crs.qs", nthreads = parallel::detectCores())
