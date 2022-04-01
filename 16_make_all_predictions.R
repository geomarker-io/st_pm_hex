library(tidyr)
library(dplyr)
library(data.table)
library(grf)

safe_harbor_h3 <- readRDS("us_h3_3_population_20k_minimum_hex_ids.rds")

## cincinnati_h3_3s <-
##   h3::geo_to_h3(c(39.162, -84.45689), res = 3) %>%
##   h3::k_ring(radius = 5)

## detroit_h3_3s <-
##   h3::geo_to_h3(c(42.331429, -83.045753), res = 3) %>%
##   h3::k_ring(radius = 1)

pred_names <- c(
  "nearby_pm",
  "x",
  "doy",
  "air.2m",
  "nei_event",
  "hpbl",
  "y",
  "vwnd.10m"
)

safe_s3_check <- function(h3 = cincinnati_h3_3s[85]) {
  ## s3_uris <- paste0("s3://pm25-brokamp/", h3, "_", as.character(2000:2020), "_h3pm.fst")
  s3_uris <- paste0("s3://pm25-brokamp/", h3, "_2020_h3pm.fst")
  safe_check <- purrr::possibly(s3:::s3_check_for_file_s3, otherwise = FALSE)
  ## purrr::map_lgl(s3_uris, safe_check)
  safe_check(s3_uris)
}

create_predictions <-
  function(the_geohash = safe_harbor_h3[100], force = FALSE) {

    in_name <- glue::glue("h3_data/{the_geohash}_h3data.qs")

    if (safe_s3_check(the_geohash)){
      message(glue::glue("seems like all years for {the_geohash} are already available"))
      return(invisible(NULL))
    }

    if (!fs::file_exists(in_name)) {
      fs::dir_create("h3_data")
      system(glue::glue("aws s3 cp s3://geomarker/st_pm_hex/{in_name} ./h3_data/"))
    }

    d <- qs::qread(in_name, nthreads = 4)

    d <- d %>%
      group_by(year) %>%
      nest()

    fs::dir_create("h3_pm")
    fst::threads_fst(nr_of_threads = 8)

    read_in_forest_and_predict <- function(yr) {
      message(yr)

      out_name <- glue::glue("h3_pm/{the_geohash}_{yr}_h3pm.fst")
      if (fs::file_exists(out_name)) {
        message(out_name, " already exists")
        return(invisible(NULL))
      }

      tictoc::tic()
      grf <- qs::qread(glue::glue("grf/st_pm_hex_grf_{yr}.qs"), nthreads = 8)
      d_pred <- filter(d, year == yr) %>%
        pull(data) %>%
        .[[1]]
      grf_preds <- predict(grf,
        select(d_pred, all_of(pred_names)),
        estimate.variance = TRUE,
        num.threads = parallel::detectCores()
      )
      pm_pred <- grf_preds$predictions
      pm_se <- sqrt(grf_preds$variance.estimates)
      d_out <- tibble(pm_pred = pm_pred, pm_se = pm_se)
      d_out <- mutate_at(d_out, vars(starts_with("pm_")), signif, digits = 8)
      d_out$h3 <- d_pred$h3
      d_out$date <- d_pred$date
      tictoc::toc()
      fst::write_fst(d_out, out_name, compress = 100)
      system(glue::glue("aws s3 cp {out_name} s3://pm25-brokamp/{the_geohash}_{yr}_h3pm.fst"))
    }

    purrr::walk(2000:2020, read_in_forest_and_predict)
  }

purrr::walk(safe_harbor_h3, create_predictions)

# get all with hyphen in them
strsplit(safe_harbor_h3, "-") %>%
  purrr::map_dbl(length) %>%
  purrr::map_lgl(~ !{. == 1}) %>%
  which() %>%
  safe_harbor_h3[.]

# do all length 2
strsplit(safe_harbor_h3, "-") %>%
  purrr::map_dbl(length) %>%
  purrr::map_lgl(~ . == 2) %>%
  which() %>%
  safe_harbor_h3[.] %>%
  purrr::walk(create_predictions)

# do all length 3
strsplit(safe_harbor_h3, "-") %>%
  purrr::map_dbl(length) %>%
  purrr::map_lgl(~ . == 3) %>%
  which() %>%
  safe_harbor_h3[.] %>%
  purrr::walk(create_predictions)

create_predictions("83281afffffffff")
