library(tidyr)
library(dplyr)
library(data.table)
library(grf)

safe_harbor_h3 <- readRDS("us_h3_3_population_20k_minimum_hex_ids.rds")

# get all length 3+
big_safe_harbor_h3 <-
  strsplit(safe_harbor_h3, "-") %>%
  purrr::map_dbl(length) %>%
  purrr::map_lgl(~ . > 3) %>%
  which() %>%
  safe_harbor_h3[.]

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

safe_s3_check <- function(h3 = big_safe_harbor_h3[5]) {
  s3_uris <- paste0("s3://pm25-brokamp/", h3, "_2020_h3pm.fst")
  safe_check <- purrr::possibly(s3:::s3_check_for_file_s3, otherwise = FALSE)
  safe_check(s3_uris)
}

predict_all_for_a_geohash <- function(the_geohash = big_safe_harbor_h3[15], super_small = FALSE) {
  in_name <- glue::glue("h3_data/{the_geohash}_h3data.qs")

  if (safe_s3_check(the_geohash)) {
    message(glue::glue("seems like all years for {the_geohash} are already available"))
    return(invisible(NULL))
  }

  if (!fs::file_exists(in_name)) {
    fs::dir_create("h3_data")
    system(glue::glue("aws s3 cp s3://geomarker/st_pm_hex/{in_name} ./h3_data/"))
  }

  d <- qs::qread(in_name, nthreads = 8)

  if (super_small) {
    d$week <- format(d$date, "%w")
    d <- d %>%
      group_by(year, week) %>%
      nest()
  } else {
    d$month <- format(d$date, "%m")
    d <- d %>%
      group_by(year, month) %>%
      nest()
  }

  fs::dir_create("h3_pm")
  fst::threads_fst(nr_of_threads = 8)

  predict_all_for_a_year <- function(yr = "2015") {
    message("", yr)

    out_name <- glue::glue("h3_pm/{the_geohash}_{yr}_h3pm.fst")
    if (fs::file_exists(out_name)) {
      message(out_name, " already exists")
      return(invisible(NULL))
    }

    grf <- qs::qread(glue::glue("grf/st_pm_hex_grf_{yr}.qs"), nthreads = 8)

    d_pred <-
      filter(d, year == yr) %>%
      pull(data)

    d_pred_out <-
      purrr::map(d_pred, ~ {
        predict(
          grf,
          select(.x, all_of(pred_names)),
          estimate.variance = TRUE,
          num.threads = parallel::detectCores()
        )
      })

    d_pred_out <-
      d_pred_out %>%
      bind_rows() %>%
      as_tibble() %>%
      transmute(
        pm_pred = predictions,
        pm_se = sqrt(variance.estimates)
      ) %>%
      mutate_at(vars(starts_with("pm_")), signif, digits = 8) %>%
      mutate(
        h3 = do.call(c, purrr::transpose(d_pred)$h3),
        date = do.call(c, purrr::transpose(d_pred)$date)
      )

    fst::write_fst(d_pred_out, out_name, compress = 100)
    system(glue::glue("aws s3 cp {out_name} s3://pm25-brokamp/{the_geohash}_{yr}_h3pm.fst --acl public-read"))
  }

  purrr::walk(as.character(2000:2020), predict_all_for_a_year)
}

purrr::map_lgl(big_safe_harbor_h3, safe_s3_check)

purrr::walk(big_safe_harbor_h3, predict_all_for_a_geohash)

# run preds for those that are very large
super_big_safe_harbor_h3 <- big_safe_harbor_h3[c(5, 12, 14, 15)]
purrr::walk(super_big_safe_harbor_h3, predict_all_for_a_geohash, super_small = TRUE)

# run preds for any that don't exist online
purrr::walk(safe_harbor_h3[!purrr::map_lgl(safe_harbor_h3, safe_s3_check)][-1], predict_all_for_a_geohash)
purrr::walk(rev(safe_harbor_h3[!purrr::map_lgl(safe_harbor_h3, safe_s3_check)][-1]), predict_all_for_a_geohash)

purrr::walk(safe_harbor_h3[!purrr::map_lgl(safe_harbor_h3, safe_s3_check)][6:10], predict_all_for_a_geohash)

