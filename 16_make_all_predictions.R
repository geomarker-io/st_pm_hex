library(tidyr)
library(dplyr)
library(data.table)
library(grf)

safe_harbor_h3 <- readRDS("us_h3_4_population_20k_minimum_hex_ids.rds")

cincinnati_h3_6s <- c(
  "832a93fffffffff",
  "832a90fffffffff",
  "83266dfffffffff",
  "832a9efffffffff"
)

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

create_predictions <-
  function(the_geohash = cincinnati_h3_6s[1], force = FALSE) {

    out_name <- glue::glue("h3_pm/{the_geohash}_h3pm.qs")

    if (fs::file_exists(out_name) & !force) {
      message(out_name, " already exists")
      return(invisible(NULL))
    }

    in_name <- glue::glue("h3_data/{the_geohash}_h3data.qs")

    if (!fs::file_exists(in_name)) {
      fs::dir_create("h3_data")
      system(glue::glue("aws s3 cp s3://geomarker/st_pm_hex/{in_name} ./h3_data/"))
    }

    d <- qs::qread(in_name, nthreads = parallel::detectCores())

    d <- d %>%
      group_by(year) %>%
      nest()

    read_in_forest_and_predict <- function(yr) {
      message(yr)
      tictoc::tic()
      grf <- qs::qread(glue::glue("grf/st_pm_hex_grf_{yr}.qs"), nthreads = parallel::detectCores())
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
      d_out <- mutate_at(d_out, vars(starts_with("pm_")), signif, digits = 4)
      d_out$h3 <- d_pred$h3
      d_out$date <- d_pred$date
      tictoc::toc()
      return(d_out)
    }

    d$grf_preds <- purrr::map(2000:2020, read_in_forest_and_predict)

    # unnest it or write each one as a file?

    ## save it and upload
    fs::dir_create("h3_pm")
    qs::qsave(d_out, out_name, nthreads = parallel::detectCores())
    system(glue::glue("aws s3 cp {out_name} s3://geomarker/st_pm_hex/{out_name}"))
  }

purrr::walk(safe_harbor_h3, create_predictions)
