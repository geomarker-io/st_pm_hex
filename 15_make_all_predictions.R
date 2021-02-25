library(grf)
library(magrittr)

safe_harbor_h3 <- readRDS("us_h3_4_population_20k_minimum_hex_ids.rds")

cincinnati_h3_6s <- c(
  "832a93fffffffff",
  "832a90fffffffff",
  "83266dfffffffff",
  "832a9efffffffff"
)

tictoc::tic()
grf <- qs::qread("st_pm_hex_grf.qs", nthreads = parallel::detectCores())
tictoc::toc()

create_predict_data <-
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

    d_out <-
      tibble::tibble(
        h3 = d$h3,
        date = d$date
      )

    d$nei_year <- NULL
    d$nlcd_year <- NULL
    d$county_fips <- NULL
    d$dow <- as.numeric(d$dow)
    d$holiday <- as.numeric(d$holiday)
    d$year <- as.numeric(d$year)
    d$date <- NULL
    d$h3 <- NULL

    tictoc::tic()
    ## grf_preds <-
      predict(grf, d[1:3, ], estimate.variance = TRUE, num.threads = parallel::detectCores())
    tictoc::toc()

    # TODO is it possible to chunk this to save on memory?
    purrr::map(seq_len(nrow(d)), ~ predict(grf, d[., ], estimate.variance = TRUE, num.threads = 1))

    purrr::map(seq_len(3), ~ predict(grf, d[., ], estimate.variance = TRUE, num.threads = 1))

    mappp::mappp(seq_len(3), ~ predict(grf, d[., ], estimate.variance = TRUE, num.threads = 1), parallel = TRUE)

    d_out$pm_pred <- grf_preds$predictions
    d_out$pm_se <- sqrt(grf_preds$variance.estimates)

    # TODO round all predictions and SEs to 4 significant digits???
    # will this save any size?
    ## d <- d %>%
    ##   mutate_at(vars(starts_with("pm_pred_")), signif, digits = 4)

    ## save it and upload
    fs::dir_create("h3_pm")
    qs::qsave(d_out, out_name, nthreads = parallel::detectCores())
    system(glue::glue("aws s3 cp {out_name} s3://geomarker/st_pm_hex/{out_name}"))
  }

purrr::walk(safe_harbor_h3, create_predict_data)
