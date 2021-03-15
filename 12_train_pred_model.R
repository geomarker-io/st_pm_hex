library(data.table)
library(tidyr)
library(dplyr)
library(tibble)
library(magrittr)
library(grf)

set.seed(224)

pred_names <- c(
  "population_density",
  "year",
  "doy", "dow",
  "s1100_dist",
  "x", "y",
  "nei_dist",
  "nearby_pm",
  "holiday",
  "nei_point", "nei_nonroad", "nei_onroad", "nei_nonpoint", "nei_event",
  "hpbl", "vis", "uwnd.10m", "vwnd.10m", "air.2m", "rhum.2m", "prate", "pres.sfc",
  "impervious", "green", "primary_urban", "primary_rural",
  "secondary_urban", "secondary_rural",
  "tertiary_urban", "tertiary_rural",
  "thinned_urban", "thinned_rural",
  "nonroad_urban", "nonroad_rural",
  "energyprod_urban", "energyprod_rural",
  "nonimpervious",
  "aod",
  "fire_pm25", "fire_area"
)

pred_names <- c(
  "nearby_pm",
  "x",
  "hpbl",
  "doy",
  "nei_event",
  "vwnd.10m",
  "air.2m",
  "rhum.2m",
  "y",
  "vis",
  "nei_nonroad",
  "uwnd.10m",
  "pres.sfc",
  "population_density",
  "nei_dist",
  "prate",
  "green",
  "holiday",
  "impervious",
  "nonimpervious",
  "nonroad_urban",
  "dow",
  "aod"
)

# make forests for all years using selected variables
d <-
  fst::read_fst("h3data_train.fst", as.data.table = TRUE) %>%
  select(all_of(c(pred_names, "pm25", "h3", "year", "date")))

d$holiday <- as.numeric(d$holiday)
d$dow <- as.numeric(d$dow)

# round training data to 4 significant digits
d$pm25 <- signif(d$pm25, digits = 4)

## summarize number of clusters and cluster sizes
n_distinct(d$h3) # 1,684

n_distinct(select(d, h3, year)) #17,813

train_and_save_grf <- function(yr) {

  d_train <- filter(d, year == yr)

  grf <-
    regression_forest(
      X = select(d_train, all_of(pred_names)),
      Y = d_train$pm25,
      seed = 224,
      num.threads = parallel::detectCores(),
      compute.oob.predictions = TRUE,
      sample.fraction = 0.5,
      num.trees = 2000,
      mtry = 14,
      min.node.size = 1, # default 5
      alpha = 0.05,
      imbalance.penalty = 0,
      honesty = FALSE,
      clusters = as.factor(d_train$h3), # 1,912 possible h3 'levels'
      equalize.cluster.weights = FALSE,
      tune.parameters = "none"
    )

  ## var_imp <- variable_importance(grf)
  ## vi <- tibble(var_imp = round(var_imp, 4),
  ##              variable = names(select(d_train, all_of(pred_names))))
  ## knitr::kable(arrange(vi, desc(var_imp)))
  ## median(abs(grf$predictions - grf$Y.orig))

  qs::qsave(grf,
    glue::glue("st_pm_hex_grf_{yr}.qs"),
    nthreads = parallel::detectCores()
  )

  system(glue::glue(
    "aws s3 cp ",
    "st_pm_hex_grf_{yr}.qs ",
    "s3://geomarker/st_pm_hex/st_pm_hex_grf_{yr}.qs"
  ))

  grf_preds_oob <- predict(grf, estimate.variance = TRUE)

  d_out <- select(d_train, h3, date)

  d_out$pm_pred_oob <- grf_preds_oob$predictions
  d_out$pm_pred_oob_se <- sqrt(grf_preds_oob$variance.estimates)

  grf_preds <- predict(grf, select(d_train, all_of(pred_names)), estimate.variance = TRUE)

  d_out$pm_pred <- grf_preds$predictions
  d_out$pm_pred_se <- sqrt(grf_preds$variance.estimates)

# round all predictions and SEs to 4 significant digits
  d_out <-
    d_out %>%
    mutate_at(vars(starts_with("pm_pred_")), signif, digits = 4)

  qs::qsave(d_out,
    glue::glue("st_pm_hex_grf_preds_{yr}.qs"),
    nthreads = parallel::detectCores()
  )
}

purrr::walk(2000:2020, train_and_save_grf)

## collect all predictions, combine, upload

purrr::map(2000:2020, ~ qs::qread(paste0("st_pm_hex_grf_preds_", ., ".qs"))) %>%
  set_names(2000:2020) %>%
  tibble::enframe(name = "year", value = "data") %>%
  unnest(cols = c(data)) %>%
  saveRDS("st_pm_hex_grf_preds.rds")

system("aws s3 cp st_pm_hex_grf_preds.rds s3://geomarker/st_pm_hex/st_pm_hex_grf_preds.rds")
