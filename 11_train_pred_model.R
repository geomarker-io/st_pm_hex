library(data.table)
library(tidyr)
library(dplyr)
library(tibble)
library(magrittr)
library(grf)

set.seed(224)

pred_names <- c(
  "nearby_pm25",
  "hpbl",
  "doy",
  "x",
  "air.2m",
  "vwnd.10m",
  "y",
  "pres.sfc",
  "vis",
  "rhum.2m",
  "nei_event",
  "nei_nonpoint",
  "prate",
  "nei_dist",
  "uwnd.10m",
  "nei_point",
  "nei_nonroad",
  "green",
  "nei_onroad"
)

# system("aws s3 cp s3://geomarker/st_pm_hex/h3data_train.fst .")
d <-
  fst::read_fst("h3data_train.fst", as.data.table = TRUE) %>%
  select(all_of(c(pred_names, "pm25", "h3", "year", "date")))

# round training data to 4 significant digits
d$pm25 <- signif(d$pm25, digits = 4)

## summarize number of clusters and cluster sizes
n_distinct(d$h3) # 1,684

n_distinct(select(d, h3, year)) #17,813

d %>%
  group_by(h3, year) %>%
  summarize(n = n()) %>%
  pull(n) %>%
  summary()

# 100 is about equal to the 1st quartile of cluster sizes (n = 97)

d %>%
  group_by(h3, year) %>%
  summarize(n = n()) %>%
  group_by(n >= 100) %>%
  summarize(n = n())

# keeps 13,221 of 17,813 possible clusters

d <-
  d %>%
  group_by(year, h3) %>%
  nest() %>%
  mutate(n = purrr::map_dbl(data, nrow)) %>%
  filter(n > 100) %>%
  select(-n) %>%
  unnest(cols = c(data)) %>%
  group_by(year) %>%
  nest()

train_and_save_grf <- function(year) {

  d_train <-
    d %>%
    filter(year == year) %>%
    pull(data) %>%
    .[[1]]

  grf <-
    regression_forest(
      X = select(d_train, all_of(pred_names)),
      Y = d_train$pm25,
      seed = 224,
      num.threads = parallel::detectCores(),
      compute.oob.predictions = TRUE,
      sample.fraction = 0.5,
      num.trees = 2000, # default 2000
      mtry = 19, # 19 total predictors now
      min.node.size = 1, # default 5
      alpha = 0.05,
      imbalance.penalty = 0,
      honesty = FALSE,
      clusters = as.factor(d_train$h3), # 1,912 possible h3 'levels'
      equalize.cluster.weights = FALSE,
      tune.parameters = "none"
    )

  qs::qsave(grf,
    glue::glue("st_pm_hex_grf_{year}.qs"),
    nthreads = parallel::detectCores()
  )

  system(glue::glue(
    "aws s3 cp ",
    "st_pm_hex_grf_{year}.qs ",
    "s3://geomarker/st_pm_hex/st_pm_hex_grf_{year}.qs"
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
    glue::glue("st_pm_hex_grf_preds_{year}.qs"),
    nthreads = parallel::detectCores()
  )
}

## train_and_save_grf(2015)

purrr::walk(2000:2020, train_and_save_grf)

## collect all predictions, combine, upload
