library(data.table)
library(tidyr)
library(dplyr)
library(tibble)
library(magrittr)
library(ranger)

set.seed(224)

d <- fst::read_fst("h3data_train_imputed.fst", as.data.table = TRUE)

# select inbag manually so that we resample by grid, not grid-day
d_inbag <- d[TRUE, .N, by = c("h3")]

# subsample with binom 0.632 probability for each station to replicate bootstrap
# but each prediction will still use ALL trees without the station, so total predictions will be made using more than 63.2% of the stations
inbag_list <- lapply(1:500, function(x) rep(rbinom(length(d_inbag$h3), 1, 0.632), times = d_inbag$N))

# # subsample with binom 0.9 probability for each station to replicate 10-fold subsample
# inbag_list <- lapply(1:500, function(x) rep(rbinom(length(d_inbag$h3), 1, 0.9), times = d_inbag$N))

# # leave one location out (subsample all but one station)
# # but this requires more trees (as many as unique h3s we have = 1,912)
# make_inbag_LOLO <- function(tree_h3_number) {
#   in_or_out <- rep(1, times = length(d_inbag$h3))
#   in_or_out[tree_h3_number] <- 0
#   rep(in_or_out, times = d_inbag$N)
# }
# inbag_list <- lapply(1:length(d_inbag$h3), make_inbag_LOLO)

# rf for resample by station
# on 8 cores, this takes ~ 200 GB of RAM to complete
rf_llo <- ranger(
  d = as.data.frame(d) %>% select(-date, -h3, -nei_year),
  inbag = inbag_list,
  mtry = 6,
  seed = 224,
  min.node.size = 1,
  num.trees = length(inbag_list),
  dependent.variable.name = "pm25",
  importance = "impurity",
  write.forest = TRUE,
  keep.inbag = TRUE,
  oob.error = TRUE,
  save.memory = TRUE
)

qs::qsave(rf_llo, "st_pm_hex_rf_llo.qs", compress = 22, nthreads = parallel::detectCores())
system("aws s3 cp st_pm_hex_rf_llo.qs s3://geomarker/st_pm_hex/st_pm_hex_rf_llo.qs", ignore.stdout = TRUE)
# rf_llo <- qs::qread("st_pm_hex_rf_llo.qs", nthreads = parallel::detectCores())

# rf for oob
rf_oob <- ranger(
  d = as.data.frame(d) %>% select(-date, -h3, -nei_year),
  mtry = 6,
  seed = 224,
  min.node.size = 1,
  num.trees = 500,
  dependent.variable.name = "pm25",
  importance = "impurity",
  write.forest = TRUE,
  keep.inbag = TRUE,
  oob.error = TRUE,
  save.memory = TRUE
)

qs::qsave(rf_oob, "st_pm_hex_rf_oob.qs", compress = 22, nthreads = parallel::detectCores())
system("aws s3 cp st_pm_hex_rf_oob.qs s3://geomarker/st_pm_hex/st_pm_hex_rf_oob.qs", ignore.stdout = TRUE)
# rf_oob <- qs::qread("st_pm_hex_rf_oob.qs", nthreads = parallel::detectCores())

# add in LLO and OOB predictions from forests
d$pm_pred_llo <- rf_llo$predictions
d$pm_pred_oob <- rf_oob$predictions

fst::write_fst(d, "h3data_oob_preds_llo.fst", compress = 100)
system("aws s3 cp h3data_oob_preds_llo.fst s3://geomarker/st_pm_hex/h3data_oob_preds_llo.fst", ignore.stdout = TRUE)




# "new" preds from forest
d$pm_pred <- predict(rf_oob, data = d, type = "response")$predictions

# does the S Wager package suffer from these problems too?

# se of preds from prediction forest
d$pm_pred_se <- predict(rf_oob, data = d, type = "se", se.method = "infjack")$se