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
rf <- ranger(
  d = as.data.frame(d) %>% select(-date),
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

# oob preds from LLO forest
d$oob_pm_pred <- rf$predictions
# 
fst::write_fst(d, "h3data_oob_preds_llo.fst", compress = 100)
system("aws s3 cp h3data_oob_preds_llo.fst s3://geomarker/st_pm_hex/h3data_oob_preds_llo.fst", ignore.stdout = TRUE)

# rf for final predictions

rf_final <- ranger(
  d = as.data.frame(d) %>% select(-date),
  mtry = 6,
  seed = 224,
  min.node.size = 1,
  num.trees = 500,
  dependent.variable.name = "pm25",
  importance = "impurity",
  write.forest = TRUE,
  keep.inbag = FALSE,
  oob.error = FALSE,
  save.memory = TRUE
)


# trees: 500
# sample size: 3345299
# n vars: 43
# mtry: 6
# target node size: 1
# splitrule: variance

# use this to generate pred se when predicting on all data
# d$pm_se <- predict(rf_final, data = d, type = "se", se.method = "infjack")$se

qs::qsave(rf_final, "st_pm_hex_rf.qs", compress = 22, nthreads = parallel::detectCores())
system("aws s3 cp st_pm_hex_rf.qs s3://geomarker/st_pm_hex/st_pm_hex_rf.qs", ignore.stdout = TRUE)
