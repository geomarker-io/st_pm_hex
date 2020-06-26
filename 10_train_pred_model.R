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

# subsample with binom 0.632 probability for each station
inbag_list <- lapply(1:500, function(x) rep(rbinom(length(d_inbag$h3), 1, 0.632), times = d_inbag$N))

## # subsample all but one station
## make_inbag_LOLO <- function(tree_h3_number) {
##   in_or_out <- rep(1, times = length(d_inbag$h3))
##   in_or_out[tree_h3_number] <- 0
##   rep(in_or_out, times = d_inbag$N)
## }
## inbag_list <- lapply(1:length(d_inbag$h3), make_inbag_LOLO)

# rf for resample by station
rf <- ranger(
  d = as.data.frame(d) %>% select(-date),
  inbag = inbag_list,
  mtry = 6,
  seed = 224,
  min.node.size = 1,
  num.trees = length(inbag_list),
  dependent.variable.name = "pm25",
  importance = "impurity",
  write.forest = FALSE,
  keep.inbag = TRUE,
  oob.error = TRUE,
  save.memory = TRUE
)

# oob preds
d$pm_pred <- rf$predictions

## # pred se
## d$pm_se <- predict(rf, data = d, type = "se", se.method = "infjack")$se

fst::write_fst(d, "h3data_oob_preds_llo.fst", compress = 100)
system("aws s3 cp h3data_oob_preds_llo.fst s3://geomarker/st_pm_hex/h3data_oob_preds_llo.fst", ignore.stdout = TRUE)

## # rf for final predictions

rf_final <- ranger(
  d = as.data.frame(d) %>% select(-date),
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

qs::qsave(rf_final, "st_pm_hex_rf.qs", compress = 22, nthreads = parallel::detectCores())
system("aws s3 cp st_pm_hex_rf.qs s3://geomarker/st_pm_hex/st_pm_hex_rf.qs", ignore.stdout = TRUE)
