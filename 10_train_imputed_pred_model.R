library(data.table)
library(tidyr)
library(dplyr)
library(tibble)
library(magrittr)
library(ranger)

set.seed(224)

d <- fst::read_fst("h3data_train_imputed.fst", as.data.table = TRUE)

# select inbag manually so that we select by grid, not grid-day

d_inbag <- d[TRUE, .N, by = c("h3")]
inbag_list <- lapply(1:500, function(x) rep(rbinom(length(d_inbag$h3), 1, 0.632), times = d_inbag$N))


# rf on all except date
rf <- ranger(
  d = as.data.frame(d) %>% select(-date),
  inbag = inbag_list,
  mtry = 6,
  seed = 224,
  min.node.size = 1,
  num.trees = 500,
  dependent.variable.name = "pm25",
  importance = "impurity",
  write.forest = TRUE,
  keep.inbag = TRUE,
  oob.error = TRUE,
  save.memory = FALSE
)

qs::qsave(rf, "rf.qs", compress = 22, nthreads = parallel::detectCores())
system("aws s3 cp rf.qs s3://geomarker/st_pm_hex/rf.qs", ignore.stdout = TRUE)

rf
sqrt(rf$prediction.error)
cor(rf$predictions, d$pm25)^2

# oob preds
d$pm_pred <- rf$predictions

## # pred se
## d$pm_se <- predict(rf, data = d, type = "se", se.method = "infjack")$se

fst::write_fst(d, "h3data_oob_preds.fst", compress = 100)
system("aws s3 cp h3data_oob_preds.fst s3://geomarker/st_pm_hex/h3data_oob_preds.fst", ignore.stdout = TRUE)
