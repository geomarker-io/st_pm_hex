library(data.table)
library(tidyr)
library(dplyr)
library(tibble)
library(magrittr)
library(ranger)

set.seed(224)

d <- fst::read_fst("h3data_train.fst", as.data.table = TRUE)

d_aod <- d[!is.na(aod)][, c("date") := NULL]
d_noaod <- d[is.na(aod)][, c("date", "aod") := NULL]

rm(d)

# ranger can't deal with missing data :(
# for now impute using zero....
# TODO impute with this? https://github.com/mayer79/missRanger
# TODO double check why some years have a median value of zero...??
d_aod <- replace_na(d_aod, list(nei_nonroad = 0, nei_onroad = 0, nei_nonpoint = 0, nei_event = 0))
# TODO we should check later if these actually improve performance and remove if they do not...

#### train aod rf
rf_aod <- ranger(
  d = as.data.frame(d_aod),
  mtry = 6,
  seed = 224,
  min.node.size = 1,
  num.trees = 500,
  dependent.variable.name = "pm25",
  write.forest = TRUE,
  importance = "impurity",
  keep.inbag = TRUE
)

qs::qsave(rf_aod, "rf_aod.qs", compress = 22, nthreads = parallel::detectCores())
system("aws s3 cp rf_aod.qs s3://geomarker/st_pm_hex/rf_aod.qs", ignore.stdout = TRUE)

rf_aod
sqrt(rf_aod$prediction.error)
cor(rf_aod$predictions, d_aod$pm25)^2

# oob preds
d_aod$pm_pred <- rf_aod$predictions

## # pred se
## rf_aod_se <- predict(rf_aod, data = d_aod, type = "se", se.method = "infjack")
## d_aod$pm_se <- rf_aod_se$se

d_noaod <- replace_na(d_noaod, list(nei_nonroad = 0, nei_onroad = 0, nei_nonpoint = 0, nei_event = 0))

#### train noaod rf
rf_noaod <- ranger(
  d = as.data.frame(d_noaod),
  mtry = 12,
  seed = 224,
  num.trees = 500,
  min.node.size = 1,
  dependent.variable.name = "pm25",
  write.forest = TRUE,
  importance = "impurity",
  keep.inbag = TRUE
)

qs::qsave(rf_noaod, "rf_noaod.qs", compress = 22, nthreads = parallel::detectCores())
system("aws s3 cp rf_noaod.qs s3://geomarker/st_pm_hex/rf_noaod.qs", ignore.stdout = TRUE)

rf_noaod
sqrt(rf_noaod$prediction.error)
cor(rf_noaod$predictions, d_noaod$pm25)^2

d_noaod$pm_pred <- rf_noaod$predictions

## # pred se
## rf_noaod_se <- predict(rf_noaod, , data = d_noaod, type = "se", se.method = "infjack")
## d_noaod$pm_se <- rf_noaod_se$se

## merge back into training data to save
d <- fst::read_fst("h3data_train.fst", as.data.table = TRUE)

d[!is.na(d$aod), "pm_pred"] <- d_aod$pm_pred
d[is.na(d$aod), "pm_pred"] <- d_noaod$pm_pred
## d[!is.na(d$aod), "pm_se"] <- d_aod$pm_se
## d[is.na(d$aod), "pm_se"] <- d_noaod$pm_se

fst::write_fst(d, "h3data_oob_preds.fst", compress = 100)
system("aws s3 cp h3data_oob_preds.fst s3://geomarker/st_pm_hex/h3data_oob_preds.fst", ignore.stdout = TRUE)

## oob cv error summary stats
d %>%
  summarize(
    n = n(),
    mae = median(abs(pm25 - pm_pred)),
    rmse = sqrt(mean((pm25 - pm_pred)^2)),
    rsq = cor(pm25, pm_pred, use = "pairwise.complete")^2,
    slope = lm(pm_pred ~ pm25 - 1, data = .) %>% coefficients() %>% .["pm25"]
  ) %>%
  knitr::kable(digits = 2)

## |       n| mae| rmse|  rsq| slope|
## |-------:|---:|----:|----:|-----:|
## | 3221123| 1.7| 3.97| 0.72|  0.87|

d %>%
  group_by(is.na(aod)) %>%
  summarize(
    n = n(),
    mae = median(abs(pm25 - pm_pred)),
    rmse = sqrt(mean((pm25 - pm_pred)^2)),
    rsq = cor(pm25, pm_pred, use = "pairwise.complete")^2,
    slope = lm(pm_pred ~ pm25 - 1, data = .) %>% coefficients() %>% .["pm25"]
  ) %>%
  knitr::kable(digits = 2)

## |is.na(aod) |       n| mae| rmse|  rsq| slope|
## |:----------|-------:|---:|----:|----:|-----:|
## |FALSE      |   10090| 4.2| 13.8| 0.63|  0.87|
## |TRUE       | 3211033| 1.7|  3.9| 0.72|  0.87|
