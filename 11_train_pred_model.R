# using CCHMC cluster for this training:
# bsub -Is -M 200000 -n 24 -W 24:00 -R "span[ptile=24]" "module load singularity; ~/singr_4.0.sif"

# https://linuxhandbook.com/increase-swap-ubuntu/

library(data.table)
library(tidyr)
library(dplyr)
library(tibble)
library(magrittr)
library(grf)

set.seed(224)

# system("aws s3 cp s3://geomarker/st_pm_hex/h3data_train.fst .")
d <-
  fst::read_fst("h3data_train.fst", as.data.table = TRUE) %>%
  select(-nei_year) %>%
  mutate(dow = as.numeric(dow),
         holiday = as.numeric(holiday))

# round training data to 4 significant digits
d$pm25 <- signif(d$pm25, digits = 4)

## summarize number of clusters and  cluster sizes
n_distinct(d$h3)

d %>%
  group_by(h3) %>%
  summarize(n = n()) %>%
  pull(n) %>%
  summary()

tictoc::tic()
grf <-
    regression_forest(
        X = as.data.frame(d) %>% select(-date, -h3, -pm25, -county_fips),
        Y = d$pm25,
        seed = 224,
        num.threads = parallel::detectCores(),
        compute.oob.predictions = TRUE,
        sample.fraction = 0.5,
        num.trees = 500,
        mtry = 27,
        min.node.size = 1,
        alpha = 0.05,
        imbalance.penalty = 0,
        honesty = FALSE,
        clusters = as.factor(d$h3), # 1,912 possible h3 'levels'
        equalize.cluster.weights = FALSE,
        tune.parameters = "none"
    )
tictoc::toc()

qs::qsave(grf, "st_pm_hex_grf.qs", compress = 22, nthreads = parallel::detectCores())
system("aws s3 cp st_pm_hex_grf.qs s3://geomarker/st_pm_hex/st_pm_hex_grf.qs")

tictoc::tic()
grf_preds_oob <- predict(grf, estimate.variance = TRUE)
tictoc::toc()

d$pm_pred_oob <- grf_preds_oob$predictions
d$pm_pred_oob_se <- sqrt(grf_preds_oob$variance.estimates)

tictoc::tic()
grf_preds <- predict(grf, as.data.frame(d) %>% select(-date, -h3, -pm25, -county_fips, -pm_pred_oob, -pm_pred_oob_se), estimate.variance = TRUE)
tictoc::toc()

d$pm_pred <- grf_preds$predictions
d$pm_pred_se <- sqrt(grf_preds$variance.estimates)

# round all predictions and SEs to 4 significant digits
d <- d %>%
    mutate_at(vars(starts_with("pm_pred_")), signif, digits = 4)

# save it
qs::qsave(d, "h3_data_grf_preds.qs", compress = 22, nthreads = parallel::detectCores())
system("aws s3 cp h3_data_grf_preds.qs s3://geomarker/st_pm_hex/h3_data_grf_preds.qs")
