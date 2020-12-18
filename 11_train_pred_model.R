library(data.table)
library(tidyr)
library(dplyr)
library(tibble)
library(magrittr)
library(grf)

set.seed(224)

# using CCHMC cluster for this training:
# bsub -Is -M 100000 -n 24 -W 24:00 -R "span[ptile=24]" "module load singularity; ~/singr_4.0.sif"

# d <- fst::read_fst("h3data_train_imputed.fst", as.data.table = TRUE)

# system("aws s3 cp s3://geomarker/st_pm_hex/h3data_train.fst .")
d <- fst::read_fst("h3data_train.fst", as.data.table = TRUE)
d$nei_year <- NULL
d$dow <- as.numeric(d$dow)
d$holiday <- as.numeric(d$holiday)

# round training data to 4 significant digits
d$pm25 <- signif(d$pm25, digits = 4)

tictoc::tic()

grf_llo <-
    regression_forest(
        X = as.data.frame(d) %>% select(-date, -h3, -pm25),
        Y = d$pm25,
        honesty = TRUE,
        num.threads = parallel::detectCores(),
        seed = 224,
        compute.oob.predictions = FALSE,
        tune.parameters = "all",
        clusters = as.factor(d$h3), # 1,912 possible h3 'levels'
        sample.fraction = 0.5
    )

tictoc::toc()

qs::qsave(grf_llo, "st_pm_hex_grf_llo.qs", compress = 22, nthreads = parallel::detectCores())
system("aws s3 cp st_pm_hex_grf_llo.qs s3://geomarker/st_pm_hex/st_pm_hex_grf_llo.qs", ignore.stdout = TRUE)
# grf_llo <- qs::qread("st_pm_hex_grf_llo.qs", nthreads = parallel::detectCores())

tictoc::tic()

# use grf::merge_forests() to put together many individual forests

# check out grf::boosted_regression_forest(), but estimate.variance is not available for this!

grf <-
    regression_forest(
        X = as.data.frame(d) %>% select(-date, -h3, -pm25),
        Y = d$pm25,
        honesty = FALSE,
        num.threads = parallel::detectCores(),
        seed = 224,
        compute.oob.predictions = FALSE,
        mtry = 6,
        min.node.size = 5,
        clusters = NULL,
        num.trees = 500, # make 2000?
        sample.fraction = 0.5
    )

tictoc::toc()

qs::qsave(grf, "st_pm_hex_grf.qs", compress = 22, nthreads = parallel::detectCores())
system("aws s3 cp st_pm_hex_grf.qs s3://geomarker/st_pm_hex/st_pm_hex_grf.qs", ignore.stdout = TRUE)
# grf <- qs::qread("st_pm_hex_grf.qs", nthreads = parallel::detectCores())

tictoc::tic()


grf_oob_preds <- predict(grf, estimate.variance = TRUE)


grf_preds <- predict(grf, as.data.frame(d) %>% select(-date, -h3, -nei_year, -pm25), estimate.variance = TRUE)


# we have to grow enough trees to make the `excess.error` negligible
# https://grf-labs.github.io/grf/reference/predict.regression_forest.html#value

pm_pred <- grf_preds$predictions
pm_pred_se <- sqrt(grf_preds$variance.estimates)

tictoc::toc()

# should we round our results too? GRF recommends to 8 significant digits
# d$pm25 <- signif(d$pm25, digits = 4)

qs::qsave(grf_preds, "st_pm_hex_grf_preds.qs", compress = 22, nthreads = parallel::detectCores())
system("aws s3 cp st_pm_hex_grf_preds.qs s3://geomarker/st_pm_hex/st_pm_hex_grf_preds.qs", ignore.stdout = TRUE)