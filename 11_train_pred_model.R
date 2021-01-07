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
d <- fst::read_fst("h3data_train.fst", as.data.table = TRUE)
d$nei_year <- NULL
d$dow <- as.numeric(d$dow)
d$holiday <- as.numeric(d$holiday)

# round training data to 4 significant digits
d$pm25 <- signif(d$pm25, digits = 4)

tictoc::tic()
grf_cluster <-
    regression_forest(
        X = as.data.frame(d) %>% select(-date, -h3, -pm25),
        Y = d$pm25,
        seed = 224,
        num.threads = parallel::detectCores(),
        compute.oob.predictions = FALSE,
        sample.fraction = 0.5,
        num.trees = 1000,
        mtry = 26,
        min.node.size = 5,
        alpha = 0.05,
        honesty = TRUE, honesty.fraction = 0.5, honesty.prune.leaves = TRUE,
        tune.parameters = "none",
        clusters = as.factor(d$h3), # 1,912 possible h3 'levels'
        equalize.cluster.weights = FALSE
    )
tictoc::toc()

qs::qsave(grf_cluster, "st_pm_hex_grf_cluster.qs", compress = 22, nthreads = parallel::detectCores())
system("aws s3 cp st_pm_hex_grf_cluster.qs s3://geomarker/st_pm_hex/st_pm_hex_grf_cluster.qs")

grf_cluster <- qs::qread("st_pm_hex_grf_cluster.qs", nthreads = parallel::detectCores())

grf_cluster_preds_oob <- predict(grf_cluster, estimate.variance = TRUE)

tictoc::tic()
grf_cluster_preds <- predict(grf_cluster, as.data.frame(d) %>% select(-date, -h3, -pm25), estimate.variance = TRUE)
tictoc::toc()

d$pm_pred_cluster <- grf_cluster_preds$predictions
d$pm_pred_cluster_se <- sqrt(grf_cluster_preds$variance.estimates)
d$pm_pred_cluster_oob <- grf_cluster_preds_oob$predictions
d$pm_pred_cluster_oob_se <- sqrt(grf_cluster_preds_oob$variance.estimates)

# round all predictions and SEs to 4 significant digits
d <- d %>%
    mutate_at(vars(starts_with("pm_pred_")), signif, digits = 4)

# save it
qs::qsave(d, "h3_data_grf_preds.qs", compress = 22, nthreads = parallel::detectCores())
system("aws s3 cp h3_data_grf_preds.qs s3://geomarker/st_pm_hex/h3_data_grf_preds.qs")
