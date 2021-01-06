# using CCHMC cluster for this training:
# bsub -Is -M 200000 -n 24 -W 24:00 -R "span[ptile=24]" "module load singularity; ~/singr_4.0.sif"

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

write_a_grf_llo <- function(forest_number) {
    message(glue::glue("creating forest number {forest_number}"))
    tictoc::tic()
    seeder <- runif(1, 0, .Machine$integer.max)
    grf_llo <-
        regression_forest(
            X = as.data.frame(d) %>% select(-date, -h3, -pm25),
            Y = d$pm25,
            num.threads = parallel::detectCores(),
            compute.oob.predictions = FALSE,
            sample.fraction = 0.5,
            num.trees = 500,
            mtry = 26,
            min.node.size = 5,
            alpha = 0.05,
            honesty = TRUE, honesty.fraction = 0.5, honesty.prune.leaves = TRUE,
            tune.parameters = "none",
            clusters = as.factor(d$h3), # 1,912 possible h3 'levels'
            equalize.cluster.weights = FALSE
        )
    tictoc::toc()
    out_file_name <- glue::glue("st_pm_hex_grf_llo_seed_{seeder}.qs")
    qs::qsave(grf_llo,
        out_file_name,
        compress = 22, nthreads = parallel::detectCores()
    )
    system(glue::glue(
        "aws s3 cp {out_file_name}",
        " s3://geomarker/st_pm_hex/{out_file_name}"
    ))
}

purrr::walk(1:10, ~ write_a_grf_llo(.))

write_a_grf <- function(forest_number) {
    message(glue::glue("creating forest number {forest_number}"))
    tictoc::tic()
    seeder <- runif(1, 0, .Machine$integer.max)
    grf <-
        regression_forest(
            X = as.data.frame(d) %>% select(-date, -h3, -pm25),
            Y = d$pm25,
            num.threads = parallel::detectCores(),
            compute.oob.predictions = FALSE,
            sample.fraction = 0.5,
            num.trees = 500,
            mtry = 26,
            min.node.size = 5,
            alpha = 0.05,
            honesty = TRUE, honesty.fraction = 0.5, honesty.prune.leaves = TRUE,
            tune.parameters = "none",
            equalize.cluster.weights = FALSE
        )
    tictoc::toc()
    out_file_name <- glue::glue("st_pm_hex_grf_seed_{seeder}.qs")
    qs::qsave(grf,
        out_file_name,
        compress = 22, nthreads = parallel::detectCores()
    )
    system(glue::glue(
        "aws s3 cp {out_file_name}",
        " s3://geomarker/st_pm_hex/{out_file_name}"
    ))
}

purrr::walk(1:10, ~ write_a_grf(.))

# join together to create larger forest objects (grf and grf_llo)
grf <-
    fs::dir_ls(glob = "st_pm_hex_grf_seed_*.qs")[1:2] %>%
    purrr::map(qs::qread, nthreads = parallel::detectCores()) %>%
    grf::merge_forests(compute.oob.predictions = FALSE)

grf_llo <-
    fs::dir_ls(glob = "st_pm_hex_grf_llo_seed_*.qs") %>%
    purrr::map(qs::qread, nthreads = parallel::detectCores()) %>%
    grf::merge_forests(compute.oob.predictions = FALSE)


## determine the number of trees needed to get low "excess error"


grfs_llo <- fs::dir_ls(glob = "st_pm_hex_grf_llo_seed_*.qs")

grf_llo_1 <- qs::qread(grfs_llo[1], nthreads = parallel::detectCores())
grf_llo_2 <- qs::qread(grfs_llo[2], nthreads = parallel::detectCores())

grf_llo_1_preds <- predict(grf_llo_1, estimate.variance = TRUE)

head(grf_llo_1_preds)

d$pm_pred_llo_1 <- grf_preds_llo_1$predictions
d$pm_pred_llo_se_1 <- sqrt(grf_preds_llo_1$variance.estimates)

grf_llo_2_preds <- predict(grf_llo_2, estimate.variance = TRUE)

head(grf_llo_2_preds)

d$pm_pred_llo_2 <- grf_preds_llo_2$predictions
d$pm_pred_se_llo_2 <- sqrt(grf_preds_llo_2$variance.estimates)

cor.test(grf_llo_1_preds$predictions, grf_llo_2_preds$predictions)

head(d %>% select(pm_pred_1, pm_pred_se_1, pm_pred_2, pm_pred_se_2, pm25, date, h3))


# save final RF object for prediction (how many trees do we need for this?)
# we have to grow enough trees to make the `excess.error` negligible compared to `variance.estimates`
# https://grf-labs.github.io/grf/reference/predict.regression_forest.html#value

# final model predictions and standard errors
grf_preds <- predict(grf, as.data.frame(d) %>% select(-date, -h3, -pm25), estimate.variance = TRUE)
d$pm_pred <- grf_preds$predictions
d$pm_pred_se <- sqrt(grf_preds$variance.estimates)

# LLO predictions and standard errors
grf_oob_preds <- predict(grf_llo, estimate.variance = TRUE)
d$pm_llo_pred <- grf_oob_preds$predictions
d$pm_llo_pred_se <- sqrt(grf_oob_preds$variance.estimates)


# round all predictions and SEs to 4 significant digits and save
d$pm_pred <- signif(d$pm_pred, digits = 4)
d$pm_pred_se <- signif(d$pm_pred_se, digits = 4)
d$pm_llo_pred <- signif(d$pm_llo_pred, digits = 4)
d$pm_llo_pred_se <- signif(d$pm_llo_pred_se, digits = 4)

qs::qsave(d, "h3_data_grf_preds.qs", compress = 22, nthreads = parallel::detectCores())
system("aws s3 cp h3_data_grf_preds.qs s3://geomarker/st_pm_hex/h3_data_grf_preds.qs")