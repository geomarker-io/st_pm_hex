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

d %>%
  group_by(h3) %>%
  summarize(n = n()) %>%
  group_by(n >= 365) %>%
  summarize(n = n())

# keeps 1,271 of 1,684 possible clusters

h3_keeps <-
  d %>%
  group_by(h3) %>%
  summarize(n = n()) %>%
  filter(n >= 365) %>%
  pull(h3)

d_for_predict <-
  d %>%
  filter(h3 %in% h3_keeps) %>%
  select(-county_fips)
  
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

## TODO filter to only clusters with at least n observations; use equalize.cluster.weights


## TODO how can we plot OOB error rate with number of trees

## consider using lots of smaller forests and then unioning these

tictoc::tic()
grf <-
    regression_forest(
        X = select(d_for_predict, all_of(pred_names)),
        Y = d_for_predict$pm25,
        seed = 224,
        num.threads = parallel::detectCores(),
        compute.oob.predictions = TRUE,
        sample.fraction = 0.5,
        num.trees = 2000, # default 2000
        mtry = 27, # 19 total predictors now
        min.node.size = 1, # default 5
        alpha = 0.05,
        imbalance.penalty = 0,
        honesty = FALSE,
        clusters = as.factor(d_for_predict$h3), # 1,912 possible h3 'levels'
        equalize.cluster.weights = TRUE,
        tune.parameters = "none"
    )
tictoc::toc()

median(abs(grf$Y.orig - grf$predictions))
# starting point (with equal cluster weights): 1.37
# only using variables with var_imp > 0.0001: 1.305
# 2000 trees: 1.300
# don't equalize cluster weights: 1.18

var_imp <- tibble(
  var_imp = round(variable_importance(grf, decay.exponent = 2, max.depth = 4), digits = 5),
  variable = names(select(d_for_predict, pred_names))
) %>%
  arrange(desc(var_imp))
knitr::kable(var_imp)

