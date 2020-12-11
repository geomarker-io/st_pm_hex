library(tidyr)
library(dplyr)
library(tibble)
library(magrittr)

dir.create("./cv_output", showWarnings = FALSE)

# make sure we have the latest OOB predictions (and RF??)
system("aws s3 cp s3://geomarker/st_pm_hex/h3data_oob_preds_llo.fst .")

d <-
  fst::read_fst("h3data_oob_preds_llo.fst") %>%
  as_tibble() %>%
  select(date, pm25, h3, year, x, y, pm_pred, aod)

#' oob cv error summary stats
d %>%
  summarize(
    n = n(),
    mean_pm25 = mean(pm25),
    median_pm25 = median(pm25),
    min_pm25 = min(pm25),
    max_pm25 = max(pm25),
    var_pm25 = var(pm25),
    mae = median(abs(pm25 - pm_pred)),
    rmse = sqrt(mean((pm25 - pm_pred)^2)),
    rsq = cor(pm25, pm_pred, use = "pairwise.complete")^2,
    pseudo_rsq = 1 - (rmse^2 / var_pm25),
    slope = lm(pm_pred ~ pm25 - 1, data = .) %>% coefficients() %>% .["pm25"]
  ) %>%
  knitr::kable(digits = 2) %>%
  cat(file = "cv_output/cv_accuracy_overall.md", sep = "\n")

#' plot overall obs versus oob predicted
library(ggplot2)
library(hexbin)
library(scales)

ggplot(d, aes(pm25, pm_pred)) +
    stat_bin_hex(binwidth = c(log10(1.065), log10(1.065)))+
    viridis::scale_fill_viridis(option = "C", name = "Count") +
    geom_abline(slope = 1, intercept = 0, lty = 2, alpha = 0.8, color = "darkgrey") +
    scale_x_log10(limits = c(1, 200)) + scale_y_log10(limits = c(1, 200)) +
    xlab(expression(Observed ~ paste(PM[2.5], " (", mu, "g/", m^{3}, ") "))) +
    ylab(expression(CV ~ Predicted ~ paste(PM[2.5], " (", mu, "g/", m^{3}, ") "))) +
    CB::theme_cb() +
    coord_fixed()

ggsave("cv_output/cv_scatter_plot.pdf", width = 6, height = 6)

#' plot overall obs versus se

#' plot overall deviance versus se

#' for each site, generate predictions for daily pm2.5 and averages for week, month, year, and and entire time period

library(data.table)

d <- as.data.table(d, key = c("h3", "year", "date"))
d$month <- lubridate::month(d$date)
d$week <- lubridate::week(d$date)

d_temporal_cv <- list()

#' 2000 - 2019
d_temporal_cv$all <-
  d[,
    .(.N, mean_pm25 = mean(pm25), mean_pm_pred = mean(pm_pred)),
    by = c("h3")
  ][,
    .(
      .N,
      mae = median(abs(mean_pm25 - mean_pm_pred)),
      rmse = sqrt(mean((mean_pm25 - mean_pm_pred)^2)),
      rsq = cor(mean_pm25, mean_pm_pred, use = "pairwise.complete")^2
    ),
    by = c("h3")
  ]

#' annual
d_temporal_cv$annual <-
  d[,
    .(.N, mean_pm25 = mean(pm25), mean_pm_pred = mean(pm_pred)),
    by = c("h3", "year")
  ][,
    .(
      .N,
      mae = median(abs(mean_pm25 - mean_pm_pred)),
      rmse = sqrt(mean((mean_pm25 - mean_pm_pred)^2)),
      rsq = cor(mean_pm25, mean_pm_pred, use = "pairwise.complete")^2
    ),
    by = c("h3")
    ]

#' monthly
d_temporal_cv$monthly <-
  d[,
    .(.N, mean_pm25 = mean(pm25), mean_pm_pred = mean(pm_pred)),
    by = c("h3", "year", "month")
  ][,
    .(
      .N,
      mae = median(abs(mean_pm25 - mean_pm_pred)),
      rmse = sqrt(mean((mean_pm25 - mean_pm_pred)^2)),
      rsq = cor(mean_pm25, mean_pm_pred, use = "pairwise.complete")^2
    ),
    by = c("h3")
  ]

#' weekly
d_temporal_cv$weekly <-
  d[,
    .(.N, mean_pm25 = mean(pm25), mean_pm_pred = mean(pm_pred)),
    by = c("h3", "year", "week")
  ][,
    .(
      .N,
      mae = median(abs(mean_pm25 - mean_pm_pred)),
      rmse = sqrt(mean((mean_pm25 - mean_pm_pred)^2)),
      rsq = cor(mean_pm25, mean_pm_pred, use = "pairwise.complete")^2
    ),
    by = c("h3")
  ]

#' daily
d_temporal_cv$daily <-
  d[,
    .(.N, mean_pm25 = mean(pm25), mean_pm_pred = mean(pm_pred)),
    by = c("h3", "date")
  ][,
    .(
      .N,
      mae = median(abs(mean_pm25 - mean_pm_pred)),
      rmse = sqrt(mean((mean_pm25 - mean_pm_pred)^2)),
      rsq = cor(mean_pm25, mean_pm_pred, use = "pairwise.complete")^2
    ),
    by = c("h3")
  ]

d_temporal_cv <-
  tibble::enframe(d_temporal_cv, name = "time") %>%
  unnest(cols = c(value))

d_temporal_cv$time <-
    d_temporal_cv$time %>%
    factor(levels = c("all", "annual", "monthly", "weekly", "daily"))

d_temporal_cv %>%
  group_by(time) %>%
  summarise(
    median_N = median(N, na.rm = TRUE),
    median_mae = median(mae, na.rm = TRUE),
    median_rmse = median(rmse, na.rm = TRUE),
    median_rsq = median(rsq, na.rm = TRUE)
  ) %>%
  knitr::kable(digits = 2) %>%
  cat(file = "cv_output/cv_accuracy_by_time_resolution.md", sep = "\n")

#' boxplot these too

ggplot(d_temporal_cv, aes(time, mae)) +
    geom_boxplot(fill = "grey") +
    CB::theme_cb()
ggsave("cv_output/cv_boxplot_mae.pdf")

ggplot(d_temporal_cv, aes(time, rsq)) +
    geom_boxplot(fill = "grey") +
    CB::theme_cb()
ggsave("cv_output/cv_boxplot_rsq.pdf")

#' average CV by spatial region (h3 resolution level of 2)

sp_rs <- 2

d_temporal_cv$h3 <- purrr::map_chr(d_temporal_cv$h3, h3::h3_to_parent, res = sp_rs)

d_spatiotemporal_cv <-
    d_temporal_cv %>%
    group_by(time, h3) %>%
    summarise(
        N = median(N, na.rm = TRUE),
        mae = median(mae, na.rm = TRUE),
        rmse = median(rmse, na.rm = TRUE),
        rsq = median(rsq, na.rm = TRUE)
    )

## d_spatiotemporal_cv %>%
##   pivot_wider(names_from = time, values_from = c(N, mae, rmse, rsq)) %>%
##   knitr::kable(digits = 2)

d_spatiotemporal_cv %>%
    pivot_wider(names_from = time, values_from = c(N, mae, rmse, rsq)) %>%
    ggplot(aes(mae_daily, mae_all)) +
    geom_point() +
    CB::theme_cb()

d_spatiotemporal_cv %>%
    pivot_wider(names_from = time, values_from = c(N, mae, rmse, rsq)) %>%
    ggplot(aes(N_daily, mae_daily)) +
    geom_point() +
    CB::theme_cb()

#' create maps by h3 region

d_hex <- readRDS('us_h3_8_compact_hex_ids.rds') %>%
    purrr::map(h3::h3_to_children, res = sp_rs) %>%
    unlist() %>%
    unique()

d_sf <- sf::st_as_sf(h3::h3_to_geo_boundary_sf(d_hex))
d_sf$h3 <- d_hex

d_map <- left_join(d_sf, d_spatiotemporal_cv, by = "h3")

d_map %>%
    filter(!is.na(time)) %>%
    ggplot() +
    geom_sf(aes(fill = mae), size = 0) +
    coord_sf(crs = 5072) +
    facet_wrap(~time, ncol = 2) +
    CB::theme_map() +
    scale_fill_viridis_c() +
    theme(legend.position = c(0.76, 0.12),
          legend.direction = "horizontal",
          legend.title = element_text(size = 11, family = "sans"),
          legend.text = element_text(size = 11),
          legend.box = "hortizontal",
          legend.key.height = unit(4, "mm"),
          legend.key.width = unit(9, "mm"),
          strip.text.x = element_text(size = 11, face = "bold")) +
    labs(fill = expression(paste(MAE~(ug/m^3)))) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))

ggsave("cv_output/cv_maps_panel_2.pdf", width = 6, height = 6)

#' create these panel maps by year, etc ???
