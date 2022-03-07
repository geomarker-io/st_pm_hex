cv_out_folder <- "./cv_output"

library(tidyr)
library(dplyr)
library(tibble)
library(magrittr)

dir.create(cv_out_folder, showWarnings = FALSE, recursive = TRUE)

d <-
  readRDS("grf/st_pm_hex_grf_preds.rds") %>%
  as_tibble() %>%
  rename(
    pred_inbag = pm_pred,
    pred_oob = pm_pred_oob,
    se_inbag = pm_pred_se,
    se_oob = pm_pred_oob_se
  ) %>%
  pivot_longer(
    cols = c(pred_inbag, pred_oob, se_inbag, se_oob),
    names_to = c(".value", "oob"),
    names_pattern = "(.+)_(.+)"
  )

d_pm <- qs::qread("h3data_aqs.qs")

d <- left_join(d, d_pm, by = c("date", "h3"))

d <- d %>%
  mutate(lci = pred - se * qnorm(0.025, lower.tail = FALSE),
         uci = pred + se * qnorm(0.025, lower.tail = FALSE),
         ci_length = uci - lci,
         ci_covered = pm25 < uci & pm25 > lci)

## should we restrict CV to positive PM2.5 measurements?

#' summarize measured PM2.5
d %>%
  filter(oob == "inbag") %>%
  summarize(
    n = n(),
    min_pm25 = min(pm25),
    p25_pm25 = quantile(pm25, 0.25),
    mean_pm25 = mean(pm25),
    median_pm25 = median(pm25),
    p75_pm25 = quantile(pm25, 0.75),
    max_pm25 = max(pm25),
    var_pm25 = var(pm25)
  ) %>%
  knitr::kable(digits = 2) %>%
  cat(file = glue::glue("{cv_out_folder}/measured_pm_summary.md"), sep = "\n")


#' daily prediction errors overall
d %>%
  group_by(oob) %>%
  summarize(
    mae = median(abs(pm25 - pred)),
    rmse = sqrt(mean((pm25 - pred)^2)),
    rsq = cor(pm25, pred, use = "pairwise.complete")^2,
    slope = lm(pred ~ pm25 - 1, data = .) %>% coefficients() %>% .["pm25"],
    ci_coverage = scales::percent(sum(ci_covered) / length(ci_covered))
  ) %>%
  knitr::kable(digits = 2) %>%
  cat(file = glue::glue("{cv_out_folder}/cv_accuracy_overall.md"), sep = "\n")

#' annual summaries of model performance, and PM2.5 measurements
d %>%
  filter(oob == "oob") %>%
  group_by(year) %>%
  summarize(
    n = n(),
    mean_measured_pm = mean(pm25),
    mae = median(abs(pm25 - pred)),
    rsq = cor(pm25, pred, use = "pairwise.complete")^2
  ) %>%
  knitr::kable(digits = 2) %>%
  cat(file = glue::glue("{cv_out_folder}/cv_accuracy_by_year.md"), sep = "\n")

#' describe lengths of confidence intervals
summary(d$ci_length)
summary(d$ci_length / d$pred)

#' plot overall obs versus oob predicted
library(ggplot2)
library(hexbin)
library(scales)

d %>%
  filter(oob == "oob") %>%
  ggplot(aes(pm25, pred)) +
  stat_bin_hex(binwidth = c(0.33, 0.33))+
  viridis::scale_fill_viridis(option = "C", name = "Count") +
  geom_abline(slope = 1, intercept = 0, lty = 2, alpha = 0.8, color = "darkgrey") +
  xlim(c(0, 75)) + ylim(c(0, 75)) +
  ## scale_x_log10(limits = c(1, 650)) + scale_y_log10(limits = c(1, 650)) +
  xlab(expression(Observed ~ paste(PM[2.5], " (", mu, "g/", m^{3}, ") "))) +
  ylab(expression(CV ~ Predicted ~ paste(PM[2.5], " (", mu, "g/", m^{3}, ") "))) +
  CB::theme_cb() +
  coord_fixed()

ggsave(glue::glue("{cv_out_folder}/oob_pred_versus_obs_plot.pdf"), width = 6, height = 6)

d %>%
  filter(oob == "oob") %>%
  ggplot(aes(pm25, pred)) +
  stat_bin_hex(binwidth = c(0.33, 0.33))+
  viridis::scale_fill_viridis(option = "C", name = "Count") +
  geom_abline(slope = 1, intercept = 0, lty = 2, alpha = 0.8, color = "darkgrey") +
  xlim(c(0, 15)) + ylim(c(0, 15)) +
  ## scale_x_log10(limits = c(1, 650)) + scale_y_log10(limits = c(1, 650)) +
  xlab(expression(Observed ~ paste(PM[2.5], " (", mu, "g/", m^{3}, ") "))) +
  ylab(expression(CV ~ Predicted ~ paste(PM[2.5], " (", mu, "g/", m^{3}, ") "))) +
  CB::theme_cb() +
  coord_fixed()

ggsave(glue::glue("{cv_out_folder}/oob_pred_versus_obs_plot_zoomed.pdf"), width = 6, height = 6)

ggplot(d, aes(pm25, pred)) +
  stat_bin_hex(binwidth = c(0.2, 0.2))+
  viridis::scale_fill_viridis(option = "C", name = "Count") +
  geom_abline(slope = 1, intercept = 0, lty = 2, alpha = 0.8, color = "darkgrey") +
  xlim(c(0, 100)) + ylim(c(0, 100)) +
  ## scale_x_log10(limits = c(1, 650)) + scale_y_log10(limits = c(1, 650)) +
  xlab(expression(Observed ~ paste(PM[2.5], " (", mu, "g/", m^{3}, ") "))) +
  ylab(expression(Predicted ~ paste(PM[2.5], " (", mu, "g/", m^{3}, ") "))) +
  CB::theme_cb() +
  facet_grid(oob~year) +
  coord_fixed()

ggsave(glue::glue("{cv_out_folder}/pred_versus_obs_plot_by_year.pdf"), width = 48, height = 6)

d %>%
  filter(oob == "oob") %>%
  sample_n(1000) %>%
  ggplot(aes(pred - pm25, ci_length)) +
  geom_point() +
  xlim(-10, 10)+ ylim(0, 30) +
  xlab(expression(Residual ~ paste(PM[2.5], " (", mu, "g/", m^{3}, ") "))) +
  ylab("Length of 95% CI") +
  CB::theme_cb()

ggsave(glue::glue("{cv_out_folder}/oob_resid_versus_ci_length_plot.pdf"), width = 12, height = 6)

#' for each site, generate predictions for daily pm2.5 and averages for week, month, year, and and entire time period

## should we restrict CV to stations with at least a certain number of temporal measurements?

library(data.table)

d <- as.data.table(d, key = c("h3", "year", "date"))
d$month <- lubridate::month(d$date)
d$week <- lubridate::week(d$date)

#' aggregate predictions, actuals, and standard errors for different time periods, for each h3 cell

# function to add CIs and summarize error by a given time frame
add_mean_CIs_and_summarize_error <- function(x) {
  x %>%
    mutate(lci = mean_pm_pred - mean_pm_se * qnorm(0.025, lower.tail = FALSE),
           uci = mean_pm_pred + mean_pm_se * qnorm(0.025, lower.tail = FALSE),
           ci_covered = mean_pm25 <= uci & mean_pm25 >= lci) %>%
    group_by(h3, oob) %>%
    summarize(
      n_time_units = n(),
      n_observations = sum(N),
      mae = median(abs(mean_pm25 - mean_pm_pred)),
      rmse = sqrt(mean((mean_pm25 - mean_pm_pred)^2)),
      rsq = cor(mean_pm25, mean_pm_pred, use = "pairwise.complete")^2,
      slope = lm(mean_pm_pred ~ mean_pm25 - 1, data = .) %>% coefficients() %>% .["mean_pm25"],
      ci_coverage = scales::percent(sum(ci_covered) / length(ci_covered))
    )
}

d_temporal_cv <- list()

# alltime
d_temporal_cv$all <-
  d[,
    .(.N,
      mean_pm25 = mean(pm25),
      mean_pm_pred = mean(pred),
      mean_pm_se = sqrt(sum(se^2))),
    by = c("h3", "oob")
    ] %>%
  add_mean_CIs_and_summarize_error()

# annual
d_temporal_cv$annual <-
  d[,
    .(.N,
      mean_pm25 = mean(pm25),
      mean_pm_pred = mean(pred),
      mean_pm_se = sqrt(sum(se^2))),
    by = c("h3", "oob", "year")
    ] %>%
  add_mean_CIs_and_summarize_error()

# monthly
d_temporal_cv$monthly <-
  d[,
    .(.N,
      mean_pm25 = mean(pm25),
      mean_pm_pred = mean(pred),
      mean_pm_se = sqrt(sum(se^2))),
    by = c("h3", "oob", "year", "month")
    ] %>%
  add_mean_CIs_and_summarize_error()

# weekly
d_temporal_cv$weekly <-
  d[,
    .(.N,
      mean_pm25 = mean(pm25),
      mean_pm_pred = mean(pred),
      mean_pm_se = sqrt(sum(se^2))),
    by = c("h3", "oob", "year", "week")
    ] %>%
  add_mean_CIs_and_summarize_error()

#' daily
d_temporal_cv$daily <-
  d[,
    .(.N,
      mean_pm25 = mean(pm25),
      mean_pm_pred = mean(pred),
      mean_pm_se = sqrt(sum(se^2))),
    by = c("h3", "oob", "date")
    ] %>%
  add_mean_CIs_and_summarize_error()

d_temporal_cv <-
  tibble::enframe(d_temporal_cv, name = "time") %>%
  unnest(cols = c(value))

d_temporal_cv$time <-
    d_temporal_cv$time %>%
    factor(levels = c("all", "annual", "monthly", "weekly", "daily"))

saveRDS(d_temporal_cv, glue::glue("{cv_out_folder}/d_temporal_cv.rds"))

d_temporal_cv %>%
  group_by(time, oob) %>%
  summarize(
    median_n_time_units = median(n_time_units, na.rm = TRUE),
    median_n_observations = median(n_observations, na.rm = TRUE),
    median_mae = median(mae, na.rm = TRUE),
    median_rmse = median(rmse, na.rm = TRUE),
    median_rsq = median(rsq, na.rm = TRUE),
    median_slope = median(slope, na.rm = TRUE),
    median_ci_coverage = median(as.numeric(sub("%", "", ci_coverage)), na.rm = TRUE),
    .groups = "keep"
  ) %>%
  knitr::kable(digits = 2) %>%
  cat(file = glue::glue("{cv_out_folder}/cv_accuracy_by_time_resolution.md"), sep = "\n")

#' boxplot these too

d_temporal_cv %>%
  filter(oob == "oob") %>%
  ggplot(aes(time, mae)) +
  geom_boxplot() +
  CB::theme_cb() +
  ylab(expression(paste(CV~MAE~(ug/m^3)))) +
  xlab("Temporal Aggregation Window")

ggsave(glue::glue("{cv_out_folder}/cv_boxplot_mae.pdf"))

d_temporal_cv %>%
  filter(oob == "oob") %>%
  ggplot(aes(time, rsq)) +
  geom_boxplot() +
  CB::theme_cb() +
  ylab(expression(paste(CV~R^2))) +
  xlab("Temporal Aggregation Window")

ggsave(glue::glue("{cv_out_folder}/cv_boxplot_rsq.pdf"))


#' average CV by spatial region (h3 resolution level of 3)

sp_rs <- 2

d_temporal_cv$h3 <- purrr::map_chr(d_temporal_cv$h3, h3::h3_to_parent, res = sp_rs)

d_spatiotemporal_cv <-
  d_temporal_cv %>%
  group_by(time, h3, oob) %>%
  summarise(
    n = median(n_time_units, na.rm = TRUE),
    mae = median(mae, na.rm = TRUE),
    rmse = median(rmse, na.rm = TRUE),
    rsq = median(rsq, na.rm = TRUE),
    slope = median(slope, na.rm = TRUE),
    ci_coverage = median(as.numeric(sub("%", "", ci_coverage)), na.rm = TRUE),
  )

## d_spatiotemporal_cv %>%
##   pivot_wider(names_from = time, values_from = c(N, mae, rmse, rsq)) %>%
##   knitr::kable(digits = 2)

## d_spatiotemporal_cv %>%
##     pivot_wider(names_from = time, values_from = c(n, mae, rmse, rsq, slope)) %>%
##     ggplot(aes(mae_daily, mae_all)) +
##     geom_point() +
##     CB::theme_cb()

## d_spatiotemporal_cv %>%
##     pivot_wider(names_from = time, values_from = c(n, mae, rmse, rsq, slope)) %>%
##     ggplot(aes(n_daily, mae_daily)) +
##     geom_point() +
##     CB::theme_cb()

#' create maps by h3 region

d_hex <- readRDS('us_h3_8_compact_hex_ids.rds') %>%
  purrr::map(h3::h3_to_children, res = 8) %>%
  unlist() %>%
  purrr::map(h3::h3_to_parent, res = sp_rs) %>%
  unlist() %>%
  unique()

d_sf <- sf::st_as_sf(h3::h3_to_geo_boundary_sf(d_hex))
d_sf$h3 <- d_hex

d_map <- left_join(d_sf, d_spatiotemporal_cv, by = "h3")

saveRDS(d_map, glue::glue("{cv_out_folder}/d_map.rds"))

library(ggplot2)

d_map %>%
  filter(!is.na(time)) %>%
  ggplot() +
  geom_sf(aes(fill = mae), size = 0) +
  coord_sf(crs = 5072) +
  facet_grid(oob ~ time) +
  CB::theme_map() +
  scale_fill_viridis_c() +
  theme(legend.position = c(0.76, -0.3),
        legend.direction = "horizontal",
        legend.title = element_text(size = 11, family = "sans"),
        legend.text = element_text(size = 11),
        legend.box = "hortizontal",
        legend.key.height = unit(4, "mm"),
        legend.key.width = unit(9, "mm"),
        strip.text.x = element_text(size = 11, face = "bold", vjust = 1),
        strip.text.y = element_text(size = 11, face = "bold")) +
  labs(fill = expression(paste(MAE~(ug/m^3)))) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))

ggsave(glue::glue("{cv_out_folder}/cv_and_fitted_maps_panel_2.pdf"), width = 14, height = 6)

d_map %>%
  filter(!is.na(time)) %>%
  filter(oob == "oob") %>%
  ggplot() +
  geom_sf(aes(fill = mae), size = 0) +
  coord_sf(crs = 5072) +
  facet_wrap(~ time) +
  CB::theme_map() +
  scale_fill_viridis_c() +
  theme(legend.position = c(0.85, 0.2),
        legend.direction = "horizontal",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 9),
        legend.box = "hortizontal",
        legend.key.height = unit(4, "mm"),
        legend.key.width = unit(9, "mm"),
        strip.text.x = element_text(size = 9, face = "bold", vjust = 0.9)) +
  labs(fill = expression(paste(MAE~(ug/m^3)))) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))

ggsave(glue::glue("{cv_out_folder}/cv_maps_panel_2.pdf"), width = 6, height = 4)
