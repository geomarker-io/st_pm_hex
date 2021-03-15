library(dplyr)

var_imp <-
  tibble::tribble(
    ~var_imp, ~variable,
    0.46406, "nearby_pm25",
    0.16445, "hpbl",
    0.13367, "doy",
    0.05800, "vwnd.10m",
    0.05331, "air.2m",
    0.03565, "x",
    0.02176, "vis",
    0.02113, "pres.sfc",
    0.01625, "y",
    0.00737, "nei_event",
    0.00629, "population_density",
    0.00492, "rhum.2m",
    0.00363, "uwnd.10m",
    0.00183, "prate",
    0.00153, "nei_dist",
    0.00132, "nei_nonpoint",
    0.00106, "s1100_dist",
    0.00068, "impervious",
    0.00052, "nei_point",
    0.00049, "green",
    0.00034, "dow",
    0.00030, "nonroad_urban",
    0.00028, "nonimpervious",
    0.00027, "aod",
    0.00026, "secondary_urban",
    0.00024, "primary_urban",
    0.00018, "nei_nonroad",
    0.00016, "nei_onroad",
    0.00008, "tertiary_urban",
    0.00000, "year",
    0.00000, "holiday",
    0.00000, "primary_rural",
    0.00000, "secondary_rural",
    0.00000, "tertiary_rural",
    0.00000, "thinned_urban",
    0.00000, "thinned_rural",
    0.00000, "nonroad_rural",
    0.00000, "energyprod_urban",
    0.00000, "energyprod_rural",
    0.00000, "fire_pm25",
    0.00000, "fire_area"
  )

library(ggplot2)

var_imp %>%
  mutate(variable = factor(variable, levels = rev(variable))) %>%
  ggplot(aes(var_imp, variable)) +
  ## xlim(c(0, 0.01))+
  geom_point()
